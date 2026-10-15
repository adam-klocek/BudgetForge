# Local single-user SQLite storage. No financial data is shipped with the app.
connect_db <- function(path = file.path("data", "flowwise.sqlite")) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

init_db <- function(con) {
  sql <- c(
    "CREATE TABLE IF NOT EXISTS accounts (account_key TEXT PRIMARY KEY, bank TEXT NOT NULL, display_name TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS categories (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, is_fixed INTEGER NOT NULL DEFAULT 0, default_budget REAL NOT NULL DEFAULT 0)",
    "CREATE TABLE IF NOT EXISTS category_budgets (month TEXT NOT NULL, category_id INTEGER NOT NULL, amount REAL NOT NULL DEFAULT 0, PRIMARY KEY(month, category_id))",
    "CREATE TABLE IF NOT EXISTS income_sources (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, monthly_amount REAL NOT NULL DEFAULT 0, pay_day INTEGER NOT NULL DEFAULT 25, active INTEGER NOT NULL DEFAULT 1)",
    "CREATE TABLE IF NOT EXISTS debts (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, balance REAL NOT NULL DEFAULT 0, apr REAL NOT NULL DEFAULT 0, min_payment REAL NOT NULL DEFAULT 0, due_day INTEGER NOT NULL DEFAULT 20, active INTEGER NOT NULL DEFAULT 1)",
    "CREATE TABLE IF NOT EXISTS rules (id INTEGER PRIMARY KEY AUTOINCREMENT, pattern TEXT NOT NULL, flow_type TEXT NOT NULL, category_id INTEGER, income_source_id INTEGER, debt_id INTEGER, priority INTEGER NOT NULL DEFAULT 100)",
    "CREATE TABLE IF NOT EXISTS transactions (id INTEGER PRIMARY KEY AUTOINCREMENT, fingerprint TEXT NOT NULL UNIQUE, txn_date TEXT NOT NULL, bank TEXT NOT NULL, account_key TEXT NOT NULL, counterparty_account TEXT, payee TEXT, description TEXT NOT NULL, amount REAL NOT NULL, currency TEXT NOT NULL DEFAULT 'CZK', flow_type TEXT NOT NULL, category_id INTEGER, income_source_id INTEGER, debt_id INTEGER, is_manual INTEGER NOT NULL DEFAULT 0, matched_rule TEXT)",
    "CREATE TABLE IF NOT EXISTS settings (id INTEGER PRIMARY KEY CHECK (id = 1), savings_target REAL NOT NULL DEFAULT 0, safety_buffer REAL NOT NULL DEFAULT 0, available_cash REAL)",
    "CREATE TABLE IF NOT EXISTS debt_extra_payments (id INTEGER PRIMARY KEY AUTOINCREMENT, debt_id INTEGER NOT NULL REFERENCES debts(id) ON DELETE CASCADE, month TEXT NOT NULL, amount REAL NOT NULL CHECK(amount > 0), note TEXT NOT NULL DEFAULT '', completed INTEGER NOT NULL DEFAULT 0)"
  )
  for (query in sql) DBI::dbExecute(con, query)
  DBI::dbExecute(con, "INSERT OR IGNORE INTO settings(id,savings_target,safety_buffer) VALUES(1,0,0)")
  # Keep starter categories intentionally small. Existing custom/user categories
  # remain untouched; on first launch only these seven appear.
  default_categories <- c("Travel", "Household", "Food home", "Food outside",
                          "Hobbies", "Gifts", "Withdrawals")
  for (nm in default_categories) DBI::dbExecute(con,
    "INSERT OR IGNORE INTO categories(name,is_fixed,default_budget) VALUES(?,0,0)",params=list(nm))
  migrate_legacy_categories(con)
  invisible(TRUE)
}

# Merge only legacy starter categories with direct equivalents. Never discard
# historical transactions, overrides, or keyword rules.
migrate_legacy_categories <- function(con) {
  aliases <- c("Potraviny"="Food home", "Drogerie a domácnost"="Household",
    "Restaurace a káva"="Food outside", "Šaliny a MHD"="Travel",
    "Doprava vlak/bus"="Travel", "Auto"="Travel", "Cestování"="Travel",
    "Zábava a kultura"="Hobbies", "Vzdělávání"="Hobbies", "Dárky"="Gifts")
  for (old_name in names(aliases)) {
    old <- DBI::dbGetQuery(con,"SELECT id,default_budget FROM categories WHERE name=?",params=list(old_name))
    target <- DBI::dbGetQuery(con,"SELECT id FROM categories WHERE name=?",params=list(unname(aliases[[old_name]])))
    if(!nrow(old) || !nrow(target) || old$id[1]==target$id[1]) next
    # Keep historical monthly budgets, adding allocations if target has no override.
    DBI::dbExecute(con,paste("INSERT OR IGNORE INTO category_budgets(month,category_id,amount)",
      "SELECT month,?,amount FROM category_budgets WHERE category_id=?",
      "ON CONFLICT(month,category_id) DO UPDATE SET amount=category_budgets.amount+excluded.amount"),params=list(target$id[1],old$id[1]))
    DBI::dbExecute(con,"UPDATE transactions SET category_id=? WHERE category_id=?",params=list(target$id[1],old$id[1]))
    DBI::dbExecute(con,"UPDATE rules SET category_id=? WHERE category_id=?",params=list(target$id[1],old$id[1]))
    DBI::dbExecute(con,"DELETE FROM category_budgets WHERE category_id=?",params=list(old$id[1]))
    # Never silently combine unrelated default budget amounts; retain higher cap.
    DBI::dbExecute(con,paste("UPDATE categories SET default_budget=default_budget+? WHERE id=?"),
                   params=list(old$default_budget[1],target$id[1]))
    DBI::dbExecute(con,"DELETE FROM categories WHERE id=?",params=list(old$id[1]))
  }
  legacy <- c("Bydlení a nájem","Energie byt+internet","Energie dům",
     "Poplatky byt / SVJ","Pojištění","Zdraví a léky","Terapie",
     "Oblečení a obuv","Děti","Předplatné","Daně","Vratné výdaje",
     "Údržba a opravy bytu","Údržba a opravy domu","Ostatní výdaje")
  for(nm in legacy) {
    # Delete an obsolete seed only if it has never been configured or used.
    DBI::dbExecute(con,paste("DELETE FROM categories WHERE name=? AND default_budget=0",
      "AND NOT EXISTS(SELECT 1 FROM transactions WHERE category_id=categories.id)",
      "AND NOT EXISTS(SELECT 1 FROM category_budgets WHERE category_id=categories.id)",
      "AND NOT EXISTS(SELECT 1 FROM rules WHERE category_id=categories.id)"),params=list(nm))
  }
  invisible(TRUE)
}

get_categories <- function(con) DBI::dbGetQuery(con, "SELECT * FROM categories ORDER BY is_fixed DESC, name")
get_sources <- function(con) DBI::dbGetQuery(con, "SELECT * FROM income_sources ORDER BY name")
get_debts <- function(con) DBI::dbGetQuery(con, "SELECT * FROM debts ORDER BY name")
get_transactions <- function(con) DBI::dbGetQuery(con, "SELECT * FROM transactions ORDER BY txn_date DESC, id DESC")
get_rules <- function(con) DBI::dbGetQuery(con, "SELECT * FROM rules ORDER BY priority DESC, id DESC")
get_settings <- function(con) DBI::dbGetQuery(con, "SELECT * FROM settings WHERE id=1")[1, , drop = FALSE]

upsert_category <- function(con, name, fixed, budget, id = NA_integer_) {
  stopifnot(nzchar(trimws(name)), is.finite(budget), budget >= 0)
  if (is.na(id)) {
    DBI::dbExecute(con, "INSERT INTO categories(name,is_fixed,default_budget) VALUES(?,?,?) ON CONFLICT(name) DO UPDATE SET is_fixed=excluded.is_fixed, default_budget=excluded.default_budget", params = list(trimws(name), as.integer(fixed), budget))
  } else {
    DBI::dbExecute(con, "UPDATE categories SET name=?, is_fixed=?, default_budget=? WHERE id=?", params = list(trimws(name), as.integer(fixed), budget, as.integer(id)))
  }
}
upsert_source <- function(con, name, monthly, day, active, id=NA_integer_,
                          kind='fixed', min_amount=monthly, max_amount=monthly,
                          start_month='1900-01', end_month=NA_character_, include_budget=TRUE) {
  if (!nzchar(trimws(name)) || length(monthly)!=1L || !is.finite(monthly) || monthly<0 ||
      length(day)!=1L || !is.finite(day) || day<1 || day>31)
    stop('Enter a source name, nonnegative expected income and a pay day between 1 and 31.')
  if (!kind %in% c('fixed','variable')) stop('Income type must be fixed or variable.')
  if (kind=='fixed') min_amount <- max_amount <- monthly
  if (any(!is.finite(c(min_amount,max_amount))) || min_amount<0 || min_amount>monthly || monthly>max_amount)
    stop('Income must satisfy 0 ≤ minimum ≤ expected ≤ maximum.')
  if (!month_valid(start_month) || (!is.na(end_month) && (!month_valid(end_month) || end_month<start_month)))
    stop('Start / confirmed-through dates are invalid.')
  values <- list(trimws(name),monthly,as.integer(day),as.integer(active),kind,min_amount,max_amount,
                 start_month,end_month,as.integer(include_budget))
  if (is.na(id)) {
    DBI::dbExecute(con,paste('INSERT INTO income_sources',
      '(name,monthly_amount,pay_day,active,kind,min_amount,max_amount,start_month,end_month,include_budget)',
      'VALUES(?,?,?,?,?,?,?,?,?,?) ON CONFLICT(name) DO UPDATE SET',
      'monthly_amount=excluded.monthly_amount,pay_day=excluded.pay_day,active=excluded.active,',
      'kind=excluded.kind,min_amount=excluded.min_amount,max_amount=excluded.max_amount,',
      'start_month=excluded.start_month,end_month=excluded.end_month,include_budget=excluded.include_budget'),params=values)
  } else {
    n <- DBI::dbExecute(con,paste('UPDATE income_sources SET name=?,monthly_amount=?,pay_day=?,',
      'active=?,kind=?,min_amount=?,max_amount=?,start_month=?,end_month=?,include_budget=? WHERE id=?'),
      params=c(values,list(as.integer(id))))
    if(!n) stop('Income source not found. Choose New source instead.')
  }
  invisible(TRUE)
}

upsert_debt <- function(con, name, balance, apr, payment, day, active, id = NA_integer_) {
  stopifnot(nzchar(trimws(name)), is.finite(balance), balance >= 0, is.finite(apr), apr >= 0, is.finite(payment), payment >= 0, day >= 1, day <= 31)
  if (is.na(id)) {
    DBI::dbExecute(con, "INSERT INTO debts(name,balance,apr,min_payment,due_day,active) VALUES(?,?,?,?,?,?) ON CONFLICT(name) DO UPDATE SET balance=excluded.balance,apr=excluded.apr,min_payment=excluded.min_payment,due_day=excluded.due_day,active=excluded.active", params = list(trimws(name), balance, apr, payment, as.integer(day), as.integer(active)))
  } else {
    DBI::dbExecute(con, "UPDATE debts SET name=?,balance=?,apr=?,min_payment=?,due_day=?,active=? WHERE id=?", params = list(trimws(name), balance, apr, payment, as.integer(day), as.integer(active), as.integer(id)))
  }
}

ensure_category <- function(con, name) {
  id <- DBI::dbGetQuery(con, "SELECT id FROM categories WHERE name=?", params=list(name))$id
  if (!length(id)) {
    upsert_category(con, name, FALSE, 0)
    id <- DBI::dbGetQuery(con, "SELECT id FROM categories WHERE name=?", params=list(name))$id
  }
  as.integer(id[1])
}
ensure_source <- function(con, name, amount=0) {
  id <- DBI::dbGetQuery(con,"SELECT id FROM income_sources WHERE name=?",params=list(name))$id
  if (!length(id)) {
    upsert_source(con,name,amount,25,TRUE)
    id <- DBI::dbGetQuery(con,"SELECT id FROM income_sources WHERE name=?",params=list(name))$id
  }
  as.integer(id[1])
}
ensure_debt <- function(con, name, payment=0, apr=0) {
  id <- DBI::dbGetQuery(con,"SELECT id FROM debts WHERE name=?",params=list(name))$id
  if (!length(id)) {
    upsert_debt(con,name,0,apr,payment,20,TRUE)
    id <- DBI::dbGetQuery(con,"SELECT id FROM debts WHERE name=?",params=list(name))$id
  }
  as.integer(id[1])
}

# Planned lump-sum payments are forecasts, NOT transactions or actual reductions
# of the outstanding balance. Each record is assigned to one specific debt.
get_debt_extras <- function(con) {
  DBI::dbGetQuery(con, paste(
    "SELECT e.id, e.debt_id, d.name AS debt_name, e.month, e.amount, e.note, e.completed",
    "FROM debt_extra_payments e JOIN debts d ON d.id=e.debt_id",
    "ORDER BY e.month, d.name, e.id"))
}

upsert_debt_extra <- function(con, debt_id, month, amount, note="", id=NA_integer_) {
  valid_month <- length(month)==1L && !is.na(month) &&
    grepl("^[0-9]{4}-(0[1-9]|1[0-2])$",month)
  if (!valid_month || length(amount)!=1L || !is.finite(amount) || amount<=0)
    stop("Choose a valid month and a positive extra repayment.")
  ids <- DBI::dbGetQuery(con, "SELECT id FROM debts WHERE id=?", params=list(as.integer(debt_id)))$id
  if (!length(ids)) stop("The selected debt no longer exists.")
  note <- trimws(as.character(if(is.null(note) || length(note)==0L || is.na(note[1])) "" else note[1]))
  if (is.na(id)) {
    DBI::dbExecute(con,
      "INSERT INTO debt_extra_payments(debt_id,month,amount,note) VALUES(?,?,?,?)",
      params=list(as.integer(debt_id),month,amount,note))
  } else {
    updated <- DBI::dbExecute(con,
      "UPDATE debt_extra_payments SET debt_id=?,month=?,amount=?,note=? WHERE id=?",
      params=list(as.integer(debt_id),month,amount,note,as.integer(id)))
    if (!updated) stop("That extra repayment no longer exists.")
  }
  invisible(TRUE)
}

delete_debt_extra <- function(con, id) {
  DBI::dbExecute(con,"DELETE FROM debt_extra_payments WHERE id=?",params=list(as.integer(id)))
}

set_debt_extra_completed <- function(con, id, completed=TRUE) {
  DBI::dbExecute(con,"UPDATE debt_extra_payments SET completed=? WHERE id=?",
                 params=list(as.integer(completed),as.integer(id)))
}
