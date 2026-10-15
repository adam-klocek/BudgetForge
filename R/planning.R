# Sinking funds and planned irregular expenses. No private data is bundled.
init_planning_db <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS sinking_funds (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, annual_cost REAL NOT NULL DEFAULT 0, cycle_months INTEGER NOT NULL DEFAULT 12, saved_balance REAL NOT NULL DEFAULT 0, active INTEGER NOT NULL DEFAULT 1)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS planned_events (id INTEGER PRIMARY KEY AUTOINCREMENT, month TEXT NOT NULL, name TEXT NOT NULL, kind TEXT NOT NULL CHECK (kind IN ('expense','income')), amount REAL NOT NULL, completed INTEGER NOT NULL DEFAULT 0)")
}
get_funds <- function(con) DBI::dbGetQuery(con, "SELECT * FROM sinking_funds ORDER BY active DESC, name")
get_events <- function(con) DBI::dbGetQuery(con, "SELECT * FROM planned_events ORDER BY month, name")
get_month_events <- function(con, month) DBI::dbGetQuery(con, "SELECT * FROM planned_events WHERE month=? ORDER BY name",params=list(month))
upsert_fund <- function(con, name, amount, cycle, saved, active, id=NA_integer_) {
  stopifnot(nzchar(trimws(name)), is.finite(amount), amount>=0, is.finite(saved), saved>=0, is.finite(cycle), cycle>=1)
  if (is.na(id)) {
    DBI::dbExecute(con,"INSERT INTO sinking_funds(name, annual_cost, cycle_months, saved_balance, active) VALUES(?,?,?,?,?) ON CONFLICT(name) DO UPDATE SET annual_cost=excluded.annual_cost,cycle_months=excluded.cycle_months,saved_balance=excluded.saved_balance,active=excluded.active",params=list(trimws(name),amount,as.integer(cycle),saved,as.integer(active)))
  } else {
    DBI::dbExecute(con,"UPDATE sinking_funds SET name=?,annual_cost=?,cycle_months=?,saved_balance=?,active=? WHERE id=?",params=list(trimws(name),amount,as.integer(cycle),saved,as.integer(active),as.integer(id)))
  }
}
add_event <- function(con, month, name, kind, amount) {
  stopifnot(grepl("^[0-9]{4}-(0[1-9]|1[0-2])$",month),nzchar(trimws(name)),kind %in% c("income","expense"),is.finite(amount),amount>0)
  DBI::dbExecute(con,"INSERT INTO planned_events(month,name,kind,amount) VALUES(?,?,?,?)",params=list(month,trimws(name),kind,amount))
}
fund_monthly_contribution <- function(funds) {
  if (!nrow(funds)) return(0)
  sum(ifelse(funds$active==1 & funds$saved_balance < funds$annual_cost, funds$annual_cost/pmax(1,funds$cycle_months),0),na.rm=TRUE)
}
# Projection across the user-selected calendar. Existing entry point preserved for older tests.
forecast_months <- function(con, start_month, end_month, debt_method='minimums') {
  months <- month_sequence(start_month,end_month)
  all_events <- get_events(con)
  cats <- get_categories(con)
  settings <- get_settings(con)
  funds <- get_funds(con)
  goals <- get_goals(con)
  income <- income_projection(con,start_month,end_month)$monthly
  # Debt balances represent NOW, not the first arbitrary calendar month.
  # Align repayments to the real present so a future-start calendar does not
  # restart outstanding debts later than they should be paid.
  debt_start <- format(Sys.Date(),'%Y-%m')
  debt_projection <- simulate_payoff(get_debts(con),method=debt_method,
                                     lump_sums=get_debt_extras(con),start_month=debt_start)
  debt_months <- format(seq(as.Date(paste0(debt_start,'-01')),by='month',length.out=600L),'%Y-%m')
  output <- lapply(seq_along(months),function(month_index) {
    m <- months[month_index]
    ov <- DBI::dbGetQuery(con,'SELECT category_id,amount FROM category_budgets WHERE month=?',params=list(m))
    cat_budget <- cats$default_budget
    idx <- match(cats$id,ov$category_id)
    ok <- !is.na(idx)
    cat_budget[ok] <- ov$amount[idx[ok]]
    expense <- sum(cat_budget)
    debt_index <- match(m,debt_months)
    payment <- if(nrow(debt_projection$history) && !is.na(debt_index))
      debt_projection$history[debt_projection$history$month==debt_index,,drop=FALSE] else data.frame()
    debt <- if(nrow(payment))payment$payment[1]-payment$extra_paid[1] else 0
    extra <- if(nrow(payment))payment$extra_paid[1] else 0
    events <- all_events[all_events$month==m & all_events$kind=='expense',,drop=FALSE]
    unexpected_costs <- sum(events$amount)
    funds_due <- fund_monthly_contribution(funds)
    savings <- settings$savings_target
    goal_savings <- goal_monthly_allocation(goals,m)
    i <- income[month_index,,drop=FALSE]
    # Receivables are not earned income. Unpaid return dates represent an
    # optimistic cash-flow scenario only; never count as guaranteed/spendable.
    possible_return <- receivable_due_by_month(con,m)
    costs <- expense+debt+extra+unexpected_costs+funds_due+savings+goal_savings
    data.frame(month=m,date=i$date,income=i$expected,one_off_income=0,possible_loan_return=possible_return,
      income_guaranteed=i$guaranteed,income_low=i$low,income_high=i$high,
      income_budget=i$budget,category_budgets=expense,debt_minimums=debt,
      debt_extras=extra,one_off_expenses=unexpected_costs,
      sinking_contributions=funds_due,savings=savings,goal_savings=goal_savings,
      planned_leftover=i$budget-costs,
      leftover_guaranteed=i$guaranteed-costs,
      leftover_low=i$low-costs,leftover_expected=i$expected-costs+possible_return,
      leftover_high=i$high-costs+possible_return,stringsAsFactors=FALSE)
  })
  do.call(rbind,output)
}
forecast_12_months <- function(con,start_month,debt_method='minimums') {
  end_month <- format(seq(as.Date(paste0(start_month,'-01')),by='month',length.out=12)[12],'%Y-%m')
  forecast_months(con,start_month,end_month,debt_method)
}
