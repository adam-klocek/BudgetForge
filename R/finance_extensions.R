# BudgetForge v5: currency display, accounts, savings goals, financial health.
# All planning amounts are stored in CZK; switching display currency never
# rewrites historical balances. Imported non-CZK transactions are rejected by
# the v5 import guard until a per-transaction exchange-rate ledger is available.

init_budgetforge_db <- function(con) {
  settings_columns <- DBI::dbGetQuery(con, 'PRAGMA table_info(settings)')$name
  additions <- c(display_currency="TEXT NOT NULL DEFAULT 'CZK'",
                 eur_czk_rate="REAL NOT NULL DEFAULT 24",
                 other_assets="REAL NOT NULL DEFAULT 0")
  for(nm in names(additions)) if(!nm %in% settings_columns)
    DBI::dbExecute(con,paste('ALTER TABLE settings ADD COLUMN',nm,additions[[nm]]))
  DBI::dbExecute(con,"UPDATE settings SET display_currency='CZK' WHERE display_currency NOT IN ('CZK','EUR') OR display_currency IS NULL")
  DBI::dbExecute(con,"UPDATE settings SET eur_czk_rate=24 WHERE eur_czk_rate IS NULL OR eur_czk_rate<=0")
  fields <- DBI::dbGetQuery(con,'PRAGMA table_info(accounts)')$name
  account_additions <- c(is_owned='INTEGER NOT NULL DEFAULT 1',
    balance_czk='REAL', balance_updated='TEXT')
  for(nm in names(account_additions)) if(!nm %in% fields)
    DBI::dbExecute(con,paste('ALTER TABLE accounts ADD COLUMN',nm,account_additions[[nm]]))
  DBI::dbExecute(con,paste(
    'CREATE TABLE IF NOT EXISTS savings_goals (',
    'id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE,',
    'target_amount REAL NOT NULL DEFAULT 0 CHECK(target_amount>=0),',
    'saved_balance REAL NOT NULL DEFAULT 0 CHECK(saved_balance>=0),',
    'monthly_allocation REAL NOT NULL DEFAULT 0 CHECK(monthly_allocation>=0),',
    'target_month TEXT, emergency_available INTEGER NOT NULL DEFAULT 0,',
    'active INTEGER NOT NULL DEFAULT 1)'))
  invisible(TRUE)
}

get_accounts <- function(con) DBI::dbGetQuery(con,
  'SELECT * FROM accounts ORDER BY is_owned DESC, display_name')
normalize_account_id <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ''
  toupper(gsub('[[:space:]]','',x))
}
upsert_account <- function(con, account_key, name, bank, owned=TRUE,
                           balance_czk=NA_real_) {
  account_key <- normalize_account_id(account_key)
  name <- trimws(as.character(name))
  bank <- trimws(as.character(bank))
  if(!nzchar(account_key) || !nzchar(name)) stop('Provide a bank account identifier and display name.')
  if(!is.na(balance_czk) && !is.finite(balance_czk))stop('Invalid balance')
  DBI::dbExecute(con,paste('INSERT INTO accounts(account_key,bank,display_name,is_owned,balance_czk,balance_updated)',
    'VALUES(?,?,?,?,?,?) ON CONFLICT(account_key) DO UPDATE SET',
    'bank=excluded.bank,display_name=excluded.display_name,is_owned=excluded.is_owned,',
    'balance_czk=excluded.balance_czk,balance_updated=excluded.balance_updated'),
    params=list(account_key,bank,name,as.integer(owned),balance_czk,
      if(is.na(balance_czk))NA_character_ else as.character(Sys.Date())))
  invisible(TRUE)
}

get_goals <- function(con) {
  if(!DBI::dbExistsTable(con,'savings_goals'))return(data.frame(
    id=integer(),name=character(),target_amount=numeric(),saved_balance=numeric(),
    monthly_allocation=numeric(),target_month=character(),emergency_available=integer(),active=integer()))
  DBI::dbGetQuery(con,'SELECT * FROM savings_goals ORDER BY active DESC, name')
}
upsert_goal <- function(con,name,target,saved,monthly,target_month=NA_character_,
                        emergency=FALSE,active=TRUE,id=NA_integer_) {
  if(!nzchar(trimws(name)) || any(!is.finite(c(target,saved,monthly))) ||
     any(c(target,saved,monthly)<0)) stop('Goal needs a name and nonnegative amounts.')
  if(!is.na(target_month) && !month_valid(target_month))stop('Invalid target month.')
  values <- list(trimws(name),target,saved,monthly,target_month,
                 as.integer(emergency),as.integer(active))
  if(is.na(id)) {
    DBI::dbExecute(con,paste('INSERT INTO savings_goals',
      '(name,target_amount,saved_balance,monthly_allocation,target_month,emergency_available,active)',
      'VALUES(?,?,?,?,?,?,?) ON CONFLICT(name) DO UPDATE SET',
      'target_amount=excluded.target_amount,saved_balance=excluded.saved_balance,',
      'monthly_allocation=excluded.monthly_allocation,target_month=excluded.target_month,',
      'emergency_available=excluded.emergency_available,active=excluded.active'),params=values)
  } else {
    count<-DBI::dbExecute(con,paste('UPDATE savings_goals SET name=?,target_amount=?,',
      'saved_balance=?,monthly_allocation=?,target_month=?,emergency_available=?,active=? WHERE id=?'),
      params=c(values,list(as.integer(id))))
    if(!count)stop('Goal no longer exists.')
  }
  invisible(TRUE)
}
goal_monthly_allocation <- function(goals,month=NULL) {
  if(!nrow(goals))return(0)
  active <- goals$active==1 & goals$saved_balance < goals$target_amount
  if(is.null(month))return(sum(ifelse(active,pmin(goals$monthly_allocation,
    pmax(0,goals$target_amount-goals$saved_balance)),0),na.rm=TRUE))
  active <- active & (is.na(goals$target_month) | month<=goals$target_month)
  # Goal snapshots are current. Simulate funding month by month into the future
  # so monthly allocations stop when the target would have been reached.
  today <- format(Sys.Date(),'%Y-%m')
  elapsed <- max(0,(as.integer(substr(month,1,4))-as.integer(substr(today,1,4)))*12L+
    as.integer(substr(month,6,7))-as.integer(substr(today,6,7)))
  remaining <- pmax(0,goals$target_amount-goals$saved_balance-
    goals$monthly_allocation*elapsed)
  sum(ifelse(active,pmin(goals$monthly_allocation,remaining),0),na.rm=TRUE)
}

# Every editable financial amount is stored in CZK; outputs are converted on read.
# FX is a USER-SET planning rate, not a live market rate.
view_amount <- function(x, settings) {
  if(identical(as.character(settings$display_currency[1]),'EUR'))
    as.numeric(x)/as.numeric(settings$eur_czk_rate[1]) else as.numeric(x)
}
stored_amount <- function(x,settings) {
  if(identical(as.character(settings$display_currency[1]),'EUR'))
    as.numeric(x)*as.numeric(settings$eur_czk_rate[1]) else as.numeric(x)
}
format_money <- function(x,settings) {
  if(!length(x) || is.na(x[1]) || !is.finite(x[1]))return('—')
  euro <- identical(as.character(settings$display_currency[1]),'EUR')
  v <- view_amount(x[1],settings)
  paste0(if(euro)'€' else '',format(round(v,if(euro)2 else 0),big.mark=' ',
    decimal.mark=if(euro)',' else ',',scientific=FALSE,trim=TRUE,nsmall=if(euro)2 else 0),
    if(euro)'' else ' Kč')
}
currency_code <- function(settings) as.character(settings$display_currency[1])

# Cash and net worth snapshots: savings goal and sinking-fund amounts are
# SUBSETS of accounts, not extra assets. Repayments/receivables are assets,
# debts are liabilities. Unspecified balances never become made-up zeros.
financial_snapshot <- function(con) {
  acc<-get_accounts(con)
  tracked<-acc[acc$is_owned==1 & !is.na(acc$balance_czk),,drop=FALSE]
  s<-get_settings(con)
  cash <- if(nrow(tracked)) sum(tracked$balance_czk) else s$available_cash[1]
  has_cash <- is.finite(cash)
  loans <- get_receivables(con)
  debts <- get_debts(con)
  receivables <- sum(loans$outstanding)
  liability <- sum(debts$balance[debts$active==1])
  other <- s$other_assets[1]
  list(cash=cash,has_cash=has_cash,accounts=nrow(tracked),
       debts=liability,receivables=receivables,other_assets=other,
       net_worth=if(has_cash) cash+other+receivables-liability else NA_real_,
       goals_reserve=sum(get_goals(con)$saved_balance),
       emergency_reserve=sum(get_goals(con)$saved_balance[get_goals(con)$emergency_available==1]))
}

# A true cash-flow comparison (not allocations). Returns NA for months without
# imported or manual observations so missing history cannot look like 0 spending.
financial_monthly_metrics <- function(con,forecast) {
  month_data<-lapply(seq_len(nrow(forecast)),function(i) {
    m<-forecast$month[i]
    tx<-month_transactions(con,m)
    allocations<-loan_cash_allocations(con,m)
    loan_lent<-sum(allocations$lent)
    loan_repaid<-sum(allocations$returned)
    tx$lent_allocated<-allocations$lent[match(tx$id,allocations$id)]
    tx$returned_allocated<-allocations$returned[match(tx$id,allocations$id)]
    tx$lent_allocated[is.na(tx$lent_allocated)]<-0
    tx$returned_allocated[is.na(tx$returned_allocated)]<-0
    income<-sum(pmax(0,tx$amount[tx$flow_type=='income']-tx$returned_allocated[tx$flow_type=='income']))
    expense<-sum(pmax(0,-tx$amount[tx$flow_type=='expense']-tx$lent_allocated[tx$flow_type=='expense']))
    debt<-sum(pmax(0,-tx$amount[tx$flow_type=='debt']))
    actual_outflow<-expense+debt+loan_lent
    actual_remaining<-income+loan_repaid-actual_outflow
    planned_outflow<-forecast$category_budgets[i]+forecast$debt_minimums[i]+
      forecast$debt_extras[i]+forecast$one_off_expenses[i]
    data.frame(month=m,date=forecast$date[i],
      plan_income=forecast$income_budget[i],plan_expenses=planned_outflow,
      plan_remaining=forecast$income_budget[i]-planned_outflow,
      actual_income=if(nrow(tx))income+loan_repaid else NA_real_,
      actual_expenses=if(nrow(tx))actual_outflow else NA_real_,
      actual_remaining=if(nrow(tx))actual_remaining else NA_real_,
      observed=nrow(tx)>0)
  })
  do.call(rbind,month_data)
}

financial_kpis <- function(con, month, forecast) {
  snap <- financial_snapshot(con)
  data <- financial_monthly_metrics(con,forecast)
  current <- data[data$month==month,,drop=FALSE]
  savings <- get_goals(con)
  funds <- get_funds(con)
  essential <- sum(month_budgets(con,month)$budget[month_budgets(con,month)$is_fixed==1])+
    sum(get_debts(con)$min_payment[get_debts(con)$active==1])+
    fund_monthly_contribution(funds)
  if(essential<=0)essential<-NA_real_
  liquid <- if(snap$has_cash) snap$cash else if(snap$emergency_reserve>0) snap$emergency_reserve else NA_real_
  # Inflows and outflows, not internal savings transfers, determine actual cashflow.
  savings_rate <- if(nrow(current) && is.finite(current$actual_income[1]) && current$actual_income[1]>0)
    100*(current$actual_income[1]-current$actual_expenses[1])/current$actual_income[1] else NA_real_
  budget_income <- income_projection(con,month,month)$monthly$budget[1]
  dti <- if(budget_income>0)100*sum(get_debts(con)$min_payment[get_debts(con)$active==1])/budget_income else NA_real_
  prev <- format(as.Date(paste0(month,'-01'))-1,'%Y-%m')
  # Actual category growth is computed from booked expense transactions only.
  currtx <- month_transactions(con,month)
  prevtx <- month_transactions(con,prev)
  csum <- function(tx,id) sum(pmax(0,-tx$amount[tx$flow_type=='expense' & !is.na(tx$category_id) & tx$category_id==id]))
  categories<-get_categories(con)
  trend<-if(nrow(categories) && nrow(currtx) && nrow(prevtx)) {
    x<-sapply(categories$id,function(id)csum(currtx,id)-csum(prevtx,id))
    if(any(x>0))list(category=categories$name[which.max(x)],increase=max(x)) else list(category='No category increase',increase=0)
  } else list(category='Need two months of transactions',increase=NA_real_)
  history<-data[data$observed & data$month<format(Sys.Date(),'%Y-%m'),,drop=FALSE]
  accuracy<-if(nrow(history)) {
    error<-mean(abs(history$actual_remaining-history$plan_remaining)/pmax(1,abs(history$plan_income)))
    max(0,100*(1-error))
  } else NA_real_
  list(snapshot=snap,savings_rate=savings_rate,dti=dti,
       runway=if(is.finite(liquid) && is.finite(essential))max(0,liquid)/essential else NA_real_,
       runway_basis=if(snap$has_cash)'Account balances / cash snapshot' else 'Emergency goal snapshots only',
       trend=trend,accuracy=accuracy,monthly=data)
}
