# Synthetic-only regression tests: never require private bank statements.
make_bf_test_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(),':memory:')
  init_db(con);init_planning_db(con);init_income_db(con)
  init_receivables_db(con);init_budgetforge_db(con)
  con
}

test_that('currency conversion is reversible and does not mutate balances', {
  con <- make_bf_test_db();on.exit(DBI::dbDisconnect(con),add=TRUE)
  st <- get_settings(con)
  expect_identical(st$display_currency, 'CZK')
  expect_equal(st$eur_czk_rate,24)
  upsert_debt(con,'Example loan',24000,5,1000,20,TRUE)
  DBI::dbExecute(con,"UPDATE settings SET display_currency='EUR',eur_czk_rate=25 WHERE id=1")
  st <- get_settings(con)
  expect_equal(view_amount(2500,st),100)
  expect_equal(stored_amount(100,st),2500)
  expect_equal(format_money(2500,st),'€100,00')
  expect_equal(get_debts(con)$balance,24000)
  DBI::dbExecute(con,"UPDATE settings SET display_currency='CZK' WHERE id=1")
  expect_equal(view_amount(24000,get_settings(con)),24000)
})

test_that('monthly savings allocations are committed cash but not spending', {
  con <- make_bf_test_db();on.exit(DBI::dbDisconnect(con),add=TRUE)
  month <- format(Sys.Date(),'%Y-%m')
  upsert_goal(con,'Emergency fund',3000,1000,1500,emergency=TRUE)
  expect_equal(goal_monthly_allocation(get_goals(con),month),1500)
  next_month<-format(seq(as.Date(paste0(month,'-01')),by='month',length.out=2)[2],'%Y-%m')
  expect_equal(goal_monthly_allocation(get_goals(con),next_month),500)
  s<-monthly_summary(con,month)
  expect_equal(s$goal_reserve,1500)
  expect_equal(s$expenses,0)
  f<-forecast_months(con,month,next_month)
  expect_equal(f$goal_savings,c(1500,500))
  expect_equal(f$planned_leftover,f$income_budget-f$category_budgets-
    f$debt_minimums-f$debt_extras-f$one_off_expenses-f$sinking_contributions-
    f$savings-f$goal_savings)
})

test_that('one-sided transfers to a registered account are excluded from costs', {
  con <- make_bf_test_db();on.exit(DBI::dbDisconnect(con),add=TRUE)
  upsert_account(con,'111/2010','Current','Fio',TRUE,30000)
  upsert_account(con,'222/0800','Savings','CSAS',TRUE,45000)
  DBI::dbExecute(con,paste('INSERT INTO transactions',
    '(fingerprint,txn_date,bank,account_key,counterparty_account,description,amount,currency,flow_type)',
    "VALUES('transfer-example',?,'FIO','111/2010','222/0800','Own savings',-5000,'CZK','expense')"),
    params=list(as.character(Sys.Date())))
  mark_internal_transfers(con)
  tx<-get_transactions(con)
  expect_equal(tx$flow_type,'transfer')
  expect_equal(monthly_summary(con,format(Sys.Date(),'%Y-%m'))$expenses,0)
})

test_that('goal reserves do not inflate net worth of account balances', {
  con <- make_bf_test_db();on.exit(DBI::dbDisconnect(con),add=TRUE)
  upsert_account(con,'123/2010','Current','Fio',TRUE,20000)
  upsert_account(con,'456/0800','Savings','CSAS',TRUE,50000)
  upsert_goal(con,'Emergency',40000,30000,1000,emergency=TRUE)
  upsert_debt(con,'Student loan',15000,0,1000,20,TRUE)
  snap <- financial_snapshot(con)
  expect_equal(snap$cash,70000)
  expect_equal(snap$goals_reserve,30000)
  expect_equal(snap$emergency_reserve,30000)
  expect_equal(snap$net_worth,55000) # 70k cash minus 15k debt, not plus reserve again
})

test_that('monthly cashflow comparisons do not invent transaction history', {
  con <- make_bf_test_db();on.exit(DBI::dbDisconnect(con),add=TRUE)
  month<-format(Sys.Date(),'%Y-%m')
  f<-forecast_months(con,month,month)
  x<-financial_monthly_metrics(con,f)
  expect_false(x$observed)
  expect_true(is.na(x$actual_income))
  expect_true(is.na(x$actual_expenses))
})

test_that('version 5 migrations are idempotent and preserve old data', {
  con <- make_bf_test_db();on.exit(DBI::dbDisconnect(con),add=TRUE)
  upsert_goal(con,'Holiday',10000,1000,500)
  init_budgetforge_db(con)
  expect_equal(nrow(get_goals(con)),1)
  expect_equal(get_goals(con)$name,'Holiday')
})
