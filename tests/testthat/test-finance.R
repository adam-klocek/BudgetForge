test_that("yearly and quarterly funds use monthly allocations", {
  f <- data.frame(active=c(1L,1L,1L),annual_cost=c(12000,900,2400),cycle_months=c(12L,3L,12L),saved_balance=c(0,100,2400))
  expect_equal(fund_monthly_contribution(f),1300)
})

test_that("monthly headroom protects fixed bills, debt, sinking funds, and one-offs", {
  con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con); init_planning_db(con); init_income_db(con);init_receivables_db(con)
  upsert_category(con,"Rent",TRUE,20000)
  upsert_category(con,"Food flexible",FALSE,9000)
  upsert_source(con,"Salary",50000,25,TRUE)
  upsert_debt(con,"Loan",100000,6,5000,20,TRUE)
  upsert_fund(con,"Car insurance",12000,12,0,TRUE)
  add_event(con,"2099-05","Birthday", "expense",2000)
  DBI::dbExecute(con,"UPDATE settings SET savings_target=2000,safety_buffer=1000 WHERE id=1")
  s <- monthly_summary(con,"2099-05",now=as.Date("2099-04-01"))
  expect_equal(s$available,24000) # loan already projected repaid long before 2099
  expect_equal(s$reserve_funds,1000)
  expect_equal(s$unpaid_events,2000)
  expect_equal(s$variable$suggested_rest[s$variable$name=="Food flexible"],9000)
  f <- forecast_12_months(con,"2099-05")
  expect_equal(f$planned_leftover[1],16000) # Loan was repaid long before the 2099 horizon; don't restart it
})

test_that("repayment methods estimate amortization", {
  d <- data.frame(id=1,name="Loan",active=1,balance=12000,apr=0,min_payment=1000)
  p <- simulate_payoff(d,extra=0)
  expect_true(p$finished)
  expect_equal(p$months,12)
  expect_equal(p$interest,0)
})
