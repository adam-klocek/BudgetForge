test_that("several debt balances stay separate with colored-plot-ready histories", {
  d <- data.frame(id=c(1L,2L),name=c("Car loan","Credit card"),
                  active=1L,balance=c(12000,6000),apr=0,
                  min_payment=c(1000,500))
  r <- simulate_payoff(d,start_month="2026-11")
  expect_true(r$finished)
  expect_equal(r$months,12L)
  expect_equal(r$interest,0)
  expect_equal(nrow(r$by_debt),2L)
  expect_equal(r$history$outstanding[1],18000)
  expect_equal(sort(unique(r$by_debt_history$debt)),sort(d$name))
  expect_equal(sum(r$by_debt$interest),r$interest)
})

test_that("one scheduled lump sum changes just its assigned debt", {
  d <- data.frame(id=c(1L,2L),name=c("Car loan","Card"),
                  active=1L,balance=c(12000,12000),apr=c(12,18),
                  min_payment=c(1500,1000))
  extras <- data.frame(debt_id=1L,month="2026-12",amount=5000,completed=0L)
  base <- simulate_payoff(d,start_month="2026-11")
  plan <- simulate_payoff(d,lump_sums=extras,start_month="2026-11")
  expect_true(plan$finished)
  expect_lt(plan$interest,base$interest)
  expect_equal(sum(plan$by_debt$interest),plan$interest)
  expect_equal(plan$by_debt$extra_paid[plan$by_debt$debt_id==1L],5000)
  expect_equal(plan$by_debt$extra_paid[plan$by_debt$debt_id==2L],0)
  expect_equal(plan$by_debt$interest[plan$by_debt$debt_id==2L],
               base$by_debt$interest[base$by_debt$debt_id==2L])
  expect_true(any(plan$by_debt_history$extra_paid>0))
})

test_that("an overlarge extra repayment never overpays the intended debt", {
  d <- data.frame(id=c(1L,2L),name=c("Small","Large"),active=1L,
                  balance=c(2000,9000),apr=0,min_payment=c(500,500))
  extras <- data.frame(debt_id=1L,month="2026-11",amount=10000)
  p <- simulate_payoff(d,lump_sums=extras,start_month="2026-11")
  expect_equal(p$extra_paid,1500)
  expect_equal(p$unused_extras,8500)
  expect_equal(p$by_debt$extra_paid[2],0)
})

test_that("completed or past payments do not get simulated again", {
  d <- data.frame(id=1L,name="Loan",active=1L,balance=10000,
                  apr=0,min_payment=1000)
  x <- data.frame(debt_id=c(1L,1L),month=c("2026-10","2026-11"),
                  amount=c(9000,8000),completed=c(0L,1L))
  result <- simulate_payoff(d,lump_sums=x,start_month="2026-11")
  expect_equal(result$extra_paid,0)
  expect_equal(result$months,10L)
})

test_that("debt one-off plans persist independently in SQLite and can be edited", {
  con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  upsert_debt(con,"Credit card",50000,19,2000,15,TRUE)
  id <- get_debts(con)$id[1]
  upsert_debt_extra(con,id,"2026-12",15000,"Annual bonus")
  d <- get_debt_extras(con)
  expect_equal(nrow(d),1)
  expect_equal(d$debt_name,"Credit card")
  expect_equal(d$completed,0L)
  upsert_debt_extra(con,id,"2027-01",20000,"Changed date",d$id[1])
  updated <- get_debt_extras(con)
  expect_equal(updated$month,"2027-01")
  expect_equal(updated$amount,20000)
  set_debt_extra_completed(con,updated$id[1],TRUE)
  expect_equal(get_debt_extras(con)$completed,1L)
  delete_debt_extra(con,updated$id[1])
  expect_equal(nrow(get_debt_extras(con)),0)
})

test_that("planned extra repayments reduce category-spend headroom", {
  con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  month <- format(Sys.Date(),"%Y-%m")
  before_start <- as.Date(paste0(month,"-01")) - 1
  upsert_source(con,"Salary",50000,25,TRUE)
  upsert_debt(con,"Loan",20000,0,1000,20,TRUE)
  id <- get_debts(con)$id[1]
  no_extra <- monthly_summary(con,month,now=before_start)
  upsert_debt_extra(con,id,month,5000,"Tax refund")
  with_extra <- monthly_summary(con,month,now=before_start)
  expect_equal(no_extra$available-with_extra$available,5000)
  expect_equal(with_extra$unpaid_extra_debts,5000)
  forward <- forecast_12_months(con,month)
  expect_equal(forward$debt_extras[1],5000)
  expect_equal(forward$planned_leftover[1],44000)
  set_debt_extra_completed(con,get_debt_extras(con)$id[1],TRUE)
  expect_equal(monthly_summary(con,month,now=before_start)$unpaid_extra_debts,0)
})

test_that("income sources can be created and updated without Excel", {
  con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  upsert_source(con,"Salary",42000,20,TRUE)
  expect_equal(nrow(get_sources(con)),1L)
  id <- get_sources(con)$id[1]
  upsert_source(con,"Salary",45000,23,TRUE,id)
  expect_equal(nrow(get_sources(con)),1L)
  expect_equal(get_sources(con)$monthly_amount,45000)
  expect_equal(get_sources(con)$pay_day,23)
})
