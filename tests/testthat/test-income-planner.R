test_that('v2 incomes and planned one-off incomes migrate without duplicates', {
  con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con)
  upsert_source_old <- function() DBI::dbExecute(con,
    'INSERT INTO income_sources(name,monthly_amount,pay_day,active) VALUES(?,?,?,?)',
    params=list('Salary',45000,22L,1L))
  upsert_source_old()
  add_event(con,'2028-05','Bonus','income',12000)
  add_event(con,'2028-05','Birthday','expense',1800)
  init_income_db(con);init_receivables_db(con)
  expect_equal(get_sources(con)$kind,'fixed')
  expect_equal(get_sources(con)$min_amount,45000)
  expect_equal(get_sources(con)$max_amount,45000)
  expect_equal(get_sources(con)$include_budget,1L)
  expect_equal(nrow(get_oneoffs(con)),1L)
  expect_equal(get_oneoffs(con)$expected,12000)
  expect_equal(nrow(get_events(con)),1L) # expenses remain untouched
  init_income_db(con);init_receivables_db(con)
  expect_equal(nrow(get_oneoffs(con)),1L) # idempotent migration
})

test_that('freelance scenarios and contract expiry are honored', {
  con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  upsert_source(con,'Salary',40000,20,TRUE,kind='fixed',
                start_month='2027-01',end_month='2028-12',include_budget=TRUE)
  upsert_source(con,'Freelance',8000,25,TRUE,kind='variable',min_amount=2000,max_amount=15000,
                start_month='2027-01',end_month='2027-12',include_budget=TRUE)
  s<-income_month_scenarios(get_sources(con),get_oneoffs(con),'2027-06')
  expect_equal(c(s$guaranteed,s$low,s$expected,s$high,s$budget),
               c(40000,42000,48000,55000,42000))
  z<-income_month_scenarios(get_sources(con),get_oneoffs(con),'2028-01')
  expect_equal(c(z$guaranteed,z$low,z$expected,z$high),rep(40000,4))
  beyond<-income_month_scenarios(get_sources(con),get_oneoffs(con),'2029-01')
  expect_equal(beyond$expected,0)
  id<-get_sources(con)$id[get_sources(con)$name=='Freelance']
  upsert_source(con,'Freelance',8000,25,TRUE,id,kind='variable',min_amount=2000,max_amount=15000,
                start_month='2027-01',end_month='2027-12',include_budget=FALSE)
  expect_equal(income_month_scenarios(get_sources(con),get_oneoffs(con),'2027-06')$expected,40000)
})

test_that('one-time confirmed and uncertain incomes are not double-counted', {
  con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  upsert_source(con,'Salary',30000,20,TRUE,start_month='2027-01',include_budget=TRUE)
  id<-upsert_oneoff(con,'Bonus','2027-06',10000,10000,10000,'confirmed',TRUE)
  upsert_oneoff(con,'Freelance milestone','2027-06',0,20000,50000,'uncertain',TRUE)
  f<-income_month_scenarios(get_sources(con),get_oneoffs(con),'2027-06')
  expect_equal(c(f$guaranteed,f$low,f$expected,f$high),c(40000,40000,60000,90000))
  DBI::dbExecute(con,'UPDATE income_oneoffs SET received=1 WHERE id=?',params=list(id))
  f<-income_month_scenarios(get_sources(con),get_oneoffs(con),'2027-06')
  expect_equal(f$guaranteed,30000)
  expect_equal(f$expected,50000)
})

test_that('timeline can extend past 2027 and opening equity is applied only once', {
  con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  upsert_source(con,'Salary',35000,15,TRUE,start_month='2027-01',include_budget=TRUE)
  DBI::dbExecute(con,'UPDATE settings SET opening_equity=200000 WHERE id=1')
  p<-income_projection(con,'2028-01','2031-12')$monthly
  expect_equal(nrow(p),48)
  expect_equal(p$cum_expected[1],235000)
  expect_equal(tail(p$cum_expected,1),200000+48*35000)
  expect_error(month_sequence('2029-02','2028-01'))
})

test_that('excluded recurring deposits do not increase category spending headroom', {
  con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  upsert_source(con,'Side Job',10000,25,TRUE,start_month='2026-01',include_budget=FALSE)
  id<-get_sources(con)$id[1]
  DBI::dbExecute(con,paste('INSERT INTO transactions',
    '(fingerprint,txn_date,bank,account_key,payee,description,amount,currency,flow_type,income_source_id)',
    "VALUES('synthetic','2026-05-04','MANUAL','demo','test','side job',10000,'CZK','income',?)"),
    params=list(id))
  x<-monthly_summary(con,'2026-05',now=as.Date('2026-05-10'))
  expect_equal(x$actual_income,10000)
  expect_equal(x$budget_income_received,0)
})
