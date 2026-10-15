test_that('new accounts get seven English starter categories, not dozens', {
  con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  expect_setequal(get_categories(con)$name,
    c('Travel','Household','Food home','Food outside','Hobbies','Gifts','Withdrawals'))
  expect_equal(get_settings(con)$app_language,'en')
  expect_equal(get_settings(con)$table_rows,10)
})

test_that('multilingual merchant rules assign stable English categories', {
  cases<-list(
    Travel=c('Doprava MHD Brno','DB Bahn Fahrkarte','Bus train transport'),
    Household=c('Drogerie opravy domu','Haushalt Reparatur','Household cleaning'),
    `Food home`=c('Potraviny LIDL','REWE Supermarkt','Grocery store'),
    `Food outside`=c('Restaurace a káva','Restaurant Cafe','Bistro coffee'),
    Hobbies=c('Zábava a kultura','Kultur und Bildung','Cinema education'),
    Gifts=c('Dárky narozeniny','Geschenk','Birthday gift'),
    Withdrawals=c('Výběr z bankomatu','ATM cash withdrawal','Bargeldabhebung Geldautomat')
  )
  for(nm in names(cases)) for(text in cases[[nm]])
    expect_equal(builtin_category(text),nm,info=text)
})

test_that('partial money lent and repaid is not counted as spending or salary', {
  con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  upsert_category(con,'Food home',FALSE,0)
  month<-format(Sys.Date(),'%Y-%m')
  today<-as.character(Sys.Date())
  DBI::dbExecute(con,paste('INSERT INTO transactions',
    '(fingerprint,txn_date,bank,account_key,description,amount,currency,flow_type,category_id)',
    "VALUES('outgoing',?,'FIO','FIO','Split outgoing',-1000,'CZK','expense',?)"),
    params=list(today,get_categories(con)$id[get_categories(con)$name=='Food home']))
  out<-DBI::dbGetQuery(con,'SELECT id FROM transactions WHERE fingerprint="outgoing"')$id
  record_money_lent(con,'Friend',400,today,as.character(Sys.Date()+30),out,'Split loan')
  s<-monthly_summary(con,month,now=Sys.Date())
  expect_equal(s$expenses,600)
  expect_equal(s$loan_lent,400)
  expect_equal(s$loan_collected,0)
  expect_equal(s$receivable_due,0)
  DBI::dbExecute(con,paste('INSERT INTO transactions',
    '(fingerprint,txn_date,bank,account_key,description,amount,currency,flow_type)',
    "VALUES('incoming',?,'CSAS','CSAS','Split incoming',300,'CZK','income')"),
    params=list(today))
  incoming<-DBI::dbGetQuery(con,'SELECT id FROM transactions WHERE fingerprint="incoming"')$id
  loan<-get_receivables(con)$id
  record_loan_repayment(con,loan,100,today,incoming)
  s<-monthly_summary(con,month,now=Sys.Date())
  expect_equal(s$actual_income,200)
  expect_equal(s$loan_collected,100)
  expect_equal(s$expenses,600)
  expect_equal(get_receivables(con)$outstanding,300)
  forecast<-forecast_months(con,month,format(Sys.Date()+60,'%Y-%m'))
  expect_true(any(forecast$possible_loan_return==300))
  expect_equal(forecast$planned_leftover,forecast$income_budget-forecast$category_budgets-
    forecast$debt_minimums-forecast$debt_extras-forecast$one_off_expenses-
    forecast$sinking_contributions-forecast$savings)
})

test_that('cash lending and partial repayment preserve principal and do not double-count', {
  con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  init_db(con);init_planning_db(con);init_income_db(con);init_receivables_db(con)
  now<-as.character(Sys.Date());month<-substr(now,1,7)
  record_money_lent(con,'Colleague',1250,now,as.character(Sys.Date()+30))
  s<-monthly_summary(con,month)
  expect_equal(s$loan_lent,1250)
  expect_equal(s$expenses,0)
  loan<-get_receivables(con)$id
  record_loan_repayment(con,loan,250,now)
  expect_equal(monthly_summary(con,month)$loan_collected,250)
  expect_equal(get_receivables(con)$outstanding,1000)
  expect_error(record_loan_repayment(con,loan,1001,now),'exceeds')
})
