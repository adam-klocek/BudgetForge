# Personal loans to others are receivables: neither ordinary spending nor earned
# income. Imported bank transactions may contain more than the loan amount.
init_receivables_db <- function(con) {
  DBI::dbExecute(con,paste(
    'CREATE TABLE IF NOT EXISTS money_lent (',
    'id INTEGER PRIMARY KEY AUTOINCREMENT, borrower TEXT NOT NULL, amount REAL NOT NULL CHECK(amount>0),',
    'lent_date TEXT NOT NULL, expected_return TEXT NOT NULL, note TEXT NOT NULL DEFAULT "",',
    'include_forecast INTEGER NOT NULL DEFAULT 1, linked_txn_id INTEGER UNIQUE REFERENCES transactions(id) ON DELETE SET NULL,',
    'prior_flow TEXT, prior_category INTEGER)'))
  DBI::dbExecute(con,paste(
    'CREATE TABLE IF NOT EXISTS loan_repayments (',
    'id INTEGER PRIMARY KEY AUTOINCREMENT, loan_id INTEGER NOT NULL REFERENCES money_lent(id) ON DELETE CASCADE,',
    'received_date TEXT NOT NULL, amount REAL NOT NULL CHECK(amount>0),',
    'linked_txn_id INTEGER UNIQUE REFERENCES transactions(id) ON DELETE SET NULL,',
    'note TEXT NOT NULL DEFAULT "")'))
  invisible(TRUE)
}
get_receivables <- function(con) {
  DBI::dbGetQuery(con,paste(
    'SELECT l.*,COALESCE((SELECT SUM(r.amount) FROM loan_repayments r WHERE r.loan_id=l.id),0) AS repaid,',
    'MAX(0,l.amount-COALESCE((SELECT SUM(r.amount) FROM loan_repayments r WHERE r.loan_id=l.id),0)) AS outstanding',
    'FROM money_lent l ORDER BY l.expected_return,l.borrower,l.id'))
}
get_loan_repayments <- function(con) DBI::dbGetQuery(con,paste(
  'SELECT p.*,l.borrower FROM loan_repayments p JOIN money_lent l ON l.id=p.loan_id',
  'ORDER BY p.received_date DESC,p.id DESC'))

# Unused imported transfers suitable for linking. An existing loan/repayment is
# never linked twice, even when just part of the transaction is allocated.
selectable_loan_bank_txns <- function(con, direction='out') {
  sign <- if(direction=='out') '<0' else '>0'
  linked <- if(direction=='out') 'money_lent' else 'loan_repayments'
  DBI::dbGetQuery(con,paste('SELECT t.id,t.txn_date,t.description,t.amount,t.bank FROM transactions t',
    'WHERE t.amount',sign,"AND t.bank!='MANUAL'",
    'AND t.id NOT IN (SELECT linked_txn_id FROM',linked,'WHERE linked_txn_id IS NOT NULL)',
    'AND t.flow_type NOT IN (\'transfer\',\'withdrawal\')',
    'ORDER BY t.txn_date DESC,t.id DESC LIMIT 1500'))
}
create_manual_loan_transaction <- function(con, amount, date, description, flow) {
  key <- digest::digest(paste('loan',flow,Sys.time(),sample.int(.Machine$integer.max,1)),algo='sha256')
  DBI::dbExecute(con,paste('INSERT INTO transactions (fingerprint,txn_date,bank,account_key,',
    'description,amount,currency,flow_type,is_manual,matched_rule)',
    "VALUES(?,?, 'MANUAL','MANUAL',?,?,'CZK',?,1,'Money lent manual entry')"),
    params=list(key,date,description,amount,flow))
  DBI::dbGetQuery(con,'SELECT last_insert_rowid() AS id')$id[1]
}
record_money_lent <- function(con,borrower,amount,lent_date,expected_return,
                              linked_txn_id=NA_integer_,note='',include_forecast=TRUE) {
  if(!nzchar(trimws(borrower)) || !is.finite(amount) || amount<=0 ||
     is.na(as.Date(lent_date)) || is.na(as.Date(expected_return)) ||
     as.Date(expected_return)<as.Date(lent_date))
    stop('Enter a borrower, positive amount, and valid lending/return dates.')
  DBI::dbWithTransaction(con,{
    prior_flow <- NA_character_; prior_category <- NA_integer_
    if(is.na(linked_txn_id)) {
      linked_txn_id <- create_manual_loan_transaction(con,-amount,lent_date,
        paste0('Money lent to ',borrower),'loan_out')
    } else {
      row <- DBI::dbGetQuery(con,'SELECT * FROM transactions WHERE id=?',params=list(as.integer(linked_txn_id)))
      if(!nrow(row)||row$amount[1]>=0 || row$flow_type[1] %in% c('transfer','withdrawal','debt'))
        stop('Choose an outgoing bank transaction that is not an internal transfer or debt payment.')
      already <- DBI::dbGetQuery(con,'SELECT id FROM money_lent WHERE linked_txn_id=?',params=list(as.integer(linked_txn_id)))
      if(nrow(already))stop('This transaction is already linked to a loan.')
      if(amount > abs(row$amount[1])+0.005)stop('Loan amount cannot exceed the outgoing bank transaction.')
      lent_date <- row$txn_date[1]
      prior_flow <- row$flow_type[1];prior_category<-row$category_id[1]
      if(abs(amount+row$amount[1])<.005) {
        DBI::dbExecute(con,paste("UPDATE transactions SET flow_type='loan_out',category_id=NULL,",
          "income_source_id=NULL,matched_rule='Money lent (linked)',is_manual=1 WHERE id=?"),
          params=list(as.integer(linked_txn_id)))
      } else {
        DBI::dbExecute(con,"UPDATE transactions SET is_manual=1,matched_rule='Partly money lent (linked)' WHERE id=?",
          params=list(as.integer(linked_txn_id)))
      }
    }
    DBI::dbExecute(con,paste('INSERT INTO money_lent',
      '(borrower,amount,lent_date,expected_return,note,include_forecast,linked_txn_id,prior_flow,prior_category)',
      'VALUES(?,?,?,?,?,?,?,?,?)'),params=list(trimws(borrower),amount,as.character(lent_date),
       as.character(expected_return),trimws(note),as.integer(include_forecast),as.integer(linked_txn_id),
       prior_flow,prior_category))
  })
  invisible(TRUE)
}
update_money_lent <- function(con,id,borrower,expected_return,note,include_forecast) {
  if(!nzchar(trimws(borrower))||is.na(as.Date(expected_return)))stop('Enter a name and valid expected return date.')
  old <- DBI::dbGetQuery(con,'SELECT lent_date FROM money_lent WHERE id=?',params=list(as.integer(id)))
  if(!nrow(old)||as.Date(expected_return)<as.Date(old$lent_date[1]))stop('Return date cannot precede lending date.')
  DBI::dbExecute(con,paste('UPDATE money_lent SET borrower=?,expected_return=?,note=?,include_forecast=? WHERE id=?'),
    params=list(trimws(borrower),as.character(expected_return),trimws(note),as.integer(include_forecast),as.integer(id)))
}
record_loan_repayment <- function(con,loan_id,amount,date,linked_txn_id=NA_integer_,note='') {
  if(!is.finite(amount)||amount<=0||is.na(as.Date(date)))stop('Enter a positive repayment and valid date.')
  loan <- get_receivables(con);loan<-loan[loan$id==loan_id,,drop=FALSE]
  if(!nrow(loan)||amount>loan$outstanding[1]+0.005)stop('Repayment exceeds the outstanding amount.')
  DBI::dbWithTransaction(con,{
    if(is.na(linked_txn_id)) {
      linked_txn_id<-create_manual_loan_transaction(con,amount,date,
        paste0('Loan repaid by ',loan$borrower[1]),'loan_return')
    } else {
      t<-DBI::dbGetQuery(con,'SELECT * FROM transactions WHERE id=?',params=list(as.integer(linked_txn_id)))
      if(!nrow(t)||t$amount[1]<=0||t$flow_type[1]=='transfer') stop('Select a positive imported bank receipt.')
      if(amount>t$amount[1]+.005)stop('Repayment cannot exceed the bank receipt.')
      date <- t$txn_date[1]
      dup<-DBI::dbGetQuery(con,'SELECT id FROM loan_repayments WHERE linked_txn_id=?',params=list(as.integer(linked_txn_id)))
      if(nrow(dup))stop('This bank receipt is already linked.')
      if(abs(amount-t$amount[1])<.005)
        DBI::dbExecute(con,paste("UPDATE transactions SET flow_type='loan_return',income_source_id=NULL,",
          "matched_rule='Money returned (linked)',is_manual=1 WHERE id=?"),params=list(as.integer(linked_txn_id)))
      else DBI::dbExecute(con,"UPDATE transactions SET matched_rule='Partly loan repayment',is_manual=1 WHERE id=?",params=list(as.integer(linked_txn_id)))
    }
    DBI::dbExecute(con,paste('INSERT INTO loan_repayments(loan_id,received_date,amount,linked_txn_id,note)',
      'VALUES(?,?,?,?,?)'),params=list(as.integer(loan_id),as.character(date),amount,as.integer(linked_txn_id),trimws(note)))
  })
  invisible(TRUE)
}
# Amounts allocated from imported transactions, for non-double-counting.
loan_cash_allocations <- function(con,month) {
  t<-month_transactions(con,month)
  if(!nrow(t))return(data.frame(id=integer(),lent=numeric(),returned=numeric()))
  out<-DBI::dbGetQuery(con,paste('SELECT linked_txn_id AS id,SUM(amount) AS lent FROM money_lent',
     'WHERE linked_txn_id IS NOT NULL GROUP BY linked_txn_id'))
  incoming<-DBI::dbGetQuery(con,paste('SELECT linked_txn_id AS id,SUM(amount) AS returned FROM loan_repayments',
     'WHERE linked_txn_id IS NOT NULL GROUP BY linked_txn_id'))
  x<-data.frame(id=t$id,lent=0,returned=0)
  oi<-match(x$id,out$id);ri<-match(x$id,incoming$id)
  x$lent[!is.na(oi)]<-out$lent[oi[!is.na(oi)]]
  x$returned[!is.na(ri)]<-incoming$returned[ri[!is.na(ri)]]
  x
}
receivable_due_by_month <- function(con,month) {
  x<-get_receivables(con)
  if(!nrow(x))return(0)
  sum(x$outstanding[x$include_forecast==1 & substr(x$expected_return,1,7)==month],na.rm=TRUE)
}

delete_money_lent <- function(con,id) {
  # Removes the linked plan and its manual entries. Imported bank records are
  # never deleted; their prior classification is restored.
  DBI::dbWithTransaction(con,{
    loan<-DBI::dbGetQuery(con,'SELECT * FROM money_lent WHERE id=?',params=list(as.integer(id)))
    if(!nrow(loan))stop('Loan not found')
    reps<-DBI::dbGetQuery(con,'SELECT * FROM loan_repayments WHERE loan_id=?',params=list(as.integer(id)))
    for(i in seq_len(nrow(reps))) {
      t<-DBI::dbGetQuery(con,'SELECT * FROM transactions WHERE id=?',params=list(reps$linked_txn_id[i]))
      if(nrow(t) && t$bank[1]=='MANUAL' && t$matched_rule[1]=='Money lent manual entry')
        DBI::dbExecute(con,'DELETE FROM transactions WHERE id=?',params=list(t$id[1]))
      else if(nrow(t))DBI::dbExecute(con,paste("UPDATE transactions SET flow_type='income',matched_rule=NULL,",
        'is_manual=0 WHERE id=?'),params=list(t$id[1]))
    }
    t<-DBI::dbGetQuery(con,'SELECT * FROM transactions WHERE id=?',params=list(loan$linked_txn_id[1]))
    if(nrow(t) && t$bank[1]=='MANUAL' && t$matched_rule[1]=='Money lent manual entry')
      DBI::dbExecute(con,'DELETE FROM transactions WHERE id=?',params=list(t$id[1]))
    else if(nrow(t)) {
      DBI::dbExecute(con,paste('UPDATE transactions SET flow_type=?, category_id=?,',
        'matched_rule=NULL,is_manual=0 WHERE id=?'),params=list(
        if(is.na(loan$prior_flow[1]))'expense' else loan$prior_flow[1],
        if(is.na(loan$prior_category[1]))NA_integer_ else as.integer(loan$prior_category[1]),t$id[1]))
    }
    DBI::dbExecute(con,'DELETE FROM money_lent WHERE id=?',params=list(as.integer(id)))
  })
  invisible(TRUE)
}
