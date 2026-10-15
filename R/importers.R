# Bank parsers preserve original bank identifiers locally for safe deduplication.
clean_chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) }
parse_czk <- function(x) {
  x <- clean_chr(x)
  x <- gsub("[[:space:]\u00a0\u202f]", "", x, perl=TRUE)
  x <- ifelse(grepl(",", x, fixed=TRUE), gsub("\\.", "", x), x)
  x <- gsub(",", ".", x, fixed=TRUE)
  suppressWarnings(as.numeric(x))
}
extract_account <- function(number, code="") {
  number <- clean_chr(number); code <- clean_chr(code)
  ifelse(!nzchar(number), "", ifelse(grepl("/",number,fixed=TRUE) | !nzchar(code), number, paste0(number,"/",code)))
}
coalesce_text <- function(...) {
  fields <- lapply(list(...), clean_chr)
  if (!length(fields)) return(character())
  vapply(seq_along(fields[[1]]), function(i) paste(unique(unlist(lapply(fields, `[[`, i)))[nzchar(unique(unlist(lapply(fields, `[[`, i))))],collapse=" | "), character(1))
}
find_bank <- function(path) {
  raw <- readBin(path, "raw", n=4)
  if (length(raw) >= 2 && identical(as.integer(raw[1:2]), c(255L,254L))) return("CSAS")
  "FIO"
}
parse_fio <- function(path) {
  lines <- readLines(path,encoding="UTF-8",warn=FALSE,n=30)
  h <- grep('^"?ID operace"?;"?Datum"?;"?Objem"?;', sub("^\ufeff", "", lines))
  if (!length(h)) stop("Fio CSV header not found. Please export a standard CSV statement.")
  r <- utils::read.csv(path,sep=";",skip=h[1]-1,fileEncoding="UTF-8-BOM",check.names=FALSE,colClasses="character",stringsAsFactors=FALSE,quote='"',fill=TRUE)
  if (!all(c("ID operace","Datum","Objem","Měna") %in% names(r))) stop("Invalid Fio format: required columns missing")
  owner <- if (length(lines)) {
    # Skip statement number 4/2026 and match the actual longer bank account.
    m <- regmatches(lines[1], gregexpr("[0-9-]{6,}/[0-9]{4}",lines[1],perl=TRUE))[[1]]
    if (length(m)) tail(m,1) else "FIO-UNKNOWN"
  } else "FIO-UNKNOWN"
  amount <- parse_czk(r[["Objem"]])
  if (anyNA(amount)) stop("Fio import contains amounts that cannot be read")
  date <- as.Date(clean_chr(r[["Datum"]]),format="%d.%m.%Y")
  if (anyNA(date)) date <- as.Date(clean_chr(r[["Datum"]]),format="%Y-%m-%d")
  if (anyNA(date)) stop("Fio import contains unknown dates")
  n <- nrow(r)
  note <- if ("Poznámka" %in% names(r)) r[["Poznámka"]] else rep("",n)
  message <- if ("Zpráva pro příjemce" %in% names(r)) r[["Zpráva pro příjemce"]] else rep("",n)
  payee <- if ("Název protiúčtu" %in% names(r)) r[["Název protiúčtu"]] else rep("",n)
  tx_type <- if ("Typ" %in% names(r)) r[["Typ"]] else rep("",n)
  cp <- extract_account(r[["Protiúčet"]],r[["Kód banky"]])
  description <- substr(coalesce_text(payee,message,note,tx_type),1,500)
  out <- data.frame(txn_date=as.character(date),bank="FIO",account_key=owner,counterparty_account=cp,payee=clean_chr(payee),description=description,amount=amount,currency=clean_chr(r[["Měna"]]), external_id=clean_chr(r[["ID operace"]]),stringsAsFactors=FALSE)
  out[nzchar(out$txn_date),,drop=FALSE]
}
parse_csas <- function(path) {
  r <- utils::read.csv(path,sep=",",fileEncoding="UTF-16LE",check.names=FALSE,colClasses="character",stringsAsFactors=FALSE,quote='"')
  names(r) <- sub("^\ufeff","",names(r))
  required <- c("Číslo účtu vlastníka","Datum zaúčtování","Částka","Měna")
  if (!all(required %in% names(r))) stop("Invalid ČSAS format: required columns missing")
  amount <- parse_czk(r[["Částka"]]); if (anyNA(amount)) stop("ČSAS import contains invalid amounts")
  date <- as.Date(clean_chr(r[["Datum zaúčtování"]]),format="%d.%m.%Y")
  if (anyNA(date)) stop("ČSAS import contains unknown dates")
  cp <- extract_account(r[["Protiúčet"]],r[["Bankovní kód protiúčtu"]])
  payee <- clean_chr(r[["Název protiúčtu"]])
  # ČSAS export has no operation ID or payment memo in this particular layout.
  data.frame(txn_date=as.character(date),bank="CSAS",account_key=clean_chr(r[["Číslo účtu vlastníka"]]),counterparty_account=cp,payee=payee,description=ifelse(nzchar(payee),payee,"Bankovní transakce"),amount=amount,currency=clean_chr(r[["Měna"]]),external_id="",stringsAsFactors=FALSE)
}

standardize_import <- function(data) {
  stopifnot(all(c("txn_date","bank","amount","description") %in% names(data)))
  data <- data[!is.na(data$amount) & nzchar(data$txn_date),,drop=FALSE]
  if(nrow(data) && any(!toupper(data$currency) %in% c('CZK','KČ','KČ.')))
    stop('Only CZK-denominated statement rows can be imported into the planning ledger. Convert non-CZK rows using a documented transaction-date rate before importing.')
  if (!nrow(data)) return(data)
  # Preserve genuinely repeated same-day transactions, while deduplicating reimports.
  base_key <- paste(data$bank,data$account_key,data$txn_date,sprintf("%.2f",data$amount),data$currency,data$counterparty_account,data$payee,data$description,sep="|")
  occurrence <- ave(seq_along(base_key),base_key,FUN=seq_along)
  ids <- ifelse(nzchar(data$external_id),paste0("ID:",data$bank,":",data$account_key,":",data$external_id),paste0("ROW:",base_key,":",occurrence))
  data$fingerprint <- vapply(ids,digest::digest,character(1),algo="sha256",serialize=FALSE)
  data
}

# Czech / English / German matching. Normalize Czech and German diacritics
# before comparing; words remain intentionally conservative and editable.
normalize_merchant <- function(x) {
  x <- iconv(enc2utf8(x),from="UTF-8",to="ASCII//TRANSLIT",sub="")
  tolower(ifelse(is.na(x),"",x))
}
builtin_category <- function(description) {
  txt <- normalize_merchant(description)
  # Order matters: ATM withdrawals must be recognized before broad travel rules.
  patterns <- list(
    "Withdrawals"="\\b(atm|bankomat|vyber (z )?(bankomatu|hotovosti)|cash withdrawal|cash machine|bargeldabhebung|geldautomat|barabhebung)\\b",
    "Food outside"="restaurac|restaurant|restaurante|kavarn|kava|coffee|cafe|bistro|starbucks|foodora|wolt|takeaway|gaststatte|essen gehen|imbiss|kneipe",
    "Food home"="\\b(potraviny|lidl|kaufland|albert|billa|tesco|globus|penny|rewe|edeka|netto|rohlik|kosik|supermarket|supermarkt|grocery|groceries|lebensmittel|hofer|aldi)\\b",
    "Travel"="\\b(doprava|auto|mhd|vlak|bus|regiojet|flixbus|leo express|ceske drahy|drahy|uber|bolt|taxi|parking|parkovani|shell|orlen|benzina|omv|tankstelle|fahrkarte|bahnhof|bahn|zug|fahrzeug|transport|transit|reise|travel|train|petrol|diesel|gas station)\\b|dopravni podnik|deutsche bahn|booking[.]com|airbnb",
    "Household"="drogerie|domacnost|opravy|udrzba|\\b(rossmann|hornbach|bauhaus|obi|household|cleaning|repair|maintenance|haushalt|reinigung|reparatur|wartung|drogeriemarkt)\\b|dm droger",
    "Hobbies"="zabava|kultura|vzdelavani|\\b(hobby|hobbies|kino|divadlo|culture|education|cinema|theatre|theater|entertainment|kurs|kurse|bildung|freizeit|kultur|museum|konzert|concert|netflix|spotify)\\b",
    "Gifts"="\\b(darky|darek|darkove|gift|gifts|present|presents|geschenk|geschenke|geschenkkarte|geburtstagsgeschenk)\\b"
  )
  for (name in names(patterns)) if (grepl(patterns[[name]],txt,perl=TRUE)) return(name)
  NA_character_
}

classify_transaction <- function(description, amount, rules, categories) {
  flow <- if (amount >= 0) "income" else "expense"
  ans <- list(flow_type=flow,category_id=NA_integer_,income_source_id=NA_integer_,debt_id=NA_integer_,matched_rule=NA_character_)
  if (nrow(rules)) for (i in seq_len(nrow(rules))) {
    rule <- rules[i,]
    if (!nzchar(rule$pattern)) next
    if (grepl(rule$pattern,description,ignore.case=TRUE,fixed=TRUE)) {
      # Prevent a positive deposit being accidentally classified as a debt or expense.
      if (amount >= 0 && rule$flow_type %in% c("expense","debt")) next
      if (amount < 0 && rule$flow_type == "income") next
      ans$flow_type <- rule$flow_type
      ans$category_id <- rule$category_id
      ans$income_source_id <- rule$income_source_id
      ans$debt_id <- rule$debt_id
      ans$matched_rule <- paste0("User rule: ",rule$pattern)
      return(ans)
    }
  }
  if (amount < 0) {
    guess <- builtin_category(description)
    if (!is.na(guess)) {
      idx <- match(guess,categories$name)
      if (!is.na(idx)) { ans$category_id <- categories$id[idx]; ans$matched_rule <- paste0("Built-in: ",guess); if(guess=="Withdrawals") ans$flow_type <- "withdrawal" }
    }
  }
  ans
}

import_transactions <- function(con, files) {
  stopifnot(length(files) > 0)
  imported <- 0L; skipped <- 0L; by_bank <- character()
  DBI::dbWithTransaction(con, {
    for (path in files) {
      bank <- find_bank(path)
      df <- standardize_import(if (bank=="FIO") parse_fio(path) else parse_csas(path))
      if (!nrow(df)) next
      if (any(df$currency != "CZK")) stop("Only CZK transactions are supported at present; convert other currencies first.")
      cats <- get_categories(con); rules <- get_rules(con)
      by_bank <- c(by_bank,bank)
      for (i in seq_len(nrow(df))) {
        t <- df[i,]
        display <- paste0(bank," ····",substr(gsub("[^0-9]","",t$account_key),pmax(1,nchar(gsub("[^0-9]","",t$account_key))-7),nchar(gsub("[^0-9]","",t$account_key))))
        DBI::dbExecute(con,"INSERT OR IGNORE INTO accounts(account_key,bank,display_name) VALUES(?,?,?)",params=list(normalize_account_id(t$account_key),bank,display))
        cl <- classify_transaction(t$description,t$amount,rules,cats)
        before <- DBI::dbGetQuery(con,"SELECT id FROM transactions WHERE fingerprint=?",params=list(t$fingerprint))
        if (nrow(before)) { skipped <- skipped+1L; next }
        DBI::dbExecute(con,"INSERT INTO transactions(fingerprint,txn_date,bank,account_key,counterparty_account,payee,description,amount,currency,flow_type,category_id,income_source_id,debt_id,matched_rule) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)",params=list(t$fingerprint,t$txn_date,t$bank,t$account_key,t$counterparty_account,t$payee,t$description,t$amount,t$currency,cl$flow_type,cl$category_id,cl$income_source_id,cl$debt_id,cl$matched_rule))
        imported <- imported+1L
      }
    }
    mark_internal_transfers(con)
  })
  list(imported=imported,skipped=skipped,banks=paste(unique(by_bank),collapse=" + "))
}

mark_internal_transfers <- function(con) {
  t <- get_transactions(con)
  if (!nrow(t)) return(invisible(0L))
  columns <- DBI::dbGetQuery(con,'PRAGMA table_info(accounts)')$name
  owned <- if('is_owned' %in% columns)
    DBI::dbGetQuery(con, "SELECT account_key FROM accounts WHERE is_owned=1")$account_key
    else DBI::dbGetQuery(con, "SELECT account_key FROM accounts")$account_key
  owned <- normalize_account_id(owned)
  # An import always represents the user's statement account. Any transfer to
  # an explicit owned counterparty can be classified without both sides present.
  direct <- which(t$is_manual==0 &
    normalize_account_id(t$account_key) %in% owned &
    normalize_account_id(t$counterparty_account) %in% owned &
    nzchar(normalize_account_id(t$counterparty_account)) &
    normalize_account_id(t$account_key)!=normalize_account_id(t$counterparty_account))
  # Do not hijack transactions linked to receivables or their repayments.
  linked <- c(if(DBI::dbExistsTable(con,'money_lent'))
                DBI::dbGetQuery(con,'SELECT linked_txn_id AS id FROM money_lent WHERE linked_txn_id IS NOT NULL')$id else integer(),
              if(DBI::dbExistsTable(con,'loan_repayments'))
                DBI::dbGetQuery(con,'SELECT linked_txn_id AS id FROM loan_repayments WHERE linked_txn_id IS NOT NULL')$id else integer())
  direct <- direct[!t$id[direct] %in% linked]
  idx <- which(t$is_manual==0 & !is.na(t$counterparty_account) & nzchar(t$counterparty_account))
  found <- direct
  for (i in idx) {
    if (i %in% found) next
    other <- which(t$account_key == t$counterparty_account[i] & t$counterparty_account == t$account_key[i] & t$account_key != t$account_key[i] & t$amount == -t$amount[i] & t$currency == t$currency[i] & abs(as.numeric(as.Date(t$txn_date) - as.Date(t$txn_date[i]))) <= 2 & t$is_manual==0)
    other <- setdiff(other,c(i,found))
    # Require a unique opposite transaction; never guess if ambiguous.
    if (length(other)!=1) next
    j <- other[1]
    found <- c(found,i,j)
  }
  if (length(found)) for (id in t$id[unique(found)]) {
    DBI::dbExecute(con,"UPDATE transactions SET flow_type='transfer',category_id=NULL,income_source_id=NULL,debt_id=NULL,matched_rule='Internal account transfer' WHERE id=? AND is_manual=0",params=list(id))
  }
  invisible(length(unique(found)))
}

apply_rules_existing <- function(con) {
  t <- get_transactions(con); cats <- get_categories(con); rules <- get_rules(con)
  ids <- which(t$is_manual==0 & t$flow_type != "transfer")
  for (i in ids) {
    cl <- classify_transaction(t$description[i],t$amount[i],rules,cats)
    DBI::dbExecute(con,"UPDATE transactions SET flow_type=?, category_id=?, income_source_id=?, debt_id=?, matched_rule=? WHERE id=?",params=list(cl$flow_type,cl$category_id,cl$income_source_id,cl$debt_id,cl$matched_rule,t$id[i]))
  }
  mark_internal_transfers(con)
  invisible(length(ids))
}
