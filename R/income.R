# Income planning and backward-compatible migration for existing v2 databases.
# Amount ranges are USER-DEFINED SCENARIOS, not calibrated statistical intervals.
month_valid <- function(x) length(x)==1L && !is.na(x) &&
  grepl('^[0-9]{4}-(0[1-9]|1[0-2])$', x)
month_sequence <- function(start, end, max_months=600L) {
  if (!month_valid(start) || !month_valid(end) || end < start)
    stop('Choose a valid planning start and end month (end must follow start).')
  dates <- seq(as.Date(paste0(start,'-01')), as.Date(paste0(end,'-01')), by='month')
  if (length(dates)>max_months) stop('Please select 50 years or less for a responsive chart.')
  format(dates,'%Y-%m')
}
init_income_db <- function(con) {
  # ALTER TABLE only when missing to preserve users' previous incomes.
  columns <- DBI::dbGetQuery(con,'PRAGMA table_info(income_sources)')$name
  additions <- c(
    kind="TEXT NOT NULL DEFAULT 'fixed'",
    min_amount='REAL NOT NULL DEFAULT 0',
    max_amount='REAL NOT NULL DEFAULT 0',
    start_month="TEXT NOT NULL DEFAULT '1900-01'",
    end_month='TEXT',
    include_budget='INTEGER NOT NULL DEFAULT 1'
  )
  for (field in names(additions)) if(!field %in% columns)
    DBI::dbExecute(con,paste('ALTER TABLE income_sources ADD COLUMN',field,additions[[field]]))
  if (!'min_amount' %in% columns) {
    DBI::dbExecute(con,'UPDATE income_sources SET min_amount=monthly_amount,max_amount=monthly_amount')
  }
  DBI::dbExecute(con,paste(
    'CREATE TABLE IF NOT EXISTS income_oneoffs (',
    'id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, month TEXT NOT NULL,',
    'low REAL NOT NULL DEFAULT 0, expected REAL NOT NULL DEFAULT 0, high REAL NOT NULL DEFAULT 0,',
    "certainty TEXT NOT NULL DEFAULT 'uncertain' CHECK(certainty IN ('confirmed','uncertain')),",
    'include_budget INTEGER NOT NULL DEFAULT 0, received INTEGER NOT NULL DEFAULT 0,',
    'legacy_event_id INTEGER UNIQUE)'))
  settings_fields <- DBI::dbGetQuery(con,'PRAGMA table_info(settings)')$name
  settings_additions <- c(planner_start='TEXT',planner_end='TEXT',
    opening_equity='REAL NOT NULL DEFAULT 0',"variable_budget_basis"="TEXT NOT NULL DEFAULT 'low'",
    app_language="TEXT NOT NULL DEFAULT 'en'",table_rows="INTEGER NOT NULL DEFAULT 10")
  for (field in names(settings_additions)) if(!field %in% settings_fields)
    DBI::dbExecute(con,paste('ALTER TABLE settings ADD COLUMN',field,settings_additions[[field]]))
  today <- as.Date(format(Sys.Date(),'%Y-%m-01'))
  default_end <- format(seq(today,by='month',length.out=25L)[25L],'%Y-%m')
  DBI::dbExecute(con,'UPDATE settings SET planner_start=COALESCE(planner_start,?),planner_end=COALESCE(planner_end,?) WHERE id=1',
                 params=list(format(today,'%Y-%m'),default_end))
  # One-time migration of earlier planned income events; expense events remain unchanged.
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con,paste(
      "INSERT OR IGNORE INTO income_oneoffs(name,month,low,expected,high,certainty,include_budget,received,legacy_event_id)",
      "SELECT name,month,amount,amount,amount,'confirmed',1,completed,id",
      "FROM planned_events WHERE kind='income'"))
    DBI::dbExecute(con,"DELETE FROM planned_events WHERE kind='income'")
  })
  invisible(TRUE)
}
get_oneoffs <- function(con) DBI::dbGetQuery(con,'SELECT * FROM income_oneoffs ORDER BY month,name,id')
upsert_oneoff <- function(con,name,month,low,expected,high,certainty,include_budget,received=FALSE,id=NA_integer_) {
  if (!nzchar(trimws(name)) || !month_valid(month)) stop('One-off needs a name and valid month.')
  if (any(!is.finite(c(low,expected,high))) || low<0 || low>expected || expected>high)
    stop('Enter 0 ≤ minimum ≤ expected ≤ maximum.')
  if (!certainty %in% c('confirmed','uncertain')) stop('Choose a certainty type.')
  if (certainty=='confirmed') low <- high <- expected
  if (is.na(id)) {
    DBI::dbExecute(con,paste('INSERT INTO income_oneoffs',
      '(name,month,low,expected,high,certainty,include_budget,received) VALUES(?,?,?,?,?,?,?,?)'),
      params=list(trimws(name),month,low,expected,high,certainty,as.integer(include_budget),as.integer(received)))
    id <- DBI::dbGetQuery(con,'SELECT last_insert_rowid() AS id')$id[1]
  } else {
    count <- DBI::dbExecute(con,paste('UPDATE income_oneoffs SET name=?,month=?,low=?,expected=?,high=?,certainty=?,include_budget=?,received=? WHERE id=?'),
      params=list(trimws(name),month,low,expected,high,certainty,as.integer(include_budget),as.integer(received),as.integer(id)))
    if (!count) stop('This one-time income was not found.')
  }
  as.integer(id)
}
# Per month: guaranteed cashflow; low / expected / high scenario and safe budget policy.
# Disabled sources are excluded from forecast totals, but can be listed separately.
income_month_scenarios <- function(sources,oneoffs,month,budget_basis='low') {
  if (!month_valid(month)) stop('Invalid projection month')
  if (!budget_basis %in% c('low','expected')) stop('Invalid budget scenario')
  rows <- list()
  if(nrow(sources)) for(i in seq_len(nrow(sources))) {
    s <- sources[i,,drop=FALSE]
    eligible <- isTRUE(s$active[1]==1 && s$include_budget[1]==1) &&
      month>=s$start_month[1] && (is.na(s$end_month[1]) || month<=s$end_month[1])
    if(!eligible) next
    certain <- s$kind[1]=='fixed'
    lo <- if(certain) s$monthly_amount[1] else s$min_amount[1]
    mid <- s$monthly_amount[1]
    hi <- if(certain) s$monthly_amount[1] else s$max_amount[1]
    rows[[length(rows)+1L]] <- data.frame(key=paste0('source:',s$id[1]),name=s$name[1],
      kind=if(certain) 'fixed' else 'variable',guaranteed=if(certain) mid else 0,
      low=lo,expected=mid,high=hi,stringsAsFactors=FALSE)
  }
  if(nrow(oneoffs)) for(i in seq_len(nrow(oneoffs))) {
    s <- oneoffs[i,,drop=FALSE]
    if(s$month[1]!=month || s$received[1]==1 || s$include_budget[1]!=1) next
    certain <- s$certainty[1]=='confirmed'
    rows[[length(rows)+1L]] <- data.frame(key=paste0('oneoff:',s$id[1]),name=paste0(s$name[1],' (once)'),
      kind=if(certain)'oneoff confirmed' else 'oneoff uncertain',
      guaranteed=if(certain)s$expected[1] else 0,
      low=s$low[1],expected=s$expected[1],high=s$high[1],stringsAsFactors=FALSE)
  }
  empty <- data.frame(key=character(),name=character(),kind=character(),guaranteed=numeric(),low=numeric(),expected=numeric(),high=numeric())
  components <- if(length(rows)) do.call(rbind,rows) else empty
  totals <- setNames(vapply(c('guaranteed','low','expected','high'),function(k) sum(components[[k]]),numeric(1)),c('guaranteed','low','expected','high'))
  variable <- components$kind %in% c('variable','oneoff uncertain')
  budget <- sum(components$guaranteed[!variable])+sum(components[[budget_basis]][variable])
  list(components=components,guaranteed=unname(totals['guaranteed']),low=unname(totals['low']),
       expected=unname(totals['expected']),high=unname(totals['high']),budget=budget)
}
income_projection <- function(con,start_month,end_month) {
  months <- month_sequence(start_month,end_month)
  sources <- get_sources(con); oneoffs <- get_oneoffs(con); settings <- get_settings(con)
  opening <- settings$opening_equity[1]
  budget_basis <- settings$variable_budget_basis[1]
  list_months <- lapply(months,function(m)income_month_scenarios(sources,oneoffs,m,budget_basis))
  flows <- data.frame(month=months,date=as.Date(paste0(months,'-01')),
    guaranteed=vapply(list_months,`[[`,numeric(1),'guaranteed'),
    low=vapply(list_months,`[[`,numeric(1),'low'),
    expected=vapply(list_months,`[[`,numeric(1),'expected'),
    high=vapply(list_months,`[[`,numeric(1),'high'),
    budget=vapply(list_months,`[[`,numeric(1),'budget'))
  for(nm in c('guaranteed','low','expected','high','budget')) flows[[paste0('cum_',nm)]] <- opening+cumsum(flows[[nm]])
  # Each colored line is the cumulative contribution from one source (not assets).
  all_names <- c(paste0('source:',sources$id),paste0('oneoff:',oneoffs$id))
  all_labels <- c(sources$name,paste0(oneoffs$name,' (once)'))
  pieces <- list()
  for(i in seq_along(all_names)) {
    k <- all_names[i]
    contribution <- vapply(list_months,function(s) {
      r <- s$components
      if(!nrow(r)) return(0)
      v <- r$expected[r$key==k]
      if(length(v))v[1] else 0
    },numeric(1))
    included <- if(i<=nrow(sources)) sources$include_budget[i]==1 && sources$active[i]==1 else oneoffs$include_budget[i-nrow(sources)]==1
    pieces[[i]] <- data.frame(month=months,date=flows$date,source=all_labels[i],
                              contribution=opening+cumsum(contribution),included=included)
  }
  by_source <- if(length(pieces))do.call(rbind,pieces) else data.frame(date=as.Date(character()),source=character(),contribution=numeric(),included=logical())
  list(monthly=flows,by_source=by_source,opening=opening)
}
