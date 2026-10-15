# Financial calculations: purely illustrative planning estimates, not personal advice.
month_dates <- function(month) {
  first <- as.Date(paste0(month,"-01"))
  if (is.na(first)) stop("Invalid month")
  after <- seq(first, by="month", length.out=2)[2]
  list(first=first,after=after,last=after-1)
}
month_transactions <- function(con,month) {
  d <- month_dates(month)
  DBI::dbGetQuery(con,"SELECT * FROM transactions WHERE txn_date >= ? AND txn_date < ? ORDER BY txn_date DESC,id DESC",params=list(as.character(d$first),as.character(d$after)))
}
month_budgets <- function(con, month) {
  cats <- get_categories(con)
  overrides <- DBI::dbGetQuery(con,"SELECT category_id,amount FROM category_budgets WHERE month=?",params=list(month))
  idx <- match(cats$id,overrides$category_id)
  cats$budget <- ifelse(is.na(idx),cats$default_budget,overrides$amount[idx])
  cats$budget[is.na(cats$budget)] <- 0
  cats
}

monthly_summary <- function(con,month,now=Sys.Date(),debt_method="minimums") {
  tx <- month_transactions(con,month)
  budgets <- month_budgets(con,month)
  sources <- get_sources(con)
  debts <- get_debts(con)
  extras <- get_debt_extras(con)
  settings <- get_settings(con)
  funds <- get_funds(con)
  events <- get_month_events(con, month)
  d <- month_dates(month)
  mode <- if (d$last < now) "past" else if (d$first > now) "future" else "current"
  allocations <- loan_cash_allocations(con,month)
  lent_all <- sum(allocations$lent)
  loan_collected <- sum(allocations$returned)
  # On partly allocated bank transactions, only the non-loan portion counts as
  # household spending/earned income. Fully allocated loans use dedicated types.
  tx$lent_allocated <- allocations$lent[match(tx$id,allocations$id)]
  tx$returned_allocated <- allocations$returned[match(tx$id,allocations$id)]
  tx$lent_allocated[is.na(tx$lent_allocated)] <- 0
  tx$returned_allocated[is.na(tx$returned_allocated)] <- 0
  paid_income <- if(nrow(tx))sum(pmax(0,tx$amount[tx$flow_type=='income']-tx$returned_allocated[tx$flow_type=='income'])) else 0
  # Forecast checkbox controls the real spending budget as well: receipts already
  # assigned to an excluded source remain visible in history but are not spendable.
  allowed_ids <- sources$id[sources$include_budget==1]
  included_income_tx <- if(nrow(tx)) tx$flow_type=='income' &
    (is.na(tx$income_source_id) | tx$income_source_id %in% allowed_ids) else logical()
  budget_income_received <- if(nrow(tx))sum(pmax(0,tx$amount[included_income_tx]-tx$returned_allocated[included_income_tx]),na.rm=TRUE) else 0
  spent <- if (nrow(tx)) sum(pmax(0,-tx$amount[tx$flow_type=="expense"]-tx$lent_allocated[tx$flow_type=="expense"]),na.rm=TRUE) else 0
  cash_withdrawals <- if(nrow(tx))sum(pmax(0,-tx$amount[tx$flow_type=="withdrawal"]),na.rm=TRUE) else 0
  debt_paid <- if (nrow(tx)) sum(pmax(-tx$amount[tx$flow_type=="debt"],0),na.rm=TRUE) else 0
  unplanned <- if (nrow(tx)) {
    ix<-tx$flow_type=='income' & is.na(tx$income_source_id)
    sum(pmax(0,tx$amount[ix]-tx$returned_allocated[ix]),na.rm=TRUE)
  } else 0
  expected <- 0
  source_details <- sources
  source_details$received <- numeric(nrow(sources))
  source_details$expected <- numeric(nrow(sources))
  source_details$plan_low <- numeric(nrow(sources))
  source_details$plan_expected <- numeric(nrow(sources))
  source_details$plan_high <- numeric(nrow(sources))
  settings_basis <- settings$variable_budget_basis[1]
  for (i in seq_len(nrow(sources))) {
    id <- sources$id[i]
    rec <- if (nrow(tx)) sum(pmax(0,tx$amount[tx$flow_type=='income' & !is.na(tx$income_source_id) & tx$income_source_id==id]-tx$returned_allocated[tx$flow_type=='income' & !is.na(tx$income_source_id) & tx$income_source_id==id]),na.rm=TRUE) else 0
    source_details$received[i] <- rec
    inside <- month>=sources$start_month[i] && (is.na(sources$end_month[i]) || month<=sources$end_month[i])
    active <- sources$active[i]==1 && sources$include_budget[i]==1 && inside
    source_details$plan_low[i] <- if(active) sources$min_amount[i] else 0
    source_details$plan_expected[i] <- if(active) sources$monthly_amount[i] else 0
    source_details$plan_high[i] <- if(active) sources$max_amount[i] else 0
    eligible <- mode=='future' || (mode=='current' && min(sources$pay_day[i],as.integer(format(d$last,'%d'))) >= as.integer(format(now,'%d')))
    budget_amount <- if(sources$kind[i]=='fixed' || settings_basis=='expected') sources$monthly_amount[i] else sources$min_amount[i]
    source_details$expected[i] <- if (active && eligible) max(0,budget_amount-rec) else 0
    expected <- expected+source_details$expected[i]
  }
  # One-offs are separate from recurring incomes. They are only forecast until
  # marked received; actual imported/manual income is already counted above.
  oneoffs <- get_oneoffs(con)
  expected_once <- 0
  if(mode!='past' && nrow(oneoffs)) {
    once <- oneoffs[oneoffs$month==month & oneoffs$received==0 & oneoffs$include_budget==1,,drop=FALSE]
    if(nrow(once)) {
      scenario_amount <- if(settings_basis=='expected')once$expected else once$low
      expected_once <- sum(ifelse(once$certainty=='confirmed',once$expected,scenario_amount))
    }
  }
  expected <- expected+expected_once
  unpaid_fixed <- 0
  budgets$spent <- numeric(nrow(budgets)); budgets$unpaid <- numeric(nrow(budgets))
  for (i in seq_len(nrow(budgets))) {
    already <- if (nrow(tx)) sum(pmax(0,-tx$amount[tx$flow_type=="expense" & !is.na(tx$category_id) & tx$category_id==budgets$id[i]]-tx$lent_allocated[tx$flow_type=="expense" & !is.na(tx$category_id) & tx$category_id==budgets$id[i]]),na.rm=TRUE) else 0
    budgets$spent[i] <- already
    budgets$unpaid[i] <- if (budgets$is_fixed[i]==1 && mode!="past") max(0,budgets$budget[i]-already) else 0
    unpaid_fixed <- unpaid_fixed+budgets$unpaid[i]
  }
  # Future periods use the real payoff forecast, not today's loan minimums
  # forever. For the active month, paid bank transactions remain authoritative.
  future_debt <- NULL
  future_debt_step <- NA_integer_
  if (mode=='future' && nrow(debts)) {
    future_debt <- simulate_payoff(debts,method=debt_method,
      lump_sums=extras,start_month=format(Sys.Date(),'%Y-%m'))
    today_m <- as.integer(format(Sys.Date(),'%Y'))*12L+as.integer(format(Sys.Date(),'%m'))
    target_m <- as.integer(substr(month,1,4))*12L+as.integer(substr(month,6,7))
    future_debt_step <- target_m-today_m+1L
  }
  debts$paid <- numeric(nrow(debts)); debts$due_remaining <- numeric(nrow(debts))
  for (i in seq_len(nrow(debts))) {
    already <- if (nrow(tx)) sum(pmax(-tx$amount[tx$flow_type=="debt" & !is.na(tx$debt_id) & tx$debt_id==debts$id[i]],0),na.rm=TRUE) else 0
    debts$paid[i] <- already
    modeled_minimum <- debts$min_payment[i]
    if(mode=='future' && !is.null(future_debt)) {
      future_row <- if(nrow(future_debt$by_debt_history))
        future_debt$by_debt_history[future_debt$by_debt_history$debt_id==debts$id[i] &
                                      future_debt$by_debt_history$month==future_debt_step,,drop=FALSE] else data.frame()
      modeled_minimum <- if(nrow(future_row))
        max(0,future_row$paid[1]-future_row$extra_paid[1]) else 0
    }
    debts$due_remaining[i] <- if (debts$active[i]==1 && mode!='past') max(0,modeled_minimum-already) else 0
  }
  unpaid_debts <- sum(debts$due_remaining)
  # Scheduled one-off loan repayments reserve additional cash only while unpaid.
  # Extra debt payments already recorded this month offset the plan above the
  # contractual minimum, preventing double counting when transactions are linked.
  debts$unpaid_extra <- numeric(nrow(debts))
  for (i in seq_len(nrow(debts))) {
    scheduled <- if (nrow(extras)) sum(extras$amount[
      extras$debt_id==debts$id[i] & extras$month==month & extras$completed==0
    ],na.rm=TRUE) else 0
    if(mode=='future' && !is.null(future_debt)) {
      future_row <- if(nrow(future_debt$by_debt_history))
        future_debt$by_debt_history[future_debt$by_debt_history$debt_id==debts$id[i] &
                                      future_debt$by_debt_history$month==future_debt_step,,drop=FALSE] else data.frame()
      scheduled <- if(nrow(future_row))min(scheduled,future_row$extra_paid[1]) else 0
    }
    above_min <- max(0,debts$paid[i]-debts$min_payment[i])
    debts$unpaid_extra[i] <- if(debts$active[i]==1 && debts$balance[i]>0 && mode!="past")
      max(0,scheduled-above_min) else 0
  }
  unpaid_extra_debts <- sum(debts$unpaid_extra)
  remaining_savings <- if (mode=="past") 0 else settings$savings_target
  buffer <- if (mode=="past") 0 else settings$safety_buffer
  reserve_funds <- if (mode=="past") 0 else fund_monthly_contribution(funds)
  goal_reserve <- if (mode=="past") 0 else goal_monthly_allocation(get_goals(con),month)
  # Planned one-offs must be marked completed after appearing in bank transactions.
  # Expected one-off income is excluded until received (conservative planning).
  unpaid_events <- if (mode=="past") 0 else sum(events$amount[events$kind=="expense" & events$completed==0])
  monthly_room <- budget_income_received + loan_collected + expected - spent - lent_all - debt_paid - unpaid_fixed - unpaid_debts - unpaid_extra_debts - remaining_savings - buffer - reserve_funds - goal_reserve - unpaid_events
  cash_cap <- NA_real_
  if (!is.na(settings$available_cash) && mode=="current") {
    # available_cash is a current snapshot that already reflects lending and
    # collected repayments; do NOT add cash flows a second time.
    cash_cap <- settings$available_cash + expected - unpaid_fixed - unpaid_debts - unpaid_extra_debts - remaining_savings - buffer - reserve_funds - goal_reserve - unpaid_events
  }
  available <- if (is.na(cash_cap)) monthly_room else min(monthly_room,cash_cap)
  # Category caps cannot exceed their remaining budgets (or historical fallback),
  # nor the total actually projected free cash for the month.
  variable <- budgets[budgets$is_fixed==0,,drop=FALSE]
  history <- get_transactions(con)
  history <- history[history$flow_type=="expense" & history$txn_date < as.character(d$first),,drop=FALSE]
  if(nrow(history)) {
    halloc <- DBI::dbGetQuery(con,'SELECT linked_txn_id AS id,amount AS lent FROM money_lent WHERE linked_txn_id IS NOT NULL')
    history$loan_portion <- halloc$lent[match(history$id,halloc$id)]
    history$loan_portion[is.na(history$loan_portion)]<-0
  }
  if (nrow(history)) {
    history$month <- substr(history$txn_date,1,7)
    months <- tail(sort(unique(history$month)),3)
    history <- history[history$month %in% months,,drop=FALSE]
  }
  variable$baseline <- variable$budget
  variable$basis <- ifelse(variable$budget>0,"Monthly plan","No baseline")
  for (i in seq_len(nrow(variable))) {
    if (variable$budget[i]>0 || !nrow(history)) next
    entries <- history[!is.na(history$category_id) & history$category_id==variable$id[i],,drop=FALSE]
    if (nrow(entries)) {
      monthly <- aggregate(pmax(0,-entries$amount-entries$loan_portion),by=list(entries$month),FUN=sum)
      variable$baseline[i] <- median(monthly$x)
      variable$basis[i] <- "Historical median (observed months)"
    }
  }
  variable$remaining_plan <- pmax(0,variable$baseline-variable$spent)
  pool <- max(0,available)
  denom <- sum(variable$remaining_plan)
  variable$suggested_rest <- if (denom>0) variable$remaining_plan*min(1,pool/denom) else rep(0,nrow(variable))
  remaining_days <- if (mode=="current") as.integer(d$last-now)+1 else if (mode=="future") as.integer(d$last-d$first)+1 else 0L
  list(month=month,mode=mode,tx=tx,budgets=budgets,variable=variable,sources=source_details,debts=debts,actual_income=paid_income,budget_income_received=budget_income_received,unplanned=unplanned,expected_income=expected,expected_oneoff=expected_once,loan_lent=lent_all,loan_collected=loan_collected,withdrawn=cash_withdrawals,receivable_due=receivable_due_by_month(con,month),expenses=spent,debt_paid=debt_paid,unpaid_fixed=unpaid_fixed,unpaid_debts=unpaid_debts,unpaid_extra_debts=unpaid_extra_debts,savings=remaining_savings,buffer=buffer,reserve_funds=reserve_funds,goal_reserve=goal_reserve,unpaid_events=unpaid_events,monthly_room=monthly_room,cash_cap=cash_cap,available=max(0,available),shortfall=min(0,available),days=remaining_days)
}

# A per-debt monthly amortization forecast. The first simulation month begins
# from the CURRENT outstanding balances (not original loan principal).
# Assumptions: APR/12 monthly interest, payment after interest, no fees or
# rate changes. One-off extra payments belong to the selected debt only.
# A rollover strategy uses ONLY the freed minimum-payment budget across debts;
# lump-sum amounts are NOT redirected to a different debt if they overpay.
simulate_payoff <- function(debts, extra=0, method="minimums", max_months=600,
                            lump_sums=NULL, start_month=format(Sys.Date(), "%Y-%m")) {
  method <- match.arg(method, c("minimums", "avalanche", "snowball"))
  if (length(start_month)!=1L || is.na(start_month) ||
      !grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", start_month))
    stop("Invalid forecast start month")
  if (!is.finite(extra) || extra<0 || max_months<1) stop("Invalid repayment options")
  cols <- c("id","name","balance","apr","min_payment")
  d <- debts[!is.na(debts$active) & debts$active==1 &
               !is.na(debts$balance) & debts$balance>0, cols, drop=FALSE]
  empty <- list(history=data.frame(), by_debt_history=data.frame(), by_debt=data.frame(),
                months=0L, interest=0, paid=0, finished=TRUE, extra_paid=0,
                unused_extras=0, start_month=start_month)
  if (!nrow(d)) return(empty)
  if (any(!is.finite(as.matrix(d[,c("balance","apr","min_payment")]))) ||
      any(d$apr<0) || any(d$min_payment<0)) stop("Invalid debt input")
  n <- nrow(d)
  start_balance <- d$balance
  interest_by_debt <- numeric(n)
  paid_by_debt <- numeric(n)
  extra_by_debt <- numeric(n)
  paid_off <- rep(NA_character_,n)
  history <- vector("list",max_months+1L)
  details <- vector("list",max_months+1L)
  dates <- seq(as.Date(paste0(start_month,"-01")),by="month",length.out=max_months+1L)
  history[[1]] <- data.frame(month=0L, date=dates[1],outstanding=sum(d$balance),
                              interest=0,payment=0,extra_paid=0)
  details[[1]] <- data.frame(month=0L,date=dates[1],debt_id=d$id,debt=d$name,
                              outstanding=d$balance,interest_month=0,
                              interest_total=0,paid=0,extra_paid=0)
  if (is.null(lump_sums))
    lump_sums <- data.frame(debt_id=integer(),month=character(),amount=numeric())
  if (nrow(lump_sums) && "completed" %in% names(lump_sums))
    lump_sums <- lump_sums[!is.na(lump_sums$completed) & lump_sums$completed==0,,drop=FALSE]
  if (nrow(lump_sums) &&
      (!all(c("debt_id","month","amount") %in% names(lump_sums)) ||
       any(!is.finite(lump_sums$amount)) || any(lump_sums$amount<=0)))
    stop("Invalid scheduled extra payments")
  total_minimum_budget <- sum(d$min_payment)+extra
  total_paid <- 0
  unused_extras <- 0
  finished <- FALSE
  last_step <- 0L
  for (step in seq_len(max_months)) {
    month_label <- format(dates[step],"%Y-%m")
    month_interest <- d$balance*d$apr/1200
    if (any(!is.finite(month_interest))) break
    d$balance <- d$balance+month_interest
    interest_by_debt <- interest_by_debt+month_interest
    # First satisfy each loan's contractual minimum, capped by amount due.
    min_paid <- pmin(d$balance,d$min_payment)
    d$balance <- pmax(0,d$balance-min_paid)
    # Apply only the lump sums specifically designated for this loan/month.
    scheduled <- numeric(n)
    if (nrow(lump_sums)) {
      matching <- lump_sums[lump_sums$month==month_label,,drop=FALSE]
      if (nrow(matching)) for (i in seq_len(n))
        scheduled[i] <- sum(matching$amount[matching$debt_id==d$id[i]])
    }
    extra_paid <- pmin(d$balance,scheduled)
    unused_extras <- unused_extras+sum(scheduled-extra_paid)
    d$balance <- pmax(0,d$balance-extra_paid)
    redirected_paid <- numeric(n)
    # Only snowball/avalanche move paid-off debts' minimums to other loans.
    if (method!="minimums") {
      room <- max(0,total_minimum_budget-sum(min_paid))
      idx <- if (method=="snowball") order(d$balance,-d$apr) else order(-d$apr,d$balance)
      for (k in idx) {
        allocated <- min(room,d$balance[k])
        redirected_paid[k] <- allocated
        d$balance[k] <- d$balance[k]-allocated
        room <- room-allocated
        if (room<0.005) break
      }
    }
    payments <- min_paid+extra_paid+redirected_paid
    paid_by_debt <- paid_by_debt+payments
    extra_by_debt <- extra_by_debt+extra_paid
    total_paid <- total_paid+sum(payments)
    newly_paid <- is.na(paid_off) & d$balance<0.005
    paid_off[newly_paid] <- month_label
    d$balance[d$balance<0.005] <- 0
    last_step <- step
    history[[step+1L]] <- data.frame(month=step,date=dates[step+1L],
      outstanding=sum(d$balance), interest=sum(interest_by_debt),
      payment=sum(payments), extra_paid=sum(extra_paid))
    details[[step+1L]] <- data.frame(month=step,date=dates[step+1L],
      debt_id=d$id,debt=d$name,outstanding=d$balance,
      interest_month=month_interest,interest_total=interest_by_debt,
      paid=payments,extra_paid=extra_paid)
    if (sum(d$balance)<0.005) {finished <- TRUE; break}
  }
  summary <- data.frame(debt_id=d$id,debt=d$name,
    starting_balance=start_balance,apr=d$apr,minimum=d$min_payment,
    interest=interest_by_debt,paid=paid_by_debt,extra_paid=extra_by_debt,
    remaining=d$balance,paid_off_month=paid_off,
    status=ifelse(is.na(paid_off),"Not repaid within forecast","Projected paid off"),
    stringsAsFactors=FALSE)
  list(history=do.call(rbind,history[seq_len(last_step+1L)]),
       by_debt_history=do.call(rbind,details[seq_len(last_step+1L)]),
       by_debt=summary,months=if(finished)last_step else NA_integer_,
       interest=sum(interest_by_debt),paid=total_paid,finished=finished,
       extra_paid=sum(extra_by_debt),unused_extras=unused_extras,
       start_month=start_month)
}

format_czk <- function(value) {
  if (!length(value) || is.na(value)) return("—")
  paste0(format(round(value),big.mark=" ",decimal.mark=",",scientific=FALSE,trim=TRUE)," Kč")
}

summary_export <- function(summary) {
  data.frame(metric=c("Actual income","Additional unplanned income","Expected regular income","Expenses already paid","Debt payments already paid","Remaining fixed bills","Remaining debt minimums","Unpaid planned extra loan payments","Savings target","Safety buffer","Monthly sinking contributions","Planned one-off expenses","Free monthly capacity","Suggested available spending"),czk=c(summary$actual_income,summary$unplanned,summary$expected_income,summary$expenses,summary$debt_paid,summary$unpaid_fixed,summary$unpaid_debts,summary$unpaid_extra_debts,summary$savings,summary$buffer,summary$reserve_funds,summary$unpaid_events,summary$monthly_room,summary$available))
}
