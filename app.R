# BudgetForge — Supabase account isolation (v5 compatibility bridge)
options(shiny.maxRequestSize = 50 * 1024^2)
library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(DT)
library(httr2)
library(getPass)
source("R/database.R", encoding="UTF-8")
source("R/planning.R", encoding="UTF-8")
source("R/importers.R", encoding="UTF-8")
source("R/engine.R", encoding="UTF-8")
source("R/income.R", encoding="UTF-8")
source("R/receivables.R", encoding="UTF-8")
source("R/finance_extensions.R", encoding="UTF-8")
source("R/supabase_snapshot.R", encoding="UTF-8")

fmt <- function(x) format_czk(x) # server overrides with chosen currency
opt_ids <- function(df, prefix="— Select —") {
  labels <- if('name' %in% names(df)) df$name else if('borrower' %in% names(df)) df$borrower else as.character(df$id)
  c(setNames("new",prefix),setNames(as.character(df$id),labels))
}
month_options <- function(start=NULL,end=NULL) {
  if(is.null(start)||is.null(end)) {
    start <- format(Sys.Date(),'%Y-%m')
    end <- format(seq(as.Date(paste0(start,'-01')),by='month',length.out=25)[25],'%Y-%m')
  }
  month_sequence(start,end)
}

metric <- function(title, value, note="", accent=FALSE) {
  div(class=paste("metric",if (accent) "accent" else ""),div(class="metric-label",title),
      div(class="metric-value",value),div(class="metric-note",note))
}
section <- function(title, text=NULL) div(class="section-heading",h3(title),if (!is.null(text)) p(text))

css <- '
  body {background:#f6f8fb; color:#182a40;}
  .navbar {box-shadow:0 2px 12px rgba(20,40,70,.06);}
  .navbar-brand {font-weight:850;letter-spacing:-.5px;}
  .page-layout {max-width:1380px;margin:0 auto;}
  .toolbar {background:#fff;border-bottom:1px solid #e4ebf4;padding:13px 22px;display:flex;justify-content:space-between;align-items:center;gap:24px;flex-wrap:wrap;}
  .toolbar .form-group {margin:0;min-width:180px;}
  .eyebrow {font-size:11px;letter-spacing:1.6px;text-transform:uppercase;color:#56708b;font-weight:750;}
  .toolbar-note {font-size:13px;color:#526c85;}
  .tab-content {padding:24px 22px 60px;max-width:1380px;margin:auto;}
  h2 {font-size:1.6rem;font-weight:820;letter-spacing:-.6px;margin-top:0;}
  h3 {font-weight:750;letter-spacing:-.3px;}
  .section-heading {margin-bottom:18px;}
  .section-heading p {color:#61748a;margin:0;}
  .metric {padding:22px;border:1px solid #e5edf6; border-radius:18px;background:#fff; min-height:146px;margin-bottom:18px;box-shadow:0 6px 22px rgba(20,40,70,.03);}
  .metric.accent {background:linear-gradient(135deg,#153d60,#146b79);color:#fff;border-color:transparent;}
  .metric-label {font-weight:680;font-size:13px;opacity:.82;}
  .metric-value {font-size:32px;letter-spacing:-1px;font-weight:850;margin:11px 0 4px;}
  .metric-note {font-size:12px;opacity:.78;}
  .panel-box {padding:23px;margin-bottom:20px;background:#fff;border:1px solid #e5edf6;border-radius:18px;box-shadow:0 6px 22px rgba(20,40,70,.03);}
  .panel-box h3 {font-size:18px;margin:0 0 17px;}
  .hint {color:#65778b;font-size:13px;line-height:1.55;}
  .help-tip {background:#eef6fa;border-left:4px solid #4c94ae;padding:12px 16px;border-radius:8px;color:#24445b;margin-bottom:16px;}
  .positive {color:#0d806b;} .negative {color:#bd4055;}
  .btn-primary {background:#176b80;border-color:#176b80;}
  .btn-primary:hover {background:#105567;border-color:#105567;}
  .form-control {border-radius:9px;}
  .tab-pane > .row {margin-bottom:8px;}
  .table {font-size:13px;}
  .progress {background:#eff3f7;border-radius:7px;}
  .badge-soft {font-size:12px;background:#e9f5f2;color:#116853;padding:6px 10px;border-radius:20px;font-weight:700;}
  .bf-auth-backdrop {position:fixed;z-index:30000;inset:0;overflow:auto;display:flex;
    justify-content:center;align-items:center;padding:25px;background:#f1f6fa;}
  .bf-auth-card {width:min(470px,100%);padding:34px;background:white;border-radius:20px;
    box-shadow:0 12px 45px rgba(9,31,59,.15);}
  @media(max-width:700px){.tab-content{padding:18px 12px 35px}.metric-value{font-size:26px}.toolbar{padding:12px}.panel-box{padding:17px}}
'

ui <- bslib::page_navbar(
  title=tagList(HTML('<svg width="25" height="25" viewBox="0 0 64 64" style="vertical-align:-5px;margin-right:7px" aria-hidden="true"><path fill="currentColor" d="M4 15h56v7H48c-1 7-7 12-14 13v8h14v7H16v-7h14zM12 52h40v6H12z"/></svg>'), "BudgetForge"),
  theme=bslib::bs_theme(version=5,bootswatch="flatly",primary="#176b80"),
  header=tagList(tags$head(tags$style(HTML(css)),tags$script(src="translations.js")), div(class="toolbar",
             div(span(class="eyebrow","Shape your financial future"),div(class="toolbar-note","Know where your money goes. Make every plan count.")),
             uiOutput("bf_auth_status"),
             selectInput("month","Active budget month",choices=month_options(),selected=format(Sys.Date(),"%Y-%m"),width="200px"))),
  nav_panel("Overview",value="overview",
    section("Your monthly command center","Actual transactions, future commitments, and realistic discretionary spending in one place."),
    uiOutput("overview_kpis"),
    div(class="panel-box",h3("Planned vs actual — cash flow by month"),
      p(class="hint","Planning and actuals compare inflows, outflows and remainder. Actuals are shown only where transaction data exists; internal transfers and reserve allocations are not living expenses."),
      plotOutput("cashflow_comparison",height="335px")),
    div(class="panel-box",h3("Financial health"),
      uiOutput("financial_health"),
      p(class="hint","Net worth uses account balance snapshots + other assets + money owed to you − debts. Savings goals and sinking funds are earmarked subsets of existing cash, not additional assets.")),
    div(class="panel-box",h3("Net worth path — illustrative"),
      plotOutput("net_worth_projection",height="280px"),
      p(class="hint","Starts from the opening net worth you supply. Projections adjust for income, planned consumption and estimated interest, not transfers between accounts or payments of debt principal. They are scenario estimates, not a historical statement.")),
    uiOutput("overview_warnings"),
    fluidRow(
      column(7,div(class="panel-box",h3("Spending by category"),plotOutput("spend_plot",height="320px"))),
      column(5,div(class="panel-box",h3("Your financial runway"),uiOutput("runway")))),
    div(class="panel-box",h3("Recommended spending for the rest of the month"),
        p(class="hint","Recommendations respect remaining category budgets and your other commitments. They are not a guarantee that cash is available today."),DTOutput("recommended_table")),
    uiOutput("overview_next_move"),
    div(class="panel-box",h3("Where your money goes"),DTOutput("overview_category_table"))),
  nav_panel("Transactions",value="transactions",
    section("Transactions & automatic categorization","Import Fio + ČSAS exports, correct classifications, and teach BudgetForge your own keyword rules."),
    fluidRow(
      column(5,div(class="panel-box",h3("Import bank statements"),fileInput("bank_files","Choose CSV files",accept=c(".csv","text/csv"),multiple=TRUE),
         actionButton("import_banks","Import & categorize",class="btn-primary"),p(class="hint","Duplicate transactions are skipped on repeat imports. Imports go to your private signed-in account; please test with fictional statements first."),textOutput("import_status"))),
      column(7,div(class="panel-box",h3("Add a cash transaction or unexpected income"),
        fluidRow(column(5,dateInput("manual_date","Date",value=Sys.Date())),column(7,selectInput("manual_flow","Type",choices=c("Expense"="expense","Unexpected income"="income","Debt payment"="debt","ATM withdrawal"="withdrawal","Account transfer"="transfer")))),
        textInput("manual_desc","Description","Cash payment / bonus"),numericInput("manual_amount","Amount in CZK (positive)",value=0,min=0,step=100),
        uiOutput("manual_target_ui"),actionButton("add_manual","Add transaction",class="btn-primary"),
        p(class="hint","Only add cash or missing transactions. If the same item also appears in a later bank CSV, delete the manual entry to avoid double-counting.")))),
    div(class="panel-box",h3("Transaction history"),
      p(class="hint","Choose a row to reclassify it below. 'Unassigned' spending affects your total but cannot be recommended per category until categorized."),
      DTOutput("tx_table"),
      p(class="hint","To track a personal loan, select an outgoing bank transaction and use the Money lent tab to record its borrowed amount and expected return date. ATM withdrawals are transfers to cash, not consumption."),
      fluidRow(column(4,selectInput("review_flow","Transaction type",choices=c("Expense"="expense","Income"="income","Debt payment"="debt","ATM withdrawal"="withdrawal","Internal transfer"="transfer"))),
               column(4,uiOutput("review_target_ui")),
               column(4,div(style="margin-top:25px",actionButton("save_classification","Save classification",class="btn-primary")))),
      fluidRow(column(8,textInput("rule_keyword","Optional merchant keyword to remember (literal text)",placeholder="e.g. LIDL, CZ RENT or PAYROLL")),
               column(4,div(style="margin-top:25px",actionButton("save_rule","Save keyword rule")))),
      actionButton("reapply_rules","Reapply rules to unreviewed transactions"),
      actionButton("delete_manual","Delete selected manual entry")),
    div(class="panel-box",h3("My learned rules"),DTOutput("rules_table"),
        actionButton("delete_rule","Delete selected custom rule"))),
  nav_panel("Categories",value="categories",
    section("Build a budget you can stick to","Keep necessities, flexible spending, and your own custom categories separate."),
    fluidRow(column(5,div(class="panel-box",h3("Create or edit a category"),
        selectInput("cat_choice","Category",choices=""),
        textInput("cat_name","Name"),checkboxInput("cat_fixed","Fixed / committed expense",FALSE),
        numericInput("cat_budget","Default monthly budget (CZK)",value=0,min=0,step=100),
        actionButton("save_cat","Save category",class="btn-primary"),
        tags$hr(),h4("Override the selected month"),
        numericInput("cat_override","Budget for active month",value=0,min=0,step=100),
        actionButton("save_override","Save monthly override"),
        p(class="hint","Future months keep the default unless you change them. Category budgets cover actual expenses; do not budget the same annual bill a second time if it is funded separately."))),
      column(7,div(class="panel-box",h3("Category budgets and spending"),DTOutput("category_editor_table"))))),
  nav_panel("Income planner",value="income",
    section("Income planner","Fixed salaries, freelance ranges, contract guarantees and once-only payments—with an income-only equity scenario chart."),
    div(class="help-tip",strong("Income forecast, not a promise: "),
      "Freelance ranges are assumptions, not statistical confidence intervals. Only an income you check as 'Include in real budget' enters spending recommendations. Contracts stop contributing after their confirmed end month."),
    uiOutput("income_kpis"),
    fluidRow(
      column(5,div(class="panel-box",h3("Recurring income sources"),
        selectInput("source_choice","Income source",choices=""),
        textInput("source_name","Name",placeholder="Salary / freelance project / rental income"),
        selectInput("source_kind","Income type",choices=c("Fixed / salary"="fixed","Variable / freelance"="variable")),
        numericInput("source_amount","Expected amount per month (CZK)",value=0,min=0,step=1000),
        conditionalPanel("input.source_kind == 'variable'",
          numericInput("source_low","Minimum scenario / month (CZK)",value=0,min=0,step=500),
          numericInput("source_high","Maximum scenario / month (CZK)",value=0,min=0,step=500),
          p(class="hint","Expected may be anywhere within your minimum–maximum range. No probability distribution is assumed.")),
        numericInput("source_day","Expected pay day",value=25,min=1,max=31),
        dateInput("source_start","Source starts",value=Sys.Date(),format="dd/mm/yyyy",startview="year"),
        checkboxInput("source_has_end","Known contract / guarantee end date",FALSE),
        conditionalPanel("input.source_has_end",
          dateInput("source_end","Confirmed through",value=seq(Sys.Date(),by='year',length.out=3)[3],format="dd/mm/yyyy",startview="year")),
        checkboxInput("source_active","Active source",TRUE),
        checkboxInput("source_include","Include this income in the real budget",TRUE),
        actionButton("save_source","Save income source",class="btn-primary"),
        actionButton("new_source","+ New source"),
        textOutput("source_status"),
        p(class="hint","A fixed contract stops counting after its confirmed-through date. Unknown renewals are not projected. Imported receipts matched to this source reduce the unpaid amount for the active month."))),
      column(7,div(class="panel-box",h3("Sources & active-month receipts"),DTOutput("income_table"),
        tags$hr(),h4("Budget policy for uncertain income"),
        selectInput("income_budget_policy","Amount to count for checked variable incomes",
          choices=c("Minimum scenario (conservative)"="low","Expected scenario (less conservative)"="expected")),
        actionButton("save_income_policy","Save budgeting policy"),
        p(class="hint","Unchecked sources remain visible in the editor but contribute zero to budget totals and the income forecast until selected.")))),
    fluidRow(
      column(5,div(class="panel-box",h3("One-time expected income"),
        selectInput("once_choice","Select planned income",choices=""),
        textInput("once_name","Payment description",placeholder="Project invoice / annual bonus / gift"),
        dateInput("once_date","Expected month",value=Sys.Date(),format="mm/yyyy",startview="year"),
        selectInput("once_certainty","How certain is it?",choices=c("Confirmed amount"="confirmed","Variable / possible amount"="uncertain")),
        numericInput("once_expected","Expected amount (CZK)",value=0,min=0,step=1000),
        conditionalPanel("input.once_certainty == 'uncertain'",
          numericInput("once_low","Minimum scenario (CZK)",value=0,min=0,step=500),
          numericInput("once_high","Maximum scenario (CZK)",value=0,min=0,step=500)),
        checkboxInput("once_include","Include in real budget / forecast",FALSE),
        checkboxInput("once_received","Already received (exclude from future forecasts)",FALSE),
        actionButton("save_once","Save one-time income",class="btn-primary"),
        actionButton("new_once","+ New one-time income"),
        actionButton("delete_once","Delete selected"),
        p(class="hint","Mark received when payment is recorded as a transaction. Never count a planned one-off and the matching bank transaction as two inflows."))),
      column(7,div(class="panel-box",h3("One-time income calendar"),DTOutput("oneoff_table"),
        p(class="hint","One-time income appears in its selected month only; if uncertain, it contributes to the low / expected / high paths but not to guaranteed income.")))),
    div(class="panel-box",h3("How each source contributes over time"),
      p(class="hint","The colored thin lines show opening equity plus each included source's individual cumulative contribution; thick lines show total opening equity plus all included income. The shaded area spans user-entered low/high outcomes. Expenses, taxes, debts and investment returns are NOT deducted here."),
      plotOutput("income_projection_plot",height="445px"),
      DTOutput("income_projection_table"))
  ),
  nav_panel("Debts",value="debts",
    section("Manage debts & repayments","Each debt retains its own forecast, interest total and occasional lump-sum payments."),
    fluidRow(
      column(5,div(class="panel-box",h3("Manage each debt"),
        selectInput("debt_choice","Debt",choices=""),textInput("debt_name","Name"),
        numericInput("debt_balance","Current outstanding balance (CZK)",value=0,min=0,step=1000),
        numericInput("debt_apr","Annual interest rate (%)",value=0,min=0,step=.1),
        numericInput("debt_min","Monthly minimum payment (CZK)",value=0,min=0,step=100),
        numericInput("debt_day","Payment due day",value=20,min=1,max=31),
        checkboxInput("debt_active","Active",TRUE),
        actionButton("save_debt","Save debt",class="btn-primary"),
        actionButton("new_debt","+ Create another"),
        p(class="hint","Enter the CURRENT balance for each debt, not its original loan value. Edit the balance after actual repayments; bank transactions do not automatically reconcile principal and interest."),
        DTOutput("debts_table"))),
      column(7,div(class="panel-box",h3("Debt-free forecast"),
        radioButtons("payoff_method","When a loan is paid off, what happens to its monthly minimum?",
          choices=c("Stop paying that minimum"="minimums",
                    "Redirect to highest-APR debt"="avalanche",
                    "Redirect to smallest balance"="snowball"),
          selected="minimums"),
        p(class="hint","No extra payment is assumed every month. Add individual one-off payments below. The chart updates whenever you save a change."),
        uiOutput("payoff_result"),
        checkboxInput("debt_full_curve","Show complete debt payoff forecast (ignore calendar window)",FALSE),
        plotOutput("payoff_plot",height="350px"),
        h4("Projected interest by individual debt"),
        DTOutput("debt_interest_table"))),
    fluidRow(
      column(5,div(class="panel-box",h3("Schedule an occasional extra payment"),
        selectInput("extra_debt_choice","Which debt gets the extra payment?",choices=""),
        dateInput("extra_payment_month","Payment date (forecast groups by month)",value=Sys.Date(),format="mm/yyyy",startview="year"),
        numericInput("extra_payment_amount","Extra lump-sum amount (CZK)",value=1000,min=1,step=1000),
        textInput("extra_payment_note","Note (optional)",placeholder="e.g. Bonus, tax refund"),
        actionButton("save_extra_payment","Save occasional payment",class="btn-primary"),
        actionButton("clear_extra_payment","New payment"),
        p(class="hint","Choose a row on the right to edit it. A planned extra repayment affects the forecast and monthly headroom, but never modifies the stored loan balance until you update it after payment."))),
      column(7,div(class="panel-box",h3("Your occasional payments"),
        DTOutput("extra_payments_table"),
        actionButton("complete_extra_payment","Mark selected as paid"),
        actionButton("undo_extra_payment","Mark unpaid"),
        actionButton("delete_extra_payment","Delete selected"),
        p(class="hint","Once the payment appears in your bank transactions, classify it under that debt, mark the plan as paid, and update the outstanding balance. Paid plans remain visible in the table but are excluded from future forecasts.")))))
  ),
  nav_panel("Money lent",value="receivables",
    section("Money lent to others","Track personal loans as receivables, separately from your debts and living expenses."),
    div(class="help-tip",strong("Accounting rule: "),
      "Money lent is a cash outflow and an asset owed to you, not everyday consumption. Returns are repayments of principal, not salary. Possible future repayments never raise your conservative safe-to-spend limit."),
    uiOutput("loan_kpis"),
    fluidRow(column(5,div(class="panel-box",h3("Record money lent"),
      selectInput("loan_choice","Existing loan (edit its details)",choices=""),
      textInput("loan_borrower","Person / borrower",placeholder="Friend, relative, colleague"),
      numericInput("loan_amount","Amount lent (CZK)",value=1000,min=1,step=100),
      dateInput("loan_date","Date money was lent (taken from bank if linked)",value=Sys.Date()),
      dateInput("loan_due","Expected return date",value=Sys.Date()+30),
      selectInput("loan_bank_tx","Link an imported outgoing transaction (optional)",choices=c("Create a manual cash entry"="manual")),
      checkboxInput("loan_in_forecast","Include possible return in optimistic monthly forecast",TRUE),
      textInput("loan_note","Notes (optional)",placeholder="Agreed repayment arrangement"),
      actionButton("loan_save","Save money lent",class="btn-primary"),
      actionButton("loan_new","New loan"),
      actionButton("loan_delete","Delete selected loan"),
      p(class="hint","For partial bank transactions, enter only the amount lent. The remaining amount stays in its original expense category. When editing a saved loan, only the person, date due, notes and forecast checkbox can be changed."))),
      column(7,div(class="panel-box",h3("People who owe you money"),DTOutput("loan_table"),
        p(class="hint","A return date is a prediction, not guaranteed income. Overdue balances stay outstanding until actually repaid."),
        plotOutput("loan_forecast_plot",height="235px")))),
    fluidRow(column(5,div(class="panel-box",h3("Record a repayment received"),
      selectInput("repay_loan","Which loan is being repaid?",choices=""),
      numericInput("repay_amount","Amount returned (CZK)",value=100,min=1,step=100),
      dateInput("repay_date","Date received (taken from bank if linked)",value=Sys.Date()),
      selectInput("repay_bank_tx","Link an imported incoming transaction (optional)",choices=c("Create a manual cash entry"="manual")),
      textInput("repay_note","Repayment note",placeholder="Cash, bank transfer, instalment"),
      actionButton("repay_save","Record repayment",class="btn-primary"),
      p(class="hint","Partial repayments are supported. Attaching an imported receipt prevents it from being counted twice as earned income."))),
      column(7,div(class="panel-box",h3("Repayment history"),DTOutput("repay_table"))))),
  nav_panel("Savings goals",value="goals",
    section("Savings goals & emergency reserves","Assign monthly savings, track progress and know which reserves can be used during emergencies."),
    div(class="help-tip",strong("Important: "),
       "Money assigned to a goal remains your asset. Contributions reduce free spending capacity but are not living expenses; do not count transfers to your other accounts as new spending. A goal's saved balance is a subset of actual bank balances."),
    uiOutput("goal_kpis"),
    fluidRow(
      column(5,div(class="panel-box",h3("Create or edit a savings goal"),
        selectInput("goal_choice","Savings goal",choices=""),
        textInput("goal_name","Goal name",placeholder="Emergency reserve / new car / travel"),
        numericInput("goal_target","Target amount (CZK)",value=12000,min=0,step=1000),
        numericInput("goal_saved","Already saved (CZK)",value=0,min=0,step=500),
        numericInput("goal_monthly","Monthly allocation (CZK)",value=1000,min=0,step=100),
        checkboxInput("goal_has_date","Set target deadline",FALSE),
        conditionalPanel("input.goal_has_date",dateInput("goal_deadline","Target month",value=Sys.Date()+365,format="mm/yyyy",startview="year")),
        checkboxInput("goal_emergency","Allow this reserve to be used in an emergency",FALSE),
        checkboxInput("goal_active","Include monthly contribution in plan",TRUE),
        actionButton("save_goal","Save savings goal",class="btn-primary"),
        actionButton("new_goal","Create new goal"),
        actionButton("delete_goal","Delete selected goal"))),
      column(7,div(class="panel-box",h3("Goal progress & funding"),
        DTOutput("goals_table"),
        plotOutput("goals_plot",height="290px"),
        p(class="hint","Monthly allocations stop once a goal is funded (based on your last manually entered balance). Update balances whenever you deposit or withdraw money. Savings are not automatically transferred."))))),
  nav_panel("Sinking funds",value="funds",
    section("Make annual bills feel monthly","Set money aside each month for insurance, holidays, car servicing, gifts, and other irregular costs."),
    div(class="help-tip",strong("Example: "),"12,000 Kč annual insurance ÷ 12 = 1,000 Kč/month earmarked. This is a planning envelope, not a bank transfer; update its saved balance when you actually move money."),
    uiOutput("fund_kpis"),
    fluidRow(column(5,div(class="panel-box",h3("Create or edit a fund"),
      selectInput("fund_choice","Fund",choices=""),textInput("fund_name","What are you saving for?","Car services, insurance & registration"),
      numericInput("fund_cost","Cost per billing cycle (CZK)",value=0,min=0,step=1000),
      selectInput("fund_cycle","Billing frequency",choices=c("Monthly"=1,"Quarterly"=3,"Half-yearly"=6,"Yearly"=12),selected=12),
      numericInput("fund_balance","Already set aside (CZK)",value=0,min=0,step=500),
      checkboxInput("fund_active","Include in budget",TRUE),
      actionButton("save_fund","Save sinking fund",class="btn-primary"),
      p(class="hint","Saved balances are manual snapshots, not automatically accumulated. If a funded bill is paid from this envelope, don't also treat the reserve transfer as spending."))),
      column(7,div(class="panel-box",h3("All funding envelopes"),DTOutput("funds_table"),
        p(class="hint","Once a fund is fully covered, BudgetForge pauses its recommended monthly contribution. Surplus can be reassigned by editing balances after you move the actual money. BudgetForge does not move funds."))))),
  nav_panel("Monthly plan",value="forecast",
    section("Monthly plan","Plan expenses, debts, savings goals and reserves over your selected calendar range. Expected income is managed under Income planner."),
    div(class="help-tip","One-off expenses here are EXTRA to recurring category budgets and sinking-fund contributions. Mark them completed after paying them, so they don't get deducted twice from current-month headroom."),
    fluidRow(column(5,div(class="panel-box",h3("Add a planned one-off"),
      selectInput("event_month","Month",choices=month_options(),selected=format(Sys.Date(),"%Y-%m")),
      textInput("event_name","What is happening?","Birthday, car repair, holiday bonus"),
      selectInput("event_kind","Money flow",choices=c("Extra expense"="expense")),
      numericInput("event_amount","Amount (CZK)",value=0,min=0,step=500),
      actionButton("save_event","Add to plan",class="btn-primary"),
      tags$hr(),h4("Mark a planned event completed"),
      DTOutput("events_table"),
      actionButton("complete_event","Mark selected complete"),
      actionButton("delete_event","Delete selected"))),
      column(7,div(class="panel-box",h3("Forecast leftover by scenario"),
         p(class="hint","Includes eligible income and planned costs. Optimistic scenarios include possible returns of money you lent; the conservative budget does not. This is a projected monthly cash-flow surplus, not the bank balance."),
         plotOutput("forecast_plot",height="330px"),
         DTOutput("forecast_table"))))),
  nav_panel("Settings",value="settings",
    section("Your finance workspace","Everything can be entered directly in BudgetForge. No spreadsheet is required."),
    div(class="panel-box",h3("Display preferences"),
      fluidRow(column(5,selectInput("app_language","Application language",
        choices=c("English"="en","Deutsch"="de"),selected="en")),
        column(5,selectInput("display_rows","Rows per table (all tabs)",
          choices=c("5"="5","10"="10","20"="20","50"="50","100"="100","All"="-1"),selected="10")),
        column(2,div(style="padding-top:25px",actionButton("save_display","Save display settings",class="btn-primary")))),
      p(class="hint","Language changes across tabs immediately. Custom names and imported bank descriptions are never translated.")),
    div(class="panel-box",h3("Currency & exchange rate"),
      p(class="hint","All financial plans are stored in CZK. Choose how to view and enter amounts. The rate is a manual planning assumption, not a live rate or a transaction-date market rate."),
      fluidRow(column(4,selectInput("setting_currency","Display and input currency",choices=c("CZK"="CZK","EUR"="EUR"))),
        column(5,numericInput("setting_fx","CZK per 1 EUR",value=24,min=0.01,step=.1)),
        column(3,div(style="padding-top:25px",actionButton("save_currency","Apply currency & rate",class="btn-primary")))),
      textOutput("currency_explainer")),
    div(class="panel-box",h3("My bank accounts / internal transfers"),
      p(class="hint","Register every account you own (including accounts whose statements are not imported). Transfers to an owned account will be marked as internal, even if the other statement is missing. Identifiers must match the bank's counterparty format, including bank code where present."),
      selectInput("account_choice","Bank account",choices=""),
      fluidRow(column(4,textInput("account_key","Account number / identifier",placeholder="123456789/0800")),
               column(4,textInput("account_name","Account label",placeholder="Main current account")),
               column(4,textInput("account_bank","Bank",placeholder="Fio / ČSAS / other"))),
      fluidRow(column(4,checkboxInput("account_owned","I own this account",TRUE)),
               column(4,textInput("account_balance","Current account balance (optional)",placeholder="Leave blank if unknown")),
               column(4,div(style="padding-top:25px",actionButton("save_account","Save account",class="btn-primary"),actionButton("new_account","New account")))),
      DTOutput("accounts_table"),
      p(class="hint","Balances are manual snapshots and are not updated automatically by CSV imports. Missing balances are excluded from the net-worth snapshot. If you reclassify an imported transaction manually, your decision is preserved.")),
    div(class="panel-box",h3("Calendar / planning timeline"),
      p(class="hint","Choose any start and end date. BudgetForge builds monthly episodes from this calendar, including the Income planner, Monthly plan and active-month selectors. For responsiveness, a single view can cover up to 50 years."),
      fluidRow(column(5,dateInput("plan_start","Planning starts",value=Sys.Date(),format="mm/yyyy",startview="year")),
               column(5,dateInput("plan_end","Planning ends",value=seq(Sys.Date(),by='year',length.out=3)[3],format="mm/yyyy",startview="year")),
               column(2,div(style="padding-top:25px",actionButton("save_calendar","Apply timeline",class="btn-primary")))),
      textOutput("calendar_note")),
    fluidRow(column(6,div(class="panel-box",h3("Monthly safeguards & opening balance"),
      numericInput("setting_savings","General savings goal per month (CZK)",value=0,min=0,step=500),
      numericInput("setting_buffer","Cash safety buffer (CZK)",value=0,min=0,step=500),
      textInput("setting_cash","Optional current available cash balance (CZK)",placeholder="Leave blank if unknown"),
      numericInput("setting_equity","Opening net worth at start of chosen timeline (CZK)",value=0,step=1000),
      numericInput("setting_other_assets","Other assets not represented by accounts (CZK)",value=0,min=0,step=1000),
      p(class="hint","Starting assets minus liabilities, or another explicitly chosen financial baseline. Income graph is income-only and does not deduct spending."),
      actionButton("save_settings","Save safeguards & equity",class="btn-primary"),
      p(class="hint","Cash balance must represent liquid money across all tracked accounts, excluding balances already committed elsewhere. This can tighten the spending recommendation; keep it current or clear it."))),
      column(6,div(class="panel-box",h3("Get started in 5 steps"),
        p("1. Add salary, variable income and one-time payments under Income planner."),
        p("2. Set up custom fixed and variable categories in Categories."),
        p("3. Add outstanding debts and future lump-sum repayments under Debts."),
        p("4. Set aside amounts for annual bills under Sinking funds and record money others owe you under Money lent."),
        p("5. Set your planning calendar; optionally import bank CSVs or enter transactions manually."),
        uiOutput("setup_progress"),
        p(class="hint","No Excel template needed. Your budget is saved to your private Supabase account after each edit.")))),
    div(class="panel-box",h3("Security, accuracy & interpretation"),
      tags$ul(tags$li("Accounts sign in with Supabase. Each user has an isolated working database saved to a private, RLS-protected cloud snapshot. This is a beta; do not upload real bank statements until security review."),
              tags$li("CZK and EUR display are supported at a user-selected planning rate. Historical imported CSV transactions currently require CZK amounts; do not treat this rate as historical FX."),
              tags$li("Forecasts are monthly estimates, and an amount marked available after payday may not be available to spend today."),
              tags$li("You may enter all plans manually; bank CSV imports are optional. Source files contain no banking or Excel records."),
              tags$li("One-off debt payments are planned separately from actual bank transactions. Update each loan's current balance after paying."))),
    div(class="panel-box",h3("About BudgetForge"),p(strong("Shape your financial future")),p("Created by Mgr. Adam Klocek, Ph.D."))),
  id="main_nav"
)

# Keep the original page_navbar static so bslib/Bootstrap dependencies load.
# A full-screen gate prevents interactive use until Supabase verifies login.
ui <- tagList(ui, uiOutput('bf_access_gate'))

server <- function(input,output,session) {
  # No visitor accesses the repository's shared data/flowwise.sqlite.
  # All local SQLite files are unique to the current Shiny browser session.
  session_db <- tempfile(pattern='bf_session_', fileext='.sqlite')
  con <- bf_fresh_connection(session_db)
  auth <- reactiveValues(account=NULL, authorized=FALSE, locked=FALSE, error='')
  cloud_revision <- NULL

  session$onSessionEnded(function() {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    unlink(c(session_db,paste0(session_db,c('-journal','-wal','-shm'))))
  })

  # Update cloud state after every successful edit. If a save fails, we lock
  # the editor to avoid silently losing changes or overwriting other sessions.
  cloud_save <- function() {
    if (!isTRUE(auth$authorized) || isTRUE(auth$locked)) return(invisible(NULL))
    tryCatch({
      account <- bf_valid_access(isolate(auth$account))
      new_revision <- bf_upload_snapshot(account, con, session_db, cloud_revision)
      auth$account <- account
      cloud_revision <<- new_revision
    }, error=function(e) {
      auth$locked <- TRUE
      auth$error <- paste('Cloud synchronization failed:', conditionMessage(e),
                          'Your changes may only exist in this browser session. Reload from cloud or contact the app owner.')
      showNotification(auth$error, type='error', duration=NULL)
      stop(conditionMessage(e))
    })
    invisible(TRUE)
  }

  rev <- reactiveVal(0L)
  refresh <- function() {
    cloud_save()
    rev(isolate(rev())+1L)
  }
  inform <- function(expr, message="Saved") {
    if (!isTRUE(auth$authorized) || isTRUE(auth$locked)) return(invisible(NULL))
    tryCatch({force(expr); refresh(); showNotification(message,type='message',duration=4)},
             error=function(e)showNotification(conditionMessage(e),type='error',duration=9))
  }

  output$bf_auth_status <- renderUI({if(isTRUE(auth$authorized))tagList(
    span(style='font-size:12px;margin-right:10px',auth$account$email),
    actionButton('bf_logout','Sign out',class='btn-outline-secondary'))})

  output$bf_access_gate <- renderUI({
    if (isTRUE(auth$authorized) && !isTRUE(auth$locked)) return(NULL)
    if (isTRUE(auth$locked)) {
      return(div(class='bf-auth-backdrop',div(class='bf-auth-card',
        h2('Cloud sync paused'),p(auth$error),
        p('Reload discards the unsaved changes from this session.'),
        actionButton('bf_reload','Discard local changes and reload',class='btn-primary'),
        actionButton('bf_logout','Sign out'))))
    }
    div(class='bf-auth-backdrop',div(class='bf-auth-card',
      h2('BudgetForge'),p('Shape your financial future'),
      p('Sign in with a verified Supabase account. This beta is for fictional financial data.'),
      textInput('bf_email','Email',placeholder='you@example.com'),
      passwordInput('bf_password','Password'),
      actionButton('bf_login','Sign in',class='btn-primary'),
      if(nzchar(auth$error))p(style='color:#aa2434;margin-top:1rem',auth$error),
      tags$hr(),p(class='hint','Your account is stored separately from other users. Sign-up is managed in Supabase Authentication.')))
  })

  # Auth token stays in Shiny's server-side session, never in a browser field.
  bf_load_account <- function(account) {
    account <- bf_valid_access(account)
    snapshot <- bf_download_snapshot(account)
    # Only discard old working state after the authenticated download succeeds.
    path_new <- tempfile(pattern='bf_restore_',fileext='.sqlite')
    loaded <- tryCatch(bf_fresh_connection(path_new,snapshot$bytes),error=function(e){
      unlink(path_new);stop(e)
    })
    if(DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    unlink(c(session_db,paste0(session_db,c('-journal','-wal','-shm'))))
    # R keeps the connection object pointing at the new path; every existing
    # DBI query in the app still references the same local `con` variable.
    session_db <<- path_new
    con <<- loaded
    cloud_revision <<- snapshot$revision
    auth$account <- account
    auth$locked <- FALSE
    auth$error <- ''
    auth$authorized <- TRUE
    if(is.null(snapshot$revision)) cloud_save() # first-time account seed
    rev(isolate(rev())+1L)
  }

  observeEvent(input$bf_login,{
    tryCatch({
      account <- bf_sign_in(isolate(input$bf_email),isolate(input$bf_password))
      bf_load_account(account)
      updateTextInput(session,'bf_password',value='')
      showNotification(paste('Signed in:',account$email),type='message')
    },error=function(e){auth$authorized <- FALSE;auth$error <- conditionMessage(e)})
  })
  observeEvent(input$bf_reload,{
    tryCatch(bf_load_account(isolate(auth$account)),error=function(e){
      auth$locked <- TRUE;auth$error <- conditionMessage(e)
    })
  })
  observeEvent(input$bf_logout,{
    if(DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    unlink(c(session_db,paste0(session_db,c('-journal','-wal','-shm'))))
    session_db <<- tempfile(pattern='bf_session_',fileext='.sqlite')
    con <<- bf_fresh_connection(session_db)
    cloud_revision <<- NULL
    auth$account <- NULL;auth$authorized <- FALSE
    auth$locked <- FALSE;auth$error <- ''
    rev(isolate(rev())+1L)
  })
  cat_data <- reactive({rev();get_categories(con)})
  src_data <- reactive({rev();get_sources(con)})
  once_data <- reactive({rev();get_oneoffs(con)})
  debt_data <- reactive({rev();get_debts(con)})
  extra_data <- reactive({rev();get_debt_extras(con)})
  fund_data <- reactive({rev();get_funds(con)})
  rules_data <- reactive({rev();get_rules(con)})
  tx_data <- reactive({rev();month_transactions(con,req(input$month))})
  summ <- reactive({rev();monthly_summary(con,req(input$month),debt_method=input$payoff_method %||% "minimums")})
  forecast <- reactive({rev();s<-settings_data();forecast_months(con,s$planner_start[1],s$planner_end[1],input$payoff_method %||% "minimums")})
  income_proj <- reactive({rev();s<-settings_data();income_projection(con,s$planner_start[1],s$planner_end[1])})
  events_data <- reactive({rev();get_events(con)})
  settings_data <- reactive({rev();get_settings(con)})
  fmt <- function(x) format_money(x, settings_data())
  to_ui <- function(x) view_amount(x,settings_data())
  to_db <- function(x) stored_amount(x,settings_data())
  rv <- function(x) round(to_ui(x),2)
  currency <- reactive(currency_code(settings_data()))
  goal_data <- reactive({rev();get_goals(con)})
  account_data <- reactive({rev();get_accounts(con)})
  finance <- reactive({rev();financial_kpis(con,req(input$month),forecast())})
  loan_data <- reactive({rev();get_receivables(con)})
  repayment_data <- reactive({rev();get_loan_repayments(con)})
  table_rows <- reactive({
    v<-suppressWarnings(as.integer(input$display_rows %||% settings_data()$table_rows[1]))
    if(is.na(v) || !v %in% c(-1L,5L,10L,20L,50L,100L)) 10L else v
  })
  app_lang <- reactive({if(identical(input$app_language,'de'))'de' else 'en'})
  tr <- function(en,de) if(app_lang()=='de')de else en
  # Central wrapper ensures EVERY table (including empty and debt tables) uses
  # one persisted rows-per-page setting and localized DT pagination.
  datatable <- function(data,...,options=list()) {
    options$pageLength <- table_rows()
    options$lengthMenu <- list(c(5,10,20,50,100,-1),c('5','10','20','50','100',if(app_lang()=='de')'Alle' else 'All'))
    if(identical(options$dom,'t')) options$dom <- 'tip'
    options$language <- if(app_lang()=='de') list(
       emptyTable='Keine Daten verfügbar',zeroRecords='Keine passenden Einträge',
       info='Einträge _START_ bis _END_ von _TOTAL_',infoEmpty='Keine Einträge',
       paginate=list(previous='Zurück',`next`='Weiter'),lengthMenu='_MENU_ Einträge') else list(
       emptyTable='No data available',zeroRecords='No matching records',
       info='Showing _START_ to _END_ of _TOTAL_',infoEmpty='No entries',
       paginate=list(previous='Previous',`next`='Next'),lengthMenu='_MENU_ rows')
    DT::datatable(data,...,options=options)
  }
  observeEvent(settings_data(),{
    st<-settings_data()
    updateSelectInput(session,'app_language',selected=st$app_language[1])
    updateSelectInput(session,'display_rows',selected=as.character(st$table_rows[1]))
  },ignoreInit=FALSE)
  observeEvent(input$app_language,{
    session$sendCustomMessage('flowwise-language',list(language=app_lang()))
  },ignoreInit=FALSE)
  observeEvent(input$save_display,inform({
    DBI::dbExecute(con,'UPDATE settings SET app_language=?,table_rows=? WHERE id=1',
      params=list(app_lang(),table_rows()))
  },'Display preferences saved'))

  output$overview_kpis <- renderUI({s <- summ();fluidRow(
    column(3,metric("Estimated discretionary capacity",fmt(s$available),if(s$shortfall<0) paste("Shortfall",fmt(-s$shortfall)) else "After remaining commitments",TRUE)),
    column(3,metric("Income received",fmt(s$actual_income),paste("Unplanned received:",fmt(s$unplanned)))),
    column(3,metric("Expenses already paid",fmt(s$expenses+ s$debt_paid),paste("Including debt:",fmt(s$debt_paid),"| Money lent:",fmt(s$loan_lent)))),
    column(3,metric("Still committed",fmt(s$unpaid_fixed+s$unpaid_debts+s$unpaid_extra_debts+s$reserve_funds+s$unpaid_events+s$savings),"Bills · debts incl. extras · funds · savings"))
  )})
  output$overview_next_move <- renderUI({
    s <- summ(); active <- s$debts[s$debts$active==1 & s$debts$balance>0,,drop=FALSE]
    room <- max(0,s$available - sum(s$variable$suggested_rest))
    if (!nrow(active) || room<1) return(NULL)
    preferred <- active[order(-active$apr,active$balance),,drop=FALSE][1,,drop=FALSE]
    div(class="help-tip",tags$strong("Possible extra debt repayment: "),
        paste(fmt(room),"of projected month-end headroom would remain after suggested variable spending. You could direct some of it toward",preferred$name[1],"(highest APR), or keep it as a buffer."),
        "Only make extra payments from money that has actually arrived and is no longer needed for upcoming bills.")
  })
  output$overview_warnings <- renderUI({
    s <- summ();unknown_expenses <- sum(s$tx$flow_type=="expense" & is.na(s$tx$category_id))
    unknown_income <- sum(s$tx$flow_type=="income" & is.na(s$tx$income_source_id))
    bits <- character()
    if (unknown_expenses>0) bits <- c(bits,paste(unknown_expenses,"uncategorized expenses"))
    if (unknown_income>0 && nrow(s$sources)>0) bits <- c(bits,paste(unknown_income,"incoming payments not linked to recurring sources"))
    if (nrow(s$tx)==0) bits <- c(bits,"no bank transactions for this month")
    if (s$mode=="future") bits <- c(bits,"future month: forecast, not cash on hand")
    if (s$mode=="past") bits <- c(bits,"historical month: a retrospective result")
    if (length(bits)==0) return(NULL)
    div(class="help-tip",strong("Review recommended: "),paste(bits,collapse=" • "),". These can affect the accuracy of spending limits.")
  })
  output$spend_plot <- renderPlot({
    b <- summ()$budgets
    if (!nrow(b) || sum(b$spent)<=0) {plot.new();text(.5,.5,tr('No categorized spending yet','Noch keine kategorisierten Ausgaben'),col="#708399",cex=1.2);return()}
    b <- b[b$spent>0,,drop=FALSE];b <- b[order(b$spent,decreasing=TRUE),,drop=FALSE];b <- head(b,10);b$spent<-to_ui(b$spent)
    ggplot(b,aes(x=reorder(name,spent),y=spent,fill=as.factor(is_fixed)))+
      geom_col(width=.65,show.legend=FALSE)+coord_flip()+
      scale_fill_manual(values=c("0"="#2588a2","1"="#18466c"))+
      scale_y_continuous(labels=function(x) format(round(x),big.mark=" ",scientific=FALSE))+
      labs(x=NULL,y=tr(paste(currency(),'spent'),paste('Ausgaben in',currency())))+theme_minimal(base_size=12)+theme(panel.grid.major.y=element_blank(),plot.margin=margin(8,12,8,8))
  })
  output$runway <- renderUI({s<-summ();
    div(
      p(tags$strong("Received income counted in budget"),span(style="float:right",fmt(s$budget_income_received))),
      p(tags$strong("Expected regular income"),span(style="float:right",fmt(s$expected_income))),
      tags$hr(),p("Paid living costs",span(style="float:right",fmt(s$expenses))),
      p("Paid debt payments",span(style="float:right",fmt(s$debt_paid))),
      p("Money lent this month",span(style="float:right",fmt(s$loan_lent))),
      p("ATM cash withdrawals (cash transfer)",span(style="float:right",fmt(s$withdrawn))),
      p("Loan principal returned",span(style="float:right",fmt(s$loan_collected))),
      p("Possible returns due (not spendable yet)",span(style="float:right",fmt(s$receivable_due))),
      p("Unpaid fixed expenses",span(style="float:right",fmt(s$unpaid_fixed))),
      p("Remaining debt minimums",span(style="float:right",fmt(s$unpaid_debts))),
      p("Planned occasional debt payments",span(style="float:right",fmt(s$unpaid_extra_debts))),
      p("Sinking funds this month",span(style="float:right",fmt(s$reserve_funds))),
      p("Savings goal allocations",span(style="float:right",fmt(s$goal_reserve))),
      p("Unpaid planned extras",span(style="float:right",fmt(s$unpaid_events))),
      p("Savings + buffer",span(style="float:right",fmt(s$savings+s$buffer))),
      tags$hr(),p(tags$strong("Projected available capacity"),span(style="float:right",tags$strong(fmt(s$available)))),
      if(!is.na(s$cash_cap)) p(class="hint",paste("Current cash-aware projection:",fmt(s$cash_cap))) else p(class="hint","Add your available cash in Settings to impose an additional cash-based limit."),
      if(s$days>0) p(class="badge-soft",paste("Around",fmt(s$available/s$days),"per calendar day across all variable categories"))
    )
  })
  output$recommended_table <- renderDT({v<-summ()$variable
    if(!nrow(v)) return(datatable(data.frame(Info="Create a variable category to get a recommendation"),options=list(dom="t"),rownames=FALSE))
    show <- data.frame(Category=v$name,`Monthly baseline`=rv(v$baseline),`Already spent`=rv(v$spent),`Remaining plan`=rv(v$remaining_plan),`Suggested to spend`=rv(v$suggested_rest),Basis=v$basis,check.names=FALSE)
    datatable(show,rownames=FALSE,options=list(pageLength=12,scrollX=TRUE,dom="tip"))
  })
  output$overview_category_table <- renderDT({b<-summ()$budgets
    datatable(data.frame(Category=b$name,Type=ifelse(b$is_fixed==1,"Fixed","Variable"),Budget=rv(b$budget),Spent=rv(b$spent),Remaining=rv(b$budget-b$spent)),rownames=FALSE,options=list(pageLength=10,dom="tip"))
  })

  observeEvent(input$import_banks,{
    req(input$bank_files)
    inform({ans<-import_transactions(con,input$bank_files$datapath)
      output$import_status<-renderText(paste("Imported:",ans$imported,"| Skipped duplicates:",ans$skipped,"|",ans$banks))
    },"Import completed — review classifications below")
  })
  output$tx_table <- renderDT({x<-tx_data(); cats<-cat_data(); sources<-src_data(); debts<-debt_data()
    if (!nrow(x)) return(datatable(data.frame(Info="No transactions for the selected month"),options=list(dom="t")))
    desc<-x$description;desc[nchar(desc)>110]<-paste0(substr(desc[nchar(desc)>110],1,107),"...")
    target<-ifelse(x$flow_type=="expense",cats$name[match(x$category_id,cats$id)],ifelse(x$flow_type=="income",sources$name[match(x$income_source_id,sources$id)],ifelse(x$flow_type=="debt",debts$name[match(x$debt_id,debts$id)],
       ifelse(x$flow_type=="loan_out","Money lent",ifelse(x$flow_type=="loan_return","Loan repayment",
       ifelse(x$flow_type=="withdrawal","Withdrawals","Transfer"))))))
    target[is.na(target)]<-"Unassigned"
    datatable(data.frame(Date=x$txn_date,Bank=x$bank,Description=desc,Amount=rv(x$amount),Type=x$flow_type,Category=target,Rule=ifelse(is.na(x$matched_rule),"",x$matched_rule)),selection="single",rownames=FALSE,options=list(pageLength=12,scrollX=TRUE))
  })
  selected_tx <- reactive({i<-input$tx_table_rows_selected;x<-tx_data();if(length(i)!=1 || i>nrow(x)) return(NULL);x[i,,drop=FALSE]})
  observeEvent(selected_tx(),{
    x<-selected_tx();if(is.null(x))return()
    updateSelectInput(session,"review_flow",selected=x$flow_type[1]);updateTextInput(session,"rule_keyword",value=if(nzchar(x$payee[1])) x$payee[1] else "")
  })
  output$review_target_ui<-renderUI({t<-req(input$review_flow)
    options<-switch(t,expense=setNames(as.character(cat_data()$id),cat_data()$name),income=setNames(as.character(src_data()$id),src_data()$name),debt=setNames(as.character(debt_data()$id),debt_data()$name),withdrawal=character(),transfer=character())
    current<-selected_tx(); selected<-NULL
    if(!is.null(current)) selected<-switch(t,expense=current$category_id[1],income=current$income_source_id[1],debt=current$debt_id[1],withdrawal=NULL,transfer=NULL)
    selectInput("review_target","Category / source / debt",choices=c("Unassigned"="",options),selected=if(is.null(selected)||is.na(selected)) "" else as.character(selected))
  })
  save_review <- function(as_rule=FALSE) {
    x<-selected_tx();if(is.null(x))stop("Select a transaction in the table first.")
    t<-input$review_flow; amount<-x$amount[1]
    if(nrow(DBI::dbGetQuery(con,'SELECT id FROM money_lent WHERE linked_txn_id=?',params=list(x$id[1]))) ||
       nrow(DBI::dbGetQuery(con,'SELECT id FROM loan_repayments WHERE linked_txn_id=?',params=list(x$id[1]))))
      stop('This transaction is linked to money lent. Manage it in the Money lent tab.')
    if (amount>=0 && t %in% c("expense","debt","withdrawal")) stop("An incoming payment cannot be an expense or debt payment.")
    if (amount<0 && t=="income") stop("An outgoing payment cannot be income.")
    id<-if(nzchar(input$review_target %||% ""))as.integer(input$review_target) else NA_integer_
    category<-if(t=="expense")id else if(t=="withdrawal"){
      ix<-which(cat_data()$name=='Withdrawals');if(length(ix))cat_data()$id[ix[1]] else NA_integer_
    } else NA_integer_
    income<-if(t=="income")id else NA_integer_
    debt<-if(t=="debt")id else NA_integer_
    if(as_rule) {
      keyword<-trimws(input$rule_keyword)
      if(nchar(keyword)<3) stop("Choose a keyword of at least 3 characters.")
      DBI::dbExecute(con,"INSERT INTO rules(pattern,flow_type,category_id,income_source_id,debt_id,priority) VALUES(?,?,?,?,?,100)",params=list(keyword,t,category,income,debt))
      apply_rules_existing(con)
    }
    DBI::dbExecute(con,"UPDATE transactions SET flow_type=?,category_id=?,income_source_id=?,debt_id=?,matched_rule=?,is_manual=1 WHERE id=?",params=list(t,category,income,debt,if(as_rule)paste0("User rule: ",input$rule_keyword) else "Manually reviewed",x$id[1]))
  }
  observeEvent(input$save_classification,inform(save_review(),"Transaction updated"))
  observeEvent(input$save_rule,inform(save_review(TRUE),"Rule saved and applied"))
  observeEvent(input$reapply_rules,inform(apply_rules_existing(con),"Rules reapplied to unreviewed transactions"))
  output$rules_table<-renderDT({r<-rules_data();if(!nrow(r))return(datatable(data.frame(Info="No custom rules yet"),options=list(dom="t")))
    datatable(data.frame(Keyword=r$pattern,Classification=r$flow_type,Priority=r$priority),selection="single",rownames=FALSE,options=list(pageLength=6,dom="tip"))
  })
  observeEvent(input$delete_rule,inform({i<-input$rules_table_rows_selected;r<-rules_data()
    if(length(i)!=1 || i>nrow(r))stop("Select a rule first.")
    DBI::dbExecute(con,"DELETE FROM rules WHERE id=?",params=list(r$id[i]))
    apply_rules_existing(con)
  },"Rule deleted; remaining rules reapplied"))
  observeEvent(input$delete_manual,inform({x<-selected_tx()
    if(is.null(x))stop("Select a transaction first.")
    if(x$bank[1]!="MANUAL")stop("Bank-imported transactions cannot be deleted here.")
    if(nrow(DBI::dbGetQuery(con,'SELECT id FROM money_lent WHERE linked_txn_id=?',params=list(x$id[1]))) ||
       nrow(DBI::dbGetQuery(con,'SELECT id FROM loan_repayments WHERE linked_txn_id=?',params=list(x$id[1]))))
      stop('Remove the linked loan or repayment first in Money lent.')
    DBI::dbExecute(con,"DELETE FROM transactions WHERE id=?",params=list(x$id[1]))
  },"Manual entry deleted"))
  output$manual_target_ui<-renderUI({type<-req(input$manual_flow)
    choices<-switch(type,expense=setNames(as.character(cat_data()$id),cat_data()$name),income=setNames(as.character(src_data()$id),src_data()$name),debt=setNames(as.character(debt_data()$id),debt_data()$name),withdrawal=character(),transfer=character())
    selectInput("manual_target","Optional category / source / debt",choices=c("Unassigned / additional income"="",choices))
  })
  observeEvent(input$add_manual,inform({
    req(input$manual_date,nzchar(trimws(input$manual_desc)))
    stopifnot(is.finite(input$manual_amount),input$manual_amount>0)
    typ<-input$manual_flow;amount<-if(typ=="income") to_db(input$manual_amount) else -to_db(input$manual_amount)
    ref<-paste("MANUAL",as.integer(Sys.time()),sample.int(1e9,1),sep="-")
    id<-if(nzchar(input$manual_target %||% ""))as.integer(input$manual_target) else NA_integer_
    DBI::dbExecute(con,"INSERT INTO transactions(fingerprint,txn_date,bank,account_key,description,amount,currency,flow_type,category_id,income_source_id,debt_id,is_manual,matched_rule) VALUES(?,?,?,?,?,?,'CZK',?,?,?,?,1,'Manual entry')",params=list(digest::digest(ref,algo="sha256"),as.character(input$manual_date),"MANUAL","MANUAL",trimws(input$manual_desc),amount,typ,if(typ=="expense")id else if(typ=="withdrawal"){
      ix<-which(cat_data()$name=='Withdrawals'); if(length(ix))cat_data()$id[ix[1]] else NA_integer_
    } else NA_integer_,if(typ=="income")id else NA_integer_,if(typ=="debt")id else NA_integer_))
  },"Transaction added"))

  observe({c<-cat_data();updateSelectInput(session,"cat_choice",choices=opt_ids(c,"+ New category"),selected=isolate(input$cat_choice)%||%"new")})
  observeEvent(input$cat_choice,{req(input$cat_choice);c<-cat_data();i<-if(input$cat_choice=="new") NA_integer_ else match(as.integer(input$cat_choice),c$id)
    if(input$cat_choice=="new"||is.na(i)) {updateTextInput(session,"cat_name",value="");updateCheckboxInput(session,"cat_fixed",value=FALSE);updateNumericInput(session,"cat_budget",value=0);updateNumericInput(session,"cat_override",value=0);return()}
    updateTextInput(session,"cat_name",value=c$name[i]);updateCheckboxInput(session,"cat_fixed",value=c$is_fixed[i]==1)
    updateNumericInput(session,"cat_budget",value=to_ui(c$default_budget[i]));b<-month_budgets(con,input$month)
    updateNumericInput(session,"cat_override",value=to_ui(b$budget[match(c$id[i],b$id)]))
  })
  observeEvent(input$save_cat,inform({nm<-trimws(input$cat_name);if(!nzchar(nm))stop("Enter a category name.")
    id<-if(input$cat_choice=="new")NA_integer_ else as.integer(input$cat_choice)
    upsert_category(con,nm,isTRUE(input$cat_fixed),to_db(input$cat_budget),id)
  },"Category saved"))
  observeEvent(input$save_override,inform({req(input$cat_choice!="new");stopifnot(is.finite(input$cat_override),input$cat_override>=0)
    DBI::dbExecute(con,"INSERT INTO category_budgets(month,category_id,amount) VALUES(?,?,?) ON CONFLICT(month,category_id) DO UPDATE SET amount=excluded.amount",params=list(input$month,as.integer(input$cat_choice),to_db(input$cat_override)))
  },"Budget updated for selected month"))
  output$category_editor_table <- renderDT({b<-summ()$budgets
    datatable(data.frame(Category=b$name,Type=ifelse(b$is_fixed==1,"Fixed","Variable"),Default=rv(b$default_budget),`This month`=rv(b$budget),Spent=rv(b$spent),check.names=FALSE),rownames=FALSE,options=list(pageLength=14,dom="tip"))
  })

  # Separate income editor, preserving all v2 records after migration.
  source_status <- reactiveVal('')
  output$source_status <- renderText(source_status())
  observe({
    sources<-src_data(); selected<-isolate(input$source_choice)%||%'new'
    if(selected!='new' && !selected %in% as.character(sources$id))selected<-'new'
    updateSelectInput(session,'source_choice',choices=opt_ids(sources,'+ New income source'),selected=selected)
  })
  observeEvent(input$source_choice,{
    req(input$source_choice)
    sources<-src_data()
    i<-if(input$source_choice=='new')NA_integer_ else match(as.integer(input$source_choice),sources$id)
    if(length(i)!=1L || is.na(i)) {
      updateTextInput(session,'source_name',value='')
      updateSelectInput(session,'source_kind',selected='fixed')
      updateNumericInput(session,'source_amount',value=0)
      updateNumericInput(session,'source_low',value=0)
      updateNumericInput(session,'source_high',value=0)
      updateNumericInput(session,'source_day',value=25)
      updateDateInput(session,'source_start',value=Sys.Date())
      updateCheckboxInput(session,'source_has_end',value=FALSE)
      updateCheckboxInput(session,'source_active',value=TRUE)
      updateCheckboxInput(session,'source_include',value=TRUE)
    } else {
      x<-sources[i,,drop=FALSE]
      updateTextInput(session,'source_name',value=x$name[1])
      updateSelectInput(session,'source_kind',selected=x$kind[1])
      updateNumericInput(session,'source_amount',value=to_ui(x$monthly_amount[1]))
      updateNumericInput(session,'source_low',value=to_ui(x$min_amount[1]))
      updateNumericInput(session,'source_high',value=to_ui(x$max_amount[1]))
      updateNumericInput(session,'source_day',value=x$pay_day[1])
      updateDateInput(session,'source_start',value=as.Date(paste0(x$start_month[1],'-01')))
      has_end<-!is.na(x$end_month[1]) && nzchar(x$end_month[1])
      updateCheckboxInput(session,'source_has_end',value=has_end)
      if(has_end)updateDateInput(session,'source_end',value=as.Date(paste0(x$end_month[1],'-01')))
      updateCheckboxInput(session,'source_active',value=x$active[1]==1)
      updateCheckboxInput(session,'source_include',value=x$include_budget[1]==1)
    }
  })
  observeEvent(input$new_source,{
    updateSelectInput(session,'source_choice',selected='new')
    source_status('Enter the new source and press Save income source.')
  })
  observeEvent(input$save_source,{
    tryCatch({
      nm<-trimws(input$source_name%||%'')
      if(!nzchar(nm))stop('Enter a name for the income source.')
      kind<-input$source_kind%||%'fixed'
      avg<-to_db(input$source_amount)
      lo<-if(kind=='fixed')avg else to_db(input$source_low)
      hi<-if(kind=='fixed')avg else to_db(input$source_high)
      start<-format(as.Date(req(input$source_start)),'%Y-%m')
      end<-if(isTRUE(input$source_has_end))format(as.Date(req(input$source_end)),'%Y-%m') else NA_character_
      selected<-input$source_choice%||%'new'
      id<-if(selected=='new')NA_integer_ else as.integer(selected)
      upsert_source(con,nm,avg,input$source_day,isTRUE(input$source_active),id,
        kind=kind,min_amount=lo,max_amount=hi,start_month=start,end_month=end,
        include_budget=isTRUE(input$source_include))
      saved<-DBI::dbGetQuery(con,'SELECT id FROM income_sources WHERE name=?',params=list(nm))$id
      if(length(saved)!=1L)stop('Could not find the saved source.')
      refresh()
      session$onFlushed(function()updateSelectInput(session,'source_choice',
        choices=opt_ids(get_sources(con),'+ New income source'),selected=as.character(saved)),once=TRUE)
      source_status(paste('Saved:',nm,'—',fmt(avg),'expected / month.'))
      showNotification(paste('Income source saved:',nm),type='message')
    },error=function(e){source_status(paste('Not saved:',conditionMessage(e)));
      showNotification(conditionMessage(e),type='error',duration=9)})
  })
  output$income_table <- renderDT({
    x<-summ()$sources
    if(!nrow(x))return(datatable(data.frame(Info='Add salary, freelance or other income to begin'),options=list(dom='t'),rownames=FALSE))
    datatable(data.frame(Source=x$name,Type=ifelse(x$kind=='fixed','Fixed','Variable'),
      `Low / mo`=rv(x$min_amount),`Expected / mo`=rv(x$monthly_amount),
      `High / mo`=rv(x$max_amount),`Confirmed through`=ifelse(is.na(x$end_month),'Open-ended',x$end_month),
      `In real budget`=ifelse(x$include_budget==1,'Yes','No'),
      `Received this month`=rv(x$received),`Still expected in budget`=rv(x$expected),check.names=FALSE),
      rownames=FALSE,options=list(dom='tip',scrollX=TRUE,pageLength=10))
  })
  observe({
    s<-settings_data()
    updateSelectInput(session,'income_budget_policy',selected=s$variable_budget_basis[1])
  })
  observeEvent(input$save_income_policy,inform({
    basis<-req(input$income_budget_policy)
    if(!basis %in% c('low','expected'))stop('Invalid scenario.')
    DBI::dbExecute(con,'UPDATE settings SET variable_budget_basis=? WHERE id=1',params=list(basis))
  },'Income budget policy updated'))
  observe({
    x<-once_data();sel<-isolate(input$once_choice)%||%'new'
    if(sel!='new' && !sel %in% as.character(x$id))sel<-'new'
    choices<-c('+ New one-time income'='new',setNames(as.character(x$id),paste(x$month,x$name)))
    updateSelectInput(session,'once_choice',choices=choices,selected=sel)
  })
  observeEvent(input$once_choice,{
    x<-once_data(); selected<-input$once_choice
    if(is.null(selected))return()
    i<-if(selected=='new')NA_integer_ else match(as.integer(selected),x$id)
    if(length(i)!=1L || is.na(i)) {
      updateTextInput(session,'once_name',value='')
      updateDateInput(session,'once_date',value=Sys.Date())
      updateSelectInput(session,'once_certainty',selected='uncertain')
      updateNumericInput(session,'once_low',value=0)
      updateNumericInput(session,'once_expected',value=0)
      updateNumericInput(session,'once_high',value=0)
      updateCheckboxInput(session,'once_include',value=FALSE)
      updateCheckboxInput(session,'once_received',value=FALSE)
    } else {
      o<-x[i,,drop=FALSE]
      updateTextInput(session,'once_name',value=o$name[1])
      updateDateInput(session,'once_date',value=as.Date(paste0(o$month[1],'-01')))
      updateSelectInput(session,'once_certainty',selected=o$certainty[1])
      updateNumericInput(session,'once_low',value=to_ui(o$low[1]))
      updateNumericInput(session,'once_expected',value=to_ui(o$expected[1]))
      updateNumericInput(session,'once_high',value=to_ui(o$high[1]))
      updateCheckboxInput(session,'once_include',value=o$include_budget[1]==1)
      updateCheckboxInput(session,'once_received',value=o$received[1]==1)
    }
  })
  observeEvent(input$new_once,updateSelectInput(session,'once_choice',selected='new'))
  observeEvent(input$save_once,{
    tryCatch({
      certainty<-req(input$once_certainty);mid<-to_db(req(input$once_expected))
      low<-if(certainty=='confirmed')mid else to_db(input$once_low)
      high<-if(certainty=='confirmed')mid else to_db(input$once_high)
      id<-if((input$once_choice%||%'new')=='new')NA_integer_ else as.integer(input$once_choice)
      saved<-upsert_oneoff(con,input$once_name,format(as.Date(req(input$once_date)),'%Y-%m'),
        low,mid,high,certainty,isTRUE(input$once_include),isTRUE(input$once_received),id)
      refresh()
      session$onFlushed(function(){x<-get_oneoffs(con)
        updateSelectInput(session,'once_choice',choices=c('+ New one-time income'='new',
          setNames(as.character(x$id),paste(x$month,x$name))),selected=as.character(saved))},once=TRUE)
      showNotification('One-time income saved',type='message')
    },error=function(e)showNotification(conditionMessage(e),type='error',duration=9))
  })
  observeEvent(input$delete_once,inform({
    if((input$once_choice%||%'new')=='new')stop('Select an existing entry to delete.')
    DBI::dbExecute(con,'DELETE FROM income_oneoffs WHERE id=?',params=list(as.integer(input$once_choice)))
    updateSelectInput(session,'once_choice',selected='new')
  },'One-time income deleted'))
  output$oneoff_table <- renderDT({
    x<-once_data()
    if(!nrow(x))return(datatable(data.frame(Info='No one-time income yet'),options=list(dom='t'),rownames=FALSE))
    datatable(data.frame(Month=x$month,Name=x$name,Certainty=x$certainty,
      Low=rv(x$low),Expected=rv(x$expected),High=rv(x$high),
      Budget=ifelse(x$include_budget==1,'Include','Exclude'),
      Received=ifelse(x$received==1,'Yes','No')),
      rownames=FALSE,options=list(dom='tip',pageLength=10,scrollX=TRUE))
  })
  output$income_kpis <- renderUI({
    f<-income_proj()$monthly
    x<-f[1,,drop=FALSE]
    fluidRow(
      column(3,metric('Opening equity',fmt(income_proj()$opening),'Baseline at start of calendar')),
      column(3,metric('Guaranteed month-one income',fmt(x$guaranteed),'Eligible fixed and confirmed items')),
      column(3,metric('Expected month-one income',fmt(x$expected),'Midpoint / chosen expected amounts')),
      column(3,metric('Month-one income range',paste(fmt(x$low),'–',fmt(x$high)),'Assumed low / high outcomes'))
    )
  })
  output$income_projection_plot <- renderPlot({
    f<-income_proj(); d<-f$monthly
    sources<-f$by_source
    sources<-sources[sources$included,,drop=FALSE];sources$contribution<-to_ui(sources$contribution)
    for(nm in c("cum_low","cum_high","cum_guaranteed","cum_expected"))d[[nm]]<-to_ui(d[[nm]])
    p<-ggplot()+
      geom_ribbon(data=d,aes(x=date,ymin=cum_low,ymax=cum_high),fill='#b7dce3',alpha=.40)+
      geom_line(data=d,aes(x=date,y=cum_guaranteed),color='#334a61',linewidth=1.2,linetype='dashed')+
      geom_line(data=d,aes(x=date,y=cum_low),color='#2a94a1',linewidth=.8,linetype='dotted')+
      geom_line(data=d,aes(x=date,y=cum_high),color='#2a94a1',linewidth=.8,linetype='dotted')+
      geom_line(data=d,aes(x=date,y=cum_expected),color='#126c72',linewidth=1.35)+
      scale_y_continuous(labels=function(x)format(round(x),big.mark=' ',scientific=FALSE))+
      labs(x=tr('Calendar month','Kalendermonat'),y=tr(paste('Opening equity + cumulative income',currency()),paste('Anfangsvermögen + Einnahmen',currency())),
           color=tr('Individual income','Einzelne Einnahme'),
           caption=tr('Solid teal: expected total · dashed dark: guaranteed · teal dotted: low/high · faint lines: individual cumulative inflows',
           'Türkis: erwartetes Gesamtergebnis · dunkel gestrichelt: gesichert · gepunktet: min./max. · dünn: einzelne Einnahmen'))+
      theme_minimal(base_size=12)+theme(legend.position='bottom',panel.grid.minor=element_blank())
    if(nrow(sources))p<-p+geom_line(data=sources,aes(x=date,y=contribution,color=source,group=source),
                                    linewidth=.65,alpha=.75)
    p
  })
  output$income_projection_table <- renderDT({
    x<-income_proj()$monthly
    datatable(data.frame(Month=x$month,Guaranteed=rv(x$guaranteed),
      Low=rv(x$low),Expected=rv(x$expected),High=rv(x$high),
      `Selected budget amount`=rv(x$budget),
      `Cumulative guaranteed + equity`=rv(x$cum_guaranteed),
      `Cumulative expected + equity`=rv(x$cum_expected),check.names=FALSE),
      rownames=FALSE,options=list(dom='tip',pageLength=12,scrollX=TRUE))
  })

  observe({d<-debt_data();selected<-isolate(input$debt_choice)%||%"new"
    if(selected!="new" && !selected %in% as.character(d$id)) selected<-"new"
    updateSelectInput(session,"debt_choice",choices=opt_ids(d,"+ New debt"),selected=selected)
  })
  observeEvent(input$debt_choice,{req(input$debt_choice);d<-debt_data();i<-if(input$debt_choice=="new") NA_integer_ else match(as.integer(input$debt_choice),d$id)
    if(length(i)!=1L || is.na(i)) {updateTextInput(session,"debt_name",value="");updateNumericInput(session,"debt_balance",value=0);updateNumericInput(session,"debt_apr",value=0);updateNumericInput(session,"debt_min",value=0);updateNumericInput(session,"debt_day",value=20);updateCheckboxInput(session,"debt_active",value=TRUE);return()}
    updateTextInput(session,"debt_name",value=d$name[i]);updateNumericInput(session,"debt_balance",value=to_ui(d$balance[i]));updateNumericInput(session,"debt_apr",value=d$apr[i]);updateNumericInput(session,"debt_min",value=to_ui(d$min_payment[i]));updateNumericInput(session,"debt_day",value=d$due_day[i]);updateCheckboxInput(session,"debt_active",value=d$active[i]==1)
  })
  observeEvent(input$new_debt,updateSelectInput(session,"debt_choice",selected="new"))
  observeEvent(input$save_debt,{
    inform({
      nm<-trimws(input$debt_name %||% "")
      if(!nzchar(nm)) stop("Enter a debt name.")
      id<-if((input$debt_choice%||%"new")=="new")NA_integer_ else as.integer(input$debt_choice)
      upsert_debt(con,nm,to_db(input$debt_balance),input$debt_apr,to_db(input$debt_min),input$debt_day,isTRUE(input$debt_active),id)
      saved<-DBI::dbGetQuery(con,"SELECT id FROM debts WHERE name=?",params=list(nm))$id
      if(length(saved)==1L) session$onFlushed(function() {
        updateSelectInput(session,"debt_choice",
          choices=opt_ids(get_debts(con),"+ New debt"),selected=as.character(saved))
      },once=TRUE)
    },"Debt saved")
  })

  # Occasional repayments: selecting a row makes the form editable.
  editing_extra <- reactiveVal(NA_integer_)
  observe({d<-debt_data(); selected<-isolate(input$extra_debt_choice)
    if(is.null(selected) || !selected %in% as.character(d$id)) selected<-if(nrow(d))as.character(d$id[1]) else ""
    updateSelectInput(session,"extra_debt_choice",
      choices=setNames(as.character(d$id),d$name),selected=selected)
  })
  selected_extra <- reactive({
    i<-input$extra_payments_table_rows_selected
    e<-extra_data()
    if(length(i)!=1L || i<1 || i>nrow(e)) return(NULL)
    e[i,,drop=FALSE]
  })
  observeEvent(selected_extra(),{
    e<-selected_extra()
    if(is.null(e))return()
    editing_extra(e$id[1])
    updateSelectInput(session,"extra_debt_choice",selected=as.character(e$debt_id[1]))
    updateDateInput(session,"extra_payment_month",value=as.Date(paste0(e$month[1],"-01")))
    updateNumericInput(session,"extra_payment_amount",value=to_ui(e$amount[1]))
    updateTextInput(session,"extra_payment_note",value=e$note[1])
  })
  observeEvent(input$clear_extra_payment,{
    editing_extra(NA_integer_)
    updateNumericInput(session,"extra_payment_amount",value=1000)
    updateTextInput(session,"extra_payment_note",value="")
    updateDateInput(session,"extra_payment_month",value=Sys.Date())
    # Clear DT selection, to prevent an old row being edited again by accident.
    DT::selectRows(DT::dataTableProxy("extra_payments_table",session=session),NULL)
  })
  observeEvent(input$save_extra_payment,{
    inform({
      debt_id<-input$extra_debt_choice
      if(is.null(debt_id) || !nzchar(debt_id)) stop("Create a debt before scheduling a repayment.")
      req(input$extra_payment_month)
      upsert_debt_extra(con,as.integer(debt_id),format(input$extra_payment_month,"%Y-%m"),
        to_db(input$extra_payment_amount),input$extra_payment_note %||% "",editing_extra())
      editing_extra(NA_integer_)
      DT::selectRows(DT::dataTableProxy("extra_payments_table",session=session),NULL)
    },"Occasional payment saved. Debt forecasts updated.")
  })
  observeEvent(input$delete_extra_payment,{
    inform({e<-selected_extra();if(is.null(e))stop("Select a payment first.")
      delete_debt_extra(con,e$id[1]);editing_extra(NA_integer_)
    },"Planned payment deleted")
  })
  observeEvent(input$complete_extra_payment,{
    inform({e<-selected_extra();if(is.null(e))stop("Select a payment first.")
      set_debt_extra_completed(con,e$id[1],TRUE)
    },"Marked paid. Update this debt's current balance if needed.")
  })
  observeEvent(input$undo_extra_payment,{
    inform({e<-selected_extra();if(is.null(e))stop("Select a payment first.")
      set_debt_extra_completed(con,e$id[1],FALSE)
    },"Payment is planned again")
  })
  output$extra_payments_table <- renderDT({e<-extra_data()
    if(!nrow(e)) return(datatable(data.frame(Info="No occasional payments scheduled yet"),options=list(dom="t"),rownames=FALSE))
    datatable(data.frame(Month=e$month,Debt=e$debt_name,`Extra payment`=rv(e$amount),
      Status=ifelse(e$completed==1,"Paid","Planned"),Note=e$note,check.names=FALSE),
      selection="single",rownames=FALSE,options=list(dom="tip",pageLength=8,scrollX=TRUE))
  })
  payoff <- reactive({
    simulate_payoff(debt_data(),method=input$payoff_method %||% "minimums",
                    lump_sums=extra_data())
  })
  payoff_without_extras <- reactive({
    simulate_payoff(debt_data(),method=input$payoff_method %||% "minimums")
  })
  output$payoff_result <- renderUI({
    result<-payoff();base<-payoff_without_extras()
    if(!nrow(result$by_debt))return(tags$p(class="hint","Create your first debt to see its payoff projection and interest."))
    all_paid <- isTRUE(result$finished)
    heading<-if(all_paid)
      paste("Debt-free in",result$months,"months (",format(seq(as.Date(paste0(result$start_month,"-01")),by="month",length.out=result$months+1)[result$months],"%B %Y"),")",sep=" ")
    else "Some debts do not pay off within the 50-year projection window"
    savings <- if(isTRUE(base$finished) && all_paid) max(0,base$interest-result$interest) else NA_real_
    div(class="help-tip",
      tags$strong(heading),tags$br(),
      if(all_paid) paste("Total projected interest across all debts:",fmt(result$interest))
      else paste("Interest accrued within the forecast window:",fmt(result$interest),"(NOT lifetime interest)"),
      tags$br(),
      if(is.finite(savings)) paste("Interest avoided through planned extra payments:",fmt(savings))
      else "Interest savings are not a lifetime estimate until both scenarios pay off.",
      tags$br(),paste("One-off repayments applied in forecast:",fmt(result$extra_paid)),
      if(result$unused_extras>0.01) tags$p(class="hint",paste("Scheduled amounts exceeding loan balances:",fmt(result$unused_extras),"— excess was not shifted to other debts."))
    )
  })
  output$payoff_plot <- renderPlot({
    result<-payoff()
    if(!nrow(result$by_debt_history)){
      plot.new();text(.5,.5,"Add a debt to see its own repayment curve",col="#65778b");return()
    }
    lines<-result$by_debt_history
    total<-result$history
    lines$outstanding<-to_ui(lines$outstanding);total$outstanding<-to_ui(total$outstanding)
    if(!isTRUE(input$debt_full_curve)) {
      set<-settings_data();lo<-as.Date(paste0(set$planner_start[1],'-01'))
      hi<-seq(as.Date(paste0(set$planner_end[1],'-01')),by='month',length.out=2)[2]
      lines<-lines[lines$date>=lo & lines$date<hi,,drop=FALSE]
      total<-total[total$date>=lo & total$date<hi,,drop=FALSE]
    }
    if(!nrow(lines)) {plot.new();text(.5,.5,'No forecast points in calendar range. Choose a wider range or show full curve.');return()}
    ggplot(lines,aes(x=date,y=outstanding,color=debt,group=debt))+
      geom_line(linewidth=1.2)+
      geom_point(data=lines[lines$extra_paid>0,,drop=FALSE],size=2.6,show.legend=FALSE)+
      geom_line(data=total,aes(x=date,y=outstanding),inherit.aes=FALSE,
                color="#25364d",linewidth=.95,linetype="dashed")+
      scale_y_continuous(labels=function(x)format(round(x),big.mark=" ",scientific=FALSE))+
      labs(x=tr('Projected month','Prognosemonat'),y=tr(paste('Remaining debt',currency()),paste('Restschuld',currency())),
           color=tr('Individual debt','Einzelne Schuld'),
           caption=tr('Each color = one debt · dashed dark line = combined debt · dots = occasional extra payments',
                      'Jede Farbe = eine Schuld · dunkle gestrichelte Linie = Gesamtschuld · Punkte = Sondertilgungen'))+
      theme_minimal(base_size=11)+
      theme(legend.position="bottom",panel.grid.minor=element_blank(),
            plot.caption=element_text(color="#61748a",size=9))
  })
  output$debt_interest_table <- renderDT({
    r<-payoff();base<-payoff_without_extras();d<-r$by_debt
    if(!nrow(d))return(datatable(data.frame(Info="Add a debt to view per-debt interest"),options=list(dom="t"),rownames=FALSE))
    b<-base$by_debt[match(d$debt_id,base$by_debt$debt_id),,drop=FALSE]
    saved<-ifelse(!is.na(d$paid_off_month) & !is.na(b$paid_off_month),
      round(pmax(0,to_ui(b$interest-d$interest)),2),NA_real_)
    datatable(data.frame(Debt=d$debt,`Current balance`=rv(d$starting_balance),
      `Projected interest`=rv(d$interest),`Interest avoided`=ifelse(is.na(saved),"—",format(saved,big.mark=" ",scientific=FALSE)),
      `Extra actually applied`=rv(d$extra_paid),`Paid off (month)`=ifelse(is.na(d$paid_off_month),"Not within 50 years",d$paid_off_month),check.names=FALSE),
      rownames=FALSE,options=list(dom="t",scrollX=TRUE,pageLength=20))
  })
  output$debts_table <- renderDT({d<-debt_data();datatable(data.frame(Debt=d$name,Balance=rv(d$balance),APR=paste0(d$apr,"%"),`Minimum / month`=rv(d$min_payment),Active=ifelse(d$active==1,"Yes","No"),check.names=FALSE),rownames=FALSE,options=list(dom="tip",pageLength=8))})

  # Money lent: link an actual imported outflow OR create an explicit cash entry.
  loan_picks <- function(direction) {
    x <- selectable_loan_bank_txns(con, direction)
    if (is.null(x) || !nrow(x)) return(c('Manual cash entry'='manual'))
    labels <- paste(x$txn_date, '·', vapply(abs(x$amount), fmt, character(1)),
                    '·', substr(x$description, 1, 58))
    c('Manual cash entry'='manual', stats::setNames(as.character(x$id), labels))
  }
  observe({rev();updateSelectInput(session,'loan_bank_tx',choices=loan_picks('out'),
                                  selected=isolate(input$loan_bank_tx)%||%'manual')
               updateSelectInput(session,'repay_bank_tx',choices=loan_picks('in'),
                                  selected=isolate(input$repay_bank_tx)%||%'manual')})
  observeEvent(input$loan_choice,{
    x<-loan_data();key<-req(input$loan_choice)
    i<-if(key=='new')NA_integer_ else match(as.integer(key),x$id)
    if(length(i)!=1L || is.na(i)){
      updateTextInput(session,'loan_borrower',value='')
      updateNumericInput(session,'loan_amount',value=to_ui(1000))
      updateDateInput(session,'loan_date',value=Sys.Date())
      updateDateInput(session,'loan_due',value=Sys.Date()+30)
      updateTextInput(session,'loan_note',value='')
      updateCheckboxInput(session,'loan_in_forecast',value=TRUE)
      updateSelectInput(session,'loan_bank_tx',selected='manual')
    }else{
      updateTextInput(session,'loan_borrower',value=x$borrower[i])
      updateNumericInput(session,'loan_amount',value=to_ui(x$amount[i]))
      updateDateInput(session,'loan_date',value=as.Date(x$lent_date[i]))
      updateDateInput(session,'loan_due',value=as.Date(x$expected_return[i]))
      updateTextInput(session,'loan_note',value=x$note[i])
      updateCheckboxInput(session,'loan_in_forecast',value=x$include_forecast[i]==1)
      updateSelectInput(session,'loan_bank_tx',selected='manual')
    }
  })
  observeEvent(input$loan_new,updateSelectInput(session,'loan_choice',selected='new'))
  observeEvent(input$loan_save,inform({
    selected<-input$loan_choice%||%'new'
    if(selected=='new') {
      txid<-if(identical(input$loan_bank_tx,'manual'))NA_integer_ else as.integer(input$loan_bank_tx)
      record_money_lent(con,input$loan_borrower,to_db(input$loan_amount),
        as.character(req(input$loan_date)),as.character(req(input$loan_due)),
        txid,input$loan_note,isTRUE(input$loan_in_forecast))
    } else update_money_lent(con,as.integer(selected),input$loan_borrower,
         as.character(req(input$loan_due)),input$loan_note,isTRUE(input$loan_in_forecast))
  },'Money lent saved'))
  observeEvent(input$loan_delete,{
    id<-input$loan_choice
    if(is.null(id)||id=='new') {showNotification('Select a loan to delete',type='warning');return()}
    showModal(modalDialog(title='Delete this loan and its repayments?',
      'Imported bank records will be kept and their classification restored. Any manual cash entries created for the loan will be removed.',
      footer=tagList(modalButton('Cancel'),actionButton('confirm_loan_delete','Delete loan',class='btn-danger')),easyClose=TRUE))
  })
  observeEvent(input$confirm_loan_delete,{
    removeModal()
    inform({
      id<-req(input$loan_choice);if(id=='new')stop('Select an existing loan.')
      delete_money_lent(con,as.integer(id))
      updateSelectInput(session,'loan_choice',selected='new')
    },'Loan deleted')
  })
  observeEvent(input$repay_save,inform({
    loan_id<-as.integer(req(input$repay_loan))
    txid<-if(identical(input$repay_bank_tx,'manual'))NA_integer_ else as.integer(input$repay_bank_tx)
    record_loan_repayment(con,loan_id,to_db(input$repay_amount),as.character(req(input$repay_date)),
      txid,input$repay_note)
  },'Repayment recorded'))
  output$loan_kpis<-renderUI({x<-loan_data();open<-x[x$outstanding>0,,drop=FALSE];
    fluidRow(column(4,metric('Total still owed',fmt(sum(x$outstanding)),'Principal, not your debts')),
             column(4,metric('Overdue balances',fmt(sum(open$outstanding[as.Date(open$expected_return)<Sys.Date()])),'Unpaid past due date')),
             column(4,metric('Returned so far',fmt(sum(x$repaid)),'Received principal, not salary')))
  })
  output$loan_table<-renderDT({x<-loan_data();
    if(!nrow(x))return(datatable(data.frame(Info='No money lent recorded yet'),rownames=FALSE))
    datatable(data.frame(Person=x$borrower,`Lent on`=x$lent_date,
      Principal=rv(x$amount),Returned=rv(x$repaid),Outstanding=rv(x$outstanding),
      `Expected return`=x$expected_return,Status=ifelse(x$outstanding<.005,'Paid',
        ifelse(as.Date(x$expected_return)<Sys.Date(),'Overdue','Open')),
      `Forecast included`=ifelse(x$include_forecast==1,'Yes','No'),check.names=FALSE),
      rownames=FALSE,options=list(dom='tip',scrollX=TRUE))
  })
  output$repay_table<-renderDT({x<-repayment_data();
    if(!nrow(x))return(datatable(data.frame(Info='No repayments recorded yet'),rownames=FALSE))
    datatable(data.frame(Date=x$received_date,Person=x$borrower,Amount=rv(x$amount),
      Linked=ifelse(is.na(x$linked_txn_id),'—','Bank / manual'),Note=x$note),
      rownames=FALSE,options=list(dom='tip',scrollX=TRUE))
  })
  output$loan_forecast_plot<-renderPlot({x<-loan_data();x<-x[x$outstanding>0 & x$include_forecast==1,,drop=FALSE]
    if(!nrow(x)){plot.new();text(.5,.5,tr('No open receivables forecast','Keine offenen Forderungen'),col='#65778b');return()}
    x$due<-as.Date(x$expected_return);x$outstanding<-to_ui(x$outstanding)
    ggplot(x,aes(x=due,y=outstanding,fill=borrower))+geom_col(position='stack',width=20)+
      scale_y_continuous(labels=function(v)format(round(v),big.mark=' ',scientific=FALSE))+
      labs(x=tr('Expected return date','Erwarteter Rückzahlungstermin'),y=tr(paste('Unpaid principal',currency()),paste('Offenes Kapital',currency())),fill=tr('Borrower','Schuldner'))+
      theme_minimal(base_size=11)+theme(legend.position='bottom')
  })

  observe({f<-fund_data();updateSelectInput(session,"fund_choice",choices=opt_ids(f,"+ New sinking fund"),selected=isolate(input$fund_choice)%||%"new")})
  observeEvent(input$fund_choice,{req(input$fund_choice);f<-fund_data();i<-if(input$fund_choice=="new") NA_integer_ else match(as.integer(input$fund_choice),f$id)
    if(input$fund_choice=="new"||is.na(i)){updateTextInput(session,"fund_name",value="");updateNumericInput(session,"fund_cost",value=0);updateSelectInput(session,"fund_cycle",selected="12");updateNumericInput(session,"fund_balance",value=0);updateCheckboxInput(session,"fund_active",value=TRUE);return()}
    updateTextInput(session,"fund_name",value=f$name[i]);updateNumericInput(session,"fund_cost",value=to_ui(f$annual_cost[i]));updateSelectInput(session,"fund_cycle",selected=as.character(f$cycle_months[i]));updateNumericInput(session,"fund_balance",value=to_ui(f$saved_balance[i]));updateCheckboxInput(session,"fund_active",value=f$active[i]==1)
  })
  observeEvent(input$save_fund,inform({id<-if(input$fund_choice=="new")NA_integer_ else as.integer(input$fund_choice)
    upsert_fund(con,input$fund_name,to_db(input$fund_cost),as.integer(input$fund_cycle),to_db(input$fund_balance),isTRUE(input$fund_active),id)
  },"Fund saved"))
  output$fund_kpis <- renderUI({f<-fund_data();fluidRow(
    column(4,metric("Monthly earmarked",fmt(fund_monthly_contribution(f)),"Monthly/quarterly/yearly bills")),
    column(4,metric("Already saved in envelopes",fmt(sum(f$saved_balance)),"Manually recorded balances")),
    column(4,metric("Excess over fund targets",fmt(sum(pmax(0,f$saved_balance-f$annual_cost))),"Potentially reassign to other funds"))
  )})
  output$funds_table <- renderDT({f<-fund_data()
    if(!nrow(f))return(datatable(data.frame(Info="No sinking funds yet"),options=list(dom="t")))
    datatable(data.frame(Name=f$name,`Cost / cycle`=rv(f$annual_cost),`Every N months`=f$cycle_months,`Reserve / month`=rv(ifelse(f$active==1 & f$saved_balance < f$annual_cost,f$annual_cost/f$cycle_months,0)),`Already saved`=rv(f$saved_balance),Active=ifelse(f$active==1,"Yes","No"),check.names=FALSE),rownames=FALSE,options=list(pageLength=12,dom="tip"))
  })

  observeEvent(input$save_event,inform({add_event(con,input$event_month,input$event_name,'expense',to_db(input$event_amount))},"One-off added to your plan"))
  output$events_table <- renderDT({e<-events_data();datatable(data.frame(Month=e$month,Event=e$name,Type=e$kind,Amount=rv(e$amount),State=ifelse(e$completed==1,"Completed","Planned")),selection="single",rownames=FALSE,options=list(pageLength=6,dom="tip",scrollX=TRUE))})
  selected_event <- reactive({sel<-input$events_table_rows_selected;e<-events_data();if(length(sel)!=1 || sel>nrow(e)) return(NULL);e[sel,,drop=FALSE]})
  observeEvent(input$complete_event,inform({e<-selected_event();if(is.null(e))stop("Select an event first.")
    DBI::dbExecute(con,"UPDATE planned_events SET completed=1 WHERE id=?",params=list(e$id[1]))
  },"Event marked complete"))
  observeEvent(input$delete_event,inform({e<-selected_event();if(is.null(e))stop("Select an event first.")
    DBI::dbExecute(con,"DELETE FROM planned_events WHERE id=?",params=list(e$id[1]))
  },"Event deleted"))
  output$forecast_plot <- renderPlot({
    f<-forecast()
    for(nm in c("leftover_low","leftover_high","leftover_guaranteed","leftover_expected","planned_leftover"))f[[nm]]<-to_ui(f[[nm]])
    ggplot(f,aes(x=date))+
      geom_hline(yintercept=0,color='#8d9ca9',linewidth=.4)+
      geom_ribbon(aes(ymin=leftover_low,ymax=leftover_high),fill='#92c5d3',alpha=.4)+
      geom_line(aes(y=leftover_guaranteed),color='#384961',linetype='dashed',linewidth=.9)+
      geom_line(aes(y=leftover_expected),color='#2998aa',linewidth=.95,linetype='dotted')+
      geom_line(aes(y=planned_leftover),color='#166a73',linewidth=1.35)+
      scale_y_continuous(labels=function(x)format(round(x),big.mark=' ',scientific=FALSE))+
      labs(x=tr('Calendar month','Kalendermonat'),y=tr(paste('Planned amount left',currency()),paste('Verbleibender Betrag',currency())),
           caption=tr('Solid: real-budget policy · dashed: guaranteed-only · dotted: expected · band: low/high income scenarios',
                      'Durchgezogen: Budget · gestrichelt: gesichert · gepunktet: erwartet inkl. Darlehensrückzahlung · Band: mögliche Spanne'))+
      theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank())
  })
  output$forecast_table <- renderDT({f<-forecast();datatable(data.frame(
    Month=f$month,`Guaranteed income`=rv(f$income_guaranteed),
    `Expected income`=rv(f$income),`Budgeted income`=rv(f$income_budget),
    Categories=rv(f$category_budgets),`Debt payments`=rv(f$debt_minimums+f$debt_extras),
    `Sinking funds`=rv(f$sinking_contributions),`Extra costs`=rv(f$one_off_expenses),
    `Possible loan returns`=rv(f$possible_loan_return),
    Savings=rv(f$savings),`Goals funded`=rv(f$goal_savings),`Guaranteed leftover`=rv(f$leftover_guaranteed),
    `Budgeted leftover`=rv(f$planned_leftover),`Best-case leftover`=rv(f$leftover_high),
    check.names=FALSE),rownames=FALSE,options=list(scrollX=TRUE,pageLength=12,dom='tip'))})

  observeEvent(settings_data(),{
    st<-settings_data()
    start<-st$planner_start[1];end<-st$planner_end[1]
    choices<-month_sequence(start,end)
    selected<-isolate(input$month)%||%format(Sys.Date(),'%Y-%m')
    if(!selected %in% choices)selected<-choices[1]
    updateSelectInput(session,'month',choices=choices,selected=selected)
    updateSelectInput(session,'event_month',choices=choices,selected=selected)
    updateDateInput(session,'plan_start',value=as.Date(paste0(start,'-01')))
    updateDateInput(session,'plan_end',value=as.Date(paste0(end,'-01')))
    updateNumericInput(session,'setting_savings',value=to_ui(st$savings_target[1]))
    updateNumericInput(session,'setting_buffer',value=to_ui(st$safety_buffer[1]))
    updateNumericInput(session,'setting_equity',value=to_ui(st$opening_equity[1]))
    updateTextInput(session,'setting_cash',value=if(is.na(st$available_cash[1]))'' else as.character(to_ui(st$available_cash[1])))
  },ignoreInit=FALSE)
  output$calendar_note <- renderText({s<-settings_data();paste('Selected:',s$planner_start[1],
    'to',s$planner_end[1], '(', length(month_sequence(s$planner_start[1],s$planner_end[1])), 'monthly episodes )')})
  observeEvent(input$save_calendar,inform({
    start<-format(as.Date(req(input$plan_start)),'%Y-%m')
    end<-format(as.Date(req(input$plan_end)),'%Y-%m')
    month_sequence(start,end)
    DBI::dbExecute(con,'UPDATE settings SET planner_start=?,planner_end=? WHERE id=1',params=list(start,end))
  },'Planning timeline updated'))
  observeEvent(input$save_settings,inform({
    if(!is.finite(input$setting_savings)||input$setting_savings<0 || !is.finite(input$setting_buffer)||input$setting_buffer<0)stop("Savings and buffer must be nonnegative.")
    cash<-if(!nzchar(trimws(input$setting_cash)))NA_real_ else to_db(parse_czk(input$setting_cash))
    if(!is.na(cash) && !is.finite(cash))stop("Invalid cash balance")
    equity<-to_db(req(input$setting_equity))
    if(length(equity)!=1L || !is.finite(equity))stop('Opening equity must be a valid number.')
    DBI::dbExecute(con,'UPDATE settings SET savings_target=?,safety_buffer=?,available_cash=?,opening_equity=?,other_assets=? WHERE id=1',
                   params=list(to_db(input$setting_savings),to_db(input$setting_buffer),cash,equity,to_db(input$setting_other_assets)))
  },"Safeguards saved"))
  # Currency settings + manual FX. Previously saved CZK monetary amounts remain unchanged.
  observeEvent(settings_data(),{
    st<-settings_data()
    # Re-render every editor's monetary fields when display settings change.
    # Unsaved edits are deliberately superseded by the persisted values.
    src<-get_sources(con);i<-match(isolate(input$source_choice),as.character(src$id))
    if(length(i)==1L && !is.na(i))for(k in c('source_amount','source_low','source_high')) {
      nm<-switch(k,source_amount='monthly_amount',source_low='min_amount',source_high='max_amount')
      updateNumericInput(session,k,value=to_ui(src[[nm]][i]))
    }
    d<-get_debts(con);i<-match(isolate(input$debt_choice),as.character(d$id))
    if(length(i)==1L && !is.na(i)){updateNumericInput(session,'debt_balance',value=to_ui(d$balance[i]));updateNumericInput(session,'debt_min',value=to_ui(d$min_payment[i]))}
    c<-get_categories(con);i<-match(isolate(input$cat_choice),as.character(c$id))
    if(length(i)==1L && !is.na(i)){updateNumericInput(session,'cat_budget',value=to_ui(c$default_budget[i]));
      b<-month_budgets(con,isolate(input$month)%||%format(Sys.Date(),'%Y-%m'))
      updateNumericInput(session,'cat_override',value=to_ui(b$budget[match(c$id[i],b$id)]))}
    o<-get_oneoffs(con);i<-match(isolate(input$once_choice),as.character(o$id))
    if(length(i)==1L && !is.na(i))for(pair in list(c('once_low','low'),c('once_expected','expected'),c('once_high','high')))
      updateNumericInput(session,pair[1],value=to_ui(o[[pair[2]]][i]))
    st<-settings_data()
    # Currency change can occur while any tab is open. Ensure saved selections
    # are shown in the newly chosen unit when users return to their editors.
    f<-get_funds(con);i<-match(isolate(input$fund_choice),as.character(f$id))
    if(length(i)==1L && !is.na(i)){updateNumericInput(session,'fund_cost',value=to_ui(f$annual_cost[i]));updateNumericInput(session,'fund_balance',value=to_ui(f$saved_balance[i]))}
    goals<-get_goals(con);i<-match(isolate(input$goal_choice),as.character(goals$id))
    if(length(i)==1L && !is.na(i)){updateNumericInput(session,'goal_target',value=to_ui(goals$target_amount[i]));
      updateNumericInput(session,'goal_saved',value=to_ui(goals$saved_balance[i]));updateNumericInput(session,'goal_monthly',value=to_ui(goals$monthly_allocation[i]))}
    extras<-get_debt_extras(con);i<-match(isolate(input$extra_payments_table_rows_selected),seq_len(nrow(extras)))
    if(length(i)==1L && !is.na(i))updateNumericInput(session,'extra_payment_amount',value=to_ui(extras$amount[i]))
    loans<-get_receivables(con);i<-match(isolate(input$loan_choice),as.character(loans$id))
    if(length(i)==1L && !is.na(i))updateNumericInput(session,'loan_amount',value=to_ui(loans$amount[i]))
    accounts<-get_accounts(con);i<-match(isolate(input$account_choice),accounts$account_key)
    if(length(i)==1L && !is.na(i))updateTextInput(session,'account_balance',value=if(is.na(accounts$balance_czk[i]))'' else as.character(to_ui(accounts$balance_czk[i])))
    updateSelectInput(session,'setting_currency' ,selected=st$display_currency[1])
    updateNumericInput(session,'setting_fx',value=st$eur_czk_rate[1])
    updateNumericInput(session,'setting_other_assets',value=to_ui(st$other_assets[1]))
    session$sendCustomMessage('budgetforge-currency',list(currency=st$display_currency[1]))
  },ignoreInit=FALSE)
  output$currency_explainer<-renderText({
    st<-settings_data();paste('1 EUR =',st$eur_czk_rate[1],'CZK. Amounts shown in',currency(),
      '— existing stored values are not overwritten when you switch display currency.')
  })
  observeEvent(input$save_currency,inform({
    curr<-req(input$setting_currency);rate<-req(input$setting_fx)
    if(!curr %in% c('CZK','EUR') || !is.finite(rate) || rate<=0 || rate>10000)
      stop('Choose EUR or CZK and a valid CZK/EUR exchange rate.')
    DBI::dbExecute(con,'UPDATE settings SET display_currency=?,eur_czk_rate=? WHERE id=1',params=list(curr,rate))
  },'Currency preferences saved'))

  # Owned bank accounts: adding another account lets one-sided transfers be found.
  observe({a<-account_data();choices<-c('+ New account'='new',setNames(a$account_key,a$display_name))
    sel<-isolate(input$account_choice)%||%'new';if(!sel %in% choices)sel<-'new'
    updateSelectInput(session,'account_choice',choices=choices,selected=sel)
  })
  observeEvent(input$account_choice,{
    a<-account_data();i<-match(input$account_choice,a$account_key)
    if(length(i)!=1L || is.na(i)){
      updateTextInput(session,'account_key',value='');updateTextInput(session,'account_name',value='')
      updateTextInput(session,'account_bank',value='');updateTextInput(session,'account_balance',value='')
      updateCheckboxInput(session,'account_owned',value=TRUE)
    } else {
      updateTextInput(session,'account_key',value=a$account_key[i]);updateTextInput(session,'account_name',value=a$display_name[i])
      updateTextInput(session,'account_bank',value=a$bank[i]);updateCheckboxInput(session,'account_owned',value=a$is_owned[i]==1)
      updateTextInput(session,'account_balance',value=if(is.na(a$balance_czk[i]))'' else as.character(to_ui(a$balance_czk[i])))
    }
  })
  observeEvent(input$new_account,updateSelectInput(session,'account_choice',selected='new'))
  observeEvent(input$save_account,inform({
    key<-normalize_account_id(req(input$account_key));name<-req(input$account_name)
    val<-trimws(input$account_balance%||%'')
    balance<-if(nzchar(val))to_db(parse_czk(val)) else NA_real_
    if(nzchar(val) && !is.finite(balance))stop('Invalid account balance.')
    # Account identifiers are keys; creating a different identifier does not
    # silently remap statements belonging to another original account.
    upsert_account(con,key,name,input$account_bank,isTRUE(input$account_owned),balance)
    mark_internal_transfers(con)
    session$onFlushed(function()updateSelectInput(session,'account_choice',selected=key),once=TRUE)
  },'Account saved; transfers rechecked'))
  output$accounts_table<-renderDT({
    a<-account_data();if(!nrow(a))return(datatable(data.frame(Info='No bank accounts registered'),rownames=FALSE))
    datatable(data.frame(Account=a$display_name,Identifier=a$account_key,Bank=a$bank,
      Owned=ifelse(a$is_owned==1,'Yes','No'),Balance=ifelse(is.na(a$balance_czk),'Unknown',
        vapply(a$balance_czk,fmt,character(1))),check.names=FALSE),
      rownames=FALSE,options=list(dom='tip',pageLength=table_rows(),scrollX=TRUE))
  })

  # Goal allocations are forward-looking earmarks, not bank transfers or costs.
  observe({g<-goal_data();selected<-isolate(input$goal_choice)%||%'new'
    if(selected!='new' && !selected %in% as.character(g$id))selected<-'new'
    updateSelectInput(session,'goal_choice',choices=opt_ids(g,'+ New savings goal'),selected=selected)
  })
  observeEvent(input$goal_choice,{
    g<-goal_data();i<-if(is.null(input$goal_choice) || identical(input$goal_choice,'new'))NA_integer_ else match(as.integer(input$goal_choice),g$id)
    if(length(i)!=1L || is.na(i)){
      updateTextInput(session,'goal_name',value='')
      updateNumericInput(session,'goal_target',value=to_ui(12000))
      updateNumericInput(session,'goal_saved',value=0);updateNumericInput(session,'goal_monthly',value=to_ui(1000))
      updateCheckboxInput(session,'goal_has_date',value=FALSE)
      updateCheckboxInput(session,'goal_emergency',value=FALSE)
      updateCheckboxInput(session,'goal_active',value=TRUE)
    } else {
      x<-g[i,,drop=FALSE]
      updateTextInput(session,'goal_name',value=x$name[1]);updateNumericInput(session,'goal_target',value=to_ui(x$target_amount[1]))
      updateNumericInput(session,'goal_saved',value=to_ui(x$saved_balance[1]))
      updateNumericInput(session,'goal_monthly',value=to_ui(x$monthly_allocation[1]))
      updateCheckboxInput(session,'goal_has_date',value=!is.na(x$target_month[1]))
      if(!is.na(x$target_month[1]))updateDateInput(session,'goal_deadline',value=as.Date(paste0(x$target_month[1],'-01')))
      updateCheckboxInput(session,'goal_emergency',value=x$emergency_available[1]==1)
      updateCheckboxInput(session,'goal_active',value=x$active[1]==1)
    }
  })
  observeEvent(input$new_goal,updateSelectInput(session,'goal_choice',selected='new'))
  observeEvent(input$save_goal,inform({
    id<-if((input$goal_choice%||%'new')=='new')NA_integer_ else as.integer(input$goal_choice)
    target_date<-if(isTRUE(input$goal_has_date))format(as.Date(req(input$goal_deadline)),'%Y-%m') else NA_character_
    upsert_goal(con,input$goal_name,to_db(input$goal_target),to_db(input$goal_saved),
      to_db(input$goal_monthly),target_date,isTRUE(input$goal_emergency),isTRUE(input$goal_active),id)
  },'Savings goal saved'))
  observeEvent(input$delete_goal,inform({
    if((input$goal_choice%||%'new')=='new')stop('Select a goal to delete.')
    DBI::dbExecute(con,'DELETE FROM savings_goals WHERE id=?',params=list(as.integer(input$goal_choice)))
    updateSelectInput(session,'goal_choice',selected='new')
  },'Goal deleted'))
  output$goal_kpis<-renderUI({g<-goal_data()
    fluidRow(column(4,metric('Saved in goals',fmt(sum(g$saved_balance)),'Earmarked cash; not extra assets')),
      column(4,metric('Monthly goal funding',fmt(goal_monthly_allocation(g,input$month)),'Reserved in the budget')),
      column(4,metric('Emergency-accessible goals',fmt(sum(g$saved_balance[g$emergency_available==1])),'Available in emergencies if still liquid')))
  })
  output$goals_table<-renderDT({g<-goal_data()
    if(!nrow(g))return(datatable(data.frame(Info='Create your first goal'),rownames=FALSE))
    datatable(data.frame(Goal=g$name,Target=rv(g$target_amount),Saved=rv(g$saved_balance),
      `Remaining to fund`=rv(pmax(0,g$target_amount-g$saved_balance)),
      `Monthly allocation`=rv(ifelse(g$active==1 & g$target_amount>g$saved_balance,g$monthly_allocation,0)),
      Progress=paste0(round(ifelse(g$target_amount>0,100*pmin(1,g$saved_balance/g$target_amount),100)),'%'),
      Deadline=ifelse(is.na(g$target_month),'None',g$target_month),
      Emergency=ifelse(g$emergency_available==1,'Yes','No'),check.names=FALSE),
      rownames=FALSE,options=list(pageLength=table_rows(),dom='tip',scrollX=TRUE))
  })
  output$goals_plot<-renderPlot({g<-goal_data();if(!nrow(g)){
    plot.new();text(.5,.5,'Add a savings goal to show progress');return()}
    g$progress<-ifelse(g$target_amount>0,100*pmin(1,g$saved_balance/g$target_amount),100)
    ggplot(g,aes(x=reorder(name,progress),y=progress,fill=emergency_available==1))+
      geom_col(width=.64,show.legend=FALSE)+coord_flip()+
      scale_y_continuous(limits=c(0,100),labels=function(x)paste0(x,'%'))+
      labs(x=NULL,y=tr('Saved toward target','Gespart zum Ziel'))+
      theme_minimal(base_size=11)
  })

  output$cashflow_comparison<-renderPlot({
    m<-finance()$monthly
    if(!nrow(m)){plot.new();return()}
    # Paired planned and actual for each requested metric; separate facets.
    v<-do.call(rbind,lapply(c('Income','Expenses','Remaining'),function(label){
      nm<-tolower(label)
      rbind(data.frame(date=m$date,metric=label,kind='Planned',value=to_ui(m[[paste0('plan_',nm)]])),
            data.frame(date=m$date,metric=label,kind='Actual',value=to_ui(m[[paste0('actual_',nm)]])))
    }))
    ggplot(v[is.finite(v$value),,drop=FALSE],aes(x=date,y=value,fill=kind))+
      geom_col(position=position_dodge2(preserve='single',padding=.17),width=23)+
      facet_wrap(~metric,ncol=1,scales='free_y')+
      scale_fill_manual(values=c(Actual='#1b7c8a',Planned='#a2bac9'))+
      labs(x=tr('Calendar month','Kalendermonat'),y=currency(),fill=NULL,
           caption=tr('No actual bar means no imported/manual transactions that month. Goal transfers and internal account moves are excluded.',
             'Fehlende Ist-Balken bedeuten keine gebuchten Transaktionen. Eigene Umbuchungen und Sparziele zählen nicht als Konsum.'))+
      theme_minimal(base_size=11)+theme(legend.position='top',panel.grid.minor=element_blank())
  })
  output$financial_health<-renderUI({m<-finance();s<-m$snapshot
    pct<-function(x)if(!is.finite(x))'—' else sprintf('%.1f%%',x)
    fluidRow(
      column(4,metric('Savings rate',pct(m$savings_rate),'Actual inflows less actual outflows / inflows (cash-flow proxy)')),
      column(4,metric('Debt-to-income',pct(m$dti),'Active minimum payments / budgeted income')),
      column(4,metric('Estimated net worth',fmt(s$net_worth),'Needs entered account or cash balance')),
      column(4,metric('Financial runway',if(is.finite(m$runway))sprintf('%.1f months',m$runway) else '—','Liquid balance / essential monthly costs')),
      column(4,metric('Spending trend',if(is.finite(m$trend$increase))paste(m$trend$category,fmt(m$trend$increase)) else m$trend$category,'Largest category increase vs previous month')),
      column(4,metric('Forecast accuracy',pct(m$accuracy),'Historical cash flow: 100% minus normalized mean absolute error')))
  })
  output$net_worth_projection<-renderPlot({
    f<-forecast();s<-settings_data()
    # Principal repayments change debt and assets equally, therefore only
    # estimated interest (and living expenses) diminish modeled net worth.
    debt_start<-format(Sys.Date(),'%Y-%m')
    pay<-simulate_payoff(debt_data(),method=input$payoff_method%||%'minimums',
      lump_sums=extra_data(),start_month=debt_start)
    interest_month<-numeric(nrow(f))
    if(nrow(pay$by_debt_history))for(i in seq_len(nrow(f))) {
      d<-pay$by_debt_history
      interest_month[i]<-sum(d$interest_month[format(d$date,'%Y-%m')==f$month[i]])
    }
    changes<-f$income-f$category_budgets-f$one_off_expenses-interest_month
    f$worth<-to_ui(s$opening_equity[1]+cumsum(changes))
    ggplot(f,aes(x=date,y=worth))+geom_line(color='#176b80',linewidth=1.2)+geom_point(size=1.1,color='#176b80')+
      scale_y_continuous(labels=function(x)format(round(x,2),big.mark=' ',scientific=FALSE))+
      labs(x=tr('Calendar month','Kalendermonat'),y=paste('Modeled net worth',currency()))+
      theme_minimal(base_size=11)
  })

  output$setup_progress <- renderUI({
    n_sources<-nrow(src_data());n_debts<-nrow(debt_data());n_cats<-sum(cat_data()$default_budget>0)
    n_funds<-nrow(fund_data())
    div(class="help-tip",tags$strong("Your setup progress"),
      tags$p(paste(n_sources,"income sources ·",n_cats,"categories budgeted ·",
        n_debts,"debts entered ·",n_funds,"sinking funds")))
  })
}

`%||%` <- function(a,b) if(is.null(a)||length(a)==0||all(is.na(a))) b else a
shinyApp(ui,server)
