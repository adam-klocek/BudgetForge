# BudgetForge v5

**Shape your financial future**  
Created by **Mgr. Adam Klocek, Ph.D.**

A local-first, single-user R Shiny budgeting tool for Fio banka and Česká spořitelna (ČSAS) CZK transaction exports, personal income planning, debt schedules, receivables, savings goals and currency-flexible financial forecasts.

## Quick start

1. Extract `BudgetForge_v5.zip` and open the `BudgetForge` directory as your working directory in RStudio.
2. Run `source("launch.R")`. R will install missing packages and launch the app in your browser.
3. In **Settings**, choose English/German, rows per table, preferred display currency (CZK/EUR), and the manually entered rate in CZK per EUR (default **24**).
4. Register **your own bank accounts**, including the accounts you transfer money into, and optionally enter current balances for net-worth estimates.
5. Set the start and end of the planning calendar. Add income, category budgets, debts, savings goals and sinking funds directly in the app. No Excel file is required.
6. Optionally import your Fio or ČSAS CSV exports (CZK statements). Review automated categories and financial classifications.

## What is new in v5

- **BudgetForge branding:** anvil icon, subtitle and author attribution under Settings.
- **EUR/CZK settings:** a configurable EUR-to-CZK planning exchange rate, used to convert money amounts on input, in tables, charts, debts and forecasts. All planning records remain stored internally in CZK; changing the rate never overwrites saved principal. This is a manually assumed display rate, **not** live, historical, or bank settlement FX.
- **Savings goals:** target amount, saved amount, monthly contribution, optional deadline, emergency-use flag and goal progress. Planned contributions reduce safe-to-spend figures, but don't count as consumption or create extra assets. Forecast contributions taper and stop at the projected target.
- **Owned bank accounts:** each account can be registered with its identifier and optional current balance. Transfers between registered owned accounts are classified as internal, even if only one side appears in an imported statement. Existing manually reviewed classifications are preserved. You may manually reclassify ambiguous transactions.
- **Monthly cash-flow bar chart:** plan vs actual inflows, outflows and remainder, with unavailable transaction history treated as missing (not zero).
- **Financial metrics:** cash-flow savings-rate proxy, debt-payment-to-budgeted-income ratio, estimated net worth, cash runway, top month-over-month category increase, and historical forecast accuracy.
- **Modeled net-worth chart:** opening net worth plus budgeted earnings, consumption and debt interest. Transfers and debt principal repayment are not treated as net-worth losses.

## Financial interpretation / important limitations

- **Currency:** Every planned financial amount is stored as CZK. You can enter/view amounts in EUR by choosing a custom rate; the app immediately converts newly entered data into CZK. Changing the rate changes displayed EUR amounts, **not** underlying balances. Only CZK-denominated Fio/ČSAS transactions can be imported: multi-currency ledger and actual historical exchange-rate accounting are not implemented.
- **Owned transfers:** account IDs should exactly match statement counterparty IDs including bank code (`/2010`, `/0800`), after whitespace normalization. Internal-transfer detection is conservative. Manually reviewed imports are not overwritten.
- **Goals are envelopes:** `saved_balance` is a manually updated snapshot of earmarked money held in one of your accounts. Allocations don't move actual money. Do **not** count these reserves twice as assets or mark account transfers as expenses. The global general savings target is additional to the named goal allocations; set it to 0 if you use only named goals.
- **Savings rate:** calculated as positive/negative monthly cash-flow remainder divided by observed inflows; it is a *proxy*, not a validated measure of actual transfers to savings or investment growth. Payments on debt principal and new loans to others affect cash flow but not net worth in the same way as ordinary spending.
- **Net worth:** snapshot is `(balances entered for owned accounts, if any, otherwise manual available cash) + other assets + outstanding personal receivables - outstanding active debt`. Missing account balance information makes the estimate incomplete. Goal and sinking-fund reserves are subsets of tracked cash, not added again. `opening equity` is the baseline for a **modeled** projected net-worth scenario and can differ from current net worth.
- **Runway:** liquid balance divided by fixed category budgets, minimum debt repayments, and planned sinking contributions. If bank/cash balance snapshots are absent, the emergency-accessible goal balance is used only as a proxy. This is not a guaranteed survival or financial safety metric.
- **Forecast accuracy:** a normalized historical actual-versus-planned net cash-flow comparison, only for months containing transactions. Plan amounts need to have been entered accurately at the time; the app does not archive older budget revisions.
- **Debt forecasts:** use monthly APR/12 approximation, not bank-specific daily interest, fees, variable rates or other lender details.
- **Storage/security:** user data stays in local SQLite file `data/flowwise.sqlite`. The app has **no user authentication or encryption**, so do not expose it on a public server without adding proper security.

## Upgrading from FlowWise v4

1. Close v4 and **back up** its `data/flowwise.sqlite`.
2. Unzip the new v5 bundle and copy the backed-up SQLite file into the new `BudgetForge/data/` folder.
3. Launch. The migration adds account metadata, goal records, exchange-rate fields and asset settings without dropping previous records.
4. Check existing account identifiers and newly recognized transfers. Check that any saved balances match the actual bank snapshots.

## Project structure

- `app.R`: Shiny UI, inputs and reactive outputs.
- `R/database.R`: core SQLite schema and CRUD.
- `R/importers.R`: Fio and ČSAS CSV parsers, categorization rules, transfer detection.
- `R/income.R`: fixed/uncertain income projections.
- `R/planning.R`: calendar forecasts and reserve allocations.
- `R/engine.R`: monthly spending limits and debt amortization.
- `R/receivables.R`: money lent, planned returns and partial repayment records.
- `R/finance_extensions.R`: v5 savings goals, owned accounts, currency, cash flow and metrics.
- `www/translations.js`: English/German text handling and currency-aware labels.
- `tests/testthat/`: regression tests with synthetic data only.

## Tests

In RStudio with the project directory as the working directory:

```r
source("tests/testthat.R")
```

No personal statements or budget spreadsheets are included in the source archive. The original private files are not needed for using BudgetForge.
