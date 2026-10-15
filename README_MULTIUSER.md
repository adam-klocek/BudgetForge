# BudgetForge v5.1 — Supabase sign-in and separate personal budgets

**Status: testing/beta, not audited for storing real bank statements.**

This is a compatibility bridge so the existing Shiny budget app continues using its tested R/SQLite calculations while each **authenticated Supabase user** receives a private, revisioned database snapshot stored with Supabase Row Level Security (RLS).

**Important architecture note:** This release does **not** query or populate the 15 individual Supabase financial tables from the earlier `01_supabase_schema.sql`. Instead, it adds one `budgetforge_snapshots` row per user. That is a transitional step; a production release should eventually use the 15 relational tables directly with transaction-level atomic updates, proper audit trails, and security review.

## 1. Run the additional SQL in Supabase

Open [Supabase Dashboard](https://supabase.com/dashboard) → your project → SQL Editor → New query. Paste the contents of:

`supabase/02_user_snapshots.sql`

Click Run. It creates `public.budgetforge_snapshots` with a `user_id` defaulting to the signed-in user's Supabase Auth UUID, RLS restricting SELECT/INSERT/UPDATE to the same user, and a `revision` used to prevent lost updates between two sessions using one account.

This new table is **additional** to your original 15 tables. Do not delete those tables. Nothing is imported from your original shared `data/flowwise.sqlite` automatically.

## 2. Configure the application locally

In RStudio, open the unzipped BudgetForge directory and install dependencies:

```r
install.packages(c('shiny','bslib','DBI','RSQLite','digest','dplyr',
                   'ggplot2','DT','testthat','httr2','jsonlite','rsconnect'))
```

Set the following **in the Console only** for a temporary test:

```r
Sys.setenv(
  SUPABASE_URL = 'https://YOUR-PROJECT.supabase.co',
  SUPABASE_PUBLISHABLE_KEY = 'sb_publishable_YOUR_KEY'
)
source('launch.R')
```

Use the **publishable** key. Never use a Supabase `sb_secret_` or `service_role` key. Never commit passwords, tokens or `.Renviron` to GitHub. A locally stored `.Renviron` file is optional and already ignored by `.gitignore`.

## 3. Test two separate users

You have already created two test accounts in Supabase Authentication. Use **fictional data only**:

- Sign in as User A. Add a category and debt. Confirm “Saved” notifications.
- Sign out, sign in as User B. A's category/debt must be absent. Add a different fictional entry.
- Close the browser, sign in as User A again. A's entry should persist.
- Open the *same* account in two separate windows. Edit it in Window 1, then try to edit it in Window 2. The second window should show **Cloud sync paused** rather than silently overwriting Window 1. Reload from cloud to get the latest data.
- Verify Supabase `Table Editor → budgetforge_snapshots` contains distinct rows, one per test user. DO NOT share the snapshot content (it encodes private financial data).

Check policies using SQL Editor:

```sql
SELECT tablename, rowsecurity FROM pg_tables
WHERE schemaname='public' AND tablename='budgetforge_snapshots';

SELECT policyname, roles, cmd FROM pg_policies
WHERE schemaname='public' AND tablename='budgetforge_snapshots';
```

Both the two-user security test from the previous package and the snapshot test should pass before you invite users.

## 4. Deploy changes to GitHub / Posit Connect Cloud

- Commit **app.R**, **R/supabase_snapshot.R**, and the remaining project source files. The `supabase/*.sql` SQL files are safe to commit; they contain no private keys.
- **Do not commit** your `data/flowwise.sqlite` or any bank exports.
- From the project folder in RStudio, run `rsconnect::writeManifest(appDir='.')` and commit the updated `manifest.json` so Connect Cloud installs `httr2` and `jsonlite`.
- In Posit Connect Cloud open your BudgetForge content → Settings → Variables (or Advanced settings during publishing). Add:
  - `SUPABASE_URL` = project URL
  - `SUPABASE_PUBLISHABLE_KEY` = your publishable API key
- Republish from the GitHub branch. No database password or service-role key is needed.

**Note:** The `source('launch.R')` local launcher installs packages and calls `shiny::runApp('.')`. Posit should point to `app.R` instead of the launcher.

## 5. Limitations / what this is not

- This release is **not fully audited or production-ready for real banking data**. It is for functional multi-user testing with synthetic data.
- A whole-database snapshot is uploaded after each user-initiated save. For large data it is inefficient; it is intentionally capped at **12 MB of SQLite data**.
- A temporary local database exists for each live Shiny session. Supabase owns the durable snapshot. If cloud synchronization fails, the editor locks; a browser closing before successful sync can lose the last changes.
- Two sessions of the **same** user cannot save concurrently without conflict: the second session must reload, losing uncommitted changes. Different users do not conflict.
- Login UI uses Shiny's server-side login, with JWT refresh. Passwords are sent over HTTPS to the Shiny service and Supabase; credentials are not written to your database.
- The existing local SQLite file is **not** automatically uploaded (by design, to avoid exposing the author's historical finance data to new users).
- For a production-grade consumer finance app, complete the migration to the 15 user-owned Postgres tables, audit authorization paths, backups, import sanitization, rate limiting, monitoring, and data retention. Consider independent security review.

## Troubleshooting

- `relation "budgetforge_snapshots" does not exist` or `404`: run `supabase/02_user_snapshots.sql` in the **same Supabase project** referenced by your URL.
- `Invalid login credentials`: create/confirm each test user in Supabase Auth, then retry.
- `Please use sb_publishable_`: check you copied **publishable** key, not a secret/legacy key.
- `Cloud sync paused`: a network failure, database policy rejection, account conflict, or oversized backup stopped saving. Do not continue editing. Reload safely after checking Supabase.
- Posit says the `httr2` or `jsonlite` package is missing: regenerate and commit `manifest.json`.
