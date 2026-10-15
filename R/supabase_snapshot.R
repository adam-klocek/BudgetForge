# BudgetForge account authentication and private state persistence.
# Transitional architecture: local per-Shiny-session SQLite + one private,
# revisioned SQLite snapshot per Supabase Auth user. The remote snapshot table
# MUST be protected by Row Level Security. See supabase/02_user_snapshots.sql.
# Never put a service-role/secret key in this app.

bf_config <- function() {
  url <- sub('/+$', '', Sys.getenv('SUPABASE_URL', unset=''))
  key <- Sys.getenv('SUPABASE_PUBLISHABLE_KEY', unset='')
  if (!grepl('^https://[^/]+$', url) || !nzchar(key)) {
    stop('Set SUPABASE_URL and SUPABASE_PUBLISHABLE_KEY in the deployment settings.')
  }
  if (grepl('^(sb_secret_|eyJ)', key)) {
    # Legacy anon JWT keys begin eyJ, and may be valid, but require an
    # unambiguous publishable key for this deployment to prevent misconfiguration.
    stop('Use the sb_publishable_ API key, NOT a service-role or secret key.')
  }
  if (!startsWith(key, 'sb_publishable_')) stop('Invalid publishable key format.')
  list(url=url, key=key)
}

bf_json <- function(response) {
  jsonlite::fromJSON(httr2::resp_body_string(response), simplifyVector=FALSE)
}

bf_auth_request <- function(path) {
  cfg <- bf_config()
  httr2::request(paste0(cfg$url, path)) |>
    httr2::req_headers(apikey=cfg$key, Accept='application/json') |>
    httr2::req_timeout(25)
}

bf_sign_in <- function(email, password) {
  if (!nzchar(trimws(email)) || !nzchar(password)) stop('Enter email and password.')
  result <- bf_auth_request('/auth/v1/token') |>
    httr2::req_url_query(grant_type='password') |>
    httr2::req_body_json(list(email=trimws(email),password=password)) |>
    httr2::req_perform() |> bf_json()
  if (is.null(result$access_token) || is.null(result$refresh_token)) stop('Login did not return session tokens.')
  user <- bf_auth_request('/auth/v1/user') |>
    httr2::req_headers(Authorization=paste('Bearer',result$access_token)) |>
    httr2::req_perform() |> bf_json()
  if (is.null(user$id) || !nzchar(user$id)) stop('Could not verify the logged-in identity.')
  list(
    email=as.character(user$email), id=as.character(user$id),
    access_token=as.character(result$access_token),
    refresh_token=as.character(result$refresh_token),
    expires_at=as.numeric(Sys.time())+as.numeric(result$expires_in %||% 3600)
  )
}

bf_valid_access <- function(account) {
  if (is.null(account) || is.null(account$access_token)) stop('Please sign in.')
  # Refresh before expiry, including if a Shiny session runs for hours.
  if (as.numeric(Sys.time()) >= (account$expires_at - 120)) {
    updated <- bf_auth_request('/auth/v1/token') |>
      httr2::req_url_query(grant_type='refresh_token') |>
      httr2::req_body_json(list(refresh_token=account$refresh_token)) |>
      httr2::req_perform() |> bf_json()
    account$access_token <- as.character(updated$access_token)
    account$refresh_token <- as.character(updated$refresh_token)
    account$expires_at <- as.numeric(Sys.time())+as.numeric(updated$expires_in %||% 3600)
  }
  account
}

bf_snapshot_request <- function(account) {
  cfg <- bf_config()
  httr2::request(paste0(cfg$url, '/rest/v1/budgetforge_snapshots')) |>
    httr2::req_headers(
      apikey=cfg$key,
      Authorization=paste('Bearer',account$access_token),
      Accept='application/json'
    ) |> httr2::req_timeout(45)
}

bf_download_snapshot <- function(account) {
  response <- bf_snapshot_request(account) |>
    httr2::req_url_query(select='revision,snapshot_b64') |>
    httr2::req_perform() |> bf_json()
  if (!length(response)) return(list(revision=NULL, bytes=NULL))
  if (length(response) != 1L) stop('Unexpected remote state: more than one account row returned.')
  record <- response[[1L]]
  bytes <- jsonlite::base64_dec(record$snapshot_b64)
  if (length(bytes) < 16L || rawToChar(bytes[1:15]) != 'SQLite format 3') {
    stop('Remote financial backup is not a valid SQLite database.')
  }
  list(revision=as.integer(record$revision), bytes=bytes)
}

bf_encode_local_db <- function(con, db_path) {
  # SQLite is deliberately in DELETE journal mode rather than WAL: one
  # completed statement at a time. No shared SQLite connection across users.
  if (!DBI::dbIsValid(con)) stop('The local database is closed.')
  if (!file.exists(db_path)) stop('Local database file is missing.')
  n <- file.info(db_path)$size
  if (is.na(n) || n <= 0 || n > 12 * 1024^2) {
    stop('Your account exceeds the 12 MB snapshot safety limit. Contact the app owner for database migration.')
  }
  bytes <- readBin(db_path, what='raw', n=n)
  if (length(bytes) < 16L || rawToChar(bytes[1:15]) != 'SQLite format 3') stop('Invalid local SQLite snapshot.')
  jsonlite::base64_enc(bytes)
}

# Optimistic locking (compare-and-swap). Avoids silently overwriting updates
# from a second browser window signed in to the SAME account.
bf_upload_snapshot <- function(account, con, db_path, revision=NULL) {
  b64 <- bf_encode_local_db(con, db_path)
  if (is.null(revision)) {
    response <- bf_snapshot_request(account) |>
      httr2::req_headers(Prefer='return=representation') |>
      httr2::req_body_json(list(snapshot_b64=b64,revision=1L)) |>
      httr2::req_perform() |> bf_json()
  } else {
    response <- bf_snapshot_request(account) |>
      httr2::req_url_query(revision=paste0('eq.',as.integer(revision))) |>
      httr2::req_headers(Prefer='return=representation') |>
      httr2::req_body_json(list(snapshot_b64=b64,revision=as.integer(revision)+1L,
                                updated_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC'))) |>
      httr2::req_method('PATCH') |>
      httr2::req_perform() |> bf_json()
  }
  if (length(response) != 1L) {
    stop('Cloud save conflict: your account changed in another session. Reload your account before editing again.')
  }
  as.integer(response[[1]]$revision)
}

bf_fresh_connection <- function(path, remote_bytes=NULL) {
  if (file.exists(path)) unlink(path)
  if (!is.null(remote_bytes)) writeBin(remote_bytes, path)
  c <- connect_db(path)
  # Avoid SQLite write-ahead sidecars when snapshotting.
  DBI::dbGetQuery(c, 'PRAGMA journal_mode=DELETE')
  check <- DBI::dbGetQuery(c, 'PRAGMA integrity_check')[[1]][1]
  if (!identical(check, 'ok')) { DBI::dbDisconnect(c); stop('Remote snapshot failed its SQLite integrity check.') }
  init_db(c); init_planning_db(c); init_income_db(c)
  init_receivables_db(c); init_budgetforge_db(c)
  c
}
