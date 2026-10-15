# STATIC source verification only; run the R runtime tests locally.
from pathlib import Path
root=Path(__file__).resolve().parents[1]
a=(root/'app.R').read_text()
r=(root/'R/supabase_snapshot.R').read_text()
assert 'con <- connect_db()' not in a
assert 'bf_fresh_connection(session_db)' in a
assert 'source("R/supabase_snapshot.R"' in a
assert "bf_upload_snapshot(account, con, session_db, cloud_revision)" in a
assert "bf_sign_in(" in a
assert "uiOutput('bf_access_gate')" in a
assert "paginate=list(previous='Zurück',next=" not in a
assert 'icon("anvil")' not in a
assert "httr2::req_method('PATCH')" in r
assert 'service-role' in r
print('Static integration checks passed.')
