test_that("Fio quoted preamble and Czech decimal commas are recognized", {
  p <- tempfile(fileext=".csv")
  on.exit(unlink(p),add=TRUE)
  txt <- c(
    '"Výpis č. 4/2026 z účtu ""987654321/2010"""',
    '"Období: 01.04.2026 - 30.04.2026"',
    '',
    '"ID operace";"Datum";"Objem";"Měna";"Protiúčet";"Název protiúčtu";"Kód banky";"Poznámka";"Zpráva pro příjemce";"Typ"',
    '"987";"04.04.2026";"-123,45";"CZK";"12345";"LIDL";"0800";"";"";"Platba kartou"'
  )
  writeLines(enc2utf8(txt),p,useBytes=TRUE)
  x <- parse_fio(p)
  expect_equal(nrow(x),1)
  expect_equal(x$amount,-123.45)
  expect_equal(x$txn_date,"2026-04-04")
  expect_equal(x$payee,"LIDL")
  expect_equal(x$account_key,"987654321/2010")
})

test_that("ČSAS export is recognized from UTF-16LE BOM", {
  p <- tempfile(fileext=".csv")
  on.exit(unlink(p),add=TRUE)
  contents <- paste(
    '"Název účtu vlastníka","Číslo účtu vlastníka","Datum zaúčtování","Název protiúčtu","IBAN","BIC","Protiúčet","Bankovní kód protiúčtu","Částka","Měna"',
    '"Plus účet","987654/0800","05.04.2026","LIDL","","","12345","2010","-23,90","CZK"',
    sep="\r\n"
  )
  bytes <- iconv(enc2utf8(contents),from="UTF-8",to="UTF-16LE",toRaw=TRUE)[[1]]
  con <- file(p,"wb")
  writeBin(as.raw(c(255L,254L)),con)
  writeBin(bytes,con)
  close(con)
  expect_identical(find_bank(p),"CSAS")
  d <- parse_csas(p)
  expect_equal(nrow(d),1)
  expect_equal(d$amount,-23.90)
  expect_equal(d$txn_date,"2026-04-05")
})
