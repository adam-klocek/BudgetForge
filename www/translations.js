/* BudgetForge interface localization. Only UI strings are translated; imported
 * descriptions and user names are not rewritten in the data store. */
(function () {
  'use strict';
  const de = {
    'Savings goals & emergency reserves':'Sparziele & Notfallrücklagen',
    'Assign monthly savings, track progress and know which reserves can be used during emergencies.':'Monatlich Geld zurücklegen, Fortschritte verfolgen und Notfallrücklagen im Blick behalten.',
    'Money assigned to a goal remains your asset. Contributions reduce free spending capacity but are not living expenses; do not count transfers to your other accounts as new spending. A goal\'s saved balance is a subset of actual bank balances.':'Ein Sparziel bleibt dein Vermögen. Einzahlungen verringern dein frei verfügbares Budget, gelten aber nicht als Konsumausgaben. Umbuchungen zwischen eigenen Konten zählen nicht als Ausgaben.',
    'Earmarked cash; not extra assets':'Zugeordnetes Geld; kein zusätzliches Vermögen',
    'Reserved in the budget':'Im Budget zurückgelegt',
    'Available in emergencies if still liquid':'Im Notfall verfügbar, soweit liquide',
    'Saved in goals':'In Sparzielen gespart','Monthly goal funding':'Monatliche Sparzielrate',
    'Emergency-accessible goals':'Verfügbare Notfallrücklagen',
    'A goal\'s saved balance is a subset of actual bank balances.':'Sparzielbeträge sind Teil deiner Kontoguthaben.',
    'Monthly allocations stop once a goal is funded (based on your last manually entered balance). Update balances whenever you deposit or withdraw money. Savings are not automatically transferred.':'Die monatliche Planung stoppt, wenn das Sparziel erreicht wäre. Aktualisiere die Beträge nach jeder tatsächlichen Ein- oder Auszahlung. Es finden keine automatischen Überweisungen statt.',
    'Target':'Ziel','Saved':'Gespart','Remaining to fund':'Noch zu sparen',
    'Monthly allocation':'Monatlicher Beitrag','Progress':'Fortschritt',
    'Deadline':'Frist','Emergency':'Notfallreserve','None':'Kein Termin',
    'No category increase':'Keine steigende Kategorie',
    'Need two months of transactions':'Zwei Monate Transaktionen erforderlich',
    'Savings rate':'Sparquote','Debt-to-income':'Schuldendienstquote',
    'Estimated net worth':'Geschätztes Nettovermögen','Financial runway':'Finanzielle Reichweite',
    'Spending trend':'Ausgabentrend','Forecast accuracy':'Prognosegenauigkeit',
    'Actual':'Ist','Planned':'Plan','Expenses':'Ausgaben','Remaining':'Verbleibend',
    'Current account balance (optional)':'Aktueller Kontostand (optional)',
    'Balances are manual snapshots and are not updated automatically by CSV imports. Missing balances are excluded from the net-worth snapshot. If you reclassify an imported transaction manually, your decision is preserved.':'Kontostände werden manuell gepflegt und beim CSV-Import nicht automatisch fortgeschrieben. Fehlende Guthaben fehlen auch in der Vermögensschätzung. Manuelle Zuordnungen bleiben erhalten.',
    'Register every account you own (including accounts whose statements are not imported). Transfers to an owned account will be marked as internal, even if the other statement is missing. Identifiers must match the bank\'s counterparty format, including bank code where present.':'Erfasse alle eigenen Konten, auch ohne Kontoauszug. Umbuchungen zu eigenen Konten werden als intern erkannt, selbst wenn die Gegenbuchung fehlt. Die Kontokennung muss dem CSV-Format einschließlich Bankleitzahl entsprechen.',
    'All financial plans are stored in CZK. Choose how to view and enter amounts. The rate is a manual planning assumption, not a live rate or a transaction-date market rate.':'Alle Planungswerte werden intern in CZK gespeichert. Wähle deine Anzeige- und Eingabewährung. Der Wechselkurs ist eine manuelle Annahme, kein Live- oder historischer Kurs.',
    'Planned vs actual — cash flow by month':'Plan vs. Ist — Geldfluss je Monat',
    'Planning and actuals compare inflows, outflows and remainder. Actuals are shown only where transaction data exists; internal transfers and reserve allocations are not living expenses.':'Planung und Ist vergleichen Einzahlungen, Auszahlungen und Überschuss. Istwerte erscheinen nur bei vorhandenen Buchungen; eigene Umbuchungen und Rücklagen sind keine Konsumausgaben.',
    'Net worth uses account balance snapshots + other assets + money owed to you − debts. Savings goals and sinking funds are earmarked subsets of existing cash, not additional assets.':'Nettovermögen berücksichtigt erfasste Kontostände, sonstige Vermögenswerte und Forderungen abzüglich Schulden. Sparziele und Rücklagen werden nicht doppelt als Vermögen gezählt.',
    'Starts from the opening net worth you supply. Projections adjust for income, planned consumption and estimated interest, not transfers between accounts or payments of debt principal. They are scenario estimates, not a historical statement.':'Ausgangspunkt ist dein angegebenes Anfangsvermögen. Das Modell berücksichtigt Einnahmen, Konsumausgaben und geschätzte Zinsen, nicht Kontoumbuchungen oder Tilgung von Darlehenskapital. Es ist keine historische Vermögensbilanz.',
    'Estimated discretionary capacity':'Verfügbares Ausgabenbudget',
    'Needs entered account or cash balance':'Kontostand oder Bargeldbestand eingeben',
    'Actual inflows less actual outflows / inflows (cash-flow proxy)':'Ist-Einzahlungen minus Auszahlungen im Verhältnis zu Einzahlungen (Näherung)',
    'Active minimum payments / budgeted income':'Mindesttilgungen im Verhältnis zu Budgeteinnahmen',
    'Liquid balance / essential monthly costs':'Liquide Mittel / notwendige Monatsausgaben',
    'Largest category increase vs previous month':'Größter Ausgabenanstieg gegenüber Vormonat',
    'Historical cash flow: 100% minus normalized mean absolute error':'Historischer Geldfluss: 100 % minus normalisierter mittlerer absoluter Fehler',
    'Savings goals':'Sparziele','Savings goal':'Sparziel','Savings goals & emergency reserves':'Sparziele & Notfallreserven',
    'Create or edit a savings goal':'Sparziel erstellen oder bearbeiten','Goal name':'Name des Sparziels',
    'Target amount (CZK)':'Zielbetrag (CZK)','Already saved (CZK)':'Bereits gespart (CZK)',
    'Monthly allocation (CZK)':'Monatliche Zuweisung (CZK)',
    'Set target deadline':'Zieltermin festlegen','Target month':'Zielmonat',
    'Allow this reserve to be used in an emergency':'Diese Reserve im Notfall verwenden',
    'Include monthly contribution in plan':'Monatliche Einzahlung einplanen',
    'Save savings goal':'Sparziel speichern','Create new goal':'Neues Sparziel',
    'Delete selected goal':'Ausgewähltes Sparziel löschen',
    'Goal progress & funding':'Sparfortschritt & Finanzierung',
    'Financial health':'Finanzkennzahlen','Savings rate':'Sparquote',
    'Debt-to-income':'Schuldendienstquote','Estimated net worth':'Geschätztes Nettovermögen',
    'Financial runway':'Finanzielle Reichweite','Spending trend':'Ausgabentrend',
    'Forecast accuracy':'Prognosegenauigkeit',
    'Planned vs actual — cash flow by month':'Geplant vs. tatsächlich — monatlicher Geldfluss',
    'Net worth path — illustrative':'Nettovermögen — Modellverlauf',
    'Currency & exchange rate':'Währung & Wechselkurs',
    'Display and input currency':'Anzeige- und Eingabewährung','CZK per 1 EUR':'CZK je 1 EUR',
    'Apply currency & rate':'Währung und Kurs anwenden',
    'My bank accounts / internal transfers':'Eigene Bankkonten / interne Überweisungen',
    'Bank account':'Bankkonto','Account number / identifier':'Kontonummer / Kennung',
    'Account label':'Kontobezeichnung','Bank':'Bank','I own this account':'Dieses Konto gehört mir',
    'Current account balance (optional)':'Aktueller Kontostand (optional)',
    'Save account':'Konto speichern','New account':'Neues Konto',
    'Other assets not represented by accounts (CZK)':'Sonstige Vermögenswerte (CZK)',
    'Opening net worth at start of chosen timeline (CZK)':'Nettovermögen zu Beginn des Planungszeitraums (CZK)',
    'Shape your financial future':'Gestalte deine finanzielle Zukunft',
    'Know where your money goes. Make every plan count.':'Behalte dein Geld im Blick. Plane mit Klarheit.',
    'About BudgetForge':'Über BudgetForge','Created by Mgr. Adam Klocek, Ph.D.':'Erstellt von Mgr. Adam Klocek, Ph.D.',
    'Overview':'Übersicht','Transactions':'Transaktionen','Categories':'Kategorien',
    'Income planner':'Einnahmenplaner','Debts':'Schulden','Money lent':'Verliehenes Geld',
    'Sinking funds':'Rücklagen','Monthly plan':'Monatsplanung','Settings':'Einstellungen',
    'Travel':'Verkehr & Reisen','Household':'Haushalt','Food home':'Lebensmittel',
    'Food outside':'Essen außer Haus','Hobbies':'Hobbys','Gifts':'Geschenke',
    'Withdrawals':'Bargeldabhebungen','ATM withdrawal':'Bargeldabhebung',
    'YOUR MONEY, WITH A PLAN':'DEIN GELD, DEIN PLAN',
    'Track what happened. Plan what comes next.':'Vergangene Ausgaben verstehen. Die Zukunft planen.',
    'Active budget month':'Aktiver Budgetmonat',
    'Your monthly command center':'Deine Monatsübersicht',
    'Actual transactions, future commitments, and realistic discretionary spending in one place.':'Buchungen, offene Verpflichtungen und verfügbares Budget auf einen Blick.',
    'Spending by category':'Ausgaben nach Kategorie','Your financial runway':'Dein finanzieller Überblick',
    'Recommended spending for the rest of the month':'Empfohlene Ausgaben für den Rest des Monats',
    'Recommendations respect remaining category budgets and your other commitments. They are not a guarantee that cash is available today.':'Empfehlungen berücksichtigen verbleibende Budgets und Verpflichtungen. Das Geld muss heute noch nicht verfügbar sein.',
    'Where your money goes':'Wofür dein Geld ausgegeben wird',
    'Transactions & automatic categorization':'Transaktionen & automatische Kategorisierung',
    'Import Fio + ČSAS exports, correct classifications, and teach BudgetForge your own keyword rules.':'Fio- und ČSAS-Exporte importieren, Zuordnungen korrigieren und eigene Suchregeln anlegen.',
    'Import bank statements':'Kontoauszüge importieren','Choose CSV files':'CSV-Dateien auswählen',
    'Import & categorize':'Importieren & kategorisieren',
    'Duplicate transactions are skipped on repeat imports. Imports stay in your local SQLite database.':'Duplikate werden übersprungen. Die Daten bleiben in deiner lokalen SQLite-Datenbank.',
    'Add a cash transaction or unexpected income':'Barbuchung oder ungeplante Einnahme',
    'Date':'Datum','Type':'Typ','Expense':'Ausgabe','Unexpected income':'Ungeplante Einnahme',
    'Income':'Einnahme','Debt payment':'Schuldentilgung','Account transfer':'Kontoumbuchung',
    'Internal transfer':'Eigene Überweisung','Description':'Beschreibung','Amount in CZK (positive)':'Betrag in CZK (positiv)',
    'Category / source / debt':'Kategorie / Quelle / Schuld','Optional category / source / debt':'Optionale Kategorie / Quelle / Schuld',
    'Add transaction':'Buchung hinzufügen','Transaction history':'Buchungsverlauf',
    'Choose a row to reclassify it below. \'Unassigned\' spending affects your total but cannot be recommended per category until categorized.':'Wähle eine Zeile, um sie neu zuzuordnen. Nicht zugewiesene Ausgaben zählen zum Gesamtbetrag, aber nicht zum Kategoriebudget.',
    'Transaction type':'Buchungstyp','Save classification':'Zuordnung speichern',
    'Optional merchant keyword to remember (literal text)':'Optionales Suchwort des Händlers (wörtlich)',
    'Save keyword rule':'Suchregel speichern','Reapply rules to unreviewed transactions':'Regeln auf ungeprüfte Buchungen anwenden',
    'Delete selected manual entry':'Manuelle Buchung löschen','My learned rules':'Meine eigenen Regeln',
    'Delete selected custom rule':'Eigene Regel löschen',
    'Build a budget you can stick to':'Ein realistisches Budget erstellen',
    'Keep necessities, flexible spending, and your own custom categories separate.':'Pflichtausgaben, variable Ausgaben und eigene Kategorien getrennt verwalten.',
    'Create or edit a category':'Kategorie erstellen oder bearbeiten','Category':'Kategorie',
    'Name':'Name','Fixed / committed expense':'Fixe / verpflichtende Ausgabe',
    'Default monthly budget (CZK)':'Monatliches Standardbudget (CZK)','Save category':'Kategorie speichern',
    'Override the selected month':'Budget für ausgewählten Monat anpassen','Budget for active month':'Budget für aktiven Monat',
    'Save monthly override':'Monatsbudget speichern','Category budgets and spending':'Kategoriebudgets und Ausgaben',
    'Fixed salaries, freelance ranges, contract guarantees and once-only payments—with an income-only equity scenario chart.':'Feste Gehälter, variable Honorare, Vertragslaufzeiten und Einmalzahlungen – mit Einnahmenszenarien.',
    'Income forecast, not a promise:':'Einnahmenprognose, keine Garantie:',
    'Recurring income sources':'Regelmäßige Einnahmequellen','Income source':'Einnahmequelle',
    'Income type':'Einnahmeart','Fixed / salary':'Fest / Gehalt','Variable / freelance':'Variabel / Freiberuflich',
    'Expected amount per month (CZK)':'Erwarteter Betrag pro Monat (CZK)',
    'Minimum scenario / month (CZK)':'Minimum-Szenario pro Monat (CZK)',
    'Maximum scenario / month (CZK)':'Maximum-Szenario pro Monat (CZK)',
    'Expected pay day':'Erwarteter Zahltag','Source starts':'Beginn der Einnahmequelle',
    'Known contract / guarantee end date':'Vertragsende / gesichertes Enddatum bekannt',
    'Confirmed through':'Gesichert bis','Active':'Aktiv','Include this income in the real budget':'Diese Einnahme im echten Budget berücksichtigen',
    'Save income source':'Einnahmequelle speichern','New income source':'Neue Einnahmequelle',
    'Sources & active-month receipts':'Einnahmequellen & Zahlungen des Monats',
    'Budget policy for uncertain income':'Budgetregel für unsichere Einnahmen',
    'Amount to count for checked variable incomes':'Für aktive variable Einnahmen anrechnen',
    'Use minimum / conservative':'Minimum / konservativ','Use expected amount':'Erwarteten Betrag nutzen',
    'Save budgeting policy':'Budgetregel speichern',
    'One-time expected income':'Erwartete Einmaleinnahme','Select planned income':'Geplante Einnahme wählen',
    'Payment month':'Zahlungsmonat','Payment date (forecast groups by month)':'Zahlungstermin (Monatsprognose)',
    'Minimum (CZK)':'Minimum (CZK)','Expected (CZK)':'Erwartet (CZK)',
    'Maximum (CZK)':'Maximum (CZK)','Certainty':'Sicherheit','Confirmed':'Gesichert',
    'Uncertain':'Unsicher','Include in real budget':'Im echten Budget berücksichtigen',
    'Already received':'Bereits erhalten','Save one-time income':'Einmaleinnahme speichern',
    'New one-time income':'Neue Einmaleinnahme','Delete one-time income':'Einmaleinnahme löschen',
    'One-time income calendar':'Kalender für Einmaleinnahmen',
    'Income source projections':'Prognosen der Einnahmequellen',
    'Opening equity':'Anfangsvermögen','Guaranteed month-one income':'Gesicherte Einnahmen im ersten Monat',
    'Expected month-one income':'Erwartete Einnahmen im ersten Monat',
    'Month-one income range':'Einnahmespanne im ersten Monat',
    'Manage debts & repayments':'Schulden & Tilgungen verwalten',
    'Each debt retains its own forecast, interest total and occasional lump-sum payments.':'Jede Schuld hat eine eigene Prognose, Zinsberechnung und Sondertilgungen.',
    'Manage each debt':'Einzelne Schuld verwalten','Debt':'Schuld',
    'Current outstanding balance (CZK)':'Aktuelle Restschuld (CZK)',
    'Annual interest rate (%)':'Jahreszinssatz (%)','Monthly minimum payment (CZK)':'Monatliche Mindestrate (CZK)',
    'Payment due day':'Fälligkeitstag','Save debt':'Schuld speichern','Create another':'Weitere anlegen',
    'Debt-free forecast':'Prognose bis zur Schuldenfreiheit',
    'When a loan is paid off, what happens to its monthly minimum?':'Was geschieht mit der Mindestrate nach vollständiger Tilgung?',
    'Stop paying that minimum':'Mindestrate entfällt','Redirect to highest-APR debt':'Auf Schuld mit höchstem Zins umleiten',
    'Redirect to smallest balance':'Auf kleinste Restschuld umleiten',
    'Show complete debt payoff forecast (ignore calendar window)':'Gesamte Tilgungsprognose anzeigen (Kalender ignorieren)',
    'Projected interest by individual debt':'Voraussichtliche Zinsen je Schuld',
    'Schedule an occasional extra payment':'Einmalige Sondertilgung planen',
    'Which debt gets the extra payment?':'Welche Schuld erhält die Sondertilgung?',
    'Extra lump-sum amount (CZK)':'Sondertilgungsbetrag (CZK)',
    'Note (optional)':'Notiz (optional)','Save occasional payment':'Sondertilgung speichern',
    'New payment':'Neue Zahlung','Your occasional payments':'Deine Sondertilgungen',
    'Mark selected as paid':'Als bezahlt markieren','Mark unpaid':'Als unbezahlt markieren',
    'Delete selected':'Auswahl löschen',
    'Make annual bills feel monthly':'Jährliche Rechnungen monatlich ansparen',
    'Create or edit a fund':'Rücklage erstellen oder bearbeiten','Fund':'Rücklage',
    'What are you saving for?':'Wofür sparst du?','Cost per billing cycle (CZK)':'Kosten pro Abrechnungszeitraum (CZK)',
    'Billing frequency':'Abrechnungsrhythmus','Monthly':'Monatlich','Quarterly':'Vierteljährlich',
    'Half-yearly':'Halbjährlich','Yearly':'Jährlich','Already set aside (CZK)':'Bereits zurückgelegt (CZK)',
    'Include in budget':'Im Budget berücksichtigen','Save sinking fund':'Rücklage speichern',
    'All funding envelopes':'Alle Rücklagen','Monthly plan':'Monatsplanung',
    'Plan expenses and debt over the calendar range you select in Settings. Expected income is managed under Income planner.':'Ausgaben und Schulden für den gewählten Zeitraum planen. Einnahmen verwaltest du im Einnahmenplaner.',
    'Add a planned one-off':'Einmalige Ausgabe planen','Month':'Monat',
    'What is happening?':'Was steht an?','Money flow':'Geldfluss','Extra expense':'Zusätzliche Ausgabe',
    'Amount (CZK)':'Betrag (CZK)','Add to plan':'Zur Planung hinzufügen',
    'Mark a planned event completed':'Geplanten Posten abschließen','Mark selected complete':'Als erledigt markieren',
    'Delete selected':'Auswahl löschen','Forecast leftover by scenario':'Monatsüberschuss nach Szenario',
    'Your finance workspace':'Dein Finanzbereich',
    'Everything can be entered directly in BudgetForge. No spreadsheet is required.':'Alles kann direkt in BudgetForge eingegeben werden. Keine Tabellenkalkulation nötig.',
    'Display preferences':'Anzeigeeinstellungen','Application language':'App-Sprache',
    'Rows per table (all tabs)':'Zeilen pro Tabelle (alle Tabs)','All':'Alle',
    'Save display settings':'Anzeige speichern',
    'Language changes across tabs immediately. Custom names and imported bank descriptions are never translated.':'Die Sprache wechselt sofort in allen Bereichen. Eigene Namen und Bankbeschreibungen bleiben unverändert.',
    'Calendar / planning timeline':'Kalender / Planungszeitraum',
    'Planning starts':'Planung beginnt','Planning ends':'Planung endet','Apply timeline':'Zeitraum übernehmen',
    'Monthly safeguards & opening balance':'Monatliche Reserven & Anfangsbestand',
    'General savings goal per month (CZK)':'Monatliches allgemeines Sparziel (CZK)',
    'Cash safety buffer (CZK)':'Sicherheitsreserve (CZK)',
    'Optional current available cash balance (CZK)':'Optionaler aktueller verfügbarer Kassenbestand (CZK)',
    'Opening equity at start of chosen timeline (CZK)':'Anfangsvermögen zu Beginn des Zeitraums (CZK)',
    'Save safeguards & equity':'Reserven & Vermögen speichern',
    'Get started in 5 steps':'In fünf Schritten starten',
    'Security, accuracy & interpretation':'Sicherheit, Genauigkeit & Interpretation',
    'Money lent to others':'Geld an andere verliehen',
    'Track personal loans as receivables, separately from your debts and living expenses.':'Private Darlehen als Forderungen verwalten, getrennt von Schulden und Lebenshaltungskosten.',
    'Accounting rule:':'Buchungsregel:',
    'Record money lent':'Verliehenes Geld erfassen','Existing loan (edit its details)':'Bestehendes Darlehen (Details bearbeiten)',
    'Person / borrower':'Person / Schuldner','Amount lent (CZK)':'Verliehener Betrag (CZK)',
    'Date money was lent':'Datum der Auszahlung','Expected return date':'Erwarteter Rückzahlungstermin',
    'Link an imported outgoing transaction (optional)':'Importierte ausgehende Buchung verknüpfen (optional)',
    'Include possible return in optimistic monthly forecast':'Mögliche Rückzahlung in optimistischer Monatsprognose berücksichtigen',
    'Notes (optional)':'Notizen (optional)','Save money lent':'Darlehen speichern',
    'New loan':'Neues Darlehen','People who owe you money':'Personen, die dir Geld schulden',
    'Record a repayment received':'Erhaltene Rückzahlung erfassen',
    'Which loan is being repaid?':'Für welches Darlehen?',
    'Amount returned (CZK)':'Zurückgezahlter Betrag (CZK)',
    'Date received':'Eingangsdatum','Link an imported incoming transaction (optional)':'Importierte eingehende Buchung verknüpfen (optional)',
    'Repayment note':'Rückzahlungsnotiz','Record repayment':'Rückzahlung erfassen',
    'Repayment history':'Rückzahlungsverlauf','Create a manual cash entry':'Manuelle Barbuchung erstellen',
    'Total still owed':'Noch offen','Overdue balances':'Überfällige Beträge',
    'Returned so far':'Bisher zurückgezahlt','Principal, not your debts':'Forderungssumme, keine eigenen Schulden',
    'Unpaid past due date':'Überfällige offene Beträge','Received principal, not salary':'Zurückgezahltes Kapital, kein Gehalt',
    'Money lent this month':'Diesen Monat verliehenes Geld',
    'Loan principal returned':'Zurückgezahlte Darlehensbeträge',
    'Possible returns due (not spendable yet)':'Mögliche Rückzahlungen (noch nicht verfügbar)',
    'Possible loan returns':'Mögliche Darlehensrückzahlungen',
    'No open receivables forecast':'Keine offenen Forderungen in der Prognose',
    'Expected return date':'Erwarteter Rückzahlungstermin',
    'Unpaid principal (CZK)':'Offenes Kapital (CZK)','Borrower':'Schuldner',
    'Money lent saved':'Darlehen gespeichert','Repayment recorded':'Rückzahlung erfasst',
    'Display preferences saved':'Anzeigeeinstellungen gespeichert',
    'Estimated discretionary capacity':'Geschätzt frei verfügbares Budget',
    'Income received':'Erhaltene Einnahmen','Expenses already paid':'Bereits bezahlte Ausgaben',
    'Still committed':'Noch gebundene Mittel','Possible extra debt repayment:':'Mögliche Sondertilgung:',
    'Review recommended:':'Prüfung empfohlen:',
    'Received income counted in budget':'Berücksichtigte erhaltene Einnahmen',
    'Expected regular income':'Erwartete regelmäßige Einnahmen','Paid living costs':'Bezahlte Lebenshaltungskosten',
    'Paid debt payments':'Bezahlte Schuldentilgungen','Unpaid fixed expenses':'Offene Fixkosten',
    'Remaining debt minimums':'Verbleibende Mindestraten',
    'Planned occasional debt payments':'Geplante Sondertilgungen',
    'Sinking funds this month':'Rücklagen dieses Monats','Unpaid planned extras':'Offene einmalige Ausgaben',
    'Savings + buffer':'Sparziel + Reserve','Projected available capacity':'Voraussichtlich verfügbar',
    'Current cash-aware projection:':'Verfügbarkeitsprognose inklusive Bargeld:',
    'Category':'Kategorie','Monthly baseline':'Monatsbasis','Already spent':'Bereits ausgegeben',
    'Remaining plan':'Restbudget','Suggested to spend':'Empfohlener Restbetrag','Basis':'Grundlage',
    'Fixed':'Fix','Variable':'Variabel','Budget':'Budget','Spent':'Ausgegeben','Remaining':'Verbleibend',
    'Date':'Datum','Bank':'Bank','Description':'Beschreibung','Amount':'Betrag','Rule':'Regel',
    'Unassigned':'Nicht zugeordnet','Transfer':'Umbuchung','Loan repayment':'Darlehensrückzahlung',
    'Keyword':'Suchwort','Classification':'Zuordnung','Priority':'Priorität',
    'Default':'Standard','This month':'Dieser Monat','Source':'Quelle',
    'Low / mo':'Min. / Monat','Expected / mo':'Erwartet / Monat','High / mo':'Max. / Monat',
    'In real budget':'Im echten Budget','Received this month':'Diesen Monat erhalten',
    'Still expected in budget':'Noch im Budget erwartet','Open-ended':'Unbefristet',
    'Yes':'Ja','No':'Nein','Low':'Minimum','Expected':'Erwartet','High':'Maximum',
    'Include':'Berücksichtigen','Exclude':'Nicht berücksichtigen','Received':'Erhalten',
    'Guaranteed':'Gesichert','Selected budget amount':'Im Budget angesetzt',
    'Cumulative guaranteed + equity':'Kumuliert gesichert + Anfangsvermögen',
    'Cumulative expected + equity':'Kumuliert erwartet + Anfangsvermögen',
    'Calendar month':'Kalendermonat','CZK spent':'Ausgaben in CZK',
    'Opening equity + cumulative income (CZK)':'Anfangsvermögen + kumulierte Einnahmen (CZK)',
    'Individual income':'Einzelne Einnahme','Projected month':'Prognosemonat',
    'Remaining debt (CZK)':'Restschuld (CZK)','Individual debt':'Einzelne Schuld',
    'Debt':'Schuld','Balance':'Restbetrag','APR':'Jahreszins',
    'Minimum / month':'Mindestrate / Monat','Extra payment':'Sondertilgung',
    'Status':'Status','Current balance':'Aktueller Saldo',
    'Projected interest':'Voraussichtliche Zinsen','Interest saved':'Gesparte Zinsen',
    'Name':'Name','Cost / cycle':'Kosten je Zeitraum','Every N months':'Alle N Monate',
    'Reserve / month':'Rücklage / Monat','Already saved':'Schon gespart',
    'Event':'Ereignis','State':'Status','Planned':'Geplant','Completed':'Erledigt',
    'Guaranteed income':'Gesicherte Einnahmen','Expected income':'Erwartete Einnahmen',
    'Budgeted income':'Budgetierte Einnahmen','Debt payments':'Schuldentilgungen',
    'Sinking funds':'Rücklagen','Extra costs':'Zusätzliche Kosten','Savings':'Sparen',
    'Guaranteed leftover':'Gesicherter Überschuss','Budgeted leftover':'Budgetüberschuss',
    'Best-case leftover':'Überschuss im besten Fall',
    'Person':'Person','Lent on':'Verliehen am','Principal':'Kapital',
    'Returned':'Zurückgezahlt','Outstanding':'Noch offen','Expected return':'Erwartete Rückzahlung',
    'Paid':'Zurückgezahlt','Overdue':'Überfällig','Open':'Offen','Forecast included':'In Prognose',
    'Linked':'Verknüpft','Bank / manual':'Bank / manuell','Note':'Notiz',
    'No transactions for the selected month':'Keine Buchungen im gewählten Monat',
    'No sinking funds yet':'Noch keine Rücklagen','No custom rules yet':'Noch keine eigenen Regeln',
    'No money lent recorded yet':'Noch kein verliehenes Geld erfasst',
    'No repayments recorded yet':'Noch keine Rückzahlungen erfasst',
    'No one-time income yet':'Noch keine Einmaleinnahmen',
    'No data available':'Keine Daten verfügbar','No matching records':'Keine passenden Einträge',
    'Next':'Weiter','Previous':'Zurück'
  };
  Object.assign(de,{
    'Only add cash or missing transactions. If the same item also appears in a later bank CSV, delete the manual entry to avoid double-counting.':'Nur fehlende oder Barbuchungen ergänzen. Erscheint eine Buchung später im Bank-CSV, die manuelle Buchung löschen, um Doppelzählung zu vermeiden.',
    'Future months keep the default unless you change them. Category budgets cover actual expenses; do not budget the same annual bill a second time if it is funded separately.':'Für kommende Monate gilt das Standardbudget. Einmal jährlich fällige Rechnungen nicht nochmals einplanen, wenn sie bereits durch Rücklagen gedeckt sind.',
    'Expected may be anywhere within your minimum–maximum range. No probability distribution is assumed.':'Der Erwartungswert muss zwischen Minimum und Maximum liegen. Es wird keine Wahrscheinlichkeitsverteilung unterstellt.',
    'A fixed contract stops counting after its confirmed-through date. Unknown renewals are not projected. Imported receipts matched to this source reduce the unpaid amount for the active month.':'Eine feste Einnahme wird nach dem gesicherten Vertragsende nicht mehr berücksichtigt. Unbekannte Verlängerungen werden nicht angenommen. Zugeordnete Zahlungen verringern den offenen Betrag.',
    'Unchecked sources remain visible in the editor but contribute zero to budget totals and the income forecast until selected.':'Abgewählte Einnahmequellen bleiben sichtbar, fließen aber nicht in das Budget oder die Einnahmenprognose ein.',
    'Mark received when payment is recorded as a transaction. Never count a planned one-off and the matching bank transaction as two inflows.':'Nach Eingang der Zahlung als erhalten markieren. Geplante Einmalzahlung und passende Bankbuchung nicht doppelt zählen.',
    'One-time income appears in its selected month only; if uncertain, it contributes to the low / expected / high paths but not to guaranteed income.':'Einmaleinnahmen erscheinen nur im ausgewählten Monat. Unsichere Zahlungen zählen zu Szenarien, nicht zu gesicherten Einnahmen.',
    'The colored thin lines show opening equity plus each included source\'s individual cumulative contribution; thick lines show total opening equity plus all included income. The shaded area spans user-entered low/high outcomes. Expenses, taxes, debts and investment returns are NOT deducted here.':'Dünne Linien zeigen das Anfangsvermögen plus die jeweilige kumulierte Einnahmequelle; dicke Linien die Summe. Die Fläche zeigt die angenommene Spanne. Ausgaben, Steuern, Schulden und Kapitalerträge werden hier NICHT abgezogen.',
    'Enter the CURRENT balance for each debt, not its original loan value. Edit the balance after actual repayments; bank transactions do not automatically reconcile principal and interest.':'Für jede Schuld die AKTUELLE Restschuld eingeben, nicht den ursprünglichen Darlehensbetrag. Nach Tilgungen aktualisieren; Bankbuchungen gleichen Kapital und Zinsen nicht automatisch ab.',
    'No extra payment is assumed every month. Add individual one-off payments below. The chart updates whenever you save a change.':'Es werden keine monatlichen Sonderzahlungen angenommen. Unten einzelne Sondertilgungen anlegen; die Grafik aktualisiert sich nach dem Speichern.',
    'Choose a row on the right to edit it. A planned extra repayment affects the forecast and monthly headroom, but never modifies the stored loan balance until you update it after payment.':'Rechts eine Zeile wählen, um die Sondertilgung zu bearbeiten. Geplante Zahlungen beeinflussen Prognosen und Budget, ändern aber erst nach manueller Aktualisierung die gespeicherte Restschuld.',
    'Once the payment appears in your bank transactions, classify it under that debt, mark the plan as paid, and update the outstanding balance. Paid plans remain visible in the table but are excluded from future forecasts.':'Sobald die Sonderzahlung auf dem Kontoauszug erscheint, der Schuld zuordnen, als bezahlt markieren und die Restschuld aktualisieren. Bezahlte Pläne bleiben sichtbar, fließen aber nicht erneut in die Zukunft ein.',
    'For partial bank transactions, enter only the amount lent. The remaining amount stays in its original expense category. When editing a saved loan, only the person, date due, notes and forecast checkbox can be changed.':'Bei gemischten Bankbuchungen nur den verliehenen Teil eintragen. Der Rest bleibt in seiner Ausgabenkategorie. Bei gespeicherten Darlehen können Person, Fälligkeit, Notiz und Prognose-Option bearbeitet werden.',
    'A return date is a prediction, not guaranteed income. Overdue balances stay outstanding until actually repaid.':'Das Rückzahlungsdatum ist eine Schätzung, keine gesicherte Einnahme. Überfällige Forderungen bleiben offen, bis sie tatsächlich beglichen wurden.',
    'Partial repayments are supported. Attaching an imported receipt prevents it from being counted twice as earned income.':'Teilrückzahlungen sind möglich. Eine verknüpfte Bankgutschrift wird nicht gleichzeitig als verdientes Einkommen gezählt.',
    'Saved balances are manual snapshots, not automatically accumulated. If a funded bill is paid from this envelope, don\'t also treat the reserve transfer as spending.':'Rücklagenbestände müssen manuell aktualisiert werden. Wird eine Rechnung daraus bezahlt, die Rücklagenübertragung nicht zusätzlich als Ausgabe zählen.',
    'Once a fund is fully covered, BudgetForge pauses its recommended monthly contribution. Surplus can be reassigned by editing balances after you move the actual money. BudgetForge does not move funds.':'Sobald die Rücklage vollständig finanziert ist, pausiert die monatliche Empfehlung. Überschüsse können nach tatsächlicher Umbuchung neu verteilt werden. BudgetForge bewegt kein Geld.',
    'Includes eligible income and planned costs. Optimistic scenarios include possible returns of money you lent; the conservative budget does not. This is a projected monthly cash-flow surplus, not the bank balance.':'Berücksichtigt geplante Einnahmen und Kosten. Optimistische Szenarien enthalten mögliche Rückzahlungen verliehenen Geldes; das konservative Budget nicht. Dies ist ein geplanter Monatsüberschuss, kein Kontostand.',
    'Choose any start and end date. BudgetForge builds monthly episodes from this calendar, including the Income planner, Monthly plan and active-month selectors. For responsiveness, a single view can cover up to 50 years.':'Beginn und Ende frei wählen. BudgetForge berechnet Monatsabschnitte für Einnahmenplanung, Monatsplanung und Auswahlmenüs. Eine Ansicht kann maximal 50 Jahre umfassen.',
    'Starting assets minus liabilities, or another explicitly chosen financial baseline. Income graph is income-only and does not deduct spending.':'Vermögenswerte minus Verbindlichkeiten oder ein bewusst gewählter Ausgangswert. Die Einnahmengrafik zieht keine Ausgaben ab.',
    'Cash balance must represent liquid money across all tracked accounts, excluding balances already committed elsewhere. This can tighten the spending recommendation; keep it current or clear it.':'Der verfügbare Kassenbestand sollte nur wirklich liquide, noch nicht anderweitig gebundene Mittel umfassen. Wert aktuell halten oder Feld leeren.',
    'No Excel import or template is necessary. Your plans and data are stored locally in SQLite.':'Keine Excel-Vorlage nötig. Pläne und Daten werden lokal in SQLite gespeichert.',
    'Add your available cash in Settings to impose an additional cash-based limit.':'In den Einstellungen verfügbares Guthaben eingeben, um das Budget zusätzlich zu begrenzen.',
    'Create your first debt to see its payoff projection and interest.':'Eine Schuld anlegen, um Tilgungsverlauf und Zinsen zu sehen.',
    'Set money aside each month for insurance, holidays, car servicing, gifts, and other irregular costs.':'Jeden Monat Beträge für Versicherung, Urlaub, Autowartung, Geschenke und unregelmäßige Kosten zurücklegen.',
    'Local-first: this version stores finances in data/flowwise.sqlite. It has no multi-user authentication and is not designed for public hosting.':'Lokale Speicherung: Diese Version speichert Finanzen in data/flowwise.sqlite. Sie hat keine Mehrbenutzer-Anmeldung und ist nicht für öffentliche Server geeignet.',
    'Only CZK is supported. Review merchant classification, income matching, refunds, credit-card repayments, and transfers between your own accounts.':'Nur CZK wird unterstützt. Händlerzuordnungen, Eingänge, Erstattungen, Kreditkarten- und Eigenüberweisungen überprüfen.',
    'Forecasts are monthly estimates, and an amount marked available after payday may not be available to spend today.':'Prognosen sind monatliche Schätzungen. Geld, das nach Gehaltseingang frei wäre, muss heute noch nicht verfügbar sein.',
    'You may enter all plans manually; bank CSV imports are optional. Source files contain no banking or Excel records.':'Alle Pläne können manuell eingegeben werden. Bank-CSV-Import ist optional. Der Quellcode enthält keine persönlichen Bank- oder Excel-Daten.',
    'One-off debt payments are planned separately from actual bank transactions. Update each loan\'s current balance after paying.':'Sondertilgungen werden getrennt von Bankbuchungen geplant. Nach Zahlung die aktuelle Restschuld anpassen.',
    'Money lent is a cash outflow and an asset owed to you, not everyday consumption. Returns are repayments of principal, not salary. Possible future repayments never raise your conservative safe-to-spend limit.':'Verliehenes Geld ist ein Geldabfluss und gleichzeitig eine Forderung, keine Konsumausgabe. Rückzahlungen sind kein Gehalt. Mögliche zukünftige Rückzahlungen erhöhen das konservative Ausgabenlimit nicht.',
    'For partial bank transactions, enter only the amount lent. The remaining amount stays in its original expense category. When editing a saved loan, only the person, date due, notes and forecast checkbox can be changed.':'Bei gemischten Bankbuchungen nur den verliehenen Teil angeben. Der Rest bleibt bei den normalen Ausgaben. Für bestehende Darlehen lassen sich Person, Fälligkeit, Notiz und Prognose ändern.',
    'Select a loan to delete':'Darlehen zum Löschen auswählen',
    'Delete selected loan':'Ausgewähltes Darlehen löschen',
    'Delete this loan and its repayments?':'Darlehen und Rückzahlungen löschen?',
    'Imported bank records will be kept and their classification restored. Any manual cash entries created for the loan will be removed.':'Importierte Bankbuchungen bleiben bestehen und ihre ursprüngliche Zuordnung wird wiederhergestellt. Manuell erzeugte Barbuchungen werden entfernt.',
    'Delete loan':'Darlehen löschen','Cancel':'Abbrechen','Loan deleted':'Darlehen gelöscht',
    'Date money was lent (taken from bank if linked)':'Datum der Auszahlung (bei Verknüpfung aus Bank übernommen)',
    'Date received (taken from bank if linked)':'Empfangsdatum (bei Verknüpfung aus Bank übernommen)',
    'No categorized spending yet':'Noch keine kategorisierten Ausgaben',
    'No bank transactions for this month':'Diesen Monat keine Bankbuchungen',
    'Create a variable category to get a recommendation':'Variable Kategorie für eine Empfehlung anlegen',
    'Add salary, freelance or other income to begin':'Gehalt, Honorar oder andere Einnahme hinzufügen',
    'Unassigned / additional income':'Nicht zugeordnet / Zusatzeinnahme',
    'New category':'Neue Kategorie','New debt':'Neue Schuld',
    'New sinking fund':'Neue Rücklage','New income source':'Neue Einnahmequelle',
    'Expected return':'Erwartete Rückzahlung',
    'When income is confirmed':'Wenn Einnahme gesichert ist',
    'Expected return':'Erwartete Rückzahlung',
    'No entries':'Keine Einträge',
    'Manage it in the Money lent tab.':'Im Tab Verliehenes Geld verwalten.',
    'You can add your own categories and keyword rules anytime.':'Du kannst jederzeit eigene Kategorien und Suchregeln anlegen.'
  });
  let language = 'en';
  let currency = 'CZK';
  const original = new WeakMap();
  const lastRendered = new WeakMap();
  const placeholders = new WeakMap();
  const placeholderLast = new WeakMap();
  function translateTextNode(node) {
    const val = node.nodeValue || '';
    if (!val.trim()) return;
    // Distinguish externally replaced Shiny text from our own previous render,
    // even if the user changes language AND currency in either order.
    if (!original.has(node) || (lastRendered.has(node) && val.trim() !== lastRendered.get(node)))
      original.set(node, val.trim());
    const base = original.get(node);
    let next = language === 'de' ? (de[base] || base) : base;
    if (!node.parentElement?.closest('table')) next = next.replace(/\bCZK\b/g,currency);
    const match = val.match(/^(\s*)(.*?)(\s*)$/s);
    const final = (match ? match[1] : '') + next + (match ? match[3] : '');
    lastRendered.set(node,next);
    if (val !== final) node.nodeValue = final;
  }
  function walk(root) {
    if (!root) return;
    if (root.nodeType === 3) { translateTextNode(root); return; }
    if (root.nodeType !== 1) return;
    if (['SCRIPT','STYLE','TEXTAREA','CODE','PRE'].includes(root.tagName)) return;
    ['placeholder','title','aria-label'].forEach(attr => {
      const current = root.getAttribute(attr);
      if (current === null) return;
      let values = placeholders.get(root) || {};
      let last = placeholderLast.get(root) || {};
      if (!Object.prototype.hasOwnProperty.call(values,attr) ||
          (Object.prototype.hasOwnProperty.call(last,attr) && last[attr] !== current))
        values[attr] = current;
      placeholders.set(root,values);
      const source = language === 'de' ? (de[values[attr]] || values[attr]) : values[attr];
      const target = source.replace(/\bCZK\b/g,currency);
      last[attr] = target;
      placeholderLast.set(root,last);
      if (target !== current) root.setAttribute(attr,target);
    });
    [...root.childNodes].forEach(walk);
  }
  let queued = false;
  function schedule() {
    if (queued) return;
    queued = true;
    requestAnimationFrame(() => { queued = false;walk(document.body); });
  }
  function setCurrency(code) {
    currency = code === 'EUR' ? 'EUR' : 'CZK';
    schedule();
  }
  function setLanguage(lang) {
    language = lang === 'de' ? 'de' : 'en';
    document.documentElement.lang = language;
    schedule();
  }
  document.addEventListener('DOMContentLoaded', function () {
    const obs = new MutationObserver(schedule);
    obs.observe(document.body,{childList:true,characterData:true,subtree:true,attributes:true,attributeFilter:['placeholder','title','aria-label']});
    schedule();
    function install() {
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('flowwise-language',msg => setLanguage(msg.language));
        Shiny.addCustomMessageHandler('budgetforge-currency',msg => setCurrency(msg.currency));
      } else setTimeout(install,100);
    }
    install();
  });
})();
