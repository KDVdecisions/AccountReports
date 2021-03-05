
# AccountReports

This is a template of a system I built to handle very simple account tracking.  I built it because Quickbooks could not cost-effectively handle a sole member consulting business with income and expenses in multiple countries and currencies and payment services.

This software is not intended to perform all the tasks and reporting of a full accounting system.  For example, it does not track transfers among business acounts or credit card payments.  It only provides reports on a cash (not accrual) basis.  All data entry is performed manually in Excel because I have very few monthly transactions (and Quickbooks was getting more than half of them wrong).

## Shiny Application

The application reads a user uploaded Excel file of accounting data to provide (1) visual summaries of income and expenses and (2) an opportunity to download a profit and loss report.

### Input Data

I've provided an example input file **AccountReportApp/Data/Demo_Accounts.xlsx**.  The Shiny application reads data from the first three sheets.  These sheets are:

 - *Income*: includes income from all sources, all currencies
 - *Expenses*: includes expenses of all types, all currencies
 - *Clients*: currently not used in the app or report

### Report Output

I've included an example report template **AccountReportApp/Templates/YearToDate_Account_Report.Rmd**.  The app includes code to use this template to generate an MS Word document summarizing income and expenses in the general format of my Quickbooks Profit & Loss report.  The default name of the generated report is **YTD_Report.doc** and the user has the opportunity to rename and save the file wherever they choose.


