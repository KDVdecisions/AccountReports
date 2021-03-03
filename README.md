
# AccountReports

This is a template of a system I built to handle very simple account tracking.  I built it because Quickbooks could not cost-effectively scale a sole member consulting business with income and expenses in multiple countries and currencies and payment services.

This software is not intended to perform all the task and reporting of a full accounting system.  For example, it does not track transfers or credit card payments.  All data entry is performed manually in Excel because I have very few monthly transactions (and Quickbooks was getting more than half of them wrong).

Eventually, this template will include materials to generate Annual, Year-To-Date, and Quarterly profit and loss reports on a cash basis.

# Shiny Application

The application reads a user uploaded Excel file of accounting data to provide (1) visual summaries of income and expenses and (2) an opportunity to download selected reports.

## Input Data

I've provided an example input file **AccountReports/AccountReportApp/Data/Demo_Accounts.xlsx**.  The Shiny application reads data from the first three sheets (by name).  These sheets are:

 - *Income*: includes income from all sources, all currencies
 - *Expenses_US*: all US currency expenses
 - *Expenses_CAN*: all Canadian currency expenses

The remaining sheets are lists of lookup tables used to manage data entry into the Excel and other reference resources.

## Report Output

I've included an example report template **AccountReports/AccountReportApp/Data/YearToDate_Account_Report.Rmd**.  The app includes code to use this template to generate an MS Word document summarizing Year to Date income and expenses.  The default name of the generated report is YTD_Report.doc and the user has the opportunity to rename and save the file wherever they choose.


