library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(rmarkdown)

flags <- c(
	"https://lipis.github.io/flag-icon-css/flags/4x3/us.svg",
	"https://lipis.github.io/flag-icon-css/flags/4x3/ca.svg",
	"https://lipis.github.io/flag-icon-css/flags/4x3/eu.svg"
)

source("R/ar_paidInvoices.R")
source("R/ar_pendInvoices.R")

ui <- fluidPage(
	tags$head(tags$style('h1 {color:red;}')),
	fluidRow(
		column(3,
					 wellPanel(style = "background: lightgrey",
					 fileInput("bizData", "Upload the business data", accept = c(".xlsx")),
					 selectInput("type", "What report is needed?", choices = c("Year to date")),
					 numericInput("CANexchange", "Canadian exchange: 1 CAD is worth ____ USD", value=0.789),
					 numericInput("EUROexchange", "Euro exchange: 1 EURO is worth ____ USD", value=1.197),
					 downloadButton("report", "Generate Report")
					 )
		),
		column(9,
					 tabsetPanel(
					 	#income tab ----
					 	tabPanel(title = "INCOME",
					 					 wellPanel(
					 					 	fluidRow(column(12, align="center", h1("SUMMARY"))),
					 					 	fluidRow(
					 					 		column(4, align = "center",
					 					 					 h3("US Dollars"),
					 					 					 tags$img(src=flags[1], width=25, height=20, alt = "USA flag"),
					 					 					 br(),
					 					 					 htmlOutput("incUsaText")
					 					 		),
					 					 		column(4, align = "center",
					 					 					 h3("CAN Dollars"),
					 					 					 tags$img(src=flags[2], width=25, height=20, alt = "Canada flag"),
					 					 					 br(),
					 					 					 htmlOutput("incCanText")
					 					 		),
					 					 		column(4, align = "center",
					 					 					 h3("Euros"),
					 					 					 tags$img(src=flags[3], width=25, height=20, alt = "European Union flag"),
					 					 					 br(),
					 					 					 htmlOutput("incEuroText")
					 					 		)
					 					 	)
					 					 ), # end of wellPanel
					 					 hr(),
					 					 fluidRow(column(12, align="center", h1("CONSULTING INCOME"))),
					 					 h3("Paid Invoices"),
					 					 dataTableOutput("paidIncTable"),
					 					 hr(),
					 					 h3("Pending Invoices"),
					 					 dataTableOutput("pendIncTable"),
					 					 fluidRow(column(12, align="center", h1("OTHER INCOME"))),
					 					 dataTableOutput("otherIncTable")
					 	),# end income tab 
					 	
					 	# Expense tab ----
					 	tabPanel(title = "EXPENSES",
					 					 wellPanel(
					 					 	fluidRow(column(12, align="center", h1("SUMMARY"))),
					 					 	fluidRow(
					 					 		column(6, align = "center",
					 					 					 h3("US Dollars"),
					 					 					 tags$img(src=flags[1], width=25, height=20, alt = "USA flag"),
					 					 					 br(),
					 					 					 htmlOutput("expUsaText")
					 					 		),
					 					 		column(6, align = "center",
					 					 					 h3("CAN Dollars"),
					 					 					 tags$img(src=flags[2], width=25, height=20, alt = "Canada flag"),
					 					 					 br(),
					 					 					 htmlOutput("expCanText")
					 					 		)
					 					 	)
					 					 ), # end of wellPanel
					 					 hr(),
					 					 fluidRow(
					 					 	column(12, align="center", 
					 					 				 h1("ALL EXPENSES"),
					 					 				 radioButtons("view", "", choices = c("All", "Collapsed"), inline=TRUE),
					 					 	)
					 					 ),
					 					 hr(),
					 					 h3("US Expenses"),
					 					 dataTableOutput("usaExpTable"),
					 					 hr(),
					 					 h3("CAN Expenses"),
					 					 dataTableOutput("canExpTable")
					 	) # end expense tab
					 ) # end tabset
		) # end column with tabsets
	) # end large fluid row
)
# end of ui ----

server <- function(input, output){

	
	# Organize uploaded data ----
	biz <- reactiveValues()
	
	observeEvent(input$bizData,{
		file <- input$bizData
		ext <- tools::file_ext(file$datapath)
		req(file)
		validate(need(ext == "xlsx", "Please upload an Excel file"))
		biz$INCOME <- read_excel(file$datapath, sheet="Income", 
														 col_types = c("date", "text", "text", "text", "numeric", "numeric", 
														 							"text", "date", "numeric", "text", "text", 
														 							"numeric", "text"))
		biz$EXPENSES <- read_excel(file$datapath, sheet="Expenses",
													col_types = c("date", rep("text", times=6), "numeric", "text"))
		biz$CLIENTS <- read_excel(file$datapath, sheet="Clients",
															col_types = c("numeric", rep("text", times=7)))
	})
	
	# Income data ----
	
	output$paidIncTable <- renderDataTable({
		req(input$bizData)
		datatable(ar_paidInvoices(biz$INCOME))
	})
	
	output$pendIncTable <- renderDataTable({
		req(input$bizData)
		datatable(ar_pendInvoices(biz$INCOME))
	})
	
	incStatus <- reactive({
		req(input$bizData)
		byCurrency <- biz$INCOME %>%
			dplyr::filter(IncType %in% c("Consulting", "Teaching")) %>%
			dplyr::group_by(InvCurrency) %>%
			dplyr::summarize(INVOICED = round(sum(InvAmount),2),
								PAID = round(sum(PayAmount, na.rm=TRUE),2),
								PENDING = INVOICED-PAID)
	})
	
	output$incCanText <- renderUI({
		req(input$bizData)
		HTML("<b>Invoiced:</b>", incStatus()$INVOICED[incStatus()$InvCurrency =="CAN"], "<br>",
				 "<b>Paid:</b>", incStatus()$PAID[incStatus()$InvCurrency =="CAN"], "<br>",
				 "<b>Pending:</b>", incStatus()$PENDING[incStatus()$InvCurrency =="CAN"])
		
	})
	output$incUsaText <- renderUI({
		req(input$bizData)
		HTML("<b>Invoiced:</b>", incStatus()$INVOICED[incStatus()$InvCurrency =="US"], "<br>",
				 "<b>Paid:</b>", incStatus()$PAID[incStatus()$InvCurrency =="US"], "<br>",
				 "<b>Pending:</b>", incStatus()$PENDING[incStatus()$InvCurrency =="US"])
		
	})
	output$incEuroText <- renderUI({
		req(input$bizData)
		HTML("<b>Invoiced:</b>", incStatus()$INVOICED[incStatus()$InvCurrency =="EU"], "<br>",
				 "<b>Paid:</b>", incStatus()$PAID[incStatus()$InvCurrency =="EU"], "<br>",
				 "<b>Pending:</b>", incStatus()$PENDING[incStatus()$InvCurrency =="EU"])
		
	})
	
	# Expense Data ----
	
	expUsaStatus <- reactive({
		req(input$bizData)
		usaExp <- biz$EXPENSES %>%
			dplyr::filter(ExpCurrency == "US") %>%
			dplyr::group_by(ExpClass) %>%
			dplyr::summarize(TOTAL = round(sum(ExpAmount),2))
	})
	
	expCanStatus <- reactive({
		canExp <- biz$EXPENSES %>%
			dplyr::filter(ExpCurrency == "CAN") %>%
			dplyr::group_by(ExpClass) %>%
			dplyr::summarize(TOTAL = round(sum(ExpAmount),2))
	})
	
	output$expCanText <- renderUI({
		req(input$bizData)
		HTML("<b>Office:</b>", expCanStatus()$TOTAL[expCanStatus()$ExpClass =="Office"], "<br>",
				 "<b>Employees:</b>", expCanStatus()$TOTAL[expCanStatus()$ExpClass =="Employees"], "<br>",
				 "<b>Travel:</b>", expCanStatus()$TOTAL[expCanStatus()$ExpClass =="Travel"], "<br>",
				 "<b>Development:</b>", expCanStatus()$TOTAL[expCanStatus()$ExpClass =="Development"], "<br>",
				 "<b>Fees:</b>", expCanStatus()$TOTAL[expCanStatus()$ExpClass =="Service Fees"], "<br>",
				 "<b>Taxes:</b>", expCanStatus()$TOTAL[expCanStatus()$ExpClass =="Taxes"], "<br>",
				 "<b>Member Draw:</b>", expCanStatus()$TOTAL[expCanStatus()$ExpClass =="Member Draw"])
		
	})
	output$expUsaText <- renderUI({
		req(input$bizData)
		HTML("<b>Office:</b>", expUsaStatus()$TOTAL[expUsaStatus()$ExpClass =="Office"], "<br>",
				 "<b>Employees:</b>", expUsaStatus()$TOTAL[expUsaStatus()$ExpClass =="Employees"], "<br>",
				 "<b>Travel:</b>", expUsaStatus()$TOTAL[expUsaStatus()$ExpClass =="Travel"], "<br>",
				 "<b>Development:</b>", expUsaStatus()$TOTAL[expUsaStatus()$ExpClass =="Development"], "<br>",
				 "<b>Fees:</b>", expUsaStatus()$TOTAL[expUsaStatus()$ExpClass =="Service Fees"], "<br>",
				 "<b>Taxes:</b>", expUsaStatus()$TOTAL[expUsaStatus()$ExpClass =="Taxes"], "<br>",
				 "<b>Member Draw:</b>", expUsaStatus()$TOTAL[expUsaStatus()$ExpClass =="Member Draw"])
		
	})
	
	output$usaExpTable <- renderDataTable({
		req(input$bizData)
		out <- biz$EXPENSES %>%
			dplyr::filter(ExpCurrency == "US") %>%
								dplyr::select(ExpDate, ExpClass, ExpCategory, ExpItem, ExpAmount) %>%
								dplyr::arrange(ExpDate)
		if (input$view == "Collapsed"){
			out <- dplyr::group_by(out, ExpClass, ExpCategory) %>%
				dplyr::summarize(CategoryTotal = round(sum(ExpAmount), 2)) %>%
				dplyr::arrange(ExpClass)
		}
		datatable(out)
			
	})
	
	output$canExpTable <- renderDataTable({
		req(input$bizData)
		out <- biz$EXPENSES %>%
			dplyr::filter(ExpCurrency == "CAN") %>%
			dplyr::select(ExpDate, ExpClass, ExpCategory, ExpItem, ExpAmount) %>%
			dplyr::arrange(ExpDate)
		if (input$view == "Collapsed"){
			out <- dplyr::group_by(out, ExpClass, ExpCategory) %>%
				dplyr::summarize(CategoryTotal = round(sum(ExpAmount), 2)) %>%
				dplyr::arrange(ExpClass)
		}
		datatable(out)
	})
	
	# Report Generator ----
	
	output$report <- downloadHandler(
		filename = "YTD_Report.doc",
		content = function(file) {
			# Copy the report file to a temporary directory before processing it, in
			# case we don't have write permissions to the current working dir (which
			# can happen when deployed).
			tempReport <- file.path(tempdir(), "YearToDate_Account_Report.Rmd")
			file.copy("Templates/YearToDate_Account_Report.Rmd", tempReport, overwrite = TRUE)
			
			# Set up parameters to pass to Rmd document
			params <- list(income = biz$INCOME, 
										 expenses = biz$EXPENSES, 
										 CANexchange = input$CANexchange,
										 EUROexchange = input$EUROexchange)
			
			# Knit the document, passing in the `params` list, and eval it in a
			# child of the global environment (this isolates the code in the document
			# from the code in this app).
			rmarkdown::render(tempReport, output_file = file,
												params = params,
												envir = new.env(parent = globalenv())
			)
		}
	)
	
}

shinyApp(ui, server)