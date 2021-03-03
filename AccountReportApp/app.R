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

ui <- fluidPage(
	tags$head(tags$style('h1 {color:red;}')),
	fluidRow(
		column(3,
					 wellPanel(style = "background: lightgrey",
					 fileInput("bizData", "Upload the business data", accept = c(".xlsx")),
					 textInput("name", "Who is requesting this report?", placeholder = "Enter a name here"),
					 selectInput("type", "What report is needed?", choices = c("Year to date")),
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
					 					 fluidRow(column(12, align="center", h1("ALL INCOME"))),
					 					 hr(),
					 					 h3("Paid"),
					 					 dataTableOutput("paidIncTable"),
					 					 hr(),
					 					 h3("Pending"),
					 					 dataTableOutput("pendIncTable")
					 	),# end income tab 
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
					 					 "All Expenses",
					 					 tableOutput("expUS")
					 	) # end expense tab
					 ) # end tabset
		) # end column with tabsets
	) # end large fluid row
)

server <- function(input, output){
	
	# Organize uploaded data
	biz <- reactiveValues()
	
	observeEvent(input$bizData,{
		file <- input$bizData
		ext <- tools::file_ext(file$datapath)
		req(file)
		validate(need(ext == "xlsx", "Please upload an Excel file"))
		biz$INCOME <- read_excel(file$datapath, sheet="Income", 
														 col_types = c("date", "text", "text", "numeric", "numeric", 
														 							"text", "date", "numeric", "text", "text", 
														 							"numeric", "numeric"))
		biz$USA <- read_excel(file$datapath, sheet=2,
													col_types = c("date", "numeric", rep("text", times=5)))
		biz$CAN <- read_excel(file$datapath, sheet=3,
													col_types = c("date", "numeric", rep("text", times=5)))
		biz$CLIENTS <- read_excel(file$datapath, sheet=4,
															col_types = c("numeric", rep("text", times=7)))
	})
	
	# Income data ----
	
	output$paidIncTable <- renderDataTable({
		req(input$bizData)
		datatable(biz$INCOME %>%
								dplyr::filter(!is.na(PayDate)) %>%
								dplyr::select(InvNum, Client, InvAmount, InvCurrency, InvDate, PayDate, PayLocate) %>%
								dplyr::arrange(InvDate))
	})
	
	output$pendIncTable <- renderDataTable({
		req(input$bizData)
		datatable(biz$INCOME %>%
								dplyr::filter(is.na(PayDate)) %>%
								dplyr::select(InvNum, Client, InvAmount, InvCurrency, InvDate) %>%
								dplyr::arrange(InvDate))
	})
	
	incStatus <- reactive({
		req(input$bizData)
		byCurrency <- biz$INCOME %>%
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
		usaExp <- biz$USA %>%
			dplyr::group_by(Set) %>%
			dplyr::summarize(TOTAL = round(sum(Amount),2))
	})
	
	expCanStatus <- reactive({
		canExp <- biz$CAN %>%
			dplyr::group_by(Set) %>%
			dplyr::summarize(TOTAL = round(sum(Amount),2))
	})
	
	output$expCanText <- renderUI({
		req(input$bizData)
		HTML("<b>Office:</b>", expCanStatus()$TOTAL[expCanStatus()$Set =="OFFICE"], "<br>",
				 "<b>Employees:</b>", expCanStatus()$TOTAL[expCanStatus()$Set =="EMPLOYEES"], "<br>",
				 "<b>Travel:</b>", expCanStatus()$TOTAL[expCanStatus()$Set =="TRAVEL"], "<br>",
				 "<b>Development:</b>", expCanStatus()$TOTAL[expCanStatus()$Set =="DEVELOPMENT"], "<br>",
				 "<b>Fees:</b>", expCanStatus()$TOTAL[expCanStatus()$Set =="FEES"], "<br>",
				 "<b>Transfer:</b>", expCanStatus()$TOTAL[expCanStatus()$Set =="TRANSFER"])
		
	})
	output$expUsaText <- renderUI({
		req(input$bizData)
		HTML("<b>Office:</b>", expUsaStatus()$TOTAL[expUsaStatus()$Set =="OFFICE"], "<br>",
				 "<b>Employees:</b>", expUsaStatus()$TOTAL[expUsaStatus()$Set =="EMPLOYEES"], "<br>",
				 "<b>Travel:</b>", expUsaStatus()$TOTAL[expUsaStatus()$Set =="TRAVEL"], "<br>",
				 "<b>Development:</b>", expUsaStatus()$TOTAL[expUsaStatus()$Set =="DEVELOPMENT"], "<br>",
				 "<b>Fees:</b>", expUsaStatus()$TOTAL[expUsaStatus()$Set =="FEES"], "<br>",
				 "<b>Transfer:</b>", expUsaStatus()$TOTAL[expUsaStatus()$Set =="TRANSFER"])
		
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
			params <- list(name = input$name, incStatus = incStatus())
			
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