# From full income dataframe to a PENDING invoices dataframe

ar_pendInvoices <- function(incomeDf, minDate, maxDate){
	incomeDf %>%
						dplyr::filter(!is.na(InvNum) & is.na(PayDate) & as.Date(InvDate) >= minDate & as.Date(InvDate) <= maxDate) %>%
						dplyr::select(InvNum, Client, InvAmount, InvCurrency, InvDate) %>%
						dplyr::arrange(InvDate)
}