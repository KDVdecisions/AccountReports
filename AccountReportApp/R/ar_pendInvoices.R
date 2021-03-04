# From full income dataframe to a PENDING invoices dataframe

ar_pendInvoices <- function(incomeDf){
	incomeDf %>%
						dplyr::filter(is.na(PayDate)) %>%
						dplyr::select(InvNum, Client, InvAmount, InvCurrency, InvDate) %>%
						dplyr::arrange(InvDate)
}