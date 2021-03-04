# From full income dataframe to a PAID invoices dataframe

ar_paidInvoices <- function(incomeDf){
	incomeDf %>%
		dplyr::filter(!is.na(PayDate)) %>%
		dplyr::select(InvNum, Client, InvAmount, InvCurrency, InvDate, PayDate, PayLocate) %>%
		dplyr::arrange(InvDate)
}


