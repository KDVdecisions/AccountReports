# From full income dataframe to a PAID invoices dataframe

ar_paidInvoices <- function(incomeDf, minDate, maxDate){
	incomeDf %>%
		dplyr::filter(!is.na(InvNum) & !is.na(PayDate) & as.Date(PayDate) >= minDate & as.Date(PayDate) <= maxDate) %>%
		dplyr::select(InvNum, Client, InvAmount, InvCurrency, InvDate, PayDate, PayLocate) %>%
		dplyr::arrange(InvDate)
}


