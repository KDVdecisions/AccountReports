# From full income dataframe to a Other Income dataframe
# This includes all non-work income - credit card cash rewards, gifts, earned interest, grants, royalities, etc.

ar_otherIncome <- function(incomeDf, minDate, maxDate){
	incomeDf %>%
		dplyr::filter(is.na(InvNum) & !is.na(PayDate)& as.Date(PayDate) >= minDate & as.Date(PayDate) <= maxDate) %>%
		dplyr::group_by(Client) %>%
		dplyr::select(Client, PayDate, PayLocate, PayCurrency) %>%
		dplyr::arrange(Client)
}


