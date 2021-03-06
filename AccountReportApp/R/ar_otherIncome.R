# From full income dataframe to a Other Income dataframe
# This includes all non-work income - credit card cash rewards, gifts, earned interest, grants, royalities, etc.

ar_otherIncome <- function(incomeDf){
	incomeDf %>%
		dplyr::filter(is.na(InvNumber) & !is.na(PayDate)) %>%
		dplyr::group_by(Client) %>%
		dplyr::select(Client, PayDate, PayLocate, PayCurrency) %>%
		dplyr::arrange(Client)
}


