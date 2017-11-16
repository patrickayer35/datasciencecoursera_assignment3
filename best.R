best <- function(state, outcome)
{
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	states <- unique(data$State)
	if (!(state %in% states))
	{
		stop("invalid state")
	}
	if ((outcome != "heart attack") |(outcome != "heart failure") | (outcome != "pneumonia"))
	{
		stop("invalid outcome")
	}
}