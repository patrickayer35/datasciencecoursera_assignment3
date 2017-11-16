best <- function(state, outcome)
{
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	states <- unique(data$State)
	if (!(state %in% states))
	{
		stop("invalid state")
	}
	if ((outcome != "heart attack") | (outcome != "heart failure") | (outcome != "pneumonia"))
	{
		stop("invalid outcome")
	}
	
	# extract data from data frame that pertains to specified state
	state_data <- data[data$State == state, ]
	
	# create a vector of the relevant column values:
	# Hospital.Name (col 2), State (col 7)
	# if heart attack, col 11
	# if heart failure, col 17
	# if pneumonia, col 23
	
	if (outcome == "heart attack")
	{
		outcome_col <- which(colnames(data) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
	}
	else if (outcome == "heart failure")
	{
		outcome_col <- which(colnames(data) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
	}
	else
	{
		outcome_col <- which(colnames(data) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
	}
	
	rel_data <- state_data[, c(2, outcome_col)]
}