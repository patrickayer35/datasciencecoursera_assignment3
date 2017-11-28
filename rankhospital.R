rankhospital <- function(state, outcome, num = "best")
{
	raw_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	states <- unique(raw_data$State)
	if (!(state %in% states))
	{
		stop("invalid state")
	}
	if (outcome != "heart attack")
	{
		if (outcome != "heart failure")
		{
			if (outcome != "pneumonia")
			{
				stop("invalid outcome")
			}
		}
	}
	if (class(num) != "numeric")
	{
		if (num != "best")
		{
			if (num != "worst")
			{
				stop("invalid rank")
			}
		}
	}
	
	raw_state_data <- raw_data[raw_data$State == state, ]
	
	if (outcome == "heart attack")
	{
		state_data <- raw_state_data[, c(2, which(colnames(raw_state_data) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))]
	}
	else if (outcome == "heart failure")
	{
		state_data <- raw_state_data[, c(2, which(colnames(raw_state_data) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))]
	}
	else
	{
		state_data <- raw_state_data[, c(2, which(colnames(raw_state_data) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))]
	}
	
	colnames(state_data) <- c("hospital", "rate")
	state_data$rate <- as.numeric(state_data$rate)
	clean_data <- data.frame(state_data[complete.cases(state_data),])
	ordered_data <- clean_data[order(clean_data$rate, clean_data$hospital),]
	ordered_data$rank <- 1:length(ordered_data $rate)
	if (class(num) == "numeric")
	{
		if (num > length(ordered_data$rank))
		{
			return(NA)
		}
		else
		{
			return(ordered_data[num, 1])
		}
	}
	else
	{
		if (num == "best")
		{
			return(ordered_data[1, 1])
		}
		else
		{
			print(tail(ordered_data))
			last <- length(ordered_data$rank)
			print(last)
			return(ordered_data[last, 1])
		}
	}
	#return(ordered_data)
}

























