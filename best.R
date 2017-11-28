best <- function(state, outcome)
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
	ordered_data <- clean_data[order(clean_data$rate),]
	print(head(ordered_data))
	return(best_hospital(ordered_data))
}

best_hospital <- function(df)
{	
	target_rate <- df[1, 2]
	match <- target_rate
	hospitals <- vector("character")
	i <- 1
	while (match == target_rate)
	{
		hospitals <- c(hospitals, df[i, 1])
		i <- i + 1
		match <- df[i, 2]
	}
	if (length(hospitals) == 1)
	{
		return(hospitals)
	}
	else
	{
		#print("all hospitals")
		#print(hospitals)
		order_hospitals <- sort(hospitals)
		return(order_hospitals[1])
	}
}





















