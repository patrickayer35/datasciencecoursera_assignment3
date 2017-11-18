best <- function(state, outcome)
{
	raw_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	#data <- read.csv("outcome-of-care-measures.csv")
	states <- unique(raw_data$State)
	if (!(state %in% states))
	{
		stop("invalid state")
	}
	#print(outcome)
	#print(outcome == "heart attack")
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
	
	# extract data from data frame that pertains to specified state
	state_data <- data[data$State == state, ]
	
	# create a vector of the relevant column values:
	# Hospital.Name (col 2)
	# if heart attack, col 11
	# if heart failure, col 17
	# if pneumonia, col 23
	
	if (outcome == "heart attack")
	{
		rel_data <- state_data[, c(2, which(colnames(data) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))]
		#rel_data <- rel_data[order(rel_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
	}
	else if (outcome == "heart failure")
	{
		rel_data <- state_data[, c(2, which(colnames(data) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))]
		#print(head(rel_data, 50))
		#rel_data <- rel_data[order(rel_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
	}
	else
	{
		rel_data <- state_data[, c(2, which(colnames(data) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))]
		#rel_data <- rel_data[order(rel_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
	}
	colnames(rel_data) <- c("hospital", "rate")
	rel_data$rate <- as.numeric(rel_data$rate)
	#print(length(rel_data$rate))
	clean_data <- data.frame(rel_data[complete.cases(rel_data),])
	#print(length(clean_data$rate))
	#print(head(clean_data, 50))
	hospitals <- best_hosp(clean_data)
	print(hospitals)
	#return(hospitals[1])
}

best_hosp <- function(df)
{
	smallest <- 101
	for (row in 1:length(df$hospital))
	{
		#print("line 64")
		if (df[row, 2] < smallest)
		{
			smallest = df[row, 2]
		}
	}
	print(smallest)
	hospitals <- vector("character")
	for (row in 1:length(df$hospital))
	{
		#if (df[row, df$rate] == smallest)
		if (df[row, which(colnames(df) == "rate")] == smallest)
		{
			hospitals <- c(hospitals, df[row, which(colnames(df) == "hospital")])
		}
	}
	return(sort(hospitals))
}

find_smallest_bad <- function(df)
{
	best_hospitals <- df[1, 1]
	smallest <- df[1, 2]
	i = 2
	s <- df[i, 2]
	while ((!is.na(s)) & (s == smallest))
	{
		best_hospitals <- c(best_hospitals, df[i, 1])
		i <- i + 1
		s <- df[i, 2]
	}
	return(sort(best_hospitals))
}





















