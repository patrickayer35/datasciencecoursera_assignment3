rankall <- function(outcome, num = "best")
{
	# read data and get all unique states listed in the raw_data$State factor
	raw_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	unique_states <- unique(raw_data$State)
	
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
	
	# create empty vectors to contain the hospital and each corresponding state
	hospitals <- vector("character")
	states <- vector("character")
	for (s in unique_states)
	{
		
		state_data <- raw_data[raw_data$State == s, ]
		if (outcome == "heart attack")
		{
			state_data <- state_data[, c(2, which(colnames(state_data) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))]
		}
		else if (outcome == "heart failure")
		{
			state_data <- state_data[, c(2, which(colnames(state_data) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))]
		}
		else
		{
			state_data <- state_data[, c(2, which(colnames(state_data) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))]
		}
		
		colnames(state_data) <- c("hospital", "rate")								# rename column names
		state_data$rate <- as.numeric(state_data$rate)								# convert $rate values to numeric
		clean_data <- data.frame(state_data[complete.cases(state_data),])			# remove any rows with NA values
		ordered_data <- clean_data[order(clean_data$rate, clean_data$hospital),]	# order the data first by $rate value, and then by $hospital values (alphabetical)
		ordered_data$rank <- 1:length(ordered_data$rate)							# add a third column that adds a ranking
		
		if (class(num) == "numeric")
		{
			if (num > length(ordered_data$rank))
			{
				hospital <- NA							# if num is numeric, find the hospital whose $rank matches that value
			}											# set the corresponding hospital name to "hospital"
			else										# if the dataset is too small for the suggested ranking, then set "hospital" to NA
			{
				hospital <- ordered_data[num, 1]
			}
		}
		else
		{
			if (num == "best")
			{
				hospital <- ordered_data[1, 1]			# if the user asked for the "best" hospital from each state,
			}											# then the best hospital is taken to be the first one listed, since the data is already ordered
			else										# otherwise, set the hospital in the very last row to "hospital"
			{
				last <- length(ordered_data$rank)
				hospital <- ordered_data[last, 1]
			}
		}
		
		#print(s)
		#print(hospital)
		
		hospitals <- c(hospitals, hospital)				# append to the list of hospitals
		states <- c(states, s)							# with their corresponding states
		
	}
	
	data <- data.frame(hospitals, states)				# organize the hospitals and states vectors into a data frame
	colnames(data) <- c("hospital", "state")			# and return it
	return(data[order(data$state), ])
	
}


























