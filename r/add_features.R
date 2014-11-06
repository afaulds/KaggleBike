AddFeatures <- function(data, output = "output", date.attr = "date", group.attr = c())
{
	# Takes in data frame that has a time element and
	# builds velocity, count, and sum fields.
	#
	# Args:
	#   data: Data frame containing original data.
	#   date.attr: Name of column in data frame that has a value.
	#   group.attr: Name of column(s) in data frame to use for grouping data for sums, counts, and velocities.
	#   date.attr: Name of column in data frame that has a value.
	#
	# Returns:
	#   Returns the index of the closest value.

	# Get all fields
	name.list <- names(data)
	name.list <- name.list[name.list != date.attr]
	name.list <- name.list[name.list != output]
	
	full.data <- data
	
	# Sort data by date.
	full.data <- full.data[order(full.data[, date.attr]), ]
	
	# Build up data related date.
	full.data$Timestamp <- as.double(full.data[, date.attr])
	full.data$Year <- as.numeric(strftime(full.data[, date.attr], "%Y"))
	full.data$Month <- as.numeric(strftime(full.data[, date.attr], "%m"))
	full.data$Day <- as.numeric(strftime(full.data[, date.attr], "%d"))
	full.data$Hour <- as.numeric(strftime(full.data[, date.attr], "%H"))
	full.data$Minute <- as.numeric(strftime(full.data[, date.attr], "%M"))
	full.data$Second <- as.numeric(strftime(full.data[, date.attr], "%S"))
	full.data$DayOfWeek <- as.numeric(strftime(full.data[, date.attr], "%w"))
	full.data$DayOfYear <- as.numeric(strftime(full.data[, date.attr], "%j"))
	full.data$WeekOfYear <- as.numeric(strftime(full.data[, date.attr], "%U"))

	for(i in 1:length(name.list))
	{
		if(is.factor(full.data[, name.list[i]]))
		{
			full.data <- CalculateFrq(full.data, name.list[i], 1)
			full.data <- CalculateFrq(full.data, name.list[i], 24)
			full.data <- CalculateFrq(full.data, name.list[i], 168)
			full.data <- CalculateFrq(full.data, name.list[i], 672)
		}
		else
		{
			full.data <- CalculateSum(full.data, name.list[i], 1)
			full.data <- CalculateSum(full.data, name.list[i], 24)
			full.data <- CalculateSum(full.data, name.list[i], 168)
			full.data <- CalculateSum(full.data, name.list[i], 672)
		}
	}
	
	# Remove datetime and any variable that has no variation
	new.name.list <- names(full.data)
	for(i in 1:length(new.name.list))
	{
		if(is.numeric(full.data[, new.name.list[i]]))
		{
			print(new.name.list[i])
			temp <- sd(full.data[, new.name.list[i]], na.rm=TRUE)
			if(temp == 0)
			{
				print("DELETE")
				full.data[, new.name.list[i]] <- NULL
			}
		}
		else if(is.factor(full.data[, new.name.list[i]]))
		{
			print(new.name.list[i])
			temp <- sd(full.data[, new.name.list[i]], na.rm=TRUE)
			if(temp == 0)
			{
				print("DELETE")
				full.data[, new.name.list[i]] <- NULL
			}
		}
	}

	return(full.data)
}

GetApproximateIndex = function(value, x)
{
	# Takes the vector x and find the index of the value.
	# This method is similar to "which" but if the value
	# is not found in x, it returns the next closest number
	# that is less than value.  If value is smaller than all
	# numbers in x than the index of the smaller number of x
	# is returned.
	#
	# Args:
	#   value: Value to search for.
	#   x: Vector of data to search in.
	#
	# Returns:
	#   Returns the index of the closest value.

	def.index <- which(x == min(x))
	matches = x[x <= value]
	if (length(matches) <= 0)
	{
		return(def.index[1])
	}
	value.approx = max(matches)
	index = which(x == value.approx)
	if(length(index) == 0)
	{
		return(def.index[1])
	}
	else
	{
		return(index[1])
	}
}

CalculateSum <- function(data, attr, hour)
{
	temp <- c()
	for(i in 1:dim(data)[1])
	{
		idx <- GetApproximateIndex(data$Timestamp[i] - hour * 60 * 60, data$Timestamp)
		
		# Calculate sum of data element.
		temp <- c(temp, sum(data[i:idx, attr]))
	}
	data[, paste0(attr, "SUM", hour)] <- temp

	return(data)
}

CalculateFrq <- function(data, attr, hour)
{
	temp <- c()
	for(i in 1:dim(data)[1])
	{
		idx <- GetApproximateIndex(data$Timestamp[i] - hour * 60 * 60, data$Timestamp)
		
		# Calculate frequency of current response.
		temp <- c(temp, sum(data[idx:i, attr] == data[i, attr]) / length(idx:i))
	}
	data[, paste0(attr, "FREQ", hour)] <- temp

	return(data)
}

CalculateVel <- function(data, attr, hour)
{
	temp <- c()
	for(i in 1:dim(data)[1])
	{
		idx <- GetApproximateIndex(data$Timestamp[i] - hour * 60 * 60, data$Timestamp)
		
		# Calculate sum of data element.
		if(data$Timestamp[i] - data$Timestamp[idx] == 0)
		{
			temp <- c(temp, 0)
		}
		else 
		{
			temp <- c(temp, (data[i, attr] - data[idx, attr]) / (data$Timestamp[i] - data$Timestamp[idx]))
		}
	}
	data[, paste0(attr, "VEL", hour)] <- temp

	return(data)
}
