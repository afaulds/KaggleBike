Clean <- function(data)
{
	# Derived variables
	data$hour <- as.numeric(format(as.POSIXct(data$datetime), "%H"))
	data$day.of.year <- as.numeric(format(as.POSIXct(data$datetime), "%j"))
	
	# Cast variables
	data$season <- as.factor(data$season)
	data$holiday <- as.factor(data$holiday)
	data$workingday <- as.factor(data$workingday)
	data$weather <- as.factor(data$weather)
	data$hour <- as.factor(data$hour)

	# Remove unnecessary fields
	data$casual <- NULL
	data$registered <- NULL
	
	return( data )
}

TrainAvg <- function(data)
{
	# Train algorithm
	obj1 <- as.data.frame(aggregate(count ~ hour, data, mean))
	obj2 <- as.data.frame(aggregate(count ~ day.of.year, data, mean))
	
	mean <- mean(data$count)
	obj1$count <- obj1$count / mean
	return( list(predict1 = obj1, predict2 = obj2, mean = mean) )
}

PredictAvg <- function(object, data)
{
	num <- dim(data)[1]
	outcome <- rep(0, num)
	for(i in 1:num)
	{
		day.factor <- object$predict2$count[object$predict2$day.of.year == data$day.of.year[i]]
		hour.factor <- object$predict1$count[object$predict1$hour == data$hour[i]]
		if(length(day.factor) == 0 && length(hour.factor) == 0)
		{
			outcome[i] <- object$mean
		}
		else if(length(day.factor) == 0)
		{
			outcome[i] <- object$mean * hour.factor
		}
		else if(length(hour.factor) == 0)
		{
			outcome[i] <- day.factor
		}
		else
		{
			outcome[i] <- day.factor * hour.factor
		}
	}
	outcome[outcome < 0] <- 0
	
	return( outcome )
}


Train <- function(data)
{
	# Train algorithm
	data$datetime <- NULL
	
	# Train and predict for average algorithm
	modelAvg <- TrainAvg(data)
	data$average <- PredictAvg(modelAvg, data)
	
	# Train main algorithm
	model <- lm(count ~ ., data)
	
	return( list(model = model, modelAvg = modelAvg) )
}

Predict <- function(object, data)
{
	data$average <- PredictAvg(object$modelAvg, data)
	outcome <- predict(object$model, data)
	outcome[outcome < 0] <- 0
	
	return( outcome )
}

Score <- function(object, data, name = "model")
{
	response <- Predict(object, data)
	rms <- sqrt(sum((log(data$count + 1) - log(response + 1)) ^ 2) / length(response))

	plot(response, data$count)
	writeLines(sprintf("\n-----------------------%s-----------------------\n", name))
	print(rms)
	
	return(response)
}

Write <- function(object, data)
{
	response <- Predict(object, data)
	output <- data.frame(datetime = data$datetime, count = response)
	write.csv(output, 'test_output.csv', row.names=FALSE)
}

Check <- function(object, data)
{
	newdata <- data
	newdata$pcount <- Predict(object, newdata)
	newdata$outlier <- (abs(data$count - newdata$pcount) / data$count) > 0.5
	
	newdata$color <- rep("", length(newdata$outlier))
	newdata$color[newdata$outlier] <- "blue"
	newdata$color[!newdata$outlier] <- "red"

	plot(newdata$pcount, newdata$count, col = newdata$color)
	
	return(newdata)
}

GetNumericVars <- function(data) {
	var.all <- names(data)
	var.num <- var.all[sapply(data, function(a) is.numeric(a) & !identical(a, rep(a[1], length(a))))]

	return(var.num)
}

library(caret)
set.seed(521)

fullTrainSet <- read.csv('train.csv', header = TRUE)
fullTestSet <- read.csv('test.csv', header = TRUE)

fullTrainSet <- Clean(fullTrainSet)
fullTestSet <- Clean(fullTestSet)

inTrain <- createDataPartition(fullTrainSet$count, p = 0.7, list = FALSE)

trainSet <- fullTrainSet[inTrain,]
validateSet <- fullTrainSet[-inTrain,]

# Train Data
model <- Train(trainSet)

# Check Training Accuracy
Score(model, trainSet, "Training Set")

# Check Testing Accuracy
Score(model, validateSet, "Validation Set")

trainNew <- Check(model, trainSet)

# Check Validation Accuracy
Write(model, fullTestSet)

