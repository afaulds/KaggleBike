Clean <- function(x)
{
	x$hour <- as.factor(format(as.POSIXct(x$datetime), "%H"))
	x$dayOfYear <- as.factor(format(as.POSIXct(x$datetime), "%j"))

	return( x )
}

Train <- function(data)
{
	# Train algorithm
	mean <- mean(data$count)

	return( list(Mean = mean) )
}

Predict <- function(object, data)
{
	outcome <- rep(object$Mean, dim(data)[1])
	
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

library(caret)
set.seed(521)

fullTrainSet <- read.csv('train.csv', header=TRUE)
fullTestSet <- read.csv('test.csv', header=TRUE)

fullTrainSet <- Clean(fullTrainSet)
fullTestSet <- Clean(fullTestSet)

inTrain <- createDataPartition(fullTrainSet$count, p=0.7, list=FALSE)

trainSet <- fullTrainSet[inTrain,]
validateSet <- fullTrainSet[-inTrain,]

# Train Data
model <- Train(trainSet)

# Check Training Accuracy
Score(model, trainSet, "Training Set")

# Check Testing Accuracy
Score(model, validateSet, "Validation Set")

# Check Validation Accuracy
Write(model, fullTestSet)

