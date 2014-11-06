Clean <- function(x)
{
	x$hour <- as.factor(format(as.POSIXct(x$datetime), "%H"))

	return( x )
}

Train <- function(data)
{
	# Train algorithm
	obj1 <- as.data.frame(aggregate(count ~ hour, data, mean))
	mean <- mean(data$count)

	return( list(MeanByHour = obj1, Mean = mean) )
}

Predict <- function(object, data)
{
	outcome <- rep(0, dim(data)[1])
	hour <- object$MeanByHour$hour
	count <- object$MeanByHour$count
	for(i in 1:length(hour))
	{
		outcome[data$hour == hour[i]] <- count[i]
	}
	
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

trainNew <- Check(model, trainSet)

# Check Validation Accuracy
Write(model, fullTestSet)

