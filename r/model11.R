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
	
	return( data )
}

Train <- function(data)
{
	# Train algorithm
	x <- data
	x$datetime <- NULL
	x$count <- NULL
	x$casual <- NULL
	model1 <- lm(registered ~ ., data = x)

	x <- data
	x$datetime <- NULL
	x$count <- NULL
	x$registered <- NULL
	model2 <- lm(casual ~ ., data = x)
	
	return( list(model1 = model1, model2 = model2) )
}

Predict <- function(object, data)
{
	outcome1 <- predict(object$model1, data)
	outcome2 <- predict(object$model2, data)
	outcome <- outcome1 + outcome2
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

