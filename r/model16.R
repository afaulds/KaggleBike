Clean <- function(data)
{
	# Derived variables
	len <- dim(data)[1]
	data$hour <- as.numeric(format(as.POSIXct(data$datetime), "%H"))
	data$day.of.year <- as.numeric(format(as.POSIXct(data$datetime), "%j"))
	data$week.day <- format(as.POSIXct(data$datetime), "%a")
	data$sunday <- data$week.day == "Sun"
	data$day.part <- rep("Night", len)
	data$day.part[data$hour < 10 & data$hour > 3] <- "Morning"
	data$day.part[data$hour < 16 & data$hour >= 10] <- "Afternoon"
	data$day.part[data$hour < 22 & data$hour >= 16] <- "Evening"
	
	# Cast variables
	data$season <- as.factor(data$season)
	data$holiday <- as.factor(data$holiday)
	data$workingday <- as.factor(data$workingday)
	data$weather <- as.factor(data$weather)
	data$hour <- as.factor(data$hour)
	data$week.day <- as.factor(data$week.day)
	data$day.part <- as.factor(data$day.part)
	data$sunday <- as.factor(data$sunday)
	
	return( data )
}

Train <- function(data)
{
	# Train algorithm
	x <- data
	x$datetime <- NULL
	x$registered <- NULL
	x$count <- NULL
	model1 <- ctree(casual ~ ., x)
	
	x <- data
	x$datetime <- NULL
	x$casual <- NULL
	x$count <- NULL
	model2 <- ctree(registered ~ ., x)

	return( list(model1 = model1, model2 = model2) )
}

Predict <- function(object, data)
{
	casual <- predict(object$model1, data)
	registered <- predict(object$model2, data)
	outcome <- casual + registered
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
	write.csv(output, 'test_output.csv', row.names = FALSE)
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
library(party)
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

