source("add_features.R")

# Read Data
data1 <- read.csv("train.csv")
data1$casual <- NULL
data1$registered <- NULL

data2 <- read.csv("test.csv")
data2$count <- rep(NA, dim(data2)[1])

data <- rbind(data1, data2)

# Remove unnecessary columns
data$datetime <- strptime(as.character(data$datetime), "%Y-%m-%d %H:%M:%S")

full.data <- AddFeatures(data, date.attr = "datetime", output = "count")

write.csv(full.data[!is.na(full.data$count), ], "train_more.csv", row.names = FALSE)
write.csv(full.data[is.na(full.data$count), ], "test_more.csv", row.names = FALSE)

# Delete memory
rm(list = c("data1", "data2", "data", "full.data"))
