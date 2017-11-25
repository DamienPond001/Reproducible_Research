library(dplyr)
library(ggplot2)

dataset <- read.csv("activity.csv", stringsAsFactors = FALSE)
byDay <- group_by(dataset, date)
sumByDay <- summarise(byDay, Total_Steps = sum(steps))


g <- ggplot(sumByDay, aes(x = Total_Steps)) + geom_histogram(fill = "red", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
g

medianSteps <- median(sumByDay$Total_Steps, na.rm = TRUE)
meanSteps <- round(mean(sumByDay$Total_Steps, na.rm = TRUE), 2)

byInterval <- group_by(dataset, interval)
avStepsByInterval <- summarise(byInterval, Mean_Steps = mean(steps, na.rm = TRUE))

g <- ggplot(avStepsByInterval, aes(x = interval, y = Mean_Steps)) + geom_point() + geom_line()
g

maxInterval <- avStepsByInterval[which.max(avStepsByInterval$Mean_Steps),]

noNA <- sum(is.na(dataset$steps))

newValue <- function(dataset, day, interval)
{
  meanByDay <- mean(dataset$steps[dataset$date == day], na.rm = TRUE)
  meanByInt <- mean(dataset$steps[dataset$interval == interval], na.rm = TRUE)
  
  round(mean(c(meanByDay, meanByInt), na.rm = TRUE), 0)
}
newSteps = numeric()
for(i in 1:length(dataset$steps))
{
  if(is.na(dataset$steps[i]))
  {
    newSteps <- c(newSteps,newValue(dataset, dataset$date[i], dataset$interval[i]))
  }
  else
  {
    newSteps <- c(newSteps,dataset$steps[i])
  }
}

newDataset <- dataset
newDataset$steps <- newSteps

byDay <- group_by(newDataset, date)
sumByDay <- summarise(byDay, Total_Steps = sum(steps))


g <- ggplot(sumByDay, aes(x = Total_Steps)) + geom_histogram(fill = "red", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
g

newMedianSteps <- median(sumByDay$Total_Steps, na.rm = TRUE)
newMeanSteps <- round(mean(sumByDay$Total_Steps, na.rm = TRUE), 2)

partOfWeek <- sapply(as.Date(newDataset$date), function(x) {if(weekdays(x) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){"Weekday"} else {"Weekend"}})
newDataset$partOfWeek <- as.factor(partOfWeek)

byWeekAndInterval <- group_by(newDataset, partOfWeek,interval)
avStepsByInterval <- summarise(byWeekAndInterval, Mean_Steps = mean(steps, na.rm = TRUE))

g <- ggplot(avStepsByInterval, aes(x = interval, y = Mean_Steps)) + geom_point() + geom_line() + facet_grid(partOfWeek ~ .)
g
