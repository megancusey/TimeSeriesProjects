library(fpp2)
## import functions I want to use
source("functions.R", local = TRUE)

## Import CSV Data.
revenue.data.original <- read.csv("Revenue Data.csv", header = TRUE)

## Convert to Time Series Object
revenue.data.ts <- ts(data = revenue.data.original[, 'Revenue'], frequency = 12, start = c(2015, 7))

## Plot Original Data
autoplot(revenue.data.ts)

## Explore Data 
ggseasonplot(revenue.data.ts, year.labels = TRUE, year.labels.left = TRUE)
ggseasonplot(revenue.data.ts, polar = TRUE, year.labels = TRUE, year.labels.left = TRUE)
ggsubseriesplot(revenue.data.ts)

## Always have more money in April... Why?? End of tax season is a thought.
## No clear upward trend per year as the order of the years as a function of revenue 
## is a bit sporatic. February is a consistent low point of revenue. There may be 
## some seasonality here.

## Indicate test/train... has to be a better way of specifying the test/train
## data thats more dynamic per data.

##############################################################################################
## 9/18/19
## I'll work on this... need to figure out how to turn the top (1) date of each divided
## data set into month/year that I can give to the ts object.

## Idea is to pass in the original data set, the frequency #, (probably eventually the %
## as a parameter (EX: 80%), then 80% train ts object, 20% test ts object would be the output.
## idk if we can assign ts to an array, but would like to output one complex object and
## then assign revenue.test.ts <- output[test.ts] // revenue.train.ts <- output[train.ts]
## Just need to figure out the syntax.

## revenue.test.ts <- splitDataIntoTestTrain(revenue.data.original,12)
## revenue.train.ts <- splitDataIntoTestTrain()

##############################################################################################

train.data <- head(revenue.data.original, 50)

head(train.data[1], 1) ## Begin: 7/1/15
tail(train.data[1],1) ## End: 8/1/19

test.data <- head(revenue.data.original, 12)