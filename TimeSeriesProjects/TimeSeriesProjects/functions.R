library(timeDate)
splitDataIntoTestTrain <- function(original.data, frequency) {

    number.of.rows.original <- nrow(original.data)

    number.of.rows.test <- as.integer(number.of.rows.original * .2)

    number.of.rows.train <- as.integer(number.of.rows.original * .8)

    if ((number.of.rows.original - number.of.rows.test - number.of.rows.train) != 0) {
        number.of.rows.train = number.of.rows.train + (number.of.rows.original-number.of.rows.test-number.of.rows.train)
    }

    train.data <- head(original.data, number.of.rows.train)

    test.data <- head(original.data, number.of.rows.test)
    train.data.start <- data.matrix(head(train.data[1],1))

    as.month(train.data.start$`ï..Month.Year`)

    list <- c(number.of.rows.test,number.of.rows.train)

    return (list)
}

findBenchmarkMethod <- function(myts.train, myts.test, myh) {
    ## Simple, Baseline approaches:

    ## Average Method which gives an average of all the historical
    ## data:
    forecast.average.method <- meanf(myts.train, h = myh)

    ## Naive Method - sets all forecast values to the last observed
    ## value:
    forecast.naive.method <- naive(myts.train, h = myh)

    ## Seasonal Naive Method, useful for highly seasonal data.
    ## Sets forecast value to the last observed value from the 
    ## previous corresponding season.
    forecast.snaive.method <- snaive(myts.train, h = myh)

    ## Drift Method applies the average change seen in historical data
    ## to the average of the historical data (similar to naive method)
    forecast.drift.method <- rwf(myts.train, h = myh, drift = TRUE)

    autoplot(myts.train) +
    autolayer(myts.test, series = "Observed Values") +
    autolayer(forecast.average.method, series = "Average Method", PI = FALSE) +
    autolayer(forecast.naive.method, series = "Naive Method", PI = FALSE) +
    autolayer(forecast.snaive.method, series = "Seasonal Naive Method", PI = FALSE) +
    autolayer(forecast.drift.method, series = "Drift Method", PI = FALSE)

    ##getwd()
    ##write.csv(x = forecast.drift.method$mean, file = "Drift_Method_Forecasts.csv", row.names = TRUE, col.names = TRUE)
    ##write.csv(x = revenue.test.ts, file = "Observed_Test_Data.csv", row.names = TRUE, col.names = TRUE)
    
    
    ##revenue.test.ts
    forecast.average.method.rmse <- as.numeric(accuracy(forecast.average.method, myts.test)[, 'RMSE']['Test set'])
    forecast.naive.method.rmse <- as.numeric(accuracy(forecast.naive.method, myts.test)[, 'RMSE']['Test set'])
    forecast.snaive.method.rmse <- as.numeric(accuracy(forecast.snaive.method, myts.test)[, 'RMSE']['Test set'])
    forecast.drift.method.rmse <- as.numeric(accuracy(forecast.drift.method, myts.test)[, 'RMSE']['Test set'])

    data = list(forecast.average.method.rmse, forecast.naive.method.rmse, forecast.snaive.method.rmse, forecast.drift.method.rmse)
    column.names <- "RMSE"
    row.names <- c("Average Method", "Naive Method", "Seasonal Method", "Drift Method")
    matrix.names <- c("Benchmark Forecast Methods")

    # Take these vectors as input to the array.
    result <- array(data = data, dim = c(4, 1, 1), dimnames = list(row.names, column.names,
                matrix.names))

    return (result)

}


ExponentialSmoothingMethods <- function(myts.train, myts.test, myh) {

  ## Simple Exponential Smoothing
  forecast.ses.method <- ses(myts.train, h=myh)
  
  ## HOLT
  
  
  ## ETS  
  ets.object <- ets(revenue.smoothed.outlier.seasadj.train.ts)
  forecast.ets.method <- forecast(object=myts.train,model=ets.object, h=myh)
  
  autoplot(myts.train) +
    autolayer(myts.test, series = "Observed Values") +
    autolayer(forecast.ses.method, series = "Simple Exponential Smoothing Method", PI = FALSE) +
    autolayer(forecast.ets.method, series = "ETS Method", PI = FALSE)
  

  ##revenue.test.ts
  forecast.ses.method.rmse <- as.numeric(accuracy(forecast.ses.method, myts.test)[, 'RMSE']['Test set'])
  forecast.ets.method.rmse <- as.numeric(accuracy(forecast.ets.method, myts.test)[, 'RMSE']['Test set'])
  
  data = list(forecast.ses.method.rmse, forecast.ets.method.rmse)
  column.names <- "RMSE"
  row.names <- c("SES Method", "ETS Method")
  matrix.names <- c("Exponential Smoothing Methods")
  
  # Take these vectors as input to the array.
  result <- array(data = data, dim = c(2, 1, 1), dimnames = list(row.names, column.names,
                                                                 matrix.names))
  
  return (result)
  
}