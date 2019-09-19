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