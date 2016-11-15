zlema <- function(x, period) {
  k <- 2 / (period + 1)
  n <- length(x)
  y <- floor((period - 1) / 2)
  urdun = 0
  urdun[ y ] <- x[ y ]
  for (i in (y + 1):n) {
  urdun[i] <- k * (2 * x[i] - x[i - y]) + (1 - k) * urdun[i - 1]
  }
  urdun[1:y-1] <- NA
  return(urdun)
}

system_zlema <- function(period, test_high, test_low, test_closed) {

mah <- zlema(test_high, period)
mal <- zlema(test_low, period)
n <- length(test_closed)
k <- 0
closed_profit <- rep(0,n)
    
    for (i in period:n-1) {
      if (i >= k && test_closed[i] > mah[i]){
        k <- i
        while (test_closed[k] >= mal[k]) {
          if (k == n) {break}
          k <- k + 1
        }
        closed_profit[i] <- test_closed[k] - test_closed[i]
      } else if (i >= k && test_closed[i] < mal[i]) {
        k <- i
        while (test_closed[k] <= mah[k]) {
          if (k == n) {break}
          k <- k + 1
        }
        closed_profit[i] <- test_closed[i] - test_closed[k]
      }
    }
      closed_profit <- closed_profit[which(closed_profit != 0)]
      number_of_trade <- length(closed_profit)
      system_profit <- sum(closed_profit)
      ave_profit_per_trade <- system_profit / number_of_trade

      result <- list(closed_profit, number_of_trade, system_profit, ave_profit_per_trade)
      names(result) <- c("closed_profit", "number_of_trade", "system_profit", "ave_profit_per_trade")
      return(result)
}


optimize_zlema <- function(period, high, low, closed) {

  total_trade <- rep(0, period)
  total_system_profit <- rep(0, period)
  average_profit_per_trade <- rep(0, period)
  n <- length(closed)
  
  for (j in 3:period){
    
    dum <- system_zlema(j, high, low, closed)
    total_trade[j] <- dum$number_of_trade
    total_system_profit[j] <- dum$system_profit
  }

  maxprofit <- max(total_system_profit)-0.00001
  optimal_period <- which(total_system_profit >= maxprofit)

  result <- list(total_trade, total_system_profit, optimal_period)
  names(result) <- c("total_trade", "total_system_profit", "optimal_period")
  return(result)
}

library(ggplot2)
eurusd <- read.csv("eurusd_dd1.csv", header = FALSE)
colnames(eurusd) <- c("high", "low", "closed")
n <- as.numeric(dim(eurusd)[1])
n_real <- floor(n * 0.7)
test_eurusd <- eurusd[-(1:n_real), ]
rownames(test_eurusd) <- NULL
eurusd <- eurusd[1:n_real, ]
rownames(eurusd) <- NULL
rm(n, n_real)

training_result <- optimize_zlema(period = 400, high = eurusd$high, low = eurusd$low, closed = eurusd$closed)
test_result <- optimize_zlema(period = 400, high = test_eurusd$high, low = test_eurusd$low, closed = test_eurusd$closed)

qplot(x = 1:400, y =  training_result$total_system_profit, geom = "area", fill = "Training Data", ylab = "Total System Profit") +
  geom_line(aes(x = 1:400, y = 4983 / 2136 * test_result$total_system_profit, color = "Test Data")) + scale_color_manual(values = "blue")
  
### when parameter 'period' is 90, the system performs best without overfitting.
### 'Best model' means the system is most profitable on both training and testing data sets.
