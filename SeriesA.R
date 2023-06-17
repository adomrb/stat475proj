library(tidyverse)
library(forecast)
library(readxl)
projectSeriesA <- read_excel("STAT 475/Project/projectSeriesA.xlsx")
View(projectSeriesA)

data.A <- ts(projectSeriesA$Adam, start = c(2003,4), frequency = 12)
head(data)
ggtsdisplay(data)

# Mean is clearly increasing over time. ACF dying down very slowly.
# These indicate the data is unstationary.

data.diff <- diff(data)
ggtsdisplay(data.diff)

# First difference looks stationary. ACF appears to be dying down
# quickly, while the PACF has spikes at lags 1 and 3. Because of this,
# an AR model seems appropriate, but the order is ambiguous.
# I will try AR(1), AR(2), and AR(3).

fit1 <- Arima(data, order=c(1,1,0))
fit2 <- Arima(data, order=c(2,1,0))
fit3 <- Arima(data, order=c(3,1,0))

fit1 %>%
  print() %>%
  checkresiduals()

fit2 %>%
  print() %>%
  checkresiduals()

fit3 %>%
  print() %>%
  checkresiduals()

# None of these look great.
# The AR(1) model has clearly correlated residuals, so we might try a mixed model.
# The same as above applies to the AR(2) model.
# AR(3) gives us possibly uncorrelated residuals, but we have an insignificant
# parameter estimate for our AR(2) component.

fit4 <- Arima(data, order=c(1,1,3))
fit5 <- Arima(data, order=c(2,1,1))
fit6 <- Arima(data, order=c(2,1,2))
fit7 <- Arima(data, order=c(2,1,3))

fit4 %>%
  print() %>%
  checkresiduals()

fit5 %>%
  print() %>%
  checkresiduals()

fit6 %>%
  print() %>%
  checkresiduals()

fit7 %>%
  print() %>%
  checkresiduals()

# None of these appear to be a significant improvement over our AR models.
# Currently, our best performing models are fit3, fit4, and fit7, which
# correspond to ARIMA(3,1,0), ARIMA(1,1,3), and ARIMA(2,1,3).

# Notice: The differenced series appears to not be centered at 0.
# Thus, I will revisit the candidates while including drift.

fit8 <- Arima(data, order=c(3,1,0), include.drift = TRUE)
fit9 <- Arima(data, order=c(1,1,3), include.drift = TRUE)
fit10 <- Arima(data, order=c(2,1,3), include.drift = TRUE)

fit8 %>%
  print() %>%
  checkresiduals()

fit9 %>%
  print() %>%
  checkresiduals()

fit10 %>%
  print() %>%
  checkresiduals()

# Much better results in terms of the Ljung-Box tests.
# I will further analyze these three models to make a final choice.

# ARIMA(3,1,0) with drift:
fit8
ggtsdisplay(fit8$residuals)
Box.test(fit8$residuals, lag=6, type=c("Ljung-Box"), fitdf=3)
Box.test(fit8$residuals, lag=12, type=c("Ljung-Box"), fitdf=3)
Box.test(fit8$residuals, lag=18, type=c("Ljung-Box"), fitdf=3)
Box.test(fit8$residuals, lag=24, type=c("Ljung-Box"), fitdf=3)


# ARIMA(1,1,3) with drift:
fit9
ggtsdisplay(fit9$residuals)
Box.test(fit9$residuals, lag=6, type=c("Ljung-Box"), fitdf=4)
Box.test(fit9$residuals, lag=12, type=c("Ljung-Box"), fitdf=4)
Box.test(fit9$residuals, lag=18, type=c("Ljung-Box"), fitdf=4)
Box.test(fit9$residuals, lag=24, type=c("Ljung-Box"), fitdf=4)

# ARIMA(2,1,3) with drift:
fit10
ggtsdisplay(fit10$residuals)
Box.test(fit10$residuals, lag=6, type=c("Ljung-Box"), fitdf=5)
Box.test(fit10$residuals, lag=12, type=c("Ljung-Box"), fitdf=5)
Box.test(fit10$residuals, lag=18, type=c("Ljung-Box"), fitdf=5)
Box.test(fit10$residuals, lag=24, type=c("Ljung-Box"), fitdf=5)

# All of the models have significant p-values for each lag tested.
# fit8's p-values are the lowest, and its ar2 coefficient is not significantly different from 0.
# fit9's p-values are slightly higher than fit8's but lower than fit10's, and
# its ma2 coefficient is just on the edge of being statistically significant.
# fit10 has the highest p-values, but it has the most parameters, the highest
# standard deviations across the board, and an insignificant ma2 coefficient.

# All of them are performing roughly evenly to me,
# so I will forecast using the simplest model, ARIMA(3,1,0).

pred <- forecast(fit8,
         h=12,
         level=c(70,90)
)
pred
autoplot(pred) +
  labs(x = "Month", y = "Sales", title = "Forecast")
