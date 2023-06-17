library(tidyverse)
library(forecast)
library(readxl)
projectSeriesC <- read_excel("STAT 475/Project/projectSeriesC.xlsx")
View(projectSeriesC)
data.C <- ts(projectSeriesC$Adam, start = c(2013,4), frequency=12)
ggtsdisplay(data)

# Data looks seasonal on a yearly basis.

data.sdiff <- diff(data, lag=12)
ggtsdisplay(data.sdiff)

# First seasonal difference looks usable.

data.diff.sdiff <- diff(data.sdiff)
ggtsdisplay(data.diff.sdiff)

# First regular difference of the first seasonal difference
# looks okay, but unnecessary, so I will proceed based on
# the first seasonal difference.

# NONSEASONAL:
# ACF: Dying down
# PACF: Cutting off at either lag 1 or 2

# SEASONAL:
# ACF: Ambiguous. Will try both, but intuition tells me dying down.
# PACF: See above. Intuition tells me cutting off at lag 1.

# So my initial candidates are:
# ARIMA(1,0,0)(1,1,0)
# ARIMA(2,0,0)(1,1,0)
# ARIMA(1,0,0)(0,1,1)
# ARIMA(2,0,0)(0,1,1)

fit1 <- Arima(data,
              order=c(1,0,0),
              seasonal=c(1,1,0))
fit2 <- Arima(data,
              order=c(2,0,0),
              seasonal=c(1,1,0))
fit3 <- Arima(data,
              order=c(1,0,0),
              seasonal=c(0,1,1))
fit4 <- Arima(data,
              order=c(2,0,0),
              seasonal=c(0,1,1))

fit1 %>%
  checkresiduals()

fit2 %>%
  checkresiduals()

fit3 %>%
  checkresiduals()

fit4 %>%
  checkresiduals()

# fit1 and fit3 show that the residuals are autocorrelated at lag 1,
# so the extra parameter introduced in fit2 and fit4 seems necessary.
# Looking closer at fit2 and fit4:

print(fit2)
ggtsdisplay(fit2$residuals)
Box.test(fit2$residuals, lag=6, type=c("Ljung-Box"), fitdf=3)
Box.test(fit2$residuals, lag=12, type=c("Ljung-Box"), fitdf=3)
Box.test(fit2$residuals, lag=18, type=c("Ljung-Box"), fitdf=3)
Box.test(fit2$residuals, lag=24, type=c("Ljung-Box"), fitdf=3)

print(fit4)
ggtsdisplay(fit4$residuals)
Box.test(fit4$residuals, lag=6, type=c("Ljung-Box"), fitdf=3)
Box.test(fit4$residuals, lag=12, type=c("Ljung-Box"), fitdf=3)
Box.test(fit4$residuals, lag=18, type=c("Ljung-Box"), fitdf=3)
Box.test(fit4$residuals, lag=24, type=c("Ljung-Box"), fitdf=3)

# Both models show very similar characteristics: their parameter
# estimates are all statistically significant, their residuals both
# appear to be similarly non-normally distributed, they pass the
# Ljung-Box test at each lag selected, and they have the same
# number of parameters.
# fit4 has slightly better results from the Box-Ljung tests,
# so I will use that one for forecasting.

pred <- forecast(fit4,
                 h=12,
                 level=c(70,90))

pred
autoplot(pred)