#============================================================
# Load packages and data
#============================================================

library("ggplot2")
library("forecast")
library("tseries")

day <- read.csv("day.csv", header = T, stringsAsFactors = F)
str(day)


hour <- read.csv("hour.csv", header = T, stringsAsFactors = F)
str(hour)

#============================================================
# Examine data
#============================================================

day$Date = as.Date(day$dteday)

ggplot(day, aes(Date, cnt)) +
        geom_line() +
        scale_x_date("month") +
        ylab("Daily Bike Checkouts")

count_ts = ts(day[,c('cnt')])

day$clean_cnt = tsclean(count_ts)

ggplot() +
        geom_line(data = day, aes(x = Date,
                                  y = clean_cnt)) +
        ylab("Cleaned Bicycle Count")

# Even when cleaned, still volatile

# Weekly or monthly moving average

day$cnt_ma = ma(day$clean_cnt, order = 7)
day$cnt_ma30 = ma(day$clean_cnt, order = 30)

ggplot() +
        geom_line(data = day, aes(x = Date,
                                  y = clean_cnt,
                                  colour = "Counts")) +
        geom_line(data = day, aes(x = Date,
                                  y = cnt_ma,
                                  colour = "Weekly Moving Average")) +
        geom_line(data = day, aes(x = Date,
                                  y = cnt_ma30,
                                  colour = "Monthly Moving Average")) +
        ylab("Bicycle Count")

#============================================================
# Calculate seasonal component of the data
#============================================================

count_ma = ts(na.omit(day$cnt_ma), frequency = 30)

decomp = stl(count_ma, s.window = "periodic")

deseasonal_cnt <- seasadj(decomp)

plot(decomp)

#============================================================
# Augmented Dickey-Fuller Test for stationarity
#============================================================

adf.test(count_ma, alternative = "stationary")

#============================================================
# Autocorrelations and choosing model order
#============================================================

Acf(count_ma)
Pacf(count_ma)

# Differencing model

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main = "ACF for Differenced Series")
Pacf(count_d1, main = "PACF for Differenced Series")

#============================================================
# Fit model
#============================================================

# Significant auto correlations at lag 1 and 2
# Significant partial correlations spike at 1 and 7

auto.arima(deseasonal_cnt, seasonal = F)

#============================================================
# Evaluate and iterate
#============================================================

fit <- auto.arima(deseasonal_cnt, seasonal = F)
tsdisplay(residuals(fit), lag.max = 45, main = '(1,1,1) Model Residuals')

# clear pattern in ACF/PACF and model residuals plots repeating at lag 7
# Try p or q at 7

fit2 = arima(deseasonal_cnt, order = c(1,1,7))

tsdisplay(residuals(fit2), lag.max = 15, main = "Seasonal Model Residuals")

#============================================================
# Forecast
#============================================================

fcast <- forecast(fit2, h = 30)
plot(fcast)

# Now use test set

hold <- window(ts(deseasonal_cnt), start = 700)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), 
                       order = c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout, h = 25)
plot(fcast_no_holdout)

lines(ts(deseasonal_cnt))

# Plotted predictions are based on the assumption that there
# will be no other seasonal fluctuations in the data and the
# change in number of bicycles from one day to another is more
# or less constant in mean and variance

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal = T)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h = 30)
plot(seas_fcast)