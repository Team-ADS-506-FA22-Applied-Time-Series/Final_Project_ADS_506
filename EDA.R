
library(zoo)
library(tidyverse)
library(fpp2) # Plot and Forecast Data
set.seed(506)
library(readxl)
df <- read_excel("Final Choice plans DMAs.xlsx", 
                 sheet = "NeuralNet")
str(df)



# Convert Quarter character to date
df$date <- as.Date(df$svc_agreement_activation_date)




max(df$date)

# Create Time Series Object
myts <- ts(df$activation_count, 
           start = c(2021, as.numeric(format(df$date[1], "%j"))),
           frequency = 364)



# Split Data
train <- window(myts, end = c(2022, 273)) #273 orig 300

# Set Forecast periods
h <- length(myts) - length(train)

############
# Naïve Models
mean_fit <- meanf(train,h=h) # Naïve: Mean
naive_fit <- naive(train,h=h,level = 95) # Naïve: Last Value
drift_fit <- rwf(train,h=h,drift=TRUE) # Naïve: Drift Method
snaive_fit <- snaive(train,h=h) # Naïve: Seasonal Naïve

#############
# Train Models
# Trend + Seasonal Model
ST <- forecast(tslm(train ~ trend + season),h=h)
# Trend + Fourier Seasonal or Harmonic Regression Model
# fourier <- forecast(tslm(train ~ trend + fourier(train, K=2)),h=h)
# Exponential Model
EXP <- forecast(tslm(train ~ trend + season, lambda = 0),h=h)
# Spline Model
# SPLINE <- forecast(tslm(train ~ trend + I(trend^2) + I(trend^3) + season),h=h)
#TBats
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
# NNAR - neural network
NNAR <- forecast(nnetar(train), h=h)#, lambda=0)
#STL
# STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)
# Auto ARIMA
ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),h=h)
# ETS
ETS <- forecast(ets(train), h=h)

DHOLT <- holt(train, damped=TRUE, phi = 0.9, h=h)


# Dynamic harmonic regression
k = 1
fit <- auto.arima(train, xreg = fourier(train, K = k), seasonal = FALSE, lambda = 0)
model <- forecast(fit, xreg=fourier(train, K=k, h=h))

Combination <- (EXP[["mean"]] + 
                  ARIMA[["mean"]] +
                  ST[["mean"]] + 
                  NNAR[["mean"]] + 
                  TBATS[["mean"]])/5




# Combination <- (EXP[["mean"]] +  
#                   TBATS[["mean"]])/2

# Plot Results
autoplot(window(myts, start=c(2022, 250))) +
  autolayer(mean_fit, series="Naïve: Mean", PI=FALSE) +
  # autolayer(drift_fit, series="Naïve: Drift Method", PI=FALSE) +
  # autolayer(snaive_fit, series="Naïve: Seasonal Naïve", PI=FALSE) +
  # autolayer(naive_fit, series="Naïve: Last Value", PI=FALSE) +
  # autolayer(ST, series="Seasonal Trend", PI=FALSE) +
  autolayer(EXP, series="Exponential", PI=FALSE) +
  # autolayer(SPLINE, series="Spline", PI=FALSE) +
  # autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  # autolayer(NNAR, series="NNAR", PI=FALSE) +
  # autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(DHOLT, series="Damped Holt's method", PI=FALSE) +
  autolayer(model, series="Dynamic harmonic regression", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Date") + 
  ylab("Activations") +
  ggtitle("Forecasts for daily activations") +
  theme_minimal() +
  guides(colour=guide_legend(title="Forecast"))

# Check Residuals
fit <- auto.arima(train, lambda=0, biasadj=TRUE)
fit <- tslm(train ~ trend + season, lambda = 0)
fit <- auto.arima(train, xreg = fourier(train, K = 1),
                  seasonal = FALSE, lambda = 0)
checkresiduals(fit)

window_df <- window(myts, start=2021)
print(strrep("#", 80))
print("Naïve: Mean")
accuracy(mean_fit, window_df)
print(strrep("#", 80))
print("Naïve: Last Value")
accuracy(naive_fit, window_df)
print(strrep("#", 80))
print("Naïve: Drift Method")
accuracy(drift_fit, window_df)
print(strrep("#", 80))
print("Naïve: Seasonal Naïve")
accuracy(snaive_fit, window_df)
print(strrep("#", 80))
print("Seasonal Trend")
accuracy(st_fit, window_df)
print(strrep("#", 80))
print("Exponential")
accuracy(exp_fit, window_df)
print(strrep("#", 80))
print("Spline")
accuracy(spine_fit, window_df)
print(strrep("#", 80))
print("ARIMA")
accuracy(arima_fit, window_df)
print(strrep("#", 80))
print("ETS")
accuracy(ets_fit, window_df)




c(
  `Naïve: Mean`= accuracy(mean_fit, myts)["Test set","RMSE"],
  `Naïve: Last Value`= accuracy(naive_fit, myts)["Test set","RMSE"],
  `Naïve: Drift Method`= accuracy(drift_fit, myts)["Test set","RMSE"],
  `Naïve: Seasonal Naïve`= accuracy(snaive_fit, myts)["Test set","RMSE"],
  DHR = accuracy(model, myts)["Test set","RMSE"],
  ETS = accuracy(ETS, myts)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, myts)["Test set","RMSE"],
  ST = accuracy(ST, myts)["Test set","RMSE"],
  NNAR = accuracy(NNAR, myts)["Test set","RMSE"],
  EXP = accuracy(EXP, myts)["Test set","RMSE"],
  TBATS = accuracy(TBATS, myts)["Test set","RMSE"],
  Combination = accuracy(Combination, myts)["Test set","RMSE"])


######################################################################
# Weekly Totals

library(tidyverse)
library(lubridate)

# Create Regular Intervals - Monthly Totals
df_weekly <- df %>%
  filter(isFuture=='FALSE') %>% 
  group_by(week =  cut(date, "week", start.on.monday = TRUE)) %>%
  summarise(Total = sum(activation_count))





# Create Time Series Object
myts <- ts(df_weekly$Total, 
           start = c(2021, 35),
           frequency = 52)




# Plot the Time Series
autoplot(myts) +
  labs(title = "Activations Over Time",
       x = "Daily",
       y = "Activations") + 
  theme_minimal()

week(df_weekly$week)

# Split Data
train <- window(myts, 
                start = c(2021, 35), 
                end = c(2022, 31)) #38 orig max 43
h=8

# Naïve Models
mean_fit <- meanf(train,h=h) # Naïve: Mean
naive_fit <- naive(train,h=h,level = 95) # Naïve: Last Value
drift_fit <- rwf(train,h=h,drift=TRUE) # Naïve: Drift Method
snaive_fit <- snaive(train,h=h) # Naïve: Seasonal Naïve
# Train Models
# Trend + Seasonal Model
st_model <- tslm(train ~ trend)
st_fit <- forecast(st_model,h=h)


# Trend + Seasonal Model
ft_model <- tslm(train ~ trend + fourier(train, K=2))
ft_fit <- forecast(ft_model,h=h)


# Exponential Model
exp_model <- tslm(train ~ trend , lambda = 0)
exp_fit <- forecast(exp_model,h=h)
# Spline Model
spline_model <- tslm(train ~ trend + I(trend^2) + I(trend^3) )
spine_fit <- forecast(spline_model,h=h)
# Auto ARIMA
arima_model <- auto.arima(train)
arima_fit <- forecast(arima_model, h=h)
# ETS
ets_model <- ets(train, model="AAN", damped=FALSE, lambda=0)
ets_fit <-  forecast(ets_model, h=h, biasadj=TRUE)

# Plot Results
autoplot(window(myts, start=c(2022, 1))) +
  autolayer(mean_fit, series="Naïve: Mean", PI=FALSE) +
  autolayer(drift_fit, series="Naïve: Drift Method", PI=FALSE) +
  # autolayer(snaive_fit, series="Naïve: Seasonal Naïve", PI=FALSE) +
  autolayer(naive_fit, series="Naïve: Last Value", PI=FALSE) +
  autolayer(st_fit, series="Seasonal Trend", PI=FALSE) +
  autolayer(exp_fit, series="Exponential", PI=FALSE) +
  autolayer(spine_fit, series="Spline", PI=FALSE) +
  autolayer(arima_fit, series="ARIMA", PI=FALSE) +
  autolayer(ets_fit, series="ETS", PI=FALSE) +
  autolayer(ft_fit, series="Fourier + Trend", PI=FALSE) +
  xlab("Date") + 
  ylab("Activations") +
  ggtitle("Forecasts for daily activations") +
  theme_minimal() +
  guides(colour=guide_legend(title="Forecast"))


df %>% filter(isFuture=='FALSE') %>%
  select(activation_count) %>% 
  splinef(lambda=0) %>%
  autoplot()

df %>% filter(isFuture=='FALSE') %>%
  select(activation_count) %>% 
  splinef(lambda=0) %>%
  checkresiduals()


window_df <- window(myts, start=2021)
print(strrep("#", 80))
print("Naïve: Mean")
accuracy(mean_fit, window_df)
print(strrep("#", 80))
print("Naïve: Last Value")
accuracy(naive_fit, window_df)
print(strrep("#", 80))
print("Naïve: Drift Method")
accuracy(drift_fit, window_df)
print(strrep("#", 80))
print("Naïve: Seasonal Naïve")
accuracy(snaive_fit, window_df)
print(strrep("#", 80))
print("Seasonal Trend")
accuracy(st_fit, window_df)
print(strrep("#", 80))
print("Exponential")
accuracy(exp_fit, window_df)
print(strrep("#", 80))
print("Spline")
accuracy(spine_fit, window_df)
print(strrep("#", 80))
print("ARIMA")
accuracy(arima_fit, window_df)
print(strrep("#", 80))
print("ETS")
accuracy(ets_fit, window_df)