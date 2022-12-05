
library(zoo)
library(tidyverse)
library(fpp2) # Plot and Forecast Data
set.seed(506)
library(readxl)
df <- read_excel("Sales_Forecasting.xlsx", 
                                col_types = c("date", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric"))
str(df)



########################################################################
# Let's get data statistics
dim(df)
summary(df$svc_agreement_activation_date)
summary( df$activation_count)

########################################################################
# Let's get the naive model benchmark to beat.

end <- as.Date("2022-09-30")
start <- end-13

(naive_prediction <- df %>%
  filter((svc_agreement_activation_date >= start ) & 
           (svc_agreement_activation_date <= end )) %>% 
  group_by(weekday) %>% 
  summarize(avg = mean(activation_count)))


sqrt(mean((df %>% filter((svc_agreement_activation_date >= as.Date("2022-10-01") ) & 
           (svc_agreement_activation_date <= as.Date("2022-10-31") )) %>% 
  left_join(., naive_prediction, by ='weekday') %>% 
  select(svc_agreement_activation_date, activation_count, avg, weekday) %>% 
    mutate(resids = activation_count - avg) %>% pull(resids))^2))


########################################################################
# EDA

# Create Time Series Object
myts <- ts(df$activation_count, 
           start = c(2021, 243),
           frequency = 365)

myts


# Plot the Time Series
autoplot(myts) +
  labs(title = "Activations Over Time",
       x = "Daily",
       y = "Activations") + 
  theme_minimal()

# 9/30 = 273
# 10/1 = 274

# Plot the Time Series
autoplot(window(myts, start = c(2022, 274))) +
  labs(title = "Activations Over Time",
       x = "Daily",
       y = "Activations") + 
  theme_minimal()


# Plot the Time Series
autoplot(diff(myts)) +
  labs(title = "Activations Over Time",
       x = "Daily",
       y = "Activations") + 
  theme_minimal()

# Plot the Time Series
autoplot(diff(diff(myts, 7))) +
  labs(title = "Activations Over Time",
       x = "Daily",
       y = "Activations") + 
  theme_minimal()


# Seasonal Decomposition of Time Series by Loess
autoplot(mstl(myts, 7))+
  labs(title = "Activations Over Time",
       x = "Daily",
       y = "Activations") + 
  theme_minimal()



# take a first order difference to make the data stationary.
# to determine the ar() part of the ARIMA model p
acf(myts, 300)
acf(diff(myts), 300)
acf(diff(myts, 7), 300)
acf(diff(diff(myts, 7)), 300)

# to determine the ma()
pacf(myts, 300)
pacf(myts, 300)
pacf(diff(myts))
pacf(diff(myts, 7))
pacf(diff(diff(myts, 7)))


# our arima model
my_arima <- arima(myts, order = c(1,1,2), seasonal = list(order = c(1,1,1), period = 7))
summary(my_arima)


auto_model <- auto.arima(myts)
summary(auto_model)



# Lag plot
gglagplot(myts)
## This plot shows that the relationship is strongly positive for lag of 7
#, reflecting the strong seasonality in the data. 


########################################################################


window(myts, end = c(2022, 397)) #273 orig 300


length(myts)-30
# Split Data
train <- window(myts, start = as.Date("2022-10-01")) #273 orig 300

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

