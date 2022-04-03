
rm(list=ls(all=TRUE)) 	# clear data
library("forecast")
library("tseries") 	
library("dplyr")
library("readxl")

data <- as.data.frame(read_excel("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.xlsx", sheet=1))
data <- data %>% select(c("Date","NYC","MI","MT","OH","CO"))

############################ Anand ####################################

#############################
### Step 1. Build a stationary series with the given data

### coverts cases data as time series object with start date and frequency (daily here)
sample_data <- diff(data$NYC[41:750],differences = 1)
yy <- ts(sample_data, frequency = 365, start = c(2020,62))		

### ploting time series to see patterns: trend, cycle, variance over time
plot.ts(yy)

### Stationarity Test - if p-value is large (> 0.10), then nonstationary
adf.test(yy)							
### Data is stationary, no need of differentiation

model_1 <- auto.arima(yy)		# fits ARIMA(p,d,q) x (P, D, Q) automatically

model_1.predict <- forecast:::forecast.Arima(model_1, h = 7,level = c(68, 90))
plot(model_1.predict)

fcst <- as.data.frame(model_1.predict$mean)
fcst

yd <- diff(yy,differences = 2)			
plot.ts(yd)								# looks stationary visually
adf.test(yd)							# estimated p = 0.01 => small p-value (< 0.10) => so yd is stationary ==> fix d = 1 in ARIMA models to be fitted


#If not using Auto arima, use this method

## Step 1. Decide AR(p) or MA(q) or both ARMA(p,q). Use the stationary series from Step 1. 

# To decide AR(p), plot Pacf. For AR(p) => Pacf becomes zero at some lag p

Pacf(yd, lag.max = 40)					# Pacf suggests p = 29 


# To decide MA, plot Acf. For MA(q) => Acf becomes zero at some lag q

Acf(yd, lag.max = 40)				# Acf suggests q = 29


## Step 2. Fit several ARIMA models. 	

m1 <- Arima(yy, order = c(29,1,29))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)

m1				# see the output of m1. The estimated phi value and its std err to assess significnace

summary(m1)		# see Accuracy using MAPE = Mean Absolute Percentage Error 

# do this for other p,d,q values

m2 <- Arima(yy, order = c(29,1,30))			
m2
summary(m2)

# Consider Seasonal ARIMA(p,d,q) x (P, D, Q) components when seasonality is expected/suspected

## Step 3. Use Information Criteria to retain the "best model" 
## Use AIC_c if sample size is small (i.e., p/T ratio is large = 10% or more)
## Use BIC if sample size is large (i.e., p/T ratio is small = 5% of less)

## Step 4. Make Out-of-Sample Forecasts with Prediction Interval based on your retained model

m1.predict <- forecast:::forecast.Arima(m1, h = 52, level = c(68, 90))
plot(m1.predict)

summary(m1.predict)	

