library(forecast)

# Create data frame.
walmart.data <- read.csv("walmartrevenue.csv")

# See the first 6 records of the file.
head(walmart.data)

walmart.ts <- ts(walmart.data$Revenue, 
                 start = c(2005, 1), end = c(2024, 4), freq = 4)

#Timeplot 
plot(walmart.ts, 
     xlab = "Time", ylab = "Revenue (in $million)", 
     ylim = c(71500,180600), xaxt = 'n',
     main = "TimePlot of Walmart Revenue 05-24")
# Establish x-axis scale interval for time in months.
axis(1, at = seq(2005, 2026, 1), labels = format(seq(2005, 2026, 1)))


#80-20 division of data. Validation set = 2021Q1–2024Q4 . Training = 2005Q1–2020Q4
nValid <- 16
nTrain <- length(walmart.ts) - nValid
train.ts <- window(walmart.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(walmart.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid)) 

#Model 1 
#FIT REGRESSION MODEL WITH LINEAR TREND 

# Use tslm() to create regression model with linear trend.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

#Model 2
# FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND 

# Use tslm()  to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() to make predictions for validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

#Model 3
# FIT REGRESSION MODEL WITH SEASONALITY 

# Use tslm() to create regression seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons.
train.season$data 

# Apply forecast()   
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

#Model 4
# FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

#Model 5
# FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() quadratic trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

# accuracy measure -Regression model with linear trend 
round(accuracy(train.lin.pred$mean, valid.ts), 3)
#Accuracy of Regression model with quadratic trend 
round(accuracy(train.quad.pred$mean, valid.ts), 3)
#Accuracy of  Regression model with seasonality
round(accuracy(train.season.pred$mean, valid.ts), 3)
#Accuracy measure of Regression model with linear trend and seasonality 
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
#accuracy measure for Regression model with quadratic trend and seasonality. 
round(accuracy(train.quad.season.pred$mean, valid.ts),3)

#Regression model with linear trend  model 1
# Use tslm() function to create linear trend model.
lin.trend <- tslm(walmart.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin.trend)

# Apply forecast() function to make predictions for ts with 
# linear trend data in 4 future peiords
lin.trend.pred <- forecast(lin.trend, h = 4, level = 0)
lin.trend.pred 

#Regression model with linear trend and seasonality  model 4
# Use tslm() function to create regression model with linear trend 
# and seasonality.
lin.season <- tslm(walmart.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 4 future peiords
lin.season.pred <- forecast(lin.season, h = 4, level = 0)
lin.season.pred

#Regression model with quadratic trend and seasonality model 5
quad.tred.season <- tslm(walmart.ts ~ trend + I(trend^2) + season)
summary(quad.tred.season)
quad.trend.season.pred <- forecast(quad.tred.season , h = 4, level = 0)
quad.trend.season.pred

# Use accuracy() function to identify common accuracy measures

#Regression model with linear trend  model 1
round(accuracy(lin.trend.pred$fitted, walmart.ts),3)
#Regression model with linear trend and seasonality  model 4
round(accuracy(lin.season.pred$fitted, walmart.ts),3)
#Regression model with quadratic trend and seasonality model 5
round(accuracy(quad.trend.season.pred$fitted, walmart.ts),3)
# for naive model
round(accuracy((naive(walmart.ts))$fitted, walmart.ts), 3)
#seasonal naive
round(accuracy((snaive(walmart.ts))$fitted, walmart.ts), 3)
