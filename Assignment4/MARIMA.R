setwd( '/Users/emilhaugstvedt/Desktop/DTU/TimeSeriesAnalysis/Time-Series-Analysis/Assignment4')

# Load marima library:
library(marima)

# Load data:
data <- read.csv("A4_data.csv")

# Separate into training and test:
num_test <- 4
num_train <- (nrow(data) - num_test)

# Create training set with NaN in last four rows:
training_data <- data
training_data[num_train:(num_train + num_test), 2:8]
training_data[(num_train + 1):(num_train + num_test), 2:8] <- matrix(data = NaN, nrow = 4, ncol = 7)

# Log transform the data:
log.Data <- training_data[2:8]
log.Data[1:4] <- log(log.Data[1:4])

# Difference the data:
difference <- c(1, 1, 2, 1, 3, 1, 4, 1, 5, 4, 6, 0, 7, 0)

dlog.Data <- define.dif(log.Data, difference)$dif.series
dLog.average <- define.dif(log.Data[1:num_train,], difference)$average

##### MARIMA #####

# Create the model:
model <- define.model(kvar=7, ar= (1), ma= (1), reg.var= c(5, 6, 7))

# Fit the model:
Marima <- marima(dlog.Data,
                 ar.pattern = model$ar.pattern, 
                 ma.pattern = model$ma.pattern,
                 Check=FALSE, 
                 Plot="log.det",
                 penalty=0.0)

# Do the forecast:
forecast <- arma.forecast(dlog.Data, marima= Marima, nstart= 115, nstep= 3)$forecasts

forecast

dLog.average

# Undifference the data:
undifference <- rbind(c(1, 1, 2, 1, 3, 1, 4, 1, 5, 4, 6, 0, 7, 0))
undifferenced_forecast <- define.sum(forecast,
                                     difference = undifference,
                                     averages = dLog.average)$series.sum

# Untransform the data:
undifferenced_and_untransformed_forecast[1:4, ] <- exp(undifferenced_forecast[1:4, ])

# Extract the predictions for the different zip codes:
predictZ2000 <- undifferenced_and_untransformed_forecast[1, ]
predictZ2800 <- undifferenced_and_untransformed_forecast[2, ]
predictZ4000 <- undifferenced_and_untransformed_forecast[3, ]
predictZ4200 <- undifferenced_and_untransformed_forecast[4, ]

# Plot predictions and true values:
par(mfrow=c(2,2))
plot(predictZ2000, type= "l", main= "True vs. Prediction z2000", col= "orange",
     ylab= "Price", ylim= c(0, 150000))
lines(data$z2000, type= "l", col= "Blue")
legend("topleft", c("Prediction", "True"), fill= c("orange", "blue"))

plot(predictZ2800, type= "l", main= "True vs. Prediction z2800", col= "orange",
     ylab= "Price", ylim= c(0, 100000))
lines(data$z2800, type= "l", col= "Blue")
legend("topleft", c("Prediction", "True"), fill= c("orange", "blue"))

plot(predictZ4000, type= "l", main= "True vs. Prediction z4000", col= "orange",
     ylab= "Price", ylim= c(0, 60000))
lines(data$z4000, type= "l", col= "Blue")
legend("topleft", c("Prediction", "True"), fill= c("orange", "blue"))

plot(predictZ4200, type= "l", main= "True vs. Prediction z42000", col= "orange",
     ylab= "Price", ylim= c(0, 30000))
lines(data$z4200, type= "l", col= "Blue")
legend("topleft", c("Prediction", "True"), fill= c("orange", "blue"))

