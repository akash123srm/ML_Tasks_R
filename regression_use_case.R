

#Loading the data

car_data <- read.csv(file.choose())

str(car_data)

summary(car_data)

head(car_data)

# We will convert the horsepower column from factor to numeric

car_data$HORSEPOWER <- as.numeric(car_data$HORSEPOWER)

# We will replace missing values with the mean 

car_data$HORSEPOWER[is.na(car_data$HORSEPOWER)] <- mean(car_data$HORSEPOWER, na.rm=TRUE)

summary(car_data)

#Now we do some exploratory data analysis

library(ggplot2)


ggplot(car_data, aes(factor(CYLINDERS), MPG)) +
  geom_boxplot( aes(fill=factor(CYLINDERS)))

# Now we look at correlations between different variables

library(psych)

pairs.panels(car_data)

# We observe that Name has little correlation with target variable MPG,hence it can be ignored while building the model

# We also observe that CYLINDERS,DISPLACEMENT & WEIGHT have high correlation among themselves.Hence 
# one is the proxy of other two.SO we remove CYLINDERS,DISPLACEMENT & keep weight

car_data$CYLINDERS <- NULL

car_data$DISPLACEMENT <- NULL

# ignore colume 8 - NAMES which is string. Regression works only on numbers.

lm_model <- lm(MPG ~ ., car_data[,-6] )
summary(lm_model)

# Testing

predicted_values <- predict.lm(lm_model, car_data)
summary(predicted_values)

# Now we will plot actual vs predicted values

plot(car_data$MPG,predicted_values,col="blue")

#find correlation between prediction and actuals. Its another way of estmating the fit of the model.

cor(car_data$MPG, predicted_values)

