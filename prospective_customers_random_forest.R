# gather data

customer_data <- read.csv(file.choose(),header=TRUE,sep=";")

str(customer_data)

summary(customer_data)

#Given the large number of variables, we will do correlation analysis in 2 parts

library(psych)

pairs.panels(customer_data[,c(1:8,17)])

pairs.panels(customer_data[, c(9:17)])

# Now we will remove predictors having very low correltion

revised_data <- customer_data[, c(1:4,7:9,12,14,15,17)]

str(revised_data)
