iris_data <- iris

iris_data

# Takiing a look at the structure of the dataset
str(iris_data)

summary(iris_data)

library(ggplot2)

plot1 <- qplot(Petal.Length,Petal.Width,data=iris_data,colour=Species,size=3) 
plot1

plot2 <- qplot(Sepal.Length,Sepal.Width,data=iris_data,colour=Species,size=3) 
plot2

#Drawing boxplots

boxplot(Petal.Length ~ Species, data=iris_data,col="red")
title("Petal Length")

boxplot(Petal.Width ~ Species, data=iris_data,col="blue")
title("Petal Width")

boxplot(Sepal.Length ~ Species, data=iris_data,col="green")
title("Sepal Length")

boxplot(Sepal.Width ~ Species, data=iris_data,col="yellow")
title("Sepal Width")

library(psych)
pairs.panels(iris_data)

#Using decison trees to predict the Species

library(caret)

inTrain <- createDataPartition(y=iris_data$Species,p=0.7,list=FALSE)
training_data <- iris_data[inTrain,]
testing_data <- iris_data[-inTrain,]

training_data
testing_data


table(training_data$Species)
table(testing_data$Species)

#Building decison tree model

library(C50)

model <- C5.0(training_data[-5],training_data$Species)

summary(model)

prediction <- predict(model,testing_data)
prediction


table(prediction)

confusionMatrix(prediction,testing_data$Species)
  