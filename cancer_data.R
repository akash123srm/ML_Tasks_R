
cancer_data <- read.csv(file.choose())

str(cancer_data)

# Explorartory data analysis

library(psych)

#given the large number of variables, split into 3 sets and see correlation to diagnosis.
pairs.panels(cancer_data[, c(2,3:10)])

pairs.panels(cancer_data[, c(2,11:20)])

pairs.panels(cancer_data[, c(2,21:32)])

#Principal Component Analysis of the data

#scale the data first
scaled_data <- scale(cancer_data[, 3:32])

#convert into principal components
pca_data <- prcomp(scaled_data)
plot(pca_data)


summary(pca_data)

#Get only the first 3 components
final_data <- data.frame(pca_data$x[,1:3])

#add diagnosis to the data frame
final_data$diagnosis <- cancer_data$diagnosis
pairs.panels(final_data)

#Modeling & Prediction
#Split Training and Testing Split training and testing datasets in the ratio of 70-30

library(caret)

inTrain <- createDataPartition(y=final_data$diagnosis ,p=0.7,list=FALSE)
training <- final_data[inTrain,]
testing <- final_data[-inTrain,]



predlist <- c("bagFDA", #Bagging
              "LogitBoost", #Boosting
              "nnet", #Neural Networks
              "svmRadialCost") #Support vector machines

#Create a result data set

results <- data.frame( Algorithm=character(), Duration=numeric(), Accuracy=numeric(),
                       stringsAsFactors=FALSE)

#loop through algorithm list and perform model building and prediction

for (i in 1:length(predlist)) {
  pred <- predlist[i]
  print(paste("Algorithm = ",pred ))
  
  #Measure Time
  
  startTime <- as.integer(Sys.time())
  
  #Build model
  
  model <- train( diagnosis ~ ., data=training, method=pred)
  
  #Predict
  
  predicted <- predict(model, testing)
  
  #Compare results
  
  matrix<- confusionMatrix(predicted, testing$diagnosis)
  
  #Measure end time
  endTime <- as.integer(Sys.time())
  
  #Store result
  thisresult <- c( as.character(pred), endTime-startTime, as.numeric(matrix$overall[1]))
  results[i,1] <- pred
  results[i,2] <- endTime-startTime
  results[i,3] <- round(as.numeric(matrix$overall[1]) * 100, 2)
}
  