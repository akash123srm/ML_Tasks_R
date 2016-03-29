# gather data

sms_data <- read.csv(file.choose(),stringsAsFactors=FALSE)

str(sms_data)

# We convert the type column into factors 

sms_data$type <- as.factor(sms_data$type)

str(sms_data)

summary(sms_data)

head(sms_data)

#We will load library for text cleaning

library(tm)

# Create a corpus for the messages

msg_corpus <- Corpus(VectorSource(sms_data$text))

#peek into the corpus

inspect(msg_corpus[1:5])

#Now we would clean the data

#cleanse the data
#remove punctuation marks
refined_corpus <- tm_map(msg_corpus, removePunctuation)
#remove white space
refined_corpus <- tm_map(refined_corpus, stripWhitespace)
#convert to lower case
refined_corpus <- tm_map(refined_corpus, content_transformer(tolower))
#remove numbers in text
refined_corpus <- tm_map(refined_corpus, removeNumbers)
#remove stop words
refined_corpus <- tm_map(refined_corpus, removeWords, stopwords())
#remove specific words
refined_corpus <- tm_map(refined_corpus, removeWords, c("else","the","are","for",
                                                        "has","they","as","a","his","on","when","is","in","already"))
#look at the processed text
inspect(refined_corpus[1:5])

#Now we will create a document term matrix where we we will covert a document in a
#matrix form  in which every document is a row while every word is a column

dtm <- DocumentTermMatrix(refined_corpus)

dtm

#We will remove all the words which occur less tan 10 times

dtm_revised <- DocumentTermMatrix(refined_corpus,list(dictionary=findFreqTerms(dtm,10)) )

dim(dtm_revised)

#inspect the contents be converting it into a matrix and transposing it
(inspect(dtm_revised)[1:25,])

#The following example shows a word cloud for both ham and spam message.
#The size of words shown in the word cloud is based on the frequency of occurance. It will clearly show that
#there is a difference in the most common occuring words between these types

library(wordcloud)

pal <- brewer.pal(9,"Dark2")

wordcloud(refined_corpus[sms_data$type=="ham"],min.freq=5,colors=pal,random.order=FALSE)


wordcloud(refined_corpus[sms_data$type=="spam"],min.freq=5,colors=pal,random.order=FALSE)


#Building model

library(caret)

inTrain <- createDataPartition(y=sms_data$type,p=0.7,list=FALSE)

#We split the raw data

train_raw <- sms_data[inTrain,]

test_raw <- sms_data[-inTrain,]

#We split the corpus

train_corpus <- refined_corpus[inTrain,]

test_corpus <- refined_corpus[-inTrain,]

# We split the document term matrix

train_dtm <- dtm_revised[inTrain,]

test_dtm <- dtm_revised[-inTrain,]

# We know take convert the numeric data from dtm into factor

conv_numeric <- function(x) {
  
  x <- ifelse(x > 0,1,0)
  x <- factor(x,levels=c(0,1),labels=c("No","Yes"))
  
}



# apply the above function

train <- apply(train_dtm,MARGIN=2,conv_numeric)

test <- apply(test_dtm,MARGIN=2,conv_numeric)

# convert to a data frame and add the target variable

df_train <- as.data.frame(train)

df_test <- as.data.frame(test)

df_train$type <- train_raw$type

df_test$type <- test_raw$type

df_train[1:10,1:10]
  

library(e1071)

#Leave out the last column (target)

modFit <- naiveBayes(df_train[,-60], df_train$type)
modFit

#Apply the model on test data set

predictions <- predict(modFit, df_test)

confusionMatrix(predictions, df_test$type)

  