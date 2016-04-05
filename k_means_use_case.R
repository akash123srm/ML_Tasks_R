
#Load the data

auto_data <- read.csv(file.choose())

str(auto_data)

summary(auto_data)

#Check for the missing values (NA's) in the data frame

sapply(auto_data, function(x) sum(is.na(x)))

#Clustering requires all numeric values to be in the same range. Hence we need to center and scale data set

scaled_data <- scale(auto_data[8:12])

auto_data[,8:12] <- scaled_data
summary(auto_data)

# Now we do exploratory data analysis

par(mfrow=c(1,5))

boxplot( auto_data$HP,col="red")

title("HP")

boxplot( auto_data$RPM,col="blue")

title("RPM")

boxplot( auto_data$MPG.CITY,col="green")

title("MPG.CITY")

boxplot( auto_data$MPG.HWY, col="maroon")

title("MPG.HWY")

boxplot( auto_data$PRICE, col="cyan")

title("PRICE")

library(class)
## Warning: package 'class' was built under R version 3.1.1
#keep the same seed for each execution. Seed impacts the initial centroid position and hence may impact #actual clusters formed.

set.seed(11111)

auto_subset <- auto_data[1:100, c(8,12)]

clusters<- kmeans(auto_subset,4)

clusters

par(mfrow=c(1,1))

plot(auto_subset$HP, auto_subset$PRICE, col=clusters$cluster,
     xlab="HP",ylab="PRICE",pch=20, cex=2)

points(clusters$centers, col="purple", pch=17, cex=3)

#Now we will do clustering over all the columns

#Convert categorical variables into numerical ones

for(i in 1:8){
  auto_data[, i] = as.numeric(auto_data[, i])
}

summary(auto_data)

set.seed(11111)

clusters<- kmeans(auto_data,4)

clusters

#finding the optimum no. of clusters

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", col="red")}
wssplot(auto_data)

#3 seems to be the optimal number of clusters for this dataset by looking at the above plot and observing the knee bend

set.seed(11111)

clusters<- kmeans(auto_data,3)

clusters
