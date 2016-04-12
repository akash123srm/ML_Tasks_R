accident_data <- read.csv(file.choose())

str(accident_data)

summary(accident_data)

# Now we need to convert the dataset into basket form to apply arules dataset

#We get all the column names
col_names <- names(accident_data)

#We will build a file in basket format - one row per transaction and each column value becoming
# a basket item in the format <column_name>=<column_value>

basket_string <- ""

for (row in 1:nrow(accident_data)) {
  
  if (row != 1) {
    basket_string <- paste0(basket_string, "\n")
  }
  basket_string <- paste0(basket_string, row,",")
  
  for (col in 2:length(col_names)) {
    if (col != 2) {
      basket_string <- paste0(basket_string, ",")
    }
    basket_string <- paste0(basket_string, col_names[col],"=",accident_data[row,col])
  }
}

path <- "C:\\Users\\Akash\\Desktop\\Applied Data Science\\material\\accident_basket.csv"

write(basket_string, path)

library(arules)

accident_transactions <- read.transactions(path, format = "basket", sep=",")

#We discover the frequently occuring patterns with arules.

rules <- apriori(accident_transactions, parameter=list(supp=0.1, conf=0.3))

#We inspect the first 30 rules

inspect(rules[1:30])

#Looking at the output of the top 30 rules, we see patterns of conditions that frequently occur together. For
#example " {Weather_Conditions=2} => {Accident_Severity=3} " is an interesting pattern since it tells us
#that when a given weather condition exists, there is a specific severity of accident that occurs