setwd("C:/Users/shreyas/Desktop/WorkSpace/IVY-R/WorkDir/market basket analysis")
library(arules)
groceries <- read.transactions("groceries.csv" , sep = ",")

summary(groceries)

#The first five transactions
inspect(groceries[1:5])

itemFrequency(groceries[,1:16])

#1tems with atleast 10% support
itemFrequencyPlot(groceries , support = 0.1)

#top 20items
itemFrequencyPlot(groceries , topN = 20 )

#visualize the sparse matrix
image(groceries[1:5])

#training a model on the data

grocery_rules  <- apriori(groceries , parameter = list(support=0.006 ,
                                     confidence=0.25 ,
                                     minlen = 2))

# "grocery_rules" contains a set of 463 association rules

#Evaluating model performance
summary(grocery_rules)

inspect(grocery_rules[1:3])

#improving model performance

##sort()
inspect(sort(grocery_rules , by = "lift")[1:5])

##subset()
berry_rules <- subset(grocery_rules , items %in% "berries")

inspect(berry_rules)

#convert results to dataframe
grocery_rules_df <- as(grocery_rules , "data.frame")
View(grocery_rules_df)

write(grocery_rules , File = "groceryRules.csv" , sep = ",")
