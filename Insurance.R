setwd("C:\\Users\\shreyas\\Desktop\\WorkSpace\\IVY-R\\Machine-Learning-with-R-datasets-master")
ins <- read.csv("insurance.csv", stringsAsFactors = TRUE)
View(ins)

str(ins)

#dependent variable is "charges".
#Prior to building a regression model, it is often helpful to check for normality.

summary(ins$charges)
#right skewed data distribution

hist(ins$charges)


table(ins$region)
#frequency distribution

cor(ins[c("age" ,"bmi", "children", "charges")])
#correlation among the features

pairs(ins[c("age","bmi","children","charges")])
#scatter plots

require(psych)
pairs.panels(ins[c("age", "bmi", "children", "charges")])

#fit to model
ins_model <- lm(charges ~ age + bmi + sex + smoker + region , data = ins)
ins_model


#evaluating model performance
summary(ins_model)

#improving model performance

##adding non-linear relationships
ins$age2 <- ins$age^2

##converting a numeric variable to a binary indicator.
##We can model this relationship by creating a binary obesity indicator variable that is 1 if the BMI is at least 30, and 0 if less.

ins$bmi30 <- ifelse(ins$bmi >= 30 ,1 ,0) 

##Specified an INTERACTION(*) between obesity and smoking
##The "*" operator is shorthand that instructs R to model
 #expenses ~ bmi30 +smokeryes + bmi30:smokeryes.

ins_model2 <- lm(charges~ age + age2 + children + bmi + sex + bmi30*smoker + region
                 , data = ins)
summary(ins_model2)


##Relative to our first model, the R-squaredvalue has improved from 0.75 to about 0.87. 
  #Similarly, the adjusted R-squared value,
  #which takes into account the fact that the model grew in complexity, also improved
  #from 0.75 to 0.87. Our model is now explaining 87 percent of the variation in medical
  #treatment costs.
