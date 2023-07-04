library(data.table)
library(ggplot2)
library(randomForest)
d1 = fread("C:/Users/ASUS/Downloads/diabetes_012_health_indicators_BRFSS2015.csv")
d2 = fread("C:/Users/ASUS/Downloads/diabetes_binary_5050split_health_indicators_BRFSS2015.csv")
d3 = fread("C:/Users/ASUS/Downloads/diabetes_binary_health_indicators_BRFSS2015.csv")
#Question 1 
# we use some boxplots to demonstrate difference between groups of people with diabetese
# and non-diabetese based on some numerical features.

boxplot(BMI ~ Diabetes_binary , data = d3)
boxplot(PhysHlth ~ Diabetes_binary , data = d3)

#next we use some bar charts to show the difference between diabetese and non-diabetese based on categorical features.
n1 = nrow(d2[HighChol == 1 & Diabetes_binary == 1, c("HighChol","Diabetes_binary")])
n2 = nrow(d2[HighChol == 0 & Diabetes_binary == 1, c("HighChol","Diabetes_binary")])
n3 = nrow(d2[HighChol == 1 & Diabetes_binary == 0, c("HighChol","Diabetes_binary")])
n4 = nrow(d2[HighChol == 0 & Diabetes_binary == 0, c("HighChol","Diabetes_binary")])
matrix = matrix(c(n1,n2,n3,n4), nrow=2, ncol = 2)
barplot(matrix, main="cholestole level" , names.arg = c("diabetes", "non-diabetes"),col = c("red","blue"), beside=TRUE)
legend("center", c("high cholestrole" , "low cholestrole"), cex = 0.7, fill = c("red","blue"))

x= length(unique(d3$GenHlth))
matrix = matrix(nrow = x , ncol = 2)
for (i in 1:x){
  for (j in 1:2){
    matrix[i,j] = nrow(d2[GenHlth == i & Diabetes_binary == j-1, c("GenHlth","Diabetes_binary")])
  }
}

barplot(matrix, main = "general health level", names.arg = c("diabetes", "non-diabetes"), beside=TRUE, col = c("darkred", "red","yellow","green","darkgreen"))
legend("center", c("1","2","3","4","5"), cex = 0.7 , fill = c("darkred", "red","yellow","green","darkgreen"))
# we conclude that these features could be used to train a model of predicting diabetese
#######################################
#Question 2


#for this section we calculate correlation of each feature with diabetese.
correlations = c(cor(d3)[1,])
correlations = correlations[correlations != 1]
barplot(correlations , main = "correlation of features" , col = "blue")
#in this chart we observe each feature's correlation with diabetese.
#larger absolute value of correlations show more power of that feature in predicting diabetese.
#######################################
#Question 3

#spliting data into train and test

ind = sample(nrow(d2), 0.7*nrow(d2))
train = d2[ind,]
test = d2[-ind,]

#we train a random forest

forest = randomForest(as.factor(Diabetes_binary) ~ ., data=train , mtry = 4, importance = TRUE)
importance(forest)
#we test our model
predicted = predict(forest, test)
table(predicted, test$Diabetes_binary)

#now we train and test a new model trained only on features with MeanDecreaseAccuracy more than 80.
#these features are HighBP, BMI, GenHlth, Age, HighChol.
forest = randomForest(as.factor(Diabetes_binary) ~ HighBP + HighChol+
                        BMI + GenHlth + Age, data=train , mtry = 2, importance = TRUE)
forest
predicted = predict(forest, test)
table(predicted, test$Diabetes_binary)
#we see that we have the same accuracy as the last model.
#########################################


# Question 4
#in the last section we answered these questions. 
#the strategy for using test and train was a 70-30 split.
################################################


#Question 5
#yes we can. by answering 5 questions and using our model, patients should know that they have diabetes or not, with a accuracy of 75%.
#patients answer the questions and by using the predict function of our model we predict their diabetes.

