

noshow_final <- read.csv(file.choose(), header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")

names(noshow_final)

table(noshow_final$Show)

dim(noshow_final)

names(noshow_final)

#TO get the number of duplicate rows  (-1 -- 1st is the index column)
dim(noshow_final[duplicated(noshow_final[,-1]),])

# Create new DF with no duplicates
noshow_patient = noshow_final[!duplicated(noshow_final[,-1]),]

noshow_final <- noshow_final[,-1]

View(noshow_final)
names(noshow_final)

table(noshow_patient$Show)

#Create DF for Random Forest
noshow_rf <- noshow_final[,c(1,2,3,4,5,6,13)]

dim(noshow_patient)

#Dosease count variable
noshow_rf$Dsease_cnt <- noshow_final$Hipertension+noshow_final$Diabetes+noshow_final$Alcoholism+noshow_final$Handcap

View(noshow_rf)

set.seed(1234)

population <- sample(nrow(noshow_rf), 0.70 * nrow(noshow_rf))
noshow_rf_train = noshow_rf[population,]
noshow_rf_test = noshow_rf[-population,]

table(noshow_rf_train$Show)
table(noshow_rf_test$Show)

str(noshow_rf)

names(noshow_rf)
# Train a model using Random Forest
library(randomForest)



model <- randomForest(as.factor(Show) ~Gender+Age+Awaiting_time+show_cnt+ratio_noshow_show+Dsease_cnt , data = noshow_rf_train)

Random_forest = model

prediction <- predict(Random_forest, newdata = noshow_rf_test)

# For confusion matrix loa dht library caret
library(caret)

table(prediction)

confusionMatrix(prediction, noshow_rf_test$Show)

#ROC Curve
install.packages('pROC')
library('pROC')

plot.roc(noshow_rf_test$Show,predictor = pred,col="Red")




# Naive Bayes classifier
library(e1071)
#Naive Bayes

names(noshow_nb)

noshow_nb=noshow_rf

# Factoring the required variable for NO show data
noshow_nb$Gender <-noshow_nb$Gender
noshow_nb$Age <-noshow_nb$Age
noshow_nb$SMS_received <-noshow_nb$SMS_received
noshow_nb$Awaiting_time <-noshow_nb$Awaiting_time
noshow_nb$show_cnt <-noshow_nb$show_cnt
noshow_nb$ratio_noshow_show <-noshow_nb$ratio_noshow_show
noshow_nb$SMS_received.1 <-noshow_nb$SMS_received.1
noshow_nb$Show <-noshow_nb$Show
noshow_nb$Dsease_cnt <-noshow_nb$Dsease_cnt

names(noshow_nb)
dim(noshow_nb)

#Create test and train dataset
set.seed(1234)
population <- sample(nrow(noshow_nb), 0.70 * nrow(noshow_nb))
noshow_nb_train = noshow_nb[population,]
noshow_nb_test = noshow_nb[-population,]

table(noshow_nb_train$Show)
table(noshow_nb_test$Show)

View(noshow_nb_train)
names(noshow_nb)

dim(noshow_nb)

attach(noshow_nb_train)


# Training the Naive Bayes model 
Model_NB <- naiveBayes(as.factor(Show) ~ Gender+Age+SMS_received+Awaiting_time+show_cnt+ratio_noshow_show+Dsease_cnt, data = noshow_nb_train)

Model_NB


# PRediction the Naive bayes model on Test data
prediction <- predict(Model_NB, newdata = noshow_nb_test)

pred = ifelse(prediction>0.5,1,0)

table(prediction)

library(caret)

confusionMatrix(prediction, noshow_nb_test$Show)

plot.roc(prediction, noshow_nb_test$Show,col="Blue")


