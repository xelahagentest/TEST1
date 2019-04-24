#load the data frames
Churn_data <- read.csv("C:/Users/ahagen/OneDrive - Convergence Consulting Group/Code Samples/Data for Samples/Historical_churn_data.csv")

library(rpart)

#Build a decision tree
my_tree <- rpart(churn ~ state + account_length + international_plan + voice_mail_plan + number_vmail_messages + total_day_minutes + total_day_charge + total_eve_minutes + total_eve_charge + total_night_minutes + total_night_charge + total_intl_minutes + total_intl_calls + total_intl_charge + number_customer_service_calls, data = Churn_data, method = "class")

#build a fancy plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(my_tree, cex=0.5)

#Create Random Forest
library(randomForest)
set.seed(27)

#Split up data
smp_size <- floor(0.67 * nrow(Churn_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Churn_data)), size = smp_size)

train <- Churn_data[train_ind, ]
test <- Churn_data[-train_ind, ]


#make the model
the_forest <- randomForest(as.factor(churn) ~ state + account_length + 
    international_plan + voice_mail_plan + total_day_minutes + total_day_charge + 
    total_eve_minutes + total_eve_charge + total_night_minutes + total_night_charge + 
    total_intl_minutes + total_intl_charge + number_customer_service_calls, 
      data = train, importance = TRUE, ntree = 1000)

#How important are the variables?
varImpPlot(the_forest)

#Add prediction into table
my_prediction <- predict(the_forest, test)
test$predicted_churn <- my_prediction

#Evaluate predictions
test$True_Positive <- ifelse(test$churn == 1 & test$my_prediction == 1, 1, 0)
#More to add
