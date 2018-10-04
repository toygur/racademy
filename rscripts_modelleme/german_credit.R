
setwd("C:/Users/Tunc/Desktop/R_ile_Modelleme")

## STEP 1: Import Dataset & Libraries #################################################################################

#car kütüphanesi içerisinden vif (variance inflation factor) fonksiyonu kullanılacaktır.
#ROCR kütüphanesi içerisinden psi (population stability index) fonksiyonu kullanılacaktır.
 
library(car)
library(ROCR)

dataset <- read.csv("datasets/GermanCredit.csv", 
                    header = T, 
                    stringsAsFactors = F)
str(dataset)

# status: Status of existing checking account 
# debtors: Other debtors / guarantors 
# present_residence: Present residence since

## STEP 2: Encoding Categorical data  #####################################################################

dataset$status <- as.numeric(factor(dataset$status,
                       levels = c("... < 100 DM",
                                  "0 <= ... < 200 DM",
                                  "... >= 200 DM / salary for at least 1 year",
                                  "no checking account"),
                       labels = c(1:4)))

dataset$credit_history <- as.numeric(factor(dataset$credit_history,
                     levels = c("no credits taken/all credits paid back duly",
                                "all credits at this bank paid back duly",
                                "existing credits paid back duly till now",
                                "delay in paying off in the past",
                                "critical account/other credits existing"),
                     labels = c(1:5)))

dataset$purpose <- as.numeric(factor(dataset$purpose,
                      levels = c("car (new)",
                                 "car (used)",
                                 "furniture/equipment",
                                 "radio/television",
                                 "domestic appliances",
                                 "repairs",
                                 "education",
                                 "retraining",
                                 "business",
                                 "others"),
                      labels = c(1:10)))

dataset$savings <- as.numeric(factor(dataset$savings,
                              levels = c("... < 100 DM",
                                         "100 <= ... < 500 DM",
                                         "500 <= ... < 1000 DM",
                                         "... >= 1000 DM",
                                         "unknown/no savings account"),
                              labels = c(1:5)))

dataset$employment_duration <- as.numeric(factor(dataset$employment_duration,
                       levels = c("unemployed",
                                  "... < 1 year",
                                  "1 <= ... < 4 years",
                                  "4 <= ... < 7 years",
                                  "... >= 7 years"),
                       labels = c(1:5)))

dataset$personal_status_sex <- as.numeric(factor(dataset$personal_status_sex,
                                   levels = c("male : divorced/separated",
                                              "female : divorced/separated/married",
                                              "male : single",
                                              "male : married/widowed"),
                                   labels = c(1:4)))

dataset$other_debtors <- as.numeric(factor(dataset$other_debtors,
                            levels = c("none",
                                        "co-applicant",
                                        "guarantor"),
                            labels = c(1:3)))

dataset$property <- as.numeric(factor(dataset$property,
                        levels = c("real estate",
                                  "building society savings agreement/life insurance",
                                  "car or other",
                                  "unknown/no property"),
                        labels = c(1:4)))

dataset$other_installment_plans <- as.numeric(factor(dataset$other_installment_plans,
                                       levels = c("bank",
                                                  "stores",
                                                  "none"),
                                        labels = c(1:3)))

dataset$housing <- as.numeric(factor(dataset$housing,
                      levels = c("rent",
                                "own",
                                "for free"),
                      labels = c(1:3)))

dataset$job <- as.numeric(factor(dataset$job,
                   levels = c("unemployed/unskilled - non-resident",
                              "unskilled - resident",
                              "skilled employee/official",
                              "management/self-employed/highly qualified employee/officer"),
                    labels = c(1:4)))

dataset$telephone <- as.numeric(factor(dataset$telephone,
                         levels = c("yes","no"),
                         labels = c(1,2)))

dataset$foreign_worker <- as.numeric(factor(dataset$foreign_worker,
                              levels = c("yes","no"),
                              labels = c(1,2)))

## STEP 3: Split dataset & Population Stability Index #####################################################################

library(caTools)

set.seed(123)
split <- sample.split(dataset$credit_risk, 
                      SplitRatio = 0.8)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

p <- rPSI::psi(as.factor(training_set$credit_risk), 
               as.factor(test_set$credit_risk))
p
summary(p)

## STEP 4: Scaling dataset  #####################################################################

training_set[,-21] <- scale(training_set[,-21])
test_set[,-21] <- scale(test_set[, -21])

## STEP 5: Stepwise Logistic Regression  #####################################################################

m.logistic <- step(glm(formula = credit_risk ~ ., 
                       data = training_set, 
                       family = binomial), 
                   direction="both", steps = 1000)

summary(m.logistic)
all(vif(m.logistic) < 2.5)
broom::tidy((vif(m.logistic)))

m.logistic.colums <- which(colnames(test_set) %in% names(m.logistic$coefficients))

pred.logistic <- predict(m.logistic, 
                         type = 'response', 
                         newdata = test_set[,m.logistic.colums])

y_pred.logistic <- ifelse(pred.logistic > 0.5, 1, 0)

cm.logistic <- table(test_set[,21], y_pred.logistic)
cm.logistic

caret::confusionMatrix(cm.logistic)

##Calculating ROC curve: (Beling et al, 2005)

pred.logistic <- prediction(pred.logistic, test_set$credit_risk)
perf.logistic <- performance(pred.logistic, "tpr", "fpr")
plot(perf.logistic)
abline(a = 0, b = 1, col = "red")

# ##Calculation KS statistic: 
# # Ks, cumulative true positive'ler ile cumulative false positive'ler arasındaki farkların maksimumudur.
# 
# max(attr(perf.logistic,'y.values')[[1]]-attr(perf.logistic,'x.values')[[1]])

## STEP 6: K-Nearest Neighbors ##################################################################################

#install.packages("class")

library(class)

y_pred.knearest <- knn(train = training_set[, -21],
                       test = test_set[, -21],
                       cl = training_set[, 21],
                       k = 5,
                       prob = TRUE)

cm.knearest <- table(test_set[, 21], y_pred.knearest)
cm.knearest

pred.knearest <- prediction(attributes(y_pred.knearest)$prob, test_set$credit_risk)
perf.knearest <- performance(pred.knearest, "tpr", "fpr")
plot(perf.knearest)
abline(a = 0, b = 1, col = "red")

## STEP 7: Logistic vs K-Nearest Neighbors ##################################################################################

plot(perf.logistic, col='red', lty=1, lwd=2, main='ROC Curve');
plot(perf.knearest, col='green', add=TRUE, lwd=2, lty=2);
abline(a = 0, b = 1, col = "black");
legend("bottomright",c('Logistic','K-Nearest'), col=c('red','green'),lwd=1)

## STEP 8: Support Vector Machine ##################################################################################

library(e1071)

m.svm <- svm(formula = credit_risk ~ .,
             data = training_set,
             type = 'C-classification',
             kernel = 'linear')

y_pred.svm <- predict(m.svm, newdata = test_set[,-21])

cm.svm <- table(test_set[, 21], y_pred.svm)
cm.svm

pred.svm <- prediction(as.numeric(y_pred.svm), test_set$credit_risk)
perf.svm <- performance(pred.svm, "tpr", "fpr")
plot(perf.svm)
abline(a = 0, b = 1, col = "red")

## STEP 9: Kernel SVM ##################################################################################

library(e1071)

m.kernel_svm <- svm(formula = credit_risk ~ .,
                    data = training_set,
                    type = 'C-classification',
                    kernel = 'sigmoid')

y_pred.kernel_svm <- predict(m.kernel_svm, newdata = test_set[,-21])

cm.kernel_svm <- table(test_set[, 21], y_pred.kernel_svm)
cm.kernel_svm

pred.kernel_svm<- prediction(as.numeric(y_pred.kernel_svm), test_set$credit_risk)
perf.kernel_svm <- performance(pred.kernel_svm, "tpr", "fpr")
plot(perf.kernel_svm)
abline(a = 0, b = 1, col = "red")

## STEP 10: Random Forest ##################################################################################

#install.packages("randomForest")

library(randomForest)
set.seed(123)
m.rforest <- randomForest(x = training_set[,-21],
                          y = as.factor(training_set$credit_risk),
                          ntree = 1000, keep.forest=FALSE,
                          importance=TRUE)

#Variable Importance measures
#The variable importance plot lists variables in terms of importance using the 
#decrease in accuracy metric, of loss of predictive power if the variable is dropped, 
#vs. the importance in terms of Gini index, a measure of separation of classes

varImpPlot(m.rforest)

y_pred.rforest <- predict(m.rforest, newdata = test_set[,-21])

cm.rforest <- table(test_set[, 21], y_pred.rforest)

pred.rforest <- prediction(as.numeric(y_pred.rforest), test_set$credit_risk)
perf.rforest <- performance(pred.rforest, "tpr", "fpr")
plot(perf.rforest)
abline(a = 0, b = 1, col = "red")

## STEP 11: Artificial Neural Network ##################################################################################

#install.packages("h2o")

library(h2o)
h2o.init(nthreads = -1)
m.ann <- h2o.deeplearning(y = 'credit_risk',
                          training_frame = as.h2o(training_set),
                          activation = 'Rectifier',
                          hidden = c(12,12),
                          epochs = 100,
                          train_samples_per_iteration = -2)

pred.ann <- h2o.predict(m.ann, 
                        newdata = as.h2o(test_set[,-21]))
y_pred.ann <- (pred.ann > 0.5)
y_pred.ann <- as.vector(y_pred.ann)

cm.ann <- table(test_set[, 21], y_pred.ann)
cm.ann

pred.ann <- prediction(as.numeric(y_pred.ann), test_set$credit_risk)
perf.ann <- performance(pred.ann, "tpr", "fpr")
plot(perf.ann)
abline(a = 0, b = 1, col = "red")

h2o.shutdown()

## ROC CURVE COMPARISON ##################################################################################

##X11() fonksyionu oluşturulan plot'u Rstudio'nun sağ alt ekranında değil, ekrana yeni bir pencere olarak açar.

X11()
plot(perf.logistic, col='red', lty=1, lwd=2, main='ROC Curve');
plot(perf.knearest, col='green', add=TRUE, lwd=2, lty=2);
plot(perf.svm, col='navy', add=TRUE, lwd=2, lty=3);
plot(perf.kernel_svm, col='brown', add=TRUE, lwd=2, lty=4);
plot(perf.rforest, col='blue', add=TRUE, lwd=2, lty=5);
plot(perf.ann, col='black', add=TRUE, lwd=2, lty=1);
abline(a = 0, b = 1, col = "gray");
legend("bottomright",
       c('Logistic','K-Nearest','SVM','Kernel SVM','Random Forest','ANN'), 
       col=c('red','green','navy','brown','blue','black'),
       lwd=2)

