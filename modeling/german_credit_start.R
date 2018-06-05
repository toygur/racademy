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