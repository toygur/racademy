
# Definitions -------------------------------------------------------------

setwd("C:/Users/Tunc/Desktop/ykb")

library(mlbench)
library(caret)

# Data Importer ----------------------------------------------------------
df <- iris

df_x <- iris[,1:4]
df_y <- iris[,5]

sapply(df, class)
str(df)

library(skimr)
skim(df)

# Correlation -------------------------------------------------------------

correlation <- cor(df_x)

#library(corrplot)
X11()
corrplot::corrplot(correlation, method = "circle")

# Visualization -------------------------------------------------------------

##Pairs
pairs(df)
X11()
pairs(Species ~ ., data=df, col=df$Species)

##Box
X11()
caret::featurePlot(x=df_x, y=df_y, plot="box")

##Density
X11()
caret::featurePlot(x=df_x, y=df_y, plot="density", 
                   scales=list(x = list(relation = "free"), 
                               y = list(relation = "free"))
                   )


# Pre-Processing ----------------------------------------------------------

#Class bazlı metotlar inputlar aynı scale de olduklarında daha iyi sonuçlar vermektedir.
#Regresyon modelleri ise inputlar standardize edildiklerinde daha iyi sonuç vermektedir.

##1. Scale Data
#Scaling bir attribute için standart sapmayı hesaplar ve her değeri standart sapmaya böler.
preprocessParams <- preProcess(df_x, method = "scale")
preprocessParams
transformed_df_x <- predict(preprocessParams, df_x)

##2. Center Data
#Merkez dönüşümü bir özniteliğin ortalamasını hesaplar ve her bir değerden çıkarır.
preprocessParams <- preProcess(df_x, method = "center")
preprocessParams
transformed_df_x <- predict(preprocessParams, df_x)

##3. Standardize Data
#Ölçeği ve merkez dönüşümlerini birleştirmek verilerinizi standartlaştıracaktır.
#Attribute'ların ortalama değeri 0 ve standart sapma 1 olacaktır.
preprocessParams <- preProcess(df_x, method=c("center", "scale"))
preprocessParams
transformed_df_x <- predict(preprocessParams, df_x)

##4. Box-Cox Transform
#Skew olan dataları Gaussian dağılıma shift ettirmek için kullanılır. Tüm değerlerin pozitif olması gerekir.


##PCA: Principal Component Analysis
preprocessParams <- preProcess(df, method=c("center", "scale","pca"))
preprocessParams
transformed_df <- predict(preprocessParams, df)


# ReSampling --------------------------------------------------------------

set.seed(123)
split <- caTools::sample.split(transformed_df$Species, SplitRatio = 0.8)

trainingData <- subset(transformed_df, split==T)
testData     <- subset(transformed_df, split==F)

#PSI: population stability Index
p <- rPSI::psi(original = trainingData$Species,
               current  = testData$Species)

summary(p)


# Naive Bayes -----------------------------------------------------

fit <- klaR::NaiveBayes(Species ~ .,  data=trainingData)

prediction <- predict(fit, testData[,-1])

confusionMatrix(prediction$class, testData$Species)


# Logistic Regression -----------------------------------------------------

data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes

fit <- glm(diabetes~., data=df, family=binomial)

summary(fit)

probabilities <- predict(fit, df[,1:8], type='response')
predictions <- ifelse(probabilities > 0.5,'pos','neg')

diabetes_org <- df$diabetes
diabetes_prd <- as.factor(predictions)

#https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/confusionMatrix
confusionMatrix(diabetes_prd, diabetes_org)




