library(PoEdata)
data(food)

#Intercept + Slope 
m1 <- lm(data=food, food_exp ~ income)
summary(m1)

#Slope
m2 <- lm(data=food, food_exp ~ income +0)
summary(m2)
