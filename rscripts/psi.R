
# psi measures the stablity of the population.

# population stays the same as the past if psi is less than 0.1
# and a significant shift can be recognised if psi is greater than 0.25.

# You can use summary function to see all of the detailed information.

##Install
library(devtools)
install_github("siyuany/rpsi")

##Usage
library(rPSI)

data("iris")

train <- sample(nrow(iris), nrow(iris) * .7)

train.species <- iris$Species[train]
test.species <- iris$Species[-train]

p <- psi(train.species, test.species)
p
summary(p)
