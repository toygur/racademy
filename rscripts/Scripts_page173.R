
# For Kontrol Yapısı#

n <- 100
datanum <- 5

data <- matrix(nrow = n, ncol = datanum)

for(i in 1:datanum){
  
  data[,i] <- rnorm(n, mean = 0, sd = 1)
  
}

sonuc<-matrix(nrow=datanum, ncol=datanum)

for(i in 1:datanum){
  for(j in 1:datanum){
    
    sonuc[i,j] <- cor(data[,i],data[,j])
    
  }
}

sonuc