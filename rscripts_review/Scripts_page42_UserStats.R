
userStats <- function(x){
  
  sonuc <- data.frame(matrix(ncol=1, nrow = 9))
  rownames(sonuc) <- c("uzunluk","min","maks","ort","var","sd",
                       "median","skew","kurt")  
  
  sonuc[1,1] <- length(x)
  sonuc[2,1] <- min(x)
  sonuc[3,1] <- max(x)
  sonuc[4,1] <- mean(x)
  sonuc[5,1] <- var(x)
  sonuc[6,1] <- sd(x)
  sonuc[7,1] <- median(x)
  sonuc[8,1] <- PerformanceAnalytics::skewness(x)
  sonuc[9,1] <- PerformanceAnalytics::kurtosis(x)
  
  return(sonuc)
}