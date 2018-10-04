
UserAllstats <- function(data) {

  testlist <- c("Uzunluk", "Minimum", "Maksimum", "Medyan", "Ortalama", "Std.S.", "Varyans", "Skewness", "Kurtosis")
  
  sonuc <- data.frame(matrix(NA, nrow = length(testlist), ncol = ncol(data)))
  colnames(sonuc) <- colnames(data)
  rownames(sonuc) <- testlist
  
  sonuc[1,] <- apply(data, 2, length)
  sonuc[2,] <- apply(data, 2, min)
  sonuc[3,] <- apply(data, 2, max)
  sonuc[4,] <- apply(data, 2, median)
  sonuc[5,] <- apply(data, 2, mean)
  sonuc[6,] <- apply(data, 2, sd)
  sonuc[7,] <- apply(data, 2, var)
  sonuc[8,] <- apply(data, 2, 
                     PerformanceAnalytics::skewness)
  sonuc[9,] <- apply(data, 2, 
                     PerformanceAnalytics::kurtosis)
  
  sonuc <- round(sonuc, digits = 6)
  
  return(sonuc)
}
