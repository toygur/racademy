
# While Kontrol Yapısı #

x <- c(8:12)
#sonuc <- vector(mode="numeric", length = length(x))
sonuc <- numeric(length(x))
i <- 1

while (i <= length(x)) {
  if (x[i] > 10) {
    sonuc[i] <- 2^x[i]
  } else if (x[i] == 10) {
    sonuc[i] <- x[i]
  } else {
    sonuc[i] <- 2*x[i]
  }
  i <- i + 1
}

sonuc

