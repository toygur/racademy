
#Aşağıdaki 3 örnek, capm datası içindeki ilk sütunu tarih formatına çevirmektedir.

###################################################################################
#Örnek1: for

capm <- readr::read_csv("data/capm.csv")

tarih <- character(nrow(capm))
for(i in 1:nrow(capm)) {
  tarih[i] <- paste0("01-",capm$Date[i])
}
capm$Date <- lubridate::dmy(tarih)
str(capm)

###################################################################################
#Örnek 2: apply

capm <- readr::read_csv("data/capm.csv")

tarih <- apply(capm[,"Date"], 1, function(x) paste0("01-",x))

capm$Date <- lubridate::dmy(tarih)
str(capm)


###################################################################################
#Örnek 3: dplyr

capm <- readr::read_csv("data/capm.csv")

library(dplyr)
library(magrittr)

capm %<>% mutate(Tarih = lubridate::dmy(paste0("01-",Date)))
