
# Missing Data #

#kütüphaneleri çalıştıralım
library(readr)
library(dplyr)

#Öncelikle explanatory datasetini içeri almamız gereklidir.
explanatory <- read_csv("explanatory.csv",
                        col_types = cols(
                          Date = col_date(
                            format = "%Y-%m-%d")
                        )
)

#Tüm sütunlarda eksik data olup olmadığını konrol?
apply(explanatory[,-1], 2, function(x) any(is.na(x)))

#dplyr ile hafta sonlarını temizleme ve hala eksik olan günler için fiyatı bir önceki günden kopyalama
explanatory <- explanatory %>% 
  mutate(flag = chron::is.weekend(Date)) %>% 
  filter(!flag) %>% 
  select(-flag) %>% 
  tidyr::fill(everything(), .direction="down")

#Eksik günleri tekrar kontrol edelim.
apply(explanatory[,-1], 2, function(x) any(is.na(x)))