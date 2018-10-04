
# Missing Data #

#kütüphaneleri çalıştıralım
library(readr)
library(dplyr)
library(magrittr)

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
explanatory %<>% 
  mutate(flag = chron::is.weekend(Date)) %>% 
  filter(!flag) %>% 
  select(-flag) %>% 
  tidyr::fill(everything(), .direction="down")

#veya kısaca
 explanatory %<>% 
  filter(!chron::is.weekend(Date)) %>%  
  tidyr::fill(everything(), .direction="down")
  
#Eksik günleri tekrar kontrol edelim.
apply(explanatory[,-1], 2, function(x) any(is.na(x)))