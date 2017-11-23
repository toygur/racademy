
#RStudio'da yeni özellik olan "Connections" modülünü etkin kullanabilmek için öncelikle "odbc" ve "pool" kütüphanesi kurulmalıdır.
install.packages(c("dplyr","DBI","odbc","pool"))

#Başlangıç:
library(DBI)
library(dplyr)
con <- dbConnect(odbc::odbc(), "r2active")

#DB'den tablo görüntüleme:
tbl(con, "parity") %>% filter(asset_code == 'EUR/TRY')

#Belirli bir formata göre data çekme:
parity_db <- tbl(con, "parity") %>% filter(asset_code == 'EUR/TRY') %>% collect()

#Left-Join Örnek:
parity_db <- tbl(con, "parity") %>% filter(asset_code == 'EUR/TRY')
currency_db <-  tbl(con, "currency") %>% filter(asset_code == 'EUR/TRY')

merged_db <- left_join(parity_db,currency_db, by="asset_code") %>% collect()

#Daha fazlası için: http://db.rstudio.com/