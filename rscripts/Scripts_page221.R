
# dplyr & odbc ile Veritabanı Bağlantısı #

#eğer sistemde aşağıdaki paketler yüklü değilse çalıştırılmalıdır.
install.packages(c("pool","odbc"))

#kütüphaneyi aktif hale getirelim.
library(dplyr)
library(odbc)

#odbc bağlantısının yapıbilmesi için öncelikle ODBC Data Source'a ilgili bağlantı bilgileri eklenmelidir.
con <- dbConnect(odbc::odbc(), "r2active")

parity <- tbl(con,"parity") %>%
  filter(asset_code == "EUR/USD",
         data_date >= "2015-02-01",
         data_date <= "2015-03-01") %>%
  select(data_date,asset_code,close) %>%
  collect()

currency <- tbl(con, "currency") %>%
  filter(asset_code == "EUR/USD") %>%
  select(asset_code, currency_code) %>%
  collect()

df <- left_join(parity, currency, by="asset_code")

df
