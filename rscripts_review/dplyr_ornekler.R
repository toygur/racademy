
#Örnek 1:
#Grup bazında en uzun kişi ile aradaki farkı alarak gözlem sayısı 1'den fazla olanları listeler.
starwars %>%
  select(homeworld,name,height) %>%
  group_by(homeworld) %>%
  mutate(max_height = max(height),
         fark = max_height - height,
         obs = n()) %>% 
  filter(obs > 1) %>% 
  select(-obs, -max_height) %>% 
  arrange(homeworld, desc(fark))

#Örnek 2:
##Yöntem 1: iki ayrı datayı ayrı ayrı db'den alarak birleştirme:
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

##Yöntem 2: iki ayrı datayı birleştirerek db'den alma:
parity_currency <- left_join(x = (tbl(con,"parity") %>% 
								  filter(asset_code == "EUR/USD",
										 data_date >= "2015-02-01",
										 data_date <= "2015-03-01") %>% 
								  select(data_date,asset_code,close)),
							   y = tbl(con, "currency") %>%
								 filter(asset_code == "EUR/USD") %>%
								 select(asset_code, currency_code),
							   by = "asset_code") %>% 
					collect()
