
library(tseries)
library(PerformanceAnalytics)
start <- "2014-01-01"
end <- "2016-12-31"
provider <- "yahoo"
compression <- "d"
quote <- "AdjClose"

#Modelde kullanılacak asset'ler ile ilgili bilgileri tanımlayalım.
tickerList <- data.frame(ticker = c("msft","^gspc","^dji","^gdaxi","^n225","aapl"),
                         title = c("msft","sp500","dow","dax","n225","aapl"),
                         stringsAsFactors = F)

temp <- list()

for(i in 1:nrow(tickerList)){

  temp[[i]] <-  as.xts(get.hist.quote(instrument=tickerList$ticker[i],
                                      start=start, 
                                      end=end,
                                      quote=quote,
                                      provider=provider,
                                      compression=compression,
                                      quiet = TRUE))
  if(i == 1){
    Prices <- temp[[i]]
  } else {
    Prices <- merge(Prices,temp[[i]])
  }
}
rm(temp)

#Prices tablosunda kolon isimlerini güncelleyelim ve NA olan satırları silelim.
colnames(Prices) <- tickerList$title
Prices <- na.exclude(Prices)

#Unit-root testi kontrol edelim.
apply(Prices, 2, function(x) adf.test(x, k=12)$p.value)

#log getirileri hesap edelim ve Na değeri silelim.
Returns <- Return.calculate(Prices, method = "log")
Returns <- na.exclude(Returns)

#Unit-root testi kontrol edelim.
apply(Returns, 2, function(x) adf.test(x, k=12)$p.value)

#xts objesinden data.frame'e geçiş yapalım.
Returns <- as.data.frame(Returns)
Returns <- cbind(Date = as.Date(rownames(Returns)), Returns)
rownames(Returns) <- 1:nrow(Returns)

library(dplyr)
library(magrittr)

#Tarih sütununu çıkarttığımız datayı farklı bir isimle belirleyelim.
Returns_noDate <- Returns[-1]

#msft haricindeki açıklayıcı değişlenler için corelation tablosunu oluşturalım.
Return_correlation <- cor(Returns_noDate[-1])

#Açıklayıcı değişkenler arasında korelasyonu 0.4 ile 1 arasındaki assetler belirlendi.
selected_assets <- Return_correlation %>% 
  as_tibble %>% 
  mutate(asset1 = colnames(Return_correlation)) %>%
  tidyr::gather(key="asset2", value="cor", -asset1) %>% 
  mutate(assets = paste0(asset1,"_",asset2)) %>% 
  filter(abs(cor)<1 & abs(cor) >=0.5) %>%
  pull(assets) %>%
  strsplit(.,"_") %>% 
  unlist %>%
  unique

selected_assets
  
library(caret)
library(e1071)

#dow, sp500, dax ve aapl ==> PCA ile oluşturulan model
pca <- caret::preProcess(x = Returns_noDate[,selected_assets],
                         method = "pca", 
                         thresh = 0.99)

pca_set <- predict(pca, Returns_noDate[,selected_assets])

data <- data.frame(Returns[2], pca_set)

model <- lm(msft ~ ., data = data)
summary(model)
broom::glance(model)


#dow, sp500 ==> PCA + aapl ile oluşturulan model
pca <- caret::preProcess(x = Returns_noDate[,c("dow","sp500")],
                         method = "pca", 
                         thresh = 0.99)


pca_set <- predict(pca, Returns_noDate[,c("dow","sp500")])

data <- data.frame(Returns[2], pca_set, Returns[7])

model <- lm(msft ~ ., data = data)
summary(model)
broom::glance(model)





