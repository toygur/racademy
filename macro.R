
library(tidyquant)
library(tsibble)
library(feasts)
library(magrittr)

macro_data <- readxl::read_xlsx("data/meta_data.xlsx") %>% 
  arrange(date) %>% 
  mutate(date = yearmonth(date)) %>% 
  gather(symbol, close, -date) %>% 
  as_tsibble(key = symbol, 
             index = date) %>% 
  group_by_key() %>% 
  fill(close, .direction = "downup")

macro_data %>%
  ggplot(aes(x = date, y = close)) +
  geom_line(aes(color = symbol), size = 0.9) +
  labs(title="", y="", x="Date") +
  facet_wrap(~symbol, ncol=2, scales = "free_y") +
  theme_tq()

#convert data.frame
macro_data_df <- macro_data %>%
  as.data.frame %>%
  spread(symbol,close) %>%
  column_to_rownames("date") %>%
  select(-period)
#convert XTS
macro_data_XTS <- as.xts(macro_data_df)

#Grafikler
GGally::ggpairs(macro_data %>% spread(symbol,close) %>% as_tibble() %>% select(-date)) 

macro_data %>% features(close, feature_set(tags = c("unitroot","portmanteau")))

macro_data %>% features(close, feat_stl)

macro_data %>% gg_season(close)




