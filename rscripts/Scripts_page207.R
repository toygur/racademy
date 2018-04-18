
# dplyr/ mutate & case_when Örnek #

library(dplyr)

starwars %>%
  mutate(bmi = mass/((height/100)^2),
         bmi_category = case_when(
           bmi < 18.5 ~ "Underweight",
           bmi >= 18.5 & bmi < 25 ~ "Normal",
           bmi >= 25 & bmi < 30 ~ "Overweight",
           TRUE ~ "Obesity"
         )) %>%
  select(name:mass, contains("bmi")) 
