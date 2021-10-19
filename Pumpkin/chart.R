library(tidyverse)
library(alex)

pumpkins = 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv') %>%
  select(id,weight_lbs) %>%
  rename(weight = weight_lbs) %>%
  separate(id, c("year", "type"), sep = "-") %>%
  filter(type == "F" | type == "P" | type == "S") %>%
  mutate(category = ifelse(type=="F","Field Pumpkin",
                           ifelse(type=="P","Giant Pumpkin","Giant Squash"))) %>%
  select(-type)

pumpkins %>%
  ggplot(aes(x = year, y = weight, fill = category)) + 
    geom_point(alpha = 0.3)