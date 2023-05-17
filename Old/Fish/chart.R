library(tidyverse)
library(alex) #custom/personal theme package
library(janitor)

fish = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv') %>% 
  clean_names()

fish = fish %>%
  filter(code == "OWID_WRL") %>%
  rename(overexploited = share_of_fish_stocks_that_are_overexploited,
         sustainable = share_of_fish_stocks_within_biologically_sustainable_levels_fao_2020) %>%
  #mutate(sustainable = -1*sustainable) %>%
  select(year, sustainable, overexploited) %>%
  pivot_longer(!year, names_to = "category", values_to = "share")

blank_years = c(2016,2014,2012,2010,2007,2005,2003,2002,2001,1999,1998,1996,1994,1993,1991,1988,1986,
                1984,1982,1980,1976,1975)
blank_years = cbind(blank_years, rep(0,22), rep(0,22))
colnames(blank_years) = c("year", "category", "share")
fish = rbind(blank_years, fish)

  ##Graph starts here
fishplot = fish %>%
  ggplot(aes(x = ifelse(category=="sustainable",yes=-share, no=share), 
             y = as.factor(year), 
             fill = category)
            ) +
  geom_col(show.legend = FALSE) + 
  scale_x_continuous(labels = abs, breaks = c(-91,0,34), limits = c(-92,50)) +
  scale_fill_manual(values = c("white", "#FC766AFF", "#5B84B1FF")) +
  theme_alex() + 
  xlab("Share of Fish Population") +
  ylab("") +
  geom_vline(xintercept=0, color = "white", linetype = "dashed")+
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title=element_text(hjust = 0),
    plot.subtitle=element_text(hjust = 0),
    plot.caption=element_text(hjust = 1)
  ) +
  annotate(geom="text",x = 40, y = "1974", color = "white", label = "1974") +
  annotate(geom="text",x = 40, y = "1980", color = "white", label = "1980") +
  annotate(geom="text",x = 40, y = "1990", color = "white", label = "1990") +
  annotate(geom="text",x = 40, y = "2000", color = "white", label = "2000") +
  annotate(geom="text",x = 40, y = "2010", color = "white", label = "2010") +
  annotate(geom="text",x = 40, y = "2017", color = "white", label = "2017") +
  annotate(geom="text",x =-40, y = "2002", color = "white", label = "Sustainable", size = 6) +
  annotate(geom="text",x = 20, y = "2002", color = "white", label = "Overexploited", size = 6) +
  labs(title = "Sustainabile Fishing over Time",
       subtitle = "Overexplotation of fish populations has increased over time",
       caption = "#TidyTuesday | Data: OurWorldInData.org \n @unstandarderror")

ggsave("TidyTuesday/Fish/fish.png", fishplot)