library(tidyverse)
library(alex)
library(png)
library(ggimage)

bgurl_long = "https://raw.githubusercontent.com/alexkinkelaar/TidyTuesday/main/Pumpkin/bg.png"

pumpkins = 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv') %>%
  select(id,weight_lbs,place) %>%
  rename(weight = weight_lbs) %>%
  separate(id, c("year", "type"), sep = "-") %>%
  filter(type == "F" | type == "P" | type == "S") %>%
  mutate(category = ifelse(type=="F","Field Pumpkin",
                           ifelse(type=="P","Giant Pumpkin","Giant Squash")),
         weight=parse_number(weight),
         place=parse_number(place)) %>%
  filter(place<=100) %>%
  select(-type)

plot = pumpkins %>%
  ggplot(aes(x = year, y = weight, color = category)) + 
  geom_jitter(alpha = 0.8) + 
  scale_color_manual(values = c("#5E32BA","#EB6123", "#96C457")) +
  scale_y_continuous(breaks = c(1000,2000)) +
  theme_alex() + 
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
    plot.caption = element_text(margin = margin(5,0,0,0,unit="cm")),
    legend.margin = margin(c(0,0,0,-30), "cm"),
    legend.text = element_text(size = 10),
    legend.justification = "left",
  ) +
  labs(x = "", 
       y = "Weight (Lbs)", 
       title = "The Great Pumpkin",
       subtitle = "Size of Top Contest Pumpkins",
       caption = "Weight of Top 100 Pumpkins\nData from Great Pumpkin Council\n@unstandarderror | #TidyTuesday")
  
final = ggbackground(plot,bgurl_long)

plot.save <- function(plot, 
                      width = 800, 
                      height = 500, 
                      text.factor = 1, 
                      filename = paste0(
                        format(
                          Sys.time(), 
                          format = '%Y%m%d-%H%M%S'), '-Rplot.png'
                      )
) {
  
  dpi <- text.factor * 100
  width.calc <- width / dpi
  height.calc <- height / dpi
  
  ggsave(filename = filename,
         dpi = dpi,
         width = width.calc,
         height = height.calc,
         units = 'in',
         plot = plot)
}


plot.save(final, width = 1080, height = 1920, text.factor = 2.5)


