library(tidyverse)
library(lubridate)
library(wesanderson)
games = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')
theme_set(theme_void())

shortenednames =  c("Civ VI", "Civ V", "Civ: Beyond Earth")
gamename = c("Sid Meier's Civilization VI","Sid Meier's Civilization V", "Sid Meier's Civilization: Beyond Earth")
key = as.data.frame(cbind(shortenednames, gamename))

civgames = games %>%
  filter(gamename == "Sid Meier's Civilization VI" |
           gamename == "Sid Meier's Civilization V" | 
           gamename == "Sid Meier's Civilization: Beyond Earth"
  )
civgames = merge(civgames, key, by = "gamename")

format_si <- function(...) {
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "K",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}

chart = civgames %>% 
  unite("date", month, year, sep = ", ", remove = FALSE) %>%
  mutate(date = parse_date_time(date, orders = "my")) %>%
  ggplot(aes(x = date,  y = avg, color = shortenednames)) +
    geom_smooth(se = FALSE, size = 0.75, linetype = "dashed") +
    geom_line(size = 1.5, alpha = 0.5) +
    geom_text(data = . %>%
              filter(month == "January" & year == 2014),
              y = 15000,
              label = "10/14 \n Civ: Beyond Earth \n Released",
              color = "#9C964A", fontface = "italic", size = 3)+
    geom_text(data = . %>%
              filter(month == "December" & year == 2017),
              y = 65000,
              label = "10/16 \n Civ: VI Released",
              color = "#F4B5BD", fontface = "italic", size = 3)+
    scale_color_manual(values = wes_palette("Moonrise3")) +
    xlab("Month, Year") + 
    ylab("Average Plays") +
    labs(
      title = "Monthly Average Players",
      subtitle = "      For the three most recent games in the \n      Civilization franchise",
      caption = "Data: SteamCharts \n @unstandarderror"
    ) + 
    scale_y_continuous(labels=format_si())+
    theme(plot.caption = element_text(face = "italic", size = 10, color = "white"),
          plot.title = element_text(face = "bold", size = 14, color = "white"),
          plot.subtitle = element_text(face = "italic", size = 11, color = "white"),
          plot.background = element_rect(fill = "grey12", color = NA),
          legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "white", size = 9),
          axis.title.y = element_text(color = "white", size = 11, angle = 90),
          axis.title.x = element_text(color = "white", size = 11),
          axis.ticks.x = element_line(color = "white"),
          axis.text.y = element_text(color = "white", size = 9),
          axis.ticks.y = element_line(color = "white"),
          axis.line.x = element_line(color = "white"),
          axis.line.y = element_line(color = "white"),
          legend.text = element_text(color = "white"),
          legend.title = element_blank())

ggsave("TidyTuesday/Steam.png", chart)
