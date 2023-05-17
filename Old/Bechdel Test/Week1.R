library(tidyverse)
raw_bechdel = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


movies$genre = str_replace(movies$genre, "Sci-Fi", "SciFi")

chart = movies %>% 
  filter(year > 1999) %>%
  select(genre, imdb_rating, binary) %>%
  separate(
    genre, c("genre1", "genre2", "genre3")
  ) %>%
  pivot_longer(
    cols = starts_with("genre"),
    names_to = "genre_#",
    names_prefix = "genre",
    values_to = "genre",
    values_drop_na = TRUE
  ) %>%
  group_by(genre, binary) %>%
  summarize(
    mean_score = mean(imdb_rating, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = genre, y = mean_score, fill = binary)) +
  geom_col(position = position_dodge(width = 0.6), alpha = 0.6) +
  theme_classic() + 
  ylab("Mean IMBD Score") + 
  xlab("Genre") + 
  labs(fill = "Bechdel Test Outcome",
       title = "The Bechdel Test (Movies Since 2000)",
       subtitle = "Mean IMDB Scores by Genre & Bechdel Test Outcome",
       caption = "Data: FiveThirtyEight\n@unstandarderror") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
 
ggsave("TidyTuesday/bechdel.png", chart)
