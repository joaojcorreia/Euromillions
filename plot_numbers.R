library(ggplot2)
library(gganimate)
library(ggthemes)
library(tidytext)

source('load_data.R')

# prepare data - pivot longer and create rank column #


freq.table.numbers.2 <- freq.table.numbers


freq.table.numbers.2 <- freq.table.numbers.2 %>% 
              pivot_longer(!N, names_to = 'Date', values_to = 'Freq')

freq.table.numbers.2$Date <- as.Date(freq.table.numbers.2$Date)

freq.table.numbers.2 <- freq.table.numbers.2 %>%
  group_by(Date) %>%
  arrange(Date, desc(Freq)) %>%
  mutate(ranking = row_number())


# load logo png #
 logo <- png::readPNG('logo euromillions.png')

# create animation # 

animation <- freq.table.numbers.2 %>%
  ggplot() +
  geom_col(aes(ranking, Freq, fill = N))+
  geom_text(aes(ranking, y=(Freq) , label = N), vjust=+0.2, hjust=-0.8, size = 3) + 
  coord_flip()+ 
  scale_x_reverse() +
  theme_minimal()+ 
  theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(1, 2, 1, 2, "cm"),
    plot.title = element_text(size = 20, colour = "darkgray")
  ) +
  labs(title = "{closest_state}")+
  #annotation_custom("logo", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1) +
  transition_states(Date, state_length = 0, transition_length = 1) +
  enter_grow() +
  view_follow()+
  ease_aes('linear') 

p <- animate(animation, duration = 120, fps = 20, width =1200, height = 800,  renderer = gifski_renderer())

anim_save("out.gif",animation = p, renderer = gifski_renderer())













