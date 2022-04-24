library(ggplot2)
library(gganimate)
library(ggthemes)

source('load_data.R')

# gganimate frequency barchart #


freq.table.numbers <- freq.table.numbers[,c(1,1130:1141)]


l.freq.table.numbers <- freq.table.numbers %>% 
              pivot_longer(!N, names_to = 'Date', values_to = 'Freq')

l.freq.table.numbers$Date <- as.Date(l.freq.table.numbers$Date)


l.freq.table.numbers %>% 
  ggplot() +
  facet_wrap(~Date)+
  aes(x=Freq, 
      y=reorder(N, Freq), 
      fill=N)+
  geom_bar(stat="identity", width=0.5)+
  geom_text(aes(label=N), vjust=+0.3 ,hjust=-0.3, size=3)+
  #coord_cartesian(xlim=c((min(Freq)-0.005),(max(Freq)+0.005)))+
  theme_fivethirtyeight()+
  theme(axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        legend.position="none"
  ) -> myplot

myplot+
  facet_null()+
  geom_text(x = 0.5 , y = 3,
            aes(label = as.character(Date)),
            size = 10, col = "grey18")+
  aes(group = N)+
  transition_time(Date) -> p


p <- animate(p, duration = 5, fps = 20, width = 1200, height = 800, renderer = gifski_renderer())

anim_save("out.gif",animation = p, renderer = gifski_renderer())










data.graph <- freq.table.numbers[,c(1,300)]

colnames(data.graph) <- c("N","D")

ggplot(data=data.graph, aes(x=D, 
                            y=reorder(N, D), 
                            fill=N))+
  geom_bar(stat="identity", width=0.5)+
  geom_text(aes(label=N), vjust=+0.3 ,hjust=-0.3, size=3)+
  coord_cartesian(xlim=c((min(data.graph$D)-0.005),(max(data.graph$D)+0.005)))+
  theme_fivethirtyeight()+
  theme(axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        legend.position="none"
  )





