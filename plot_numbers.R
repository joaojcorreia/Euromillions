library(ggplot2)
library(gganimate)
library(ggthemes)

source('load_data.R')

# gganimate frequency barchart #


freq.table.numbers %>% 
pivot_longer()








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





