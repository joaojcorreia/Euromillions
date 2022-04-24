library(tidyverse)
library(xlsx)

#Load data and generate frequency tables #

results <- read.xlsx('Results.xlsx', 1, encoding="UTF-8")

total.draws <- nrow(results)



#function to select part of the data set#

part_results <- function(a){
  
  results.numbers <<- results[-(1:(a-1)),1:6]
  results.stars <<- cbind(date = results[-(1:(a-1)),1], results[-(1:(a-1)),7:8])
  
}



                        
#function to determine the frequency of the numbers#  

freq_numbers <- function(){

  draws <- nrow(results.numbers)
    
      count.numbers <-   as.data.frame( 
            merge(
              table(results.numbers$N1), 
              table(results.numbers$N2),
              by.x ="Var1",
              by.y ="Var1",
              all=TRUE
            ) %>% 
              merge(
                table(results.numbers$N3),
                by.x ="Var1",
                by.y ="Var1",
                all=TRUE
            ) %>% 
              merge(
                table(results.numbers$N4),
                by.x ="Var1",
                by.y ="Var1",
                all=TRUE
              ) %>% 
              merge(
                table(results.numbers$N5),
                by.x ="Var1",
                by.y ="Var1",
                all=TRUE
              )
        ) 
        
      count.numbers[is.na(count.numbers)] <- 0
      
      colnames(count.numbers) <- c("N", "F1", "F2", "F3", "F4", "F5")
      
      count.numbers$Tot <-  count.numbers$F1 +
        count.numbers$F2 +
        count.numbers$F3 +
        count.numbers$F4 +
        count.numbers$F5
      
      count.numbers <- merge(data.frame(N = as.factor(1:50)), count.numbers, by.x="N", by.y = "N", all=TRUE)
      count.numbers[is.na(count.numbers)] <- 0
      
      freq.numbers  <- count.numbers[,2:7]/draws
      
      freq.numbers  <- cbind( N = as.factor(1:50), freq.numbers)
      
      freq.numbers$Tot
}

# Create date frequency table #

freq_table_numbers <- function(){
  
  freq.table.numbers <<- data.frame(N = as.factor(1:50))
  
  for (a in total.draws:1){
  
    part_results(a)
    date.draw <- results.numbers[1,1]
    freq.table.numbers <<- bind_cols(freq.table.numbers,C = freq_numbers())
    colnames(freq.table.numbers)[which(names(freq.table.numbers) == "C")] <<- as.character(date.draw)

  }
  
  
  
}


