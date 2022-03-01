library(tidyverse)
library(xlsx)

results <- read.xlsx('Results.xlsx', 1, encoding="UTF-8")

results.numbers <- results[,1:6]
results.stars <- cbind(date = results[,1], results[,7:8])

#recordar#

b <- data.frame(table(results.numbers$N2))
a <- data.frame(table(results.numbers$N1))

freq.numbers <- full_join(data.frame(table(results.numbers$N1)),
                          data.frame(table(results.numbers$N2)),
                          data.frame(table(results.numbers$N3)),
                          data.frame(table(results.numbers$N4)),
                          data.frame(table(results.numbers$N5)),
                          by = "Var1")