library(car)
library(ggplot2)
library(tidyverse)
data("Sahlins")

str(Sahlins)
# a
ggplot(data=Sahlins, mapping = aes(x=consumers, y=acres)) + geom_point()
# b
df <- Sahlins
df$group <- 0
df[1:7,]$group <- 1
df[8:13,]$group <- 2
df[14:20,]$group <- 3
dfMeans <- df %>% group_by(group) %>% summarise(count = n(),
                                                consumers = mean(consumers),
                                                acres = mean(acres))
ggplot() +
  geom_point(data=Sahlins, mapping = aes(x=consumers, y=acres)) +
  geom_point(data = dfMeans, mapping = aes(x=consumers, y=acres),
             color="darkred", size = I(5))
ggplot() +
  geom_point(data=df, mapping = aes(x=consumers, y=acres, color=group)) +
  geom_line(data = dfMeans, mapping = aes(x=consumers, y=acres),
             color="darkred", size = I(2))
# remove outliers
df2 <- df %>% filter((acres!=3.09 & group == 1) |
                       (acres!=1.31 & group == 2) |
                       (group == 3))
df2Means <- df2 %>% group_by(group) %>% summarise(count = n(),
                                                consumers = mean(consumers),
                                                acres = mean(acres))
ggplot() +
  geom_point(data=df2, mapping = aes(x=consumers, y=acres, color=group)) +
  geom_line(data = df2Means, mapping = aes(x=consumers, y=acres),
            color="darkred", size = I(2))