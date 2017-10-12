library(car)
library(ggplot2)
library(tidyverse)
data("Angell")
str(Angell)

summary(Angell)

ggplot(data = Angell, aes(x=moral)) + geom_histogram()
ggplot(data = Angell, aes(x=hetero)) + geom_histogram()
ggplot(data = Angell, aes(x=mobility)) + geom_histogram()

ggplot(data = Angell, aes(x=moral)) + geom_density()
ggplot(data = Angell, aes(x=hetero)) + geom_density()
ggplot(data = Angell, aes(x=mobility)) + geom_density()

ggplot(data = Angell, aes(sample = moral)) + stat_qq()
ggplot(data = Angell, aes(sample = hetero)) + stat_qq()
ggplot(data = Angell, aes(sample = mobility)) + stat_qq()

boxplot(Angell)
ggplot(data = Angell, aes(x=region, y=moral)) + geom_boxplot()
ggplot(data = Angell, aes(x=region, y=hetero)) + geom_boxplot()
ggplot(data = Angell, aes(x=region, y=mobility)) + geom_boxplot()

pairs(Angell)
