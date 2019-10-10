library(tidyverse)
library(plotly)


avocado <- read_csv("avocado.csv")
glimpse(avocado)
summary(avocado)
str(avocado)
dim(avocado)

ggplot(avocado, aes(y = AveragePrice, x = Date, col = type)) + 
  scale_color_manual(values = c("#5ebe2d", "#F2E880")) +
  geom_point(alpha = 0.01) +
  facet_wrap(~year) +
  labs(y = "Average Price",
       x = "Date",
       title = "Average prices of avocados by year")
 # scale_color_manual(values = c("#5ebe2d", "#F2E880"))

length(unique(avocado$region))
length(unique(avocado$type))

unique(avocado$type)

colnames(avocado)
glimpse(avocado)

rownum(avocado)

# Average prices by date
avocado %>% 
group_by(Date) %>%
  summarise(meanpriced = mean(AveragePrice)) %>%
  ggplot(aes(x = Date, y = meanpriced)) +
  geom_point(col = "#4a7337") +
  geom_smooth(col = "#d9cd65", se = FALSE) + 
  labs(y = "Average price",
       title = "Average price by date") + 
  theme_bw()

# Total volumn by date
vol <- avocado %>%
  group_by(Date) %>%
  summarise(totalvold = sum(`Total Volume`)) %>%
  ggplot(aes(x = Date, y = totalvold)) +
  geom_point(col = "#4a7337") +
  geom_smooth(col = "#d9cd65", se = FALSE) + 
  labs(y = "Total volumn sold",
       title = "Total volumn sold by date") + 
  theme_bw() 

ggplotly(vol)
