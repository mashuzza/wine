## Let's explore the wine ratings dataset

## our favorite package 
library(tidyverse)

## load the data - thanks TidyTuesday!
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

## save the datafile
write.csv(wine_ratings, file = "WineRatings.csv",row.names=FALSE, na="")

# #initial quick stats
head(wine_ratings)
str(wine_ratings)

summary(wine_ratings)

## Top 10 countries represented in the dataset
countries<-wine_ratings%>% count(country) %>% arrange(desc(n)) %>% top_n(10) 

ggplot(data = countries, aes(x = reorder(country,-n), y = n)) + 
  geom_bar(stat="identity", col = "black", fill = "darkgray") +
  labs(title = "# of rated wines by country", 
       x = '',
       y = '') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # rotate the labels on x axis

## most rated wines by variety
varieties<-wine_ratings %>% count(variety) %>% arrange(desc(n)) %>% top_n(20)
ggplot(data = varieties, aes(x = reorder(variety,-n), y = n)) + 
  geom_bar(stat="identity", col = "black", fill = "darkgray")  +
  labs(title = "Top 20 Most Rated Wines", 
       x = '',
       y = '') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## see mean scores for Sauvignon Blanc by country
wine_ratings%>%filter(variety == 'Sauvignon Blanc' & !is.na(country)) %>% group_by(country) %>% 
  summarize(mean_score = mean(points, na.rm = T), n = n()) %>% 
  ungroup() %>% 
  arrange(country, desc(n)) %>% print(n=10000) %>% filter(n>30)

##  keep countries with n >30
wine_ratings%>%filter(variety == 'Sauvignon Blanc') %>% group_by(country) %>% filter(n() >30) %>% 
  group_by(country) %>% 
  ggplot(aes(x = reorder(country, points), y = points)) + geom_boxplot() +
  labs(title = "Sauvignon Blanc Raitings by Country", 
       subtitle = 'Based on 30+ ratings',
       x = '',
       y = '') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## see prices
wine_ratings%>%filter(variety == 'Sauvignon Blanc') %>% group_by(country) %>% filter(n() >30) %>% 
  group_by(country) %>% 
  ggplot(aes(x = country, y = price)) + geom_boxplot() +
  labs(title = "Sauvignon Blanc Prices by Country", 
       subtitle = 'Based on 30+ prices',
       x = '',
       y = '') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## price vs rating by country

wine_ratings%>%filter(variety == 'Sauvignon Blanc') %>% group_by(country) %>% filter(n() >30) %>% 
  group_by(country) %>% 
  ggplot(aes(x = factor(points), y = price)) + geom_boxplot() + coord_flip()+facet_wrap(.~country)



wine_ratings%>%filter(variety == 'Sauvignon Blanc') %>% group_by(country) %>% filter(n() >30) %>% 
  group_by(country) %>% 
  ggplot(aes(x = reorder(country, points), y = points)) + geom_violin() +
  theme_bw()

## let's look at the vintage for sauvignon blank

wine_ratings%>%filter(variety == 'Sauvignon Blanc') %>% select(title) %>%
  mutate(Vintage = parse_number(title), Vintage2 = parse_integer(title)) %>% print(n=39) 

## price sanity check

wine_ratings %>% select(title, points, price) %>%arrange(desc(price)) %>% top_n(20) 

## Most expensive wine looks suspicious due to 88 points!


wine_ratings %>% select(title, points, price) %>%arrange(desc(price)) %>% top_n(20) 

## Most expensive wine looks suspicious due to 88 points!

plot(wine_ratings$points, wine_ratings$price)

ggplot(data = wine_ratings, aes(x = points, y = price)) +
  geom_point(shape = 21,col = "black", fill = 'darkgray', alpha = 0.7) +
  ##geom_jitter(width = 0.25, alpha = 0.5) +
  labs(title = "Do wines with higher ratings cost more?", 
       ##subtitle = 'Based on 30+ ratings',
       x = 'Points',
       y = 'Price') +
  theme_bw() 
  








