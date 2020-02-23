## Let's explore the wine ratings dataselt


## our favorite package 
library(tidyverse)

## load the data - thanks TidyTuesday!
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

## save the datafile
write.csv(wine_ratings, file = "WineRarings.csv",row.names=FALSE, na="")

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

## most rated wines by variety by country




## see mean scores by country
wine_ratings%>%filter(variety == 'Sauvignon Blanc' & !is.na(country)) %>% group_by(country) %>% 
  summarize(mean_score = mean(points, na.rm = T), n = n()) %>% 
  ungroup() %>% 
  arrange(country, desc(n)) %>% print(n=10000) %>% filter(n>30)

##  keep countries with n >30
wine_ratings%>%filter(variety == 'Sauvignon Blanc') %>% group_by(country) %>% filter(n() >30) %>% 
  group_by(country) %>% 
  ggplot(aes(x = reorder(country, points), y = points)) + geom_boxplot() +
  theme_bw()

## see prices
wine_ratings%>%filter(variety == 'Sauvignon Blanc') %>% group_by(country) %>% filter(n() >30) %>% 
  group_by(country) %>% 
  ggplot(aes(x = reorder(country, price), y = price)) + geom_boxplot()

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













