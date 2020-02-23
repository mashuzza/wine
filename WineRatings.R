library(readr)
library(tidyverse)


wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

write.csv(wine_ratings, file = "WineRarings.csv",row.names=FALSE, na="")

head(wine_ratings)
str(wine_ratings)
summary(wine_ratings)


countries<-wine_ratings%>% count(country) %>% arrange(desc(n)) %>% top_n(10) 
ggplot(data = countries, aes(x = reorder(country,-n),y = n)) + geom_bar(stat="identity") 

varieties<-wine_ratings%>% count(variety) %>% arrange(desc(n)) %>% top_n(100) %>% print(n=100)
ggplot(data = varieties, aes(x = reorder(varieties,-n),y = n)) + 
  geom_bar(stat="identity")  +
  theme_bw()


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













