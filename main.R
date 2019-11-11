#################
# Ramen Ratings #
#################

#load packages
library(tidyverse)

#load data
df <- read.csv("C:/workspaceR/ramen_ratings/ramen-ratings.csv")
names(df) <- c("review", "brand", "variety", "style", "country", "stars", "top_ten")

#new column for whole number star
df$stars <- as.character(df$stars)
df$whole_stars <- str_sub(df$stars, 1, 1)

#create top_rank
df$top_ten <- as.character(df$top_ten)
df$top_rank <- sub(".*#", "", df$top_ten) #select text after #

#create top_year
df$top_year <- gsub( " .*$", "", df$top_ten) #select text before space

#Frequency tables (brand, style, country, whole_stars, top_rank, top_year)
ft_cats <- df %>% dplyr::select(brand, style, country, whole_stars, top_rank, top_year) #frequency table categories
(frequency_tables <- lapply(ft_cats, table)) #sort alphabetically
(frequency_tables_sorted <- lapply(frequency_tables, sort, decreasing = TRUE)) #sort value descending

(df_frequency_tables_sorted <- lapply(frequency_tables_sorted, as.data.frame)) #convert to dataframe
(names(df_frequency_tables_sorted) <- paste0("df_", names(frequency_tables))) #rename df names
df_frequency_tables_sorted

#plot countries where instant ramen is produced
df %>% count(country) %>% ggplot(aes(x=reorder(country,n),y=n)) + geom_bar(stat = "identity") + coord_flip() + 
  xlab("Countries") + ylab("Number of instant ramen produced") + 
  ggtitle("Countries where instant ramen is produced")  +
  theme(plot.title = element_text(hjust = 0.5))

#plot frequency of whole stars
df %>% count(whole_stars) %>% ggplot(aes(x=whole_stars,y=n)) + geom_bar(stat = "identity") +
  xlab("Whole stars") + ylab("Number") + ylim(0,1200) +
  ggtitle("Number of whole star ratings")  +
  theme(plot.title = element_text(hjust = 0.5))

#Style of packaging
df$stars <- as.numeric(df$stars)
df %>% group_by(style) %>% summarize(stars=mean(stars, na.rm = TRUE))

#Countries by star rating
df %>% ggplot(aes(x=reorder(country, desc(country)),y=stars)) + geom_boxplot() + coord_flip() +
  xlab("Country") + ylab("Stars") + 
  ggtitle("Boxplot of countries and star ratings") +
  theme(plot.title = element_text(hjust = 0.5))

#Packaging styles of the top 10 most produced brands by average stars
df %>% count(brand) %>% arrange(-n) %>% head(10) %>% left_join(df, by="brand") %>% group_by(brand,style) %>%
  summarize(stars=mean(stars)) %>% ggplot(aes(x=reorder(brand, desc(brand)), y=stars, fill=style)) + geom_bar(stat = "identity", position = "stack") +
  coord_flip() + xlab("Brand") + ylab("Mean stars") + labs(fill = "Style") + 
  ggtitle("Packaging styles of the top 10 most produced brands by average stars") +
  theme(plot.title = element_text(hjust = 0.5))
  

#competition winners
df$top_rank <- as.numeric(df$top_rank)
df$top_year <- as.numeric(df$top_year)
(competition_winners <- df %>% dplyr::filter(top_rank >=1) %>% arrange(top_rank, top_year))





