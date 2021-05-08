####################
### Load packages ##
####################


library(httr)
library(curl)
library(jsonlite)
library(tidyverse)

################################################
### api key for imdb-api website: k_tg0u0849 ###
################################################

#######################################################################################################
# The following codes are used to acquire data and thus the final run in 4.24, the data was collected.#
#######################################################################################################

url_250movies <- "https://imdb-api.com/en/API/Top250Movies/k_tg0u0849" # link for list of top 250 movies

recipe_250movies <- url_250movies %>% fromJSON()

print(recipe_250movies$errorMessage)  # if there is error or not

url_popumovies <- "https://imdb-api.com/en/API/MostPopularMovies/k_tg0u0849"

recipe_popumovies <- url_popumovies %>% fromJSON() # link for list of popular movies

print(recipe_popumovies$errorMessage) # if there is error or not
df_popumovies <- as_tibble(recipe_popumovies$items)

#######################################################################################################
# The information for tv is also collected but no description or genres are included hence discarded. #
#######################################################################################################

#### tv ####
url_250tv <- "https://imdb-api.com/en/API/Top250TVs/k_tg0u0849"
recipe_250tv <- url_250tv %>% fromJSON()
print(recipe_250tv$errorMessage)
df_250tv <- as_tibble(recipe_250tv$items)

url_poputv <- "https://imdb-api.com/en/API/MostPopularTVs/k_tg0u0849"
recipe_poputv <- url_poputv %>% fromJSON()
print(recipe_poputv$errorMessage)
df_poputv <- as_tibble(recipe_poputv$items)
# no error

################################################
###   Store information into tibbles         ###
################################################

df_250movies <- as_tibble(recipe_250movies$items)
head(df_250movies)

as.numeric(df_250movies$imDbRatingCount) %>% summary()
rate_vs_count_250m <- as.data.frame(cbind(as.numeric(df_250movies$imDbRating),as.numeric(df_250movies$imDbRatingCount))
)

 
ggplot(rate_vs_count_250m, aes(x = V2, y = V1)) +
  geom_point() + xlim(c(25000, 2500000)) +
  xlab("Counts") + ylab("Rates")


as.numeric(df_popumovies$imDbRatingCount) %>% summary()
rate_vs_count_popum <- as.data.frame(cbind(as.numeric(df_popumovies$imDbRating),as.numeric(df_popumovies$imDbRatingCount))
)


ggplot(rate_vs_count_popum, aes(x = V2, y = V1)) +
  geom_point() + xlim(c(0, 2500000)) +
  xlab("Counts") + ylab("Rates")
df_popumovies %>% select(rank, rankUpDown, title, year, imDbRating, imDbRatingCount) %>% 
  filter(imDbRatingCount == "2379513") 
# The list of popular movies may alter, so we final make it as a dataset in 4.24
# df_popumovies <- as_tibble(recipe_popumovies$items)


# We could look at specific movie
inception_url <- "https://imdb-api.com/en/API/SearchAll/k_tg0u0849/Inception"

inception <- inception_url %>% fromJSON()
inception$results

# Also, we could look at some wikipedia information
try_wiki <- "https://imdb-api.com/en/API/Wikipedia/k_tg0u0849/tt1375666"

wiki <- try_wiki %>% fromJSON()
wiki$plotShort$plainText



##############
## read loaded data set
#############

extra_df = read.csv("IMDb movies.csv")
df_250m_ex = as_tibble(df_250movies) %>% select(id, rank, title, fullTitle, year, crew, imDbRating, imDbRatingCount) %>% 
  mutate("genre"=0,"duration"=0,"country"=0,"description"=0)

for (i in 1:length(extra_df$imdb_title_id)){
  for (j in 1:250){
    if (extra_df$imdb_title_id[i] == df_250m_ex$id[j]){
      df_250m_ex$genre[j] = extra_df$genre[i]
      df_250m_ex$duration[j] = extra_df$duration[i]
      df_250m_ex$country[j] = extra_df$country[i]
      df_250m_ex$description[j] = extra_df$description[i]
    }
  }
}

df_250m_ex %>% select(id, rank, title, year, genre, duration, country, description) %>% 
  filter(genre == 0 | duration == 0 | country == 0| description == 0)
df_250m_ex %>% select(id, rank, title, year) %>% filter(year>2019)
## Soul and Zack Snyder's Justice League are quite new, the make up dataset does not contain the records.



library(data.table)
# create new genres for top 250
df_250m_ex %>% count(genre, sort = T)
genres_df <- df_250m_ex %>% select(genre, imDbRating)
genres_df$Action = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Action")1 else 0)

genres_df$Adventure = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Adventure")1 else 0)
genres_df$Animation = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Animation")1 else 0)

genres_df$Biography = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Biography")1 else 0)
genres_df$Comedy = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Comedy")1 else 0)
genres_df$Crime = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Crime")1 else 0)
genres_df$Drama = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Drama")1 else 0)
genres_df$Family = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Family")1 else 0)
genres_df$Fantasy = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Fantasy")1 else 0)

genres_df$FilmNoir = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Film-Noir")1 else 0)
genres_df$History = sapply(1:250, function(x) if (genres_df[x, 1] %like% "History")1 else 0)
genres_df$Horror = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Horror")1 else 0)
genres_df$Musical = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Musical")1 else 0)
genres_df$Mystery = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Mystery")1 else 0)
genres_df$Romance = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Romance")1 else 0)
genres_df$SciFi = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Sci-Fi")1 else 0)
genres_df$Sport = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Sport")1 else 0)
genres_df$Thriller = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Thriller")1 else 0)
genres_df$War = sapply(1:250, function(x) if (genres_df[x, 1] %like% "War")1 else 0)
genres_df$Western = sapply(1:250, function(x) if (genres_df[x, 1] %like% "Western")1 else 0)

# plot mean maximum and minimum separately
means <- rep(0,20)
for (i in 1:20) {
  means[i] <- mean(as.numeric(genres_df$imDbRating[genres_df[i+2]==1]))
}
# plot the means
ge = c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Drama", "Family", "Fantasy", "FilmNoir", 
       "History", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Sport", "Thriller", "War", "Western")
barplot(means, main = "Average IMDb rating for different genres")

plotdf = as_tibble(means) %>% mutate("genre"=0)
plotdf$genre = ge
plotdf %>% mutate("maxi" = 0) 
plotdf %>% mutate("mini" = 0)
ma <- rep(0,20)
for (i in 1:20) {
  ma[i] <- max(as.numeric(genres_df$imDbRating[genres_df[i+2]==1]))
}
mi <- rep(0,20)
for (i in 1:20) {
  mi[i] <- min(as.numeric(genres_df$imDbRating[genres_df[i+2]==1]))
}

plotdf$maxi = ma
plotdf$mini = mi
plotdf %>% ggplot(aes(genre, means))+ geom_bar(stat = "identity", width = 0.5, fill = "tomato3") +
  labs(tittle = "Average IMDb rating for different genres") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


plotdf %>% ggplot(aes(genre, maxi))+ geom_bar(stat = "identity", width = 0.5, fill = "tomato3") +
  labs(tittle = "Maximum IMDb rating for different genres") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

plotdf %>% ggplot(aes(genre, mini))+ geom_bar(stat = "identity", width = 0.5, fill = "tomato3") +
  labs(tittle = "Minimum IMDb rating for different genres") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

#######################
## Genre popular 100 ##
#######################

df_popu_ex <- left_join(df_popumovies,extra_df,by = c("id" = "imdb_title_id"))%>% drop_na()

df_popu_ex %>% count(genre, sort = T)
dim(df_popu_ex)

# create new genres for popular
genres_df <- df_popu_ex %>% select(genre, imDbRating)
genres_df$Action = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Action")1 else 0)

genres_df$Adventure = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Adventure")1 else 0)

genres_df$Biography = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Biography")1 else 0)
genres_df$Comedy = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Comedy")1 else 0)
genres_df$Crime = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Crime")1 else 0)
genres_df$Drama = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Drama")1 else 0)
genres_df$Family = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Family")1 else 0)
genres_df$Fantasy = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Fantasy")1 else 0)

genres_df$Horror = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Horror")1 else 0)

genres_df$Mystery = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Mystery")1 else 0)
genres_df$Romance = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Romance")1 else 0)
genres_df$SciFi = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Sci-Fi")1 else 0)
genres_df$Sport = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Sport")1 else 0)
genres_df$Thriller = sapply(1:41, function(x) if (genres_df[x, 1] %like% "Thriller")1 else 0)
genres_df$War = sapply(1:41, function(x) if (genres_df[x, 1] %like% "War")1 else 0)


means1 <- rep(0,15)
for (i in 1:15) {
  means1[i] <- mean(as.numeric(genres_df$imDbRating[genres_df[i+2]==1]))
}
# plot the means
ge1 = c("Action", "Adventure", "Biography", "Comedy", "Crime", "Drama", "Family", "Fantasy", 
       "Horror", "Mystery", "Romance", "SciFi", "Sport", "Thriller", "War")
barplot(means1, main = "Average IMDb rating for different genres")

# plot the maximum and minimum
plotdf1 = as_tibble(means1) %>% mutate("genre"=0)
plotdf1$genre = ge1
plotdf1 %>% mutate("maxi" = 0) 
plotdf1 %>% mutate("mini" = 0)
ma1 <- rep(0,15)
for (i in 1:15) {
  ma1[i] <- max(as.numeric(genres_df$imDbRating[genres_df[i+2]==1]))
}
mi1 <- rep(0,15)
for (i in 1:15) {
  mi1[i] <- min(as.numeric(genres_df$imDbRating[genres_df[i+2]==1]))
}

plotdf1$maxi = ma1
plotdf1$mini = mi1
plotdf1 %>% ggplot(aes(genre, means1))+ geom_bar(stat = "identity", width = 0.5, fill = "tomato3") +
  labs(tittle = "Average IMDb rating for different genres") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


plotdf1 %>% ggplot(aes(genre, maxi))+ geom_bar(stat = "identity", width = 0.5, fill = "tomato3") +
  labs(tittle = "Maximum IMDb rating for different genres") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

plotdf1 %>% ggplot(aes(genre, mini))+ geom_bar(stat = "identity", width = 0.5, fill = "tomato3") +
  labs(tittle = "Minimum IMDb rating for different genres") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
