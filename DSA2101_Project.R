############################################################
##' DSA2101 Project
##' 
##' Authors: 
##' - Lian Kah Seng 
##' - Ng Siew Zhang
##' - Lee Wen Yang 
##' - Goh Xin Yi
##' - Yong Zhi Yi
############################################################

############################## 
#' 0 - Installing of Packages/Loading of libraries
############################## 
install.packages(c("tidyverse", "stringr", "lubridate"))
library(tidyverse)
library(stringr)
library(lubridate)

############################## 
#' 1 - Source file 
##############################
tuesdata <- tidytuesdayR::tt_load('2021-09-14')
billboard <- tuesdata$billboard
audio_features <- tuesdata$audio_features

############################## 
#' 2 - Helper Functions
##############################

##' Removes outer brackets, '[', ']' of `spotify_genres` column
remove_casing <- function(x){
  return(substr(x, 2, nchar(x)-1))
}

##' Merges specific genres into a general one
##' 
##' For example, 'k-pop' and 'j-pop' would be replaced by 'pop' instead
merge_big_genres <- function(x){
  if(is.na(x)){
    return(x)
  }
  if(str_detect(x, "pop") == TRUE){
    return("pop")
  }
  else if(str_detect(x, "rap") == TRUE){
    return("rap")
  }
  else if(str_detect(x, "rock") == TRUE){
    return("rock")
  }
  else if(str_detect(x, "jazz") == TRUE){
    return("jazz")
  }
  else if(str_detect(x, "metal") == TRUE){
    return("metal")
  }
  else if(str_detect(x, "disco") == TRUE){
    return("disco")
  }
  else if(str_detect(x, "hip hop") == TRUE){
    return("hip hop")
  }
  else if(str_detect(x, "contemporary") == TRUE){
    return("contemporary")
  }
  else if(str_detect(x, "blues") == TRUE){
    return("blues")
  }
  else if(str_detect(x, "soul") == TRUE){
    return("soul")
  }
  return(x)
}

##' Merges genres that involve two or more general genres into the 'fusion' genre
##' 
##' For example, 'pop rock' and 'jazz metal' would be replaced by 'fusion' instead
merge_into_fusion <- function(x){
  count = 0 
  if(is.na(x)){
    return(x)
  }
  if (str_detect(x, "fusion") == TRUE){
    return ("fusion")
  }
  types = c("pop | pop", "rap | rap", "rock | rock", "jazz | jazz", "metal | metal", "disco | disco", "edm | edm", "soul | soul")
  for(i in types){
    if(str_detect(x, i)==TRUE){
      count = count + 1
      if (count == 2){
        return("fusion")
      }
    }
  }
  return(x)
}
##' Normalises values in a vector
normalise_ms <- function(vector){
  min_m = min(vector, na.rm = TRUE)
  max_m = max(vector, na.rm = TRUE)
  vector_norm = ifelse(!is.na(vector), (vector-min_m)/(max_m-min_m), NA)
  return(vector_norm)
}

##' Summarises and obtain means of spotify audio_features from Dataframe
create_sum <- function(data){
  new <- data %>%
    select(-c(spotify_genre)) %>%
    distinct() %>%
    summarise(
      danceability = mean(danceability, na.rm = TRUE),
      energy = mean(energy, na.rm = TRUE),
      speechiness = mean(speechiness, na.rm = TRUE),
      acousticness = mean(acousticness, na.rm = TRUE),
      instrumentalness = mean(instrumentalness, na.rm = TRUE),
      liveness = mean(liveness, na.rm = TRUE),         
      valence = mean(valence, na.rm = TRUE),
      explicit = sum(spotify_track_explicit, na.rm = TRUE)/n(),
      loudness = mean(ifelse(!is.na(loudness), 10^(loudness/10), NA), na.rm = TRUE),        
      mode = mean(mode, na.rm = TRUE),
      duration = mean(duration_mins, na.rm = TRUE)
    )
  return(new)
}

##' Transposes the dataframes & then row binds
##' 
##' Takes in two summarised dataframes and their names 
create_butterfly_data <- function(right, left, right_name, left_name){
  right_t <- right %>%
    gather(key="features", value="value") %>%
    mutate(type = right_name)
  left_t <- left %>%
    gather(key="features", value="value") %>%
    mutate(type = left_name, value = - 1*value)
  butterfly_data <- rbind(right_t, left_t)
  return(butterfly_data)
}

############################## 
#' 3 - Cleaning dataset
##############################

##' Cleaning Audio Features
audio_clean <- audio_features %>%
  mutate(song_id = str_to_lower(song_id))

##' Dataframe that joins `billboard` & `audio_clean` tables together.
##' 
##' @description
##' * Column `spotify_genre` has been split such that each row only has one genre under itself 
##' * Contains normalised `spotify_track_duration_ms` column
##' * Addition of `year` column
merged <- billboard %>%
  
  #' Joining of tables `billboard` & `audio_clean`
  mutate(song_id = str_to_lower(song_id)) %>%
  left_join(audio_clean, by = "song_id", suffix = c("_billboard", "_audio")) %>%
  select(-c(url,performer_audio, spotify_track_id, song_audio, spotify_track_preview_url)) %>%
  mutate_at(vars(performer_billboard), str_to_lower) %>%
  
  #' Splitting of genres
  mutate_at(vars(spotify_genre), remove_casing) %>%         
  mutate(spotify_genre = strsplit(as.character(spotify_genre), ",")) %>%  
  unnest(spotify_genre) %>%                 
  mutate(spotify_genre = str_trim(gsub("'", "", spotify_genre))) %>%
  
  mutate_at(vars(week_id), mdy) %>%
  mutate(year = year(week_id),
         duration_mins = normalise_ms(spotify_track_duration_ms))

##' Dataframe that contains merged genres via the use of `merge_into_fusion()` and 
##'  `merge_big_genres()` functions
##'  
merged_genres <- merged %>%
  mutate(spotify_genre = lapply(spotify_genre, merge_into_fusion)) %>%
  mutate(spotify_genre = lapply(spotify_genre, merge_big_genres)) %>%
  mutate_at(vars(spotify_genre), as.character)

############################## 
#' 4 - Plots
##############################

############################## 
#' 4.1 - Density of Top 10 Genres Over the Years
##############################

##' Dataframe that contains the extraction of the top genres over the entire `merged_genres` dataset
top_genres <- merged_genres %>%
  group_by(spotify_genre) %>%
  summarize(n_songs = n()) %>%
  mutate(rank = rank(desc(n_songs))) %>%
  filter(rank <=10) %>%
  pull(spotify_genre)

##' Dataframe that contains spread of the top genres in `top_genres` over the years
top_overyears <- merged_genres %>%
  filter(spotify_genre %in% top_genres) %>%
  group_by(year, spotify_genre) %>%
  summarize(n_songs = n()) %>%
  arrange(year, desc(n_songs))

##' Creation of density plot
ggplot(top_overyears, aes(x = year, fill = spotify_genre)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ spotify_genre) +
  theme_minimal() +
  labs(
    title = "Density of Top 10 Genres Over the Years", 
    x = "Year", 
    y = "Density",
    fill = "Spotify Genre") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

############################## 
#' 4.2 - Comparing Song Characteristics from before and after 1991
##############################

##' Splitting of `merged` dataframe into two based on `year`
before_1991 <- merged %>%
  filter(year <= 1991)

after_1991 <- merged %>%
  filter(year > 1991)

butterfly_data <- create_butterfly_data(create_sum(after_1991), create_sum(before_1991), "after_1991", "before_1991")

##' Pull features in order of descending values (based on after_1991)
features_by_desc_values <- butterfly_data %>% filter(type == "after_1991") %>%
  arrange(value) %>% pull(features)

##' Creation of butterfly plot
ggplot(butterfly_data) + 
  geom_bar(
    aes(x = factor(features, level = features_by_desc_values), y = value, fill = type), 
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("after_1991" = "#ccf2f3", "before_1991" = "#f7e9cc")) + 
  geom_text(
    data = subset(butterfly_data, type == "after_1991"), 
    aes(
      x = features, 
      y = value, 
      label = round(abs(value),2)
    ), 
    size = 3, 
    vjust = .5, 
    hjust = -0.3
  ) +
  geom_text(
    data = subset(butterfly_data, type == "before_1991"), 
    aes(x = features, y = value, label = round(abs(value),2)), 
    size = 3, 
    vjust = .5, 
    hjust = 1.2
  ) +
  coord_flip(ylim = c(-1,1)) +
  scale_y_continuous(
    breaks = seq(-1,1,0.2), 
    labels = paste0(as.character(c(rev(seq(0,1,0.2)),seq(0.2,1,0.2)), 'm'))
  ) +
  theme_minimal() +
  labs(
    y = "value", 
    x = "song characteristics", 
    title = "Comparing Song Characteristics from before and after 1991"
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

############################## 
#' 4.3 - Average absolute change of song rankings each Month
##############################

##' Dataframe that selects only the necessary distinct rows from year 2011-2020
unique_songs <- merged %>%
  filter(year %in% c(2011:2020)) %>%
  select(c(year, week_id, song_id, week_position, peak_position)) %>%
  mutate(month = month(week_id, label = TRUE, abbr = TRUE), week = day(week_id)) %>%
  distinct() 

##' Dataframe with the average absolute change in song rankings and the respective year
ranking_change <- unique_songs %>%
  group_by(month) %>%
  group_by(song_id) %>%
  arrange(week_id) %>%
  arrange(month, song_id) %>%
  #' Lag the week_position to obtain previous week position of song ranking
  mutate(prev_week_pos = lag(week_position)) %>%
  na.omit() %>% 
  ungroup(month, song_id) %>%
  mutate(diff = abs(prev_week_pos-week_position)) %>%
  group_by(month) %>%
  summarise(abs_change = mean(sum(diff)/length(unique(song_id)), na.rm = TRUE))

##' Data frame with the total number of songs that stay in the top 100 billboards for less than 4 weeks per year
##' 
##' It is also joined with `ranking_change`
drop_out <- unique_songs %>% 
  group_by(month, song_id) %>%
  summarise(count = n()) %>%
  filter(count < 4) %>%
  group_by(month) %>%
  summarise(count_drop_out = n()) %>%
  full_join(ranking_change)

##' Dataframe that contains the join of `drop_out` and `ranking_change` and has a count of total number of unique songs
unique_songs_df <- unique_songs %>%
  group_by(month) %>%
  summarise(count_total = n()) %>%
  full_join(drop_out) %>%
  mutate(proportion_drop_out = 100*count_drop_out/count_total)

##' Obtaining statistics for plotting
mean_abs_change <-mean(unique_songs_df$abs_change)
sd_abs_change <- sd(unique_songs_df$abs_change)

##' Creation of Scatterplot
ggplot(unique_songs_df) +
  geom_point(aes(x = month, y = abs_change, size = count_total, color = proportion_drop_out)) +
  scale_color_continuous(trans = "reverse") +
  theme_classic() +
  labs(
    y = "Average absolute change of song rankings", 
    x = "Month", 
    title = "Average absolute change of song rankings each Month", 
    color = "Proportions of songs that \nstayed in the billboard \nfor < 4 weeks", 
    size = "Total number \nof unique songs"
  ) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
  scale_color_gradient(low = "#80b1d3", high = "#3a4180") +
  scale_y_continuous(breaks= seq(15,30,by=1)) +
  geom_hline(yintercept = mean_abs_change, linetype = 2) +
  annotate(
    'rect', 
    xmin = 0.3, 
    xmax = 12.7, 
    ymin = mean_abs_change - sd_abs_change, 
    ymax = mean_abs_change + sd_abs_change,
    alpha=.2, 
    fill='grey'
  ) +
  annotate(
    "text", 
    x = 12, 
    y = mean_abs_change - 0.6, 
    label = "Mean"
  ) +
  annotate(
    geom = "curve", 
    x = 1, 
    y = 27, 
    xend = 1.4, 
    yend = 24, 
    curvature = .3, 
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate("text", x = 2.4, y = 27.5, label="Within Standard Deviation")

############################## 
#' 4.4 - Comparing Song Characteristics from before and after 1991
##############################

##' Dataframe that selects only the necessary distinct rows from year 2011-2020 and contains 
##'  summarised values of `mean_valence`
billboard_valence_month <- merged %>%
  mutate(month = month(week_id, label = TRUE, abbr = TRUE)) %>%   
  select(year, month, song_billboard, valence) %>%
  distinct() %>%  filter(year %in% c(2011:2020)) %>%
  group_by(month, year) %>%  
  summarise(mean_valence= mean(valence, na.rm=TRUE),.groups='drop')

##' Creation of Boxplot
ggplot(billboard_valence_month, aes(x=month, y=mean_valence, fill=month)) +
  geom_boxplot() +   
  labs(x="", 
       y="Yearly Mean Valence",
       title ="Yearly Mean Valence of the Songs in 2011 to 2020, by Month") +
  scale_y_continuous(breaks = seq(0.42,0.58,by=0.02)) +  
  scale_fill_brewer(type="qual", palette = "Set3") +
  theme_classic() +  
  theme(legend.position = "none", plot.title = element_text(hjust=0.5))