install.packages("spotifyr")
install.packages("knitr")
install.packages("tidyverse")
install.packages("magrittr")

library("spotifyr")
library("knitr")
library("tidyverse")
library("magrittr")

# clientID: 49cd99b9a6304469a6be740fb835f263
  # clientSecret: 4f311bda63224956bca6b3753cabf844

Sys.setenv(SPOTIFY_CLIENT_ID='49cd99b9a6304469a6be740fb835f263')
Sys.setenv(SPOTIFY_CLIENT_SECRET='4f311bda63224956bca6b3753cabf844')

# Spotify_ClientID = '49cd99b9a6304469a6be740fb835f263'
# Spotify_ClientSecret = '4f311bda63224956bca6b3753cabf844'


access_token <- get_spotify_access_token()

ArianaGrande <- spotifyr::get_artist_audio_features('Ariana Grande')

glimpse(ArianaGrande)
summary(ArianaGrande)


# 1 

dim(ArianaGrande)
colnames(ArianaGrande)


# 2

# 6 albums
# Yours Truly (LatAm Version) is second longest
# plot below:

ArianaGrande %>%
  group_by(album_name) %>%
  mutate(albumchars=nchar(album_name)) %>%
  summarise(albumlength=mean(albumchars)) %>%
  arrange(desc(albumlength))

plot_title <- ArianaGrande %>%
  group_by(album_name) %>%
  mutate(albumchars=nchar(album_name)) %>%
  summarise(albumlength=mean(albumchars)) %>%
  arrange(desc(albumlength))

head(plot_title)
ggplot(plot_title,aes(x=album_name,y=albumlength))+geom_bar(stat = "identity")


# 3

# My Everything has 18 tracks

ArianaGrande %>%
  group_by(album_name) %>%
  summarise(tracks=length(track_name)) %>%
  arrange(desc(tracks))

# 4 

# Average duration is relatively similar accross album. Standard deviation is just .53 minutes with mean of 3.4

plot_avg_song_duration <- ArianaGrande %>%
  group_by(album_name) %>%
  summarise(track_duration=mean(duration_ms/1000/60)) %>%
  arrange(desc(track_duration))

ggplot(plot_avg_song_duration,aes(x=album_name,y=track_duration))+geom_bar(stat = "identity")

mean(ArianaGrande$duration_ms/60/1000)
mean(plot_avg_song_duration$track_duration)
sd(plot_avg_song_duration$track_duration)



# 5

# There doesn't seemto be any correlation between release time and album popularity

ggplot(ArianaGrande, aes(x=ArianaGrande$album_release_date,y=ArianaGrande$album_popularity))+geom_point()



# 6

popularity <-
  ArianaGrande %>%
group_by(album_name) %>%
  mutate(least_pop=min(track_popularity),
         most_pop=max(track_popularity))
  
  



ggplot(popularity, aes(x=popularity$least_pop,y=popularity$album_popularity))+geom_point()
ggplot(popularity, aes(x=popularity$most_pop,y=popularity$album_popularity))+geom_point()

attach(popularity)
cor(least_pop,album_popularity)
cor(most_pop,album_popularity)












