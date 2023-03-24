library(tidyverse)
library(spotifyr)

world <- get_playlist_audio_features("", "6praQPbDYhgWqp1YIk9SVc")
europe <- get_playlist_audio_features("", "6praQPbDYhgWqp1YIk9SVc")
north_america <- get_playlist_audio_features("", "6praQPbDYhgWqp1YIk9SVc")
south_america <- get_playlist_audio_features("", "6praQPbDYhgWqp1YIk9SVc")
asia <- get_playlist_audio_features("", "6praQPbDYhgWqp1YIk9SVc")

world <- world %>%
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major"))

ecm |>
  summarise(
    mean_speechiness = mean(loudness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )

grammy <- get_playlist_audio_features("", "4kQovkgBZTd8h2HCM3fF31")
edison <- get_playlist_audio_features("", "37i9dQZF1DX8mnKbIkppDf")

awards <-
  bind_rows(
    grammy |> mutate(category = "Grammys"),
    edison |> mutate(category = "Edisons")
  )

awards2 <-
  bind_rows(
    world |> mutate(category = "world")
  )

grammy |> ggplot(aes(x = energy)) + geom_histogram(binwidth = 0.1)

world |> ggplot(aes(x = tempo)) + geom_histogram(binwidth = 15)

world |> ggplot(aes(x = liveness)) + geom_histogram(binwidth = 0.07) + 
  ggtitle("Liveness")

world |> ggplot(aes(x = instrumentalness)) + geom_histogram(binwidth = 0.1) + ggtitle("Instrumentalness")

ggplot(world, aes(x=energy, y=acousticness, color=as.factor(mode))) + geom_point(aes(size=loudness)) + 
  
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp"           # Use an exp transformation to emphasise loud.
           # Remove the legend for size.
  ) +
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    minor_breaks = NULL
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Energy",
    y = "Accousticness",
    colour = "Mode"
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Dark2"        # Name of the palette is 'Paired'.
  ) +
  geom_rug(linewidth = 0.1) +
  geom_text(aes(label=ifelse(acousticness<0.75,as.character(acousticness),'')),hjust=0.5,vjust=2) +
  geom_text(aes(label=ifelse(loudness>-7,as.character("Russian"),'')), color = "black",hjust=0.5,vjust=4) +
  geom_text(aes(label=ifelse(energy>0.5,as.character(track.name),'')), position=position_jitter(height=0.06), hjust=0,vjust=0) +
  ggtitle("Acousticness vs Energy") 

