library(tidyverse)
library(ggplot2)
library(spotifyr)
library(plotly)
library(compmus)
europe <- get_playlist_audio_features("", "64wQFeHk3LgbPB2x6z3JNk")
asia <- get_playlist_audio_features("", "1YPj89O7XSAEwHSqQmquck")
americas <- get_playlist_audio_features("", "3col0xqk1N1zGenOzf5O34")




awards <-
  bind_rows(
    europe |> mutate(category = "Europe"),
    asia |> mutate(category = "Asia"),
    americas |> mutate(category = "Americas")
  )



cbPalette <- c("#8dd3c7",'#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')

barplotje <- awards %>% 
  ggplot(aes(x =tempo)) +
  geom_histogram(binwidth = 15, fill="darkgreen") +
  facet_wrap(~category) +
  ggtitle("Histogram of tempi per continent")+
  scale_fill_manual(values=cbPalette) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Tempo (Bpm)", y = "Count")


ggplotly(barplotje, tooltip = c('count'))


graveola <- get_tidy_audio_analysis("30xbueSacPQZrRR7ZD6sST")

graveola_plot <- graveola |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic() +
  ggtitle('The Dutch National Anthem')

ggplotly(graveola_plot)

saveRDS(object=graveola_plot, file ='Documents/Uva/Jaar 3/compmusic/data/grav_plot.RDS')

plot_tempogram <- readRDS(file = 'Documents/Uva/Jaar 3/compmusic/data/grav_plot.RDS')
