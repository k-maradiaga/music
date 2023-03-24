library(tidyverse)
library(spotifyr)
library(compmus)
library(plotly)
library(RColorBrewer)

#Chordogram------------------------------------------------------

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )

twenty_five <-
  get_tidy_audio_analysis("2Hplq9Qvud6B5PrNqUbxTV") |>
  compmus_align(sections, segments) |>
  select(sections) |>
  unnest(sections) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  )

twenty_five |> 
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "")

#-------------------------------------------------------------------------------------

europe <- get_playlist_audio_features("", "64wQFeHk3LgbPB2x6z3JNk")
asia <- get_playlist_audio_features("", "1YPj89O7XSAEwHSqQmquck")
americas <- get_playlist_audio_features("", "3col0xqk1N1zGenOzf5O34")


europe <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "64wQFeHk3LgbPB2x6z3JNk"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
asia <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "1YPj89O7XSAEwHSqQmquck"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
americas <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "3col0xqk1N1zGenOzf5O34"
  ) |>
  slice(1:30) |>
  add_audio_analysis()



jazz <-
  europe |>
  mutate(genre = "Europe") |>
  bind_rows(americas |> mutate(genre = "Americas"), asia |> mutate(genre = "Asia"))


jazz |>
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) |>
  unnest(sections) |>
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Continent",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )

ggplotly(jazzz)
#----------------------------------------------------

jazz |>
  mutate(
    timbre =
      map(
        segments,
        compmus_summarise,
        timbre,
        method = "mean"
      )
  ) |>
  select(genre, timbre) |>
  compmus_gather_timbre() |>
  ggplot(aes(x = basis, y = value, fill = genre)) +
  geom_violin() +
  scale_fill_viridis_d(direction = -1, option='C') +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Continent")


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
  ggplot(aes(x = key_name, fill=key_name)) +
  geom_bar() +
  facet_wrap(~category) +
  ggtitle("Histogram of Keys per continent")+
  scale_fill_manual(values=cbPalette) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Key name", y = "Count")


ggplotly(barplotje, tooltip = c('count'))


awards <-
  bind_rows(
    europe |> mutate(category = "Europe"),
    asia |> mutate(category = "Asia"),
    americas |> mutate(category = "Americas")
  )

theme_minimal(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
)

plotje <- awards |>                    # Start with awards.
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = mode,
      text = track.name
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  
  facet_wrap(~ category) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Pastel1"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp",            # Use an exp transformation to emphasise loud.
    guide = "none"            # Remove the legend for size.
  ) +
  theme_minimal()

# Use a simpler theme.
labs(                       # Make the titles nice.
  x = "Valence",
  y = "Energy",
  colour = "Mode"
)

ggplotly(plotje)

