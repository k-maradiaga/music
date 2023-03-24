library(tidyverse)
library(compmus)
library(ggplot2)

americas_chroma <-
  get_tidy_audio_analysis("3col0xqk1N1zGenOzf5O34") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

asia_chroma <-
  get_tidy_audio_analysis("1YPj89O7XSAEwHSqQmquck") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)


europe_chroma <-
  get_tidy_audio_analysis("50Ex1WYMkbe4mwdroVdLFP") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

europe_chroma |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()
