library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(ggridges)
library(ggplot2)
library(tidyverse)

# Load data
df <- read_csv("../data/raw/spotify_songs.csv") |>
  distinct(track_id, .keep_all = TRUE) |>
  mutate(duration_s = round(duration_ms/1000, 1))

