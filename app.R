library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# Data
df <- read.csv("data/raw/spotify_songs.csv", stringsAsFactors = FALSE)
df <- df[!duplicated(df$track_id), ]
df$duration_s <- round(df$duration_ms / 1000, 1)

# UI
ui <- page_fluid(
  
  theme = bs_theme(bootswatch = "flatly"),
  
  # Header
  div(
    div(
      h1("🎵 Spotifind", class = "mb-0 fs-3"),
      p("Spotify Song Explorer · TidyTuesday Dataset", class = "mb-0 opacity-75 small"),
    ),
    tags$span("v0.2.0", class = "badge bg-light text-dark"),
    class = "bg-primary text-white p-4 d-flex justify-content-between align-items-center mb-3"
  ),
  
  layout_sidebar(
    
    # Sidebar
    sidebar = sidebar(
      width = 260,
      open = "desktop",
      h5("Filter Controls"),
      hr(),
      accordion(
        accordion_panel(
          "Audio Features",
          sliderInput("danceability", "Danceability", min = 0.0, max = 1.0, value = c(0.0, 1.0), step = 0.01),
          sliderInput("energy",       "Energy",       min = 0.0, max = 1.0, value = c(0.0, 1.0), step = 0.01),
          sliderInput("valence",      "Valence (Mood)", min = 0.0, max = 1.0, value = c(0.0, 1.0), step = 0.01),
          sliderInput("acousticness", "Acousticness", min = 0.0, max = 1.0, value = c(0.0, 1.0), step = 0.01),
        ),
        accordion_panel(
          "Track Properties",
          sliderInput("tempo",      "Tempo (BPM)",          min = 0,   max = 250, value = c(0, 250),   step = 1),
          sliderInput("duration_s", "Duration (seconds)",   min = 0,   max = 600, value = c(0, 600),   step = 1),
          sliderInput("popularity", "Popularity (0–100)",   min = 0,   max = 100, value = c(0, 100),   step = 1),
          selectInput(
            "genre_filter", "Genre",
            choices  = c("All", sort(unique(na.omit(df$playlist_genre)))),
            selected = "All"
          ),
        ),
        open = "Audio Features",
      ),
      hr(),
      actionButton("reset_all", "Reset Filters", class = "btn-outline-secondary btn-sm w-100"),
    ),
    
    # KPI Value Boxes
    layout_columns(
      value_box(
        title    = "Songs Found",
        value    = textOutput("kpi_count"),
        showcase = tags$span("🎵"),
        theme    = "primary",
      ),
      value_box(
        title    = "Avg Energy",
        value    = textOutput("kpi_energy"),
        showcase = tags$span("⚡"),
        theme    = "success",
      ),
      value_box(
        title    = "Avg Danceability",
        value    = textOutput("kpi_dance"),
        showcase = tags$span("🕺"),
        theme    = "info",
      ),
      col_widths = c(4, 4, 4),
    ),
    
    # Mood Map
    card(
      card_header("Mood Map — Valence vs Energy"),
      plotOutput("plot_mood_map", height = "400px"),
      full_screen = TRUE,
    ),
    
    # Results Table + Top Genres
    layout_columns(
      card(
        card_header("Results Table"),
        tableOutput("tbl_results")
        full_screen = TRUE,
      ),
      card(
        card_header("Top Genres"),
        tableOutput("tbl_top_genre")
      ),
      col_widths = c(8, 4),
    ),
    
    # Footer
    hr(),
    p(
      HTML(
        "Spotifind | Data: TidyTuesday Spotify Songs | ",
        "Authors: Rahiq Raees, Nguyen Nguyen, Shuhang Li, Jose Davila | ",
        "<a href='https://github.com/UBC-MDS/DSCI-532_2026_37_Spotifind' target='_blank'>GitHub Repo</a> | ",
        "Last updated: February 2026"
      ),
      style = "color: grey; font-size: 0.8em; text-align: center;"
    ),
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_df <- reactive({
    data <- df
    data <- data[
      data$danceability  >= input$danceability[1]  & data$danceability  <= input$danceability[2]  &
        data$energy        >= input$energy[1]        & data$energy        <= input$energy[2]        &
        data$valence       >= input$valence[1]       & data$valence       <= input$valence[2]       &
        data$acousticness  >= input$acousticness[1]  & data$acousticness  <= input$acousticness[2]  &
        data$tempo         >= input$tempo[1]         & data$tempo         <= input$tempo[2]         &
        data$duration_s    >= input$duration_s[1]    & data$duration_s    <= input$duration_s[2]    &
        data$track_popularity >= input$popularity[1] & data$track_popularity <= input$popularity[2],
    ]
    if (input$genre_filter != "All") {
      data <- data[data$playlist_genre == input$genre_filter, ]
    }
    data
  })
  
  observeEvent(input$reset_all, {
    updateSliderInput(session, "danceability", value = c(0.0, 1.0))
    updateSliderInput(session, "energy",       value = c(0.0, 1.0))
    updateSliderInput(session, "valence",      value = c(0.0, 1.0))
    updateSliderInput(session, "acousticness", value = c(0.0, 1.0))
    updateSliderInput(session, "tempo",        value = c(0, 250))
    updateSliderInput(session, "duration_s",   value = c(0, 600))
    updateSliderInput(session, "popularity",   value = c(0, 100))
    updateSelectInput(session, "genre_filter", selected = "All")
  })
  
  output$kpi_count <- renderText({
    formatC(nrow(filtered_df()), format = "d", big.mark = ",") |> paste("songs")
  })
  
  output$kpi_energy <- renderText({
    data <- filtered_df()
    if (nrow(data) == 0) return("—")
    sprintf("%.2f / 1.0", mean(data$energy, na.rm = TRUE))
  })
  
  output$kpi_dance <- renderText({
    data <- filtered_df()
    if (nrow(data) == 0) return("—")
    sprintf("%.2f / 1.0", mean(data$danceability, na.rm = TRUE))
  })
  
  output$plot_mood_map <- renderPlot({
    data <- filtered_df()
    
    if (nrow(data) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No songs match filters", size = 6) +
        xlim(0, 1) + ylim(0, 1) + theme_void()
    } else {
      set.seed(42)
      sample_data <- data[sample(nrow(data), min(500, nrow(data))), ]
      
      ggplot(sample_data, aes(x = valence, y = energy, color = danceability)) +
        # Quadrant backgrounds
        annotate("rect", xmin = 0,   xmax = 0.5, ymin = 0.5, ymax = 1,   fill = "#c0d9f5", alpha = 0.35) +
        annotate("rect", xmin = 0.5, xmax = 1,   ymin = 0.5, ymax = 1,   fill = "#f5e6c0", alpha = 0.35) +
        annotate("rect", xmin = 0,   xmax = 0.5, ymin = 0,   ymax = 0.5, fill = "#d4c0f5", alpha = 0.35) +
        annotate("rect", xmin = 0.5, xmax = 1,   ymin = 0,   ymax = 0.5, fill = "#c0f5d0", alpha = 0.35) +
        # Divider lines
        geom_hline(yintercept = 0.5, color = "#555555", linewidth = 1.2, linetype = "dashed", alpha = 0.8) +
        geom_vline(xintercept = 0.5, color = "#555555", linewidth = 1.2, linetype = "dashed", alpha = 0.8) +
        # Points
        geom_point(alpha = 0.6, size = 2, shape = 16) +
        scale_color_viridis_c(name = "Danceability") +
        # Quadrant labels
        annotate("text", x = 0.02, y = 0.99, label = "Sad & Intense",   hjust = 0, vjust = 1, fontface = "bold", size = 4, color = "#2a5fa5", alpha = 0.85) +
        annotate("text", x = 0.52, y = 0.99, label = "Happy & Intense", hjust = 0, vjust = 1, fontface = "bold", size = 4, color = "#a57a2a", alpha = 0.85) +
        annotate("text", x = 0.02, y = 0.01, label = "Sad & Calm",      hjust = 0, vjust = 0, fontface = "bold", size = 4, color = "#6a2aa5", alpha = 0.85) +
        annotate("text", x = 0.52, y = 0.01, label = "Happy & Calm",    hjust = 0, vjust = 0, fontface = "bold", size = 4, color = "#2aa55a", alpha = 0.85) +
        labs(
          title = paste0("Mood Map  —  ", formatC(nrow(data), format = "d", big.mark = ","), " songs"),
          x     = "Valence (Sadness → Happiness)",
          y     = "Energy (Calm → Intense)"
        ) +
        xlim(0, 1) + ylim(0, 1) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(size = 13))
    }
  })
  
  output$tbl_results <- renderTable({
    filtered_df() |>
      select(
        Song       = track_name,
        Artist     = track_artist,
        Album      = track_album_name,
        Released   = track_album_release_date,
        Genre      = playlist_genre,
        Popularity = track_popularity
      ) |>
      arrange(desc(Popularity))
  })
  
    # Row indices (1-based) of high-popularity songs
    high_pop <- which(data$Popularity >= 70)
    
    datatable(
      data,
      rownames   = FALSE,
      options    = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
      class      = "display compact"
    ) |>
      formatStyle(
        "Popularity",
        target     = "row",
        backgroundColor = styleInterval(69.99, c("white", "#d4edda"))
      )
  })
  
output$tbl_top_genre <- renderTable({
  data <- filtered_df()
  if (nrow(data) == 0) return(data.frame(Genre = character(0), Count = integer(0)))
  data |>
    count(Genre = playlist_genre, name = "Count") |>
    arrange(desc(Count)) |>
    head(6)
})

shinyApp(ui, server)