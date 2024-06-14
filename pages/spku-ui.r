# Selections fo input
select_list <- c(
  "All",
  "PM10",
  "SO2",
  "CO",
  "O3",
  "NO2"
)
stations <- c(
  "All (avg)",
  "DKI1 (Bunderan HI)",
  "DKI2 (Kelapa Gading)",
  "DKI3 (Jagakarsa)",
  "DKI4 (Lubang Buaya)",
  "DKI5 (Kebon Jeruk)"
)

# Code from https://gist.github.com/micstr/49641c2767765bf0d0be716f6634a89e
dateRangeInputMonth <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateRangeInput(inputId, label, ...)
  d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
  d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

# Page Layout
page <- sidebarLayout(
  sidebarPanel(
    "",
    selectizeInput(
      "particle_input",
      label = "Particle",
      choices = select_list,
      multiple = FALSE,
      selected = "All",
      options = list(create = TRUE, placeholder = "Choose the particle")
    ),
    dateRangeInputMonth(
      "date_range_input",
      label = "Date Range",
      start = "2011-01",
      end = "2023-12",
      min = "2011-01",
      max = "2023-12",
      startview = "year",
      minview = "months",
      maxview = "years",
      format = "yyyy-mm"
    ),
    selectizeInput(
      "station_input",
      label = "Station Data",
      choices = stations,
      multiple = FALSE,
      selected = "All (avg)",
      options = list(create = TRUE, placeholder = "Choose the station")
    )
  ),
  mainPanel(
    plotlyOutput("spku_graph")
  )
)
