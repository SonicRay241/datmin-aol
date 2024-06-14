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

# Page Layout
page <- sidebarLayout(
  sidebarPanel(
    "",
    selectizeInput(
      "pollutant_input",
      label = "Pollutant",
      choices = select_list,
      multiple = FALSE,
      selected = "All",
      options = list(create = TRUE, placeholder = "Choose a pollutant")
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
