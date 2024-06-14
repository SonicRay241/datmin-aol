select_list <- c(
  "All",
  "PM10",
  "SO2",
  "CO",
  "O3",
  "NO2"
)

page <- sidebarLayout(
  sidebarPanel(
    "",
    selectizeInput(
      "pollutant_input_2",
      label = "Pollutant",
      choices = select_list,
      multiple = FALSE,
      selected = "All",
      options = list(create = TRUE, placeholder = "Choose a pollutant")
    ),
    dateRangeInputMonth(
      "date_range_input_2",
      label = "Date Range",
      start = "2011-01",
      end = "2023-01",
      min = "2011-01",
      max = "2023-01",
      startview = "years",
      minview = "years",
      maxview = "decades",
      format = "yyyy"
    ),
  ),
  mainPanel(
    plotlyOutput("pollutant_percent_graph")
  )
)
