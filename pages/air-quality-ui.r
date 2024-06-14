select_list <- c(
  "All",
  "Baik",
  "Sedang",
  "Tidak Sehat",
  "Sangat Tidak Sehat",
  "Berbahaya"
)

page <- sidebarLayout(
  sidebarPanel(
    "",
    selectizeInput(
      "air_quality_input",
      label = "Air Quality",
      choices = select_list,
      multiple = FALSE,
      selected = "All",
      options = list(create = TRUE, placeholder = "Filter air quality...")
    ),
    dateRangeInputMonth(
      "date_range_input_3",
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
    plotlyOutput("air_quality_percent_graph")
  )
)
