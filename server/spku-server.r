library(plotly)
library(dplyr)

pollutant_select_list <- c(
  "All",
  "PM10",
  "SO2",
  "CO",
  "O3",
  "NO2"
)

pollutant_select_list_2 <- c(
  "PM10",
  "SO2",
  "CO",
  "O3",
  "NO2"
)

fn <- function(input, session, monthly_data, monthly_data_station) {
  pollutant_selected <- reactive({
    input$pollutant_input
  })

  date_range <- reactive({
    input$date_range_input
  })

  station_selected <- reactive({
    input$station_input
  })

  observe({
    if (input$station_input == "All (avg)") {
      # monthly_data <- monthly_data[monthly_data$stasiun == station_selected()]
      updateSelectizeInput(
        session,
        "pollutant_input",
        choices = pollutant_select_list,
        selected = "All",
        server = TRUE
      )
    } else {
      updateSelectizeInput(
        session,
        "pollutant_input",
        choices = pollutant_select_list_2,
        selected = "PM10",
        server = TRUE
      )
    }
  })

  renderPlotly({
    get_avg_plot <- function() {
      if (pollutant_selected() == "All") {
        monthly_data %>%
          subset(month > date_range()[1] & month < date_range()[2]) %>%
          plot_ly(
            x = ~month,
            y = ~pm10,
            name = "pm10",
            type = "scatter",
            mode = "lines"
          ) %>%
          add_trace(y = ~so2, name = "so2") %>%
          add_trace(y = ~co, name = "co") %>%
          add_trace(y = ~o3, name = "o3") %>%
          add_trace(y = ~no2, name = "no2") %>%
          layout(
            title = paste("Data SPKU Jakarta", paste(
              format(date_range()[1], "%B %Y"),
              format(date_range()[2], "%B %Y"),
              sep = " - "
            )),
            hovermode = "x unified",
            xaxis = list(title = ""),
            yaxis = list(title = "pollutant level (µg/m³)")
          )
      } else {
        monthly_data %>%
          subset(month > date_range()[1] & month < date_range()[2]) %>%
          plot_ly(
            x = ~month,
            y = ~get(tolower(pollutant_selected())),
            name = "pm10",
            type = "scatter",
            mode = "lines"
          ) %>%
          layout(
            title = paste("Data SPKU Jakarta", paste(
              format(date_range()[1], "%B %Y"),
              format(date_range()[2], "%B %Y"),
              sep = " - "
            )),
            hovermode = "x unified",
            xaxis = list(title = ""),
            yaxis = list(title = paste(pollutant_selected(), "level (µg/m³)"))
          )
      }
    }

    if (station_selected() == "All (avg)") {
      get_avg_plot()
    } else {
      if (pollutant_selected() == "All") {
        # Fallback if something fails, this should not be able to be selected
        monthly_data_station %>%
          plot_ly(
            x = ~month,
            y = ~pm10,
            color = ~stasiun,
            type = "scatter",
            mode = "lines"
          ) %>%
          layout(
            title = " Data SPKU Jakarta 2011-2023",
            hovermode = "x unified"
          )
      } else {
        monthly_data_station %>%
          subset(stasiun == station_selected()) %>%
          plot_ly(
            x = ~month,
            y = ~get(tolower(pollutant_selected())),
            type = "scatter",
            mode = "lines"
          ) %>%
          layout(
            title = paste(pollutant_selected(), "level in") %>% paste(station_selected()),
            hovermode = "x unified",
            xaxis = list(title = ""),
            yaxis = list(title = paste(pollutant_selected(), "level (µg/m³)"))
          )
      }
    }
  })
}
