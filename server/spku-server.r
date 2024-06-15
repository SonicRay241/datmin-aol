library(plotly)
library(dplyr)

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
            title = paste("Jakarta's Pollutant Level from", paste(
              format(date_range()[1], "%B %Y"),
              format(date_range()[2], "%B %Y"),
              sep = " - "
            )),
            hovermode = "x unified",
            xaxis = list(title = ""),
            yaxis = list(title = "Pollutant level (µg/m³)")
          )
      } else {
        monthly_data %>%
          subset(month > date_range()[1] & month < date_range()[2]) %>%
          plot_ly(
            x = ~month,
            y = ~ pollutant_selected() %>% tolower() %>% get(),
            name = "pm10",
            type = "scatter",
            mode = "lines"
          ) %>%
          layout(
            title = paste("Jakarta's Pollutant Level from", paste(
              format(date_range()[1], "%B %Y"),
              format(date_range()[2], "%B %Y"),
              sep = " - "
            )),
            hovermode = "x unified",
            xaxis = list(title = ""),
            yaxis = list(
              title = pollutant_selected() %>% paste("level (µg/m³)")
            )
          )
      }
    }

    if (station_selected() == "All (avg)") {
      get_avg_plot()
    } else {
      if (pollutant_selected() == "All") {
        monthly_data_station %>%
          subset(stasiun == station_selected()) %>%
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
            title = "Pollutants level in" %>% paste(station_selected()),
            hovermode = "x unified",
            xaxis = list(title = ""),
            yaxis = list(
              title = pollutant_selected() %>% paste("level (µg/m³)")
            )
          )
      } else {
        monthly_data_station %>%
          subset(stasiun == station_selected()) %>%
          plot_ly(
            x = ~month,
            y = ~ pollutant_selected() %>% tolower() %>% get(),
            type = "scatter",
            mode = "lines"
          ) %>%
          layout(
            title = pollutant_selected() %>%
              paste("level in") %>%
              paste(station_selected()),
            hovermode = "x unified",
            xaxis = list(
              title = ""
            ),
            yaxis = list(
              title = pollutant_selected() %>%
                paste("level (µg/m³)")
            )
          )
      }
    }
  })
}
