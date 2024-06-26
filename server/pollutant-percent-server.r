library(plotly)
library(dplyr)

fn <- function(input, session, yearly_pollutant_percent) {
  pollutant_selected <- reactive({
    input$pollutant_input_2
  })

  date_range <- reactive({
    input$date_range_input_2
  })

  renderPlotly({
    if (pollutant_selected() == "All") {
      yearly_pollutant_percent %>%
        subset(tanggal >= date_range()[1] & tanggal <= date_range()[2]) %>%
        plot_ly(
          x = ~tanggal,
          y = ~pm10,
          type = "bar",
          name = "pm10",
          text = ~ floor(pm10)
        ) %>%
        add_trace(y = ~so2, name = "so2", text = ~ floor(so2)) %>%
        add_trace(y = ~co, name = "co", text = ~ floor(co)) %>%
        add_trace(y = ~o3, name = "o3", text = ~ floor(o3)) %>%
        add_trace(y = ~no2, name = "no2", text = ~ floor(no2)) %>%
        layout(
          xaxis = list(
            title = "",
            tickformat = "%Y",
            dtick = "M12",
            ticklabelmode = "instant"
          ),
          yaxis = list(title = "Percentage(%)"),
          barmode = "stack", title = "Jakarta's Yearly Pollutant Distribution"
        )
    } else {
      yearly_pollutant_percent %>%
        subset(tanggal >= date_range()[1] & tanggal <= date_range()[2]) %>%
        plot_ly(
          x = ~tanggal,
          y = ~ pollutant_selected() %>% tolower() %>% get(),
          type = "bar",
          name = pollutant_selected() %>% tolower(),
          text = ~ pollutant_selected() %>%
            tolower() %>%
            get() %>%
            floor()
        ) %>%
        layout(
          xaxis = list(
            title = "",
            tickformat = "%Y",
            dtick = "M12",
            ticklabelmode = "instant"
          ),
          yaxis = list(title = "Percentage(%)"),
          barmode = "stack",
          title = "Yearly"
          %>% paste(pollutant_selected())
          %>% paste("Percentage in Jakarta")
        )
    }
  })
}
