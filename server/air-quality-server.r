library(plotly)
library(dplyr)

fn <- function(input, session, yearly_air_quality_percent) {
  air_quality_selected <- reactive({
    chartr(" ", "_", input$air_quality_input)
  })

  air_quality_selected_raw <- reactive({
    input$air_quality_input
  })

  date_range <- reactive({
    input$date_range_input_3
  })

  renderPlotly({
    if (air_quality_selected() == "All") {
      yearly_air_quality_percent %>%
        subset(tanggal >= date_range()[1] & tanggal <= date_range()[2]) %>%
        plot_ly(
          x = ~tanggal,
          y = ~Baik,
          type = "bar",
          name = "Baik",
          text = ~ floor(Baik)
        ) %>%
        add_trace(
          y = ~Sedang,
          name = "Sedang",
          text = ~ floor(Sedang)
        ) %>%
        add_trace(
          y = ~Tidak_Sehat,
          name = "Tidak Sehat",
          text = ~ floor(Tidak_Sehat)
        ) %>%
        add_trace(
          y = ~Sangat_Tidak_Sehat,
          name = "Sangat Tidak Sehat",
          text = ~ floor(Sangat_Tidak_Sehat)
        ) %>%
        add_trace(
          y = ~Berbahaya,
          name = "Berbahaya",
          text = ~ floor(Berbahaya)
        ) %>%
        layout(
          xaxis = list(
            title = "",
            tickformat = "%Y",
            dtick = "M12",
            ticklabelmode = "instant"
          ),
          yaxis = list(title = "Persentase(%)"),
          barmode = "stack",
          title = "Jakarta's Yearly AQI Rating Distribution"
        )
    } else {
      yearly_air_quality_percent %>%
        subset(tanggal >= date_range()[1] & tanggal <= date_range()[2]) %>%
        plot_ly(
          x = ~tanggal,
          y = ~ air_quality_selected() %>% get(),
          type = "bar",
          name = air_quality_selected_raw(),
          text = ~ air_quality_selected() %>%
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
          yaxis = list(title = "Persentase(%)"),
          barmode = "stack",
          title = "Yearly" %>%
            paste(air_quality_selected_raw()) %>%
            paste("Rating")
        )
    }
  })
}
