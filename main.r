library(shiny)
library(bslib)

library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(ggplot2)

library(ggiraph)
library(plotly)
library(zoo)
# Load environment
readRenviron(".env")
path <- Sys.getenv("PROJECT_PATH")

get_page <- function(name) {
  source(file.path(paste(path, "/pages", sep = ""), name))$value
}

get_server <- function(name) {
  source(file.path(paste(path, "/server", sep = ""), name))$value
}

read_data <- function(name) {
  data_path <- paste(path, "/data", sep = "")
  read.csv(file.path(paste(data_path), name))
}

# Insert Data
data11 <- read_data("SPKU11.csv")
data12 <- read_data("SPKU12.csv")
data13 <- read_data("SPKU13.csv")
data14 <- read_data("SPKU14.csv")
data15 <- read_data("SPKU15.csv")
data16 <- read_data("SPKU16.csv")
data17 <- read_data("SPKU17.csv")
data18 <- read_data("SPKU18.csv")
data19 <- read_data("SPKU19.csv")
data20 <- read_data("SPKU20.csv")
data21 <- read_data("SPKU21.csv")
data22 <- read_data("SPKU22.csv")
data23 <- read_data("SPKU23.csv")

# Renaming Columns
colnames(data21) <- c("periode_data", "tanggal", "stasiun", "pm10", "pm25", "so2", "co", "o3", "no2", "max", "critical", "categori")
colnames(data22) <- c("periode_data", "tanggal", "pm10", "pm25", "so2", "co", "o3", "no2", "max", "critical", "categori", "stasiun")
data22 <- data22[, c("periode_data", "tanggal", "stasiun", "pm10", "pm25", "so2", "co", "o3", "no2", "max", "critical", "categori")]
colnames(data23) <- c("periode_data", "tanggal", "stasiun", "pm10", "pm25", "so2", "co", "o3", "no2", "max", "critical", "categori")
data21$pm25 <- NULL
data22$pm25 <- NULL
data23$pm25 <- NULL

# Combine all df into one
start_data <- rbind(data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21, data22, data23)
rm(data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21, data22, data23)

# Data Cleaning
# Fix categorical data naming
unique(start_data$stasiun)

from <- c("DKI4", "DKI2", "DKI5", "DKI1", "DKI3", "DKI5 Kebon Jeruk Jakarta Barat", "DKI1 Bunderan HI", "DKI2 Kelapa Gading", "DKI3 Jagakarsa", "DKI4 Lubang Buaya", "DKI5 Kebon Jeruk", "DKI5 (Kebon Jeruk) Jakarta Barat")
to <- c("DKI4 (Lubang Buaya)", "DKI2 (Kelapa Gading)", "DKI5 (Kebon Jeruk)", "DKI1 (Bunderan HI)", "DKI3 (Jagakarsa)", "DKI5 (Kebon Jeruk)", "DKI1 (Bunderan HI)", "DKI2 (Kelapa Gading)", "DKI3 (Jagakarsa)", "DKI4 (Lubang Buaya)", "DKI5 (Kebon Jeruk)", "DKI5 (Kebon Jeruk)")
convert <- data.frame(from, to)

for (from in convert$from) {
  start_data <- start_data %>%
    mutate(
      stasiun = as.character(stasiun),
      stasiun = if_else(
        stasiun == from,
        convert$to[convert$from == from],
        stasiun
      ),
      stasiun = as.factor(stasiun)
    )
}
start_data <- subset(start_data, grepl("[a-zA-Z]", stasiun))
start_data$stasiun <- droplevels(start_data$stasiun)

unique(start_data$stasiun)
rm(from, to, convert)

# Convert columns to numeric
data <- start_data %>%
  mutate(
    tanggal = parse_date_time(tanggal, orders = c("ymd", "dmy")),
    pm10 = as.numeric(pm10),
    so2 = as.numeric(so2),
    co = as.numeric(co),
    o3 = as.numeric(o3),
    no2 = as.numeric(no2),
    max = as.numeric(max),
  )

# Check the NA values of each columns. pm10 = 2035, s02 = 1656, co = 1494, 03 = 1725, no2 = 1655, max = 1096.
colSums(is.na(data))
colSums(is.na(data) / nrow(data) * 100)

# Using interpolation to replace NA values
data <- data %>%
  group_by(stasiun) %>%
  mutate(
    pm10 = na.approx(pm10, na.rm = FALSE),
    so2 = na.approx(so2, na.rm = FALSE),
    co = na.approx(co, na.rm = FALSE),
    o3 = na.approx(o3, na.rm = FALSE),
    no2 = na.approx(no2, na.rm = FALSE)
  ) %>%
  ungroup()

# replace values of "max" column with the max value of pm10, so2, co, o3, and no2, from the row.
data <- data %>%
  rowwise() %>%
  mutate(
    max = max(c(pm10, so2, co, o3, no2), na.rm = TRUE)
  ) %>%
  ungroup()

# Replace values in "critical" column with the column name of the max value in each row
data <- data %>%
  rowwise() %>%
  mutate(
    critical = {
      values <- c(pm10, so2, co, o3, no2)
      col_names <- c("pm10", "so2", "co", "o3", "no2")
      if (all(is.na(values))) {
        NA_character_
      } else {
        col_names[which.max(values)]
      }
    }
  ) %>%
  ungroup()

# Update "categori" column based on the "max" values
data <- data %>%
  mutate(
    categori =
      case_when(
        max > 0 & max <= 50 ~ "Baik",
        max > 50 & max <= 100 ~ "Sedang",
        max > 100 & max <= 200 ~ "Tidak Sehat",
        max > 200 & max <= 300 ~ "Sangat Tidak Sehat",
        max > 300 ~ "Berbahaya",
        TRUE ~ categori
      )
  )

# Drop Missing Values=
colSums(is.na(data))
data <- na.omit(data)
colSums(is.na(data))
lapply(data, unique)


monthly_data <- data %>%
  group_by(month = floor_date(tanggal, "month")) %>%
  summarise(
    pm10 = mean(pm10, na.rm = TRUE),
    so2 = mean(so2, na.rm = TRUE),
    co = mean(co, na.rm = TRUE),
    o3 = mean(o3, na.rm = TRUE),
    no2 = mean(no2, na.rm = TRUE),
    .groups = "drop"
  )
na.omit(monthly_data)

# test plot 2
monthly_data_station <- data %>%
  group_by(stasiun, month = floor_date(tanggal, "month")) %>%
  summarise(
    pm10 = mean(pm10, na.rm = TRUE),
    so2 = mean(so2, na.rm = TRUE),
    co = mean(co, na.rm = TRUE),
    o3 = mean(o3, na.rm = TRUE),
    no2 = mean(no2, na.rm = TRUE),
    .groups = "drop"
  )
plot <- pm10app %>%
  plot_ly(
    x = ~month,
    y = ~pm10,
    color = ~stasiun,
    type = "scatter",
    mode = "lines"
  ) %>%
  layout(title = " Data SPKU Jakarta 2011-2023", hovermode = "x unified")
plot


# View Main Logic
component1 <- get_page("component1.r")
spku_page <- get_page("spku-ui.r")

custom_theme <- bootstrapLib(
  bs_theme(
    version = 4,
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#0199F8",
    secondary = "#FF374B",
    base_font = font_google("Inter")
  )
)

ui <- fluidPage(
  theme = custom_theme,
  navbarPage(
    "AOL Datmin",
    tabPanel(
      "SPKU Jakarta",
      spku_page
    )
  )
)

spku_server <- get_server("spku-server.r")

server <- function(input, output, session) {
  output$spku_graph <- spku_server(
    input = input,
    output = output,
    session = session,
    monthly_data = monthly_data,
    monthly_data_station = monthly_data_station
  )
}

runApp(shinyApp(ui = ui, server = server))

rm(list = ls())
