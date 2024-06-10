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
path <-  Sys.getenv("PROJECT_PATH")

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
# rm(data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21, data22, data23)

# Data Cleaning
# Fix categorical data naming
unique(start_data$stasiun)

from <- c("DKI4", "DKI2", "DKI5", "DKI1","DKI3", "DKI5 Kebon Jeruk Jakarta Barat", "DKI1 Bunderan HI", "DKI2 Kelapa Gading", "DKI3 Jagakarsa", "DKI4 Lubang Buaya", "DKI5 Kebon Jeruk", "DKI5 (Kebon Jeruk) Jakarta Barat")
to <- c("DKI4 (Lubang Buaya)", "DKI2 (Kelapa Gading)", "DKI5 (Kebon Jeruk)", "DKI1 (Bunderan HI)","DKI3 (Jagakarsa)", "DKI5 (Kebon Jeruk)", "DKI1 (Bunderan HI)", "DKI2 (Kelapa Gading)", "DKI3 (Jagakarsa)", "DKI4 (Lubang Buaya)", "DKI5 (Kebon Jeruk)", "DKI5 (Kebon Jeruk)")
convert <- data.frame(from, to)

for(from in convert$from){
  start_data <- start_data %>%
    mutate(stasiun = as.character(stasiun),
           stasiun = if_else(stasiun == from, convert$to[convert$from == from], stasiun),
           stasiun = as.factor(stasiun))
}
start_data <- subset(start_data, grepl('[a-zA-Z]', stasiun))
start_data$stasiun <- droplevels(start_data$stasiun)
# rm(from, to, convert)

# Convert columns to numeric
data <- start_data %>%
  mutate(
    tanggal = parse_date_time(tanggal, orders = c('ymd', 'dmy')),
    pm10 = as.numeric(pm10),
    so2 = as.numeric(so2),
    co = as.numeric(co),
    o3 = as.numeric(o3),
    no2 = as.numeric(no2),
    max = as.numeric(max),
  )

# check the NA values of each columns. pm10 = 2035, s02 = 1656, co = 1494, 03 = 1725, no2 = 1655, max = 1096.
colSums(is.na(data))
colSums(is.na(data)/nrow(data)*100)

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

#replace NA values of "max" column with the max value of pm10, so2, co, o3, and no2, from the row.
data <- data %>%
  rowwise() %>%
  mutate(
    max = max(c(pm10, so2, co, o3, no2))
  ) %>%
  ungroup()

# Replace blank values in 'critical' column with the column name of the max value in each row
data <- data %>%
  rowwise() %>%
  mutate(
    critical = names(c("pm10", "so2", "co", "o3", "no2"))[which.max(c("pm10", "so2", "co", "o3", "no2"))]
  ) %>%
  ungroup()

View(data)

# Update 'categori' column based on the 'max' values
data <- data %>%
  mutate(
    categori = ifelse(
      categori == "TIDAK ADA DATA",
      case_when(
        max >= 1 & max <= 50 ~ "BAIK",
        max >= 51 & max <= 100 ~ "SEDANG",
        max >= 101 & max <= 200 ~ "TIDAK SEHAT",
        max >= 201 & max <= 300 ~ "SANGAT TIDAK SEHAT",
        TRUE ~ categori
      ),
      categori
    )
  )

#check lagi masi ada na values ga,klw masi ada omit aja(tinggal dikit2 doang)
colSums(is.na(data))
data <- na.omit(data)
colSums(is.na(data))
print(data)





View(data)
lapply(data, unique)







na.omit(data)
data$pm10 <- as.numeric(data$pm10)
data$so2 <- as.numeric(data$so2)
data$co <- as.numeric(data$co)
data$o3 <- as.numeric(data$o3)
data$no2 <- as.numeric(data$no2)
                  
# test plot
monthly_data <- data %>%
  group_by(month = floor_date(tanggal, "month")) %>%
  summarise(
    pm10 = mean(pm10,na.rm = TRUE),
    so2 = mean(so2,na.rm = TRUE),
    co = mean(co,na.rm = TRUE),
    o3 = mean(o3,na.rm = TRUE),
    no2 = mean(no2,na.rm = TRUE),
    .groups = 'drop'
  )
na.omit(monthly_data)

plot <- monthly_data %>%
  plot_ly(x = ~month, y = ~pm10, name="pm10", type = 'scatter', mode = 'lines') %>%
  add_trace(y=~so2, name="so2") %>%
  add_trace(y=~co, name="co") %>%
  add_trace(y=~o3, name="o3") %>%
  add_trace(y=~no2, name="no3") %>%
  layout(title="Data SPKU Jakarta 2011-2023",hovermode = "x unified")
plot

# test na.approx
pm10app <- pm10app %>%
  group_by(stasiun, month = floor_date(tanggal, "month")) %>%
  summarise(
    pm10 = mean(pm10, na.rm = TRUE),
    so2 = mean(so2, na.rm = TRUE),
    co = mean(co, na.rm = TRUE),
    o3 = mean(o3, na.rm = TRUE),
    no2 = mean(no2, na.rm = TRUE),
    .groups = 'drop'
  )
plot <- pm10app %>%
  plot_ly(x = ~month, y = ~pm10, color=~stasiun, type = 'scatter', mode = 'lines') %>%
  layout(title="Data SPKU Jakarta 2011-2023",hovermode = "x unified")
plot











# View Main Logic
component1 <- get_page("component1.r")

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
      "Component 1",
      component1
    ),
    tabPanel(
      "Component 2",
      sidebarLayout(
        sidebarPanel(
          "The wok"
        ),
        mainPanel(
          imageOutput("image"),
        )
      )
    )
  )
)

server <- function(input, output) {
  output$image <- renderImage(
    {
      list(
        src = normalizePath(file.path("./wok.jpg")),
        contentType = "image/png"
      )
    },
    deleteFile = FALSE
  )
}

run_with_themer(shinyApp(ui = ui, server = server))

rm(list = ls())
