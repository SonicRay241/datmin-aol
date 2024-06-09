library(shiny)
library(bslib)

library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(ggplot2)

library(ggiraph)

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


# baru deh mau edit apa
# kalau udah
# save (ctrl + s)

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
data <- rbind(data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21, data22, data23)
rm(data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21, data22, data23)

# Fix categorical data naming
unique(data$stasiun)

from <- c("DKI4", "DKI2", "DKI5", "DKI1","DKI3", "DKI5 Kebon Jeruk Jakarta Barat", "DKI1 Bunderan HI", "DKI2 Kelapa Gading", "DKI3 Jagakarsa", "DKI4 Lubang Buaya", "DKI5 Kebon Jeruk", "DKI5 (Kebon Jeruk) Jakarta Barat")
to <- c("DKI4 (Lubang Buaya)", "DKI2 (Kelapa Gading)", "DKI5 (Kebon Jeruk)", "DKI1 (Bunderan HI)","DKI3 (Jagakarsa)", "DKI5 (Kebon Jeruk)", "DKI1 (Bunderan HI)", "DKI2 (Kelapa Gading)", "DKI3 (Jagakarsa)", "DKI4 (Lubang Buaya)", "DKI5 (Kebon Jeruk)", "DKI5 (Kebon Jeruk)")
convert <- data.frame(from, to)

for(from in convert$from){
  data <- data %>%
    mutate(stasiun = as.character(stasiun),
           stasiun = if_else(stasiun == from, convert$to[convert$from == from], stasiun),
           stasiun = as.factor(stasiun))
}

rm(from, to, convert)

# Data Cleaning








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