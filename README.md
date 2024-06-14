# Data Mining AOL Project

## Running the app
Run the `main.r` file

## Project Structure
```
.
├── data                    # Folder consisting of data that will be used in the project
│   ├── ...
│   └── data.csv
├── pages                   # Folder consisting of shiny ui pages
│   └── page1.r             # Shiny ui page
├── server                  # Folder consisting of shiny server logic
│   └── server1.r           # Shiny server logic
├── .env                    # Your environment secrets
├── .env-example            # .env example, do not remove
├── install.r               # Script to install all dependencies
├── main.r                  # Main runtime of shiny
└── README.md               # This file
```

## Page file example
```r
library(shiny)

page1 <- sidebarLayout(
  sidebarPanel(
    "This is a sidebar"
  ),
  mainPanel(
    plotOutput("examplePlot")
  )
)
```

## Server file example
```r
library(ggplot2)

server <- function(input, output, session) {
  ... # Additional data processing logic

  output$examplePlot <- ggplot(...) # Return the plot like this
}
```