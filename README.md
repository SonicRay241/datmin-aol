# Data Mining AOL Project

## Project Structure
```
.
├── pages                   # Folder consisting of shiny ui pages
│   └── page1.r             # Shiny ui page
├── server                  # Folder consisting of shiny server logic
│   └── server1.r           # Shiny server logic
├── .env                    # Your environment secrets
├── .env-example            # .env example, do not remove
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