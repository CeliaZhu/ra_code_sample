# Step 4: Interactive election tweets map
# Please see the published app here https://celia-zhu.shinyapps.io/2020_election_tweets_map/
library(shiny)
library(tidyverse)
library(leaflet)
library(sp)
library(here)

# Load tweets----
tweets <- read_csv("tweets_shiny.csv")

# Parameters----
for (var in list("likes", "retweet_count", "user_followers_count")) {
  assign(paste0(var, "max"), max(tweets[[{{ var }}]]))
}
start <- "2020-01-01"
end <- "2020-12-31"
default_like <- 1000
default_retweet <- 1000
default_followers <- 10

# Define UI----
ui <- bootstrapPage(
  theme = shinythemes::shinytheme("simplex"),
  leaflet::leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 10, right = 10, id = "controls",
    sliderInput("min_likes", "Minimum likes", 0, likesmax, default_like),
    sliderInput("min_retweets", "Minimum retweets", 0, retweet_countmax, default_retweet),
    sliderInput("min_followers", "Minimum followers", 0, user_followers_countmax, default_followers),
    dateRangeInput(
      "date_range", "Select Date",
      start = start, end = end, format = "yyyy-mm-dd"
    ),
    actionButton("show_about", "About")
  ),
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
  ")
)

# Define server
server <- function(input, output, session) {
  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = "About",
      tags$div(
        "Tweets collected by Kaggle user",
        tags$a(href = "https://www.kaggle.com/manchunhui/us-election-2020-tweets", "Manchun Hui")
      ),
      tags$div("The radius of a circle marker corresponds to the number of likes")
    ))
  })
  output$map <- renderLeaflet({
    tweets %>%
      filter(
        created_at >= input$date_range[1],
        created_at <= input$date_range[2],
        likes >= input$min_likes,
        retweet_count >= input$min_retweets,
        user_followers_count >= input$min_followers
      ) %>%
      leaflet() %>%
      setView(-98.58, 39.82, zoom = 5) %>%
      addTiles() %>%
      addCircleMarkers(
        popup = ~tweet, radius = ~ sqrt(likes) / 10, weight = 1,
        fillOpacity = 0.2, opacity = 0.2,
        clusterOptions = markerClusterOptions()
      )
  })
}

shinyApp(ui, server)
