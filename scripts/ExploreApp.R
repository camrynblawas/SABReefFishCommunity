#The purpose of this app is to be able to explore 

#Libraries and theme
library(shiny)
library(leaflet)
library(shiny)
library(leaflet)
library(spData)
library(dplyr)
library(RColorBrewer)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(here)

my_theme <- function() {theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(size=12, face= "bold", colour= "black"), axis.title.x = element_text(size=12, face="bold", colour = "black"), axis.title.y = element_text(size=12, face="bold", colour = "black"))}


#get data
df <- read.csv(here("data", "processeddata", "Final_SEUS.csv"))
df$date <- as.Date(df$date)
df <- rbind(df, list(0, 0, 0, 0, 0, "NA", 33.8, -78, 0, 0, 0, 0, 0, 0, 0, "NA", "NA", as.Date("2024-03-19")))


####APP
ui <- fluidPage(
  titlePanel("SEUS Trawl Data"),
  mainPanel(
    h4("The purpose of this app is to look at trawl data in the South East United States. "),
    tabsetPanel(
      #For map tab
      tabPanel(title = "Map", leafletOutput("mymap"), 
               selectInput("species", 
                           label = "Choose a species to display",
                           choices = unique(df$accepted_name))), 
      #For plot tab
      tabPanel(title = "Plot Surface Temperature", plotOutput("temptrendsurf"), selectInput("species", label = "Choose a species to display", choices = unique(df$accepted_name))),
      
      #For plot tab
      tabPanel(title = "Plot Bottom Temperature", plotOutput("temptrendbottom"), selectInput("species", label = "Choose a species to display", choices = unique(df$accepted_name))),
    
    )
  )
)

server <- function(input, output) {
  #for map tab
  filteredData <- reactive({
    w <- df 
    w <- w %>% dplyr::filter(accepted_name == input$species)
    return(w)
  })
  
  output$mymap <- renderLeaflet({
    filteredData() %>% 
      leaflet() %>%
      addTiles()   %>%
      addCircles(lng = ~longitude, lat = ~latitude, color = ~accepted_name, popup = ~as.character(accepted_name), label = ~as.character(accepted_name)) 
    
  })
  
  
  #for plot tab
  tempData <- reactive({
    w <- df %>% dplyr::filter(accepted_name == input$species) 
    return(w)
  })
  
  output$temptrendsurf <- renderPlot({
    df2 <- tempData()
    label <- unique(paste0(input$species))
    ggplot(data = df2, mapping = aes(x = date, y = sst)) + geom_smooth(method = "lm", size = 1) + geom_point() + labs(title = paste0(label, " Surface Temperature"), x = "Date", y = "Temperature") + my_theme()
  })
  
  output$temptrendbottom <- renderPlot({
    df2 <- tempData()
    label <- unique(paste0(input$species))
    ggplot(data = df2, mapping = aes(x = date, y = sbt)) + geom_smooth(method = "lm", size = 1) + geom_point() + labs(title = paste0(label, " Bottom Temperature"), x = "Date", y = "Temperature") + my_theme()  

  })
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)