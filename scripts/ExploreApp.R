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
library(cowplot)


library(tidymodels)
library(tidyflow)
library(rpart.plot)
library(vip)
library(baguette)
library(ranger)

my_theme <- function() {theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(size=12, face= "bold", colour= "black"), axis.title.x = element_text(size=12, face="bold", colour = "black"), axis.title.y = element_text(size=12, face="bold", colour = "black"))}


#get data
df <- read.csv(here("data", "processeddata", "Final_SEUS.csv"))
df$date <- as.Date(df$date)
df <- rbind(df, list(0, 0, 0, 0, 0, "NA", 33.8, -78, 0, 0, 0, 0, 0, 0, 0, "NA", "NA", as.Date("2024-03-19")))

#with COB
dfwbiomass <- df %>% group_by(year, month, accepted_name) %>% mutate(monthlyspeciesbiomass = sum(wgt_cpue), biomass = wgt_cpue, avsst = mean(sst, na.rm = TRUE), avsbt = mean(sbt, na.rm = TRUE),  avdepth = mean(depth, na.rm = TRUE)) 

centerofbiomass <- dfwbiomass %>% 
  mutate(weightedLAT = (biomass/monthlyspeciesbiomass)*latitude) %>% 
  mutate(weightedLON = (biomass/monthlyspeciesbiomass)*longitude) %>% 
  group_by(accepted_name, year, month) %>% 
  summarise(CENTER_LAT = sum(weightedLAT, na.rm = TRUE), CENTER_LON = sum(weightedLON, na.rm = TRUE), avsst = mean(avsst, na.rm = TRUE), avsbt = mean(avsbt, na.rm = TRUE), avdepth = mean(avdepth, na.rm = TRUE))

centerofbiomass$CENTER_LAT <- replace(centerofbiomass$CENTER_LAT, centerofbiomass$CENTER_LAT == 0, NA)
centerofbiomass$CENTER_LON <- replace(centerofbiomass$CENTER_LON, centerofbiomass$CENTER_LON == 0, NA)


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
      tabPanel(title = "Plot Center of Biomass Data", plotOutput("temptrendsurf"), selectInput("species2", label = "Choose a species to display", choices = unique(df$accepted_name))),
      
      #For model tab
      tabPanel(title = "Model CPUE Data", plotOutput("model"), selectInput("species3", label = "Choose a species to display", choices = unique(df$accepted_name)), sliderInput("trees", "Number of trees for XGBoost", min = 20, max = 500, value = 250, step = 10)),
    
    )
  )
)

server <- function(input, output) {
  #for map tab
  filteredData <- reactive({
    w <- centerofbiomass 
    w <- w %>% dplyr::filter(accepted_name == input$species)
    return(w)
  })
  
  output$mymap <- renderLeaflet({
    filteredData() %>% 
      leaflet() %>%
      addTiles()   %>%
      addCircles(lng = ~CENTER_LON, lat = ~CENTER_LAT, color = ~year, popup = ~as.character(accepted_name), label = ~as.character(accepted_name)) 
    
  })
  
  
  #for plot tab
  tempData <- reactive({
    w <- centerofbiomass %>% dplyr::filter(accepted_name == input$species2) 
    return(w)
  })
  
  output$temptrendsurf <- renderPlot({
    df2 <- tempData()
    label <- unique(paste0(input$species2))
    p1 <- ggplot(data = df2, mapping = aes(x = year, y = CENTER_LAT)) + geom_smooth(method = "lm", size = 1) + geom_point() + labs(title = paste0(label, " Center of Biomass Latitude over Time"), x = "Date", y = "Latitude") + my_theme()
    p2 <- ggplot(data = df2, mapping = aes(x = year, y = CENTER_LON)) + geom_smooth(method = "lm", size = 1) + geom_point() + labs(title = paste0(label, " Center of Biomass Longitude over Time"), x = "Date", y = "Latitude") + my_theme()
    p3 <- ggplot(data = df2, mapping = aes(x = year, y = avsst)) + geom_smooth(method = "lm", size = 1) + geom_point() + labs(title = paste0(label, " Center of Biomass Surface Temperature over Time"), x = "Date", y = "Temperature (Degrees C)") + my_theme()
    p4 <- ggplot(data = df2, mapping = aes(x = year, y = avdepth)) + geom_smooth(method = "lm", size = 1) + geom_point() + labs(title = paste0(label, " Center of Biomass Depth over Time"), x = "Date", y = "Depth (meters)") + my_theme()
    plot_grid(p1, p2, p3, p4, ncol = 2, nrow = 2)
    
  })
  
  modelthing <- reactive({
    treesnum <- input$trees
    df2 <- centerofbiomass %>% dplyr::filter(accepted_name == input$species3) 
    df2 <- df2[, !names(df2) %in% c("accepted_name", "CENTER_LON")]
    label <- unique(paste0(input$species3))
    
    boost_mod <-
      boost_tree(mode = "regression", trees = treesnum) %>%
      set_engine("xgboost")
    
    tflow <-
      df2 %>%
      tidyflow(seed = 51231) %>%
      plug_formula(CENTER_LAT ~ .) %>%
      plug_split(initial_split) %>%
      plug_model(boost_mod)
    
    res_boost <- fit(tflow)
    
    rmse_gb_test <-
      res_boost %>%
      predict_testing() %>%
      rmse(CENTER_LAT, .pred) %>%
      pull(.estimate)
    
    vip <-  res_boost %>%
      pull_tflow_fit() %>%
      vip()
    vip <- vip[["data"]]
    
     p <- res_boost %>%
      pull_tflow_fit() %>%
      .[['fit']] %>%
      vip() +
      theme_minimal() + labs(title = paste0(label, " xgboost Variable Importances on Center of Biomass Latitude"), x = "Variable", y = "Importance") + my_theme()
     return(p)
  })
  
output$model <- renderPlot({
  q <- modelthing()
  print(q)
  })
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)