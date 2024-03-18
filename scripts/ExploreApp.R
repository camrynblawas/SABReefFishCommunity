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

my_theme <- function() {theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(size=12, face= "bold", colour= "black"), axis.title.x = element_text(size=12, face="bold", colour = "black"), axis.title.y = element_text(size=12, face="bold", colour = "black"))}

#Get data