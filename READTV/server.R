library(dplyr)
library(ggplot2)
library(shiny)
library(shinyjs)


function(input, output, session){
    callModule(eventsDisplayServer, "eventsDisplay")
}
