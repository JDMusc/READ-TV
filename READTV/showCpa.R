library(changepoint)
library(shiny)


showCpa = function(input, output, session, data) {
  start_time = data()$RelativeTime[1]
  end_time = max(data()$RelativeTime)
  methods = c("AMOC", "PELT", "SegNeigh", "BinSeg")
  penalties = c( "None", "SIC", "BIC", "MBIC", 
                 "AIC", "Hannan-Quinn", "Asymptotic", "CROPS")
  
  ns = session$ns
  getCpt = function(class = T){
    n = as.numeric(input$cpaSelect)
    time_data = withinTimeSeries(data()$RelativeTime, n = n)
    
    cpt.mean(time_data, 
             method = input$methodSelect,
             penalty = input$penaltySelect,
             Q = as.numeric(input$qSelect),
             class = class)
  }
  showModal(modalDialog(
    title = "Change Point Analysis",
    easyClose = T,
    fluidRow(
      selectInput(ns("cpaSelect"), "Select N", 1:5, selected = 4),
      selectInput(ns("methodSelect"), "Method", methods, selected = "AMOC"),
      selectInput(ns("penaltySelect"), "Penalty", 
                  penalties, selected = "MBIC"),
      selectInput(ns("qSelect"), "# Change Pts", 1:6, selected = 2)),
    renderPlot({
      n = as.numeric(input$cpaSelect)
      plot(getCpt(), 
           ylab = paste("# Events per", n*2, "time points"))
    })#,
    #renderText({
    #  getCpt(class = F)
    #})
  ))
}
