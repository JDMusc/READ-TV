library(shiny)


showCpa = function(input, output, session, data) {
  start_time = data()$Time[1]
  end_time = max(data()$Time)
  methods = c("AMOC", "PELT", "SegNeigh", "BinSeg")
  penalties = c( "None", "SIC", "BIC", "MBIC", 
                 "AIC", "Hannan-Quinn", "Asymptotic", "CROPS")
  
  ns = session$ns
  showModal(modalDialog(
    title = "Change Point Analysis",
    fluidRow(
      selectInput(ns("cpaSelect"), "Select N", 1:5, selected = 4),
      selectInput(ns("methodSelect"), "Method", methods, selected = "AMOC"),
      selectInput(ns("penaltySelect"), "Penalty", 
                  penalties, selected = "MBIC"),
      selectInput(ns("qSelect"), "# Change Pts", 1:6, selected = 2)),
    renderPlot({
      n = as.numeric(input$cpaSelect)
      time_data = withinTimeSeries(data()$RelativeTime, n = n)
      plot(cpt.mean(time_data, 
                    method = input$methodSelect,
                    penalty = input$penaltySelect,
                    Q = as.numeric(input$qSelect)), 
           ylab = paste("# Events per", n*2, "time points"))
    })
  ))
}