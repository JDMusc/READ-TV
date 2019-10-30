library(shiny)

showSource = function(data) 
  showModal(modalDialog(
    title = "Source Data",
    renderDataTable(data()[,
                           c("Case", "Phase", "Time", "FD.Type", "Notes")]
                    ),
    easyClose = TRUE,
    size = "m"
    ))