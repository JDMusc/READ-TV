library(shiny)

showSource = function(data) 
  showModal(modalDialog(
    title = "Source Data",
    renderDataTable(data()[,
                           c("Case", "Phase", "RelativeTime", "FD.Type", "Notes")]
                    ),
    easyClose = TRUE,
    size = "m"
    ))