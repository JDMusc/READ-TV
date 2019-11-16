

showSource = function(data) 
  showModal(modalDialog(
    title = "Source Data",
    renderDataTable(data()[,
                           c("Case", "Phase", "RelativeTime", "Event.Type", "Notes")]
                    ),
    easyClose = TRUE,
    size = "m"
    ))
