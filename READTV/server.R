function(input, output, session){
  
  displayCount = reactiveVal(0)
  currentEventId = reactive({
    paste0("eventsDisplay", displayCount())
  })
  
  eventDisplays = reactiveValues()
  
  observeEvent(input$addDisplay, {
    displayCount(displayCount() + 1)
    
    id = currentEventId()
    
    callModule(eventsDisplayServer, id)
    eventDisplays[[id]] = eventsDisplayUI(id)
    
    insertUI(selector = "#eventDisplayer",
             where = "beforeEnd",
             ui = eventDisplays[[id]]
             )
  })
  
  output$eventDisplayer = renderUI({
    id = currentEventId()
    callModule(eventsDisplayServer, id)
    div(
      eventsDisplayUI(id)
    )
  })
}
