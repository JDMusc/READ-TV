function(input, output, session){
  
  displayCount = reactiveVal(0)
  dCountAsId = function(dcount) paste0("eventsDisplay", dcount)
  
  currentEventId = reactive({
    dCountAsId(displayCount())
  })
  
  eventDisplays = reactiveValues()
  
  observeEvent(input$saveDisplay, {
    displayCount(displayCount() + 1)
    curr_id = currentEventId()
    
    eventDisplays[[curr_id]] = callModule(eventsDisplayServer, curr_id)
    
    insertUI(selector = "#eventDisplayer",
             where = "afterBegin",
             ui = eventsDisplayUI(curr_id)
             )
  })
  
  output$saveDisplay = renderUI({
    actionButton("saveDisplay", "Save Display")
  })
  
  output$eventDisplayer = renderUI({
    id = dCountAsId(0)
    eventDisplays[[id]] = callModule(eventsDisplayServer, id)
    
    div(
      eventsDisplayUI(id)
    )
  })
}
