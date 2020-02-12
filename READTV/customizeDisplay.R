
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}

customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  props = list(maxShapeN = 6, maxColorN = 21, maxFacetN = 21)
  
  no_selection = "None"
  
  shapeColumn = reactiveVal(no_selection)
  
  colorColumn = reactiveVal(no_selection)
  
  yColumn = reactiveVal("Event")
  
  facetColumn = reactiveVal(no_selection)
  
  facetOrder = reactiveVal()
  
  facetLabel = reactive({
    fo = facetOrder()
    
    if(is.null(fo)) return(NULL)
    
    fo %>% lapply(function(f) input[[f]]) %>% as.character
  })
  
  validColumns = function(df, fn) df %>% select_if(fn) %>% colnames
  
  validCountGen = function(n) function(df, co) length(unique(co)) <= n
  
  validCountColumns = function(df, n) df %>% 
    validColumns(function(co) length(unique(co)) <= n)
  
  shouldUpdate = function(valid_choices, current_choice) {
    should_update = F
    if(is.null(current_choice)) should_update = T
    else should_update = !(current_choice %in% valid_choices)
    
    return(should_update & length(valid_choices) > 0)
  }
  
  observe({
    update = function(valids, getset) 
      if(shouldUpdate(valids, getset()))
        getset(valids[1])
    
    update(validShapeColumns(), shapeColumn)
    
    update(validColorColumns(), colorColumn)
  })
  
  validShapeColumns = reactive({
    req(data())
    
    append(no_selection, validCountColumns(data(), props$maxShapeN))
  })
  
  validYColumns = reactive({
    req(data())
    
    data() %>%
      validColumns(function(co) class(co) %in% c("logical", "numeric")) %>%
      {append("Event", .)}
  })
  
  validColorColumns = reactive({
    req(data())
    
    no_selection %>% 
      append(validCountColumns(data(), props$maxColorN)) %>%
      union(validYColumns())
  })
  
  validFacetColumns = reactive({
    req(data())
    
    append(no_selection, validCountColumns(data(), props$maxFacetN))
  })
  
  setFacetOrder = function(){
    req(input$facetColumn != no_selection)
    
    fc = input$facetColumn
    if(!input$customizeFacets)
      facetOrder(
        data() %>%  select_(fc) %>% unique %>% {.[[fc]]}
      )
    else
      facetOrder(input$facet_list)
  }
  
  observeEvent(input$customizeDisplay, {
    selectText = function(col, maxN, ext = "") 
      paste0(col, " (Max ", maxN, " unique values", ext,")")
    
    showModal(modalDialog(
      title = "Customize Display",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Submit"),
        modalButton("Cancel")
      ),
      easyClose = T,
      selectInput(ns("yColumn"), "Y (numeric/logical)",
                  choices = validYColumns(),
                  selected = yColumn()),
      selectInput(ns("shapeColumn"), selectText("Shape", props$maxShapeN), 
                  choices = validShapeColumns(),
                  selected = shapeColumn()),
      selectInput(ns("colorColumn"), 
                  selectText(
                    "Color", props$maxColorN, ", or numeric/logical"), 
                  choices = validColorColumns(),
                  selected = colorColumn()),
      fluidRow(
        column(6,
               selectInput(ns("facetColumn"), 
                           selectText("Facet", props$maxFacetN),
                           choices = validFacetColumns(),
                           selected = facetColumn())),
        column(2, 
               checkboxInput(ns("customizeFacets"), 
                             "Customize"))
      ),
      uiOutput(ns("facetCustomize"))
    ))
    
    output$facetCustomize = renderUI({
      if(!input$customizeFacets | (input$facetColumn == no_selection))
        return()

      facet_values = data() %>% 
        select_(input$facetColumn) %>% 
        unique %>% {.[[input$facetColumn]]} %>% 
        lapply(
          function(fv) {
            fvc = as.character(fv)
            textInput(ns(fvc), fvc, value = fvc, placeholder = fvc)
          }
        )
      
      bucket_list(
        header = "Customize Facet",
        add_rank_list(
          text = "Facet Order & Display Values",
          input_id = ns("facet_list"),
          labels = facet_values
        )
      )
    })
    
    observeEvent(input$modalSubmit, {
      yColumn(input$yColumn)
      shapeColumn(input$shapeColumn)
      colorColumn(input$colorColumn)
      
      facetColumn(input$facetColumn)
      if(input$facetColumn != no_selection) setFacetOrder()
      
      removeModal()
    }, ignoreInit = T)
  })
  
  return(list(shapeColumn = shapeColumn, colorColumn = colorColumn, 
              yColumn = yColumn, facetColumn = facetColumn,
              facetOrder = facetOrder,
              facetLabeller = facetLabel,
              no_selection = no_selection))
}