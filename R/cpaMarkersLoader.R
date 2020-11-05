

cpaMarkersLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


cpaMarkersLoader = function(input, output, session, cpa_markers_sym) {
  ns = session$ns
  f = stringr::str_interp

  cpaMarkersDataF = callModule(
    fileWellServer, "filewell", "Change points")

  isFileLoaded = cpaMarkersDataF$isFileLoaded

  name = reactive({
    if(isFileLoaded()) {
      cpaMarkersDataF$fileInfo()$name
    }
  })

  datapath = reactive({
    if(isFileLoaded()) {
      cpaMarkersDataF$fileInfo()$datapath
    }
  })

  f_name_var = "cpa_markers_f"

  data = reactive({
    if(!isFileLoaded())
      return()

     rlang::set_names(datapath(), nm = f_name_var) %>%
      {eval_tidy(currentCode(), .)}
  })


  sourceString = reactive({
    req(name())
    f_name = name()

    top_row = f("${f_name_var} = \"${f_name}\" #update local file path")

    expressionsToString(
      top_row,
      currentCode()
    )
  })

  currentCode = reactive({
    if(!isFileLoaded())
      return()

    f_name = cpaMarkersDataF$fileInfo()$name
    load_expr = loadFileExpr(f_name)

    rhs = expr(!!(datapath()) %>% !!load_expr)

    expr(!!cpa_markers_sym <- !!rhs)
  })


  return(
    list(name = name, data = data,
         datapath = datapath,
         isFileLoaded = isFileLoaded,
         code = currentCode,
         sourceString = sourceString)
    )
}
