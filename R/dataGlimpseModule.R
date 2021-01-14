dataGlimpseUI = function(id) {
  ns = NS(id)

  verbatimTextOutput(ns("dataGlimpse"))
}

dataGlimpseServer = function(input, output, session, tbl) {

  output$dataGlimpse = renderText({
    req(tbl())

    tibble::glimpse(tbl()) %>%
      capture.output %>%
      paste('\n')
  })
}
