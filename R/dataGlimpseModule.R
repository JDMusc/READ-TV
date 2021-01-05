dataGlimpseUI = function(id) {
  ns = NS(id)

  verbatimTextOutput(ns("dataGlimpse"))
}

dataGlimpseServer = function(input, output, session, tbl) {

  output$dataGlimpse = renderPrint({
    req(tbl())

    str(tbl())
  })
}
