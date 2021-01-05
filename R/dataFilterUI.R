

dataFilterUI = function(id) {
  ns = NS(id)
  selectRows(ns)
}


selectRows <- function(ns) {
  uiOutput(ns('selectRows'))
}
