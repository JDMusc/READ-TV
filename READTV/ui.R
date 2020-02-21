function(request)
  fluidPage(
    bookmarkButton(),
    #uiOutput("saveDisplay"),
    uiOutput("eventDisplayer"),
    theme = shinytheme("cyborg")
  )