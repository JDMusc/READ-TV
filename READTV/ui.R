library(shiny)
library(shinyjs)

source('utils.R')

selectRows <- fluidRow(
    column(
        width = 2,
        selectInput("caseSelect", "Select Case", selectableChoices(c()))),
    column(
        width = 2,
        selectInput("phaseSelect", "Select Phase", selectableChoices(c()))),
    column(
        width = 2,
        selectInput("fdSelect", "Select FD Type", selectableChoices(c()),
                    multiple = TRUE)),
    column(
        width = 2,
        selectInput("plotType", "Plot Type", 
                    c("Time Plot" = "timePlot", "Histogram" = "hist"),
                    selected = "timePlot")),
    column(
        width = 2,
        conditionalPanel(
            condition = 'input.plotType == "timePlot"',
            checkboxInput("doStemPlot", "Stem Plot")
        )
    ),
    column(
        width = 2,
        conditionalPanel(
            condition = 'input.plotType == "timePlot"',
            actionButton(inputId = "showSource", label = "Show Source")
        )
    ),
    column(
        width = 2,
        conditionalPanel(
            condition = 'input.plotType == "timePlot"',
            actionButton(inputId = "calcCPA", label = "Show CPA")
        )
    )
)

fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(list(.invalid_query = 'background-color: #f006')),
    fluidRow(
        actionButton(inputId = "loadData", label = "Load Data"),
        actionButton(inputId = "loadMetaData", label = "Load Meta Data")
    ),
    uiOutput("metaQuery"),
    selectRows,
    conditionalPanel(
        condition = 'input.plotType == "timePlot"',
        plotOutput("timePlot"),
        verbatimTextOutput("eventStats")
    ),
    conditionalPanel(
        condition = 'input.plotType == "hist"',
        plotOutput("hist"),
        verbatimTextOutput("fdStats")
    )
)
