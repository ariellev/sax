require(shinydashboard)
require(rCharts)


header <- dashboardHeader(title = "SAX")

sidebar <- dashboardSidebar(
  h6("Symbolic Aggregate Approximation", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;"),
  h6("Apple Inc. (AAPL) opening trade prices 2015", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;"),
  div(style = "position: fixed; margin-top: 20px; margin-bottom: 20px;",
            div(sliderInput("window", "window", min = 1, max = 50, value = 25), style = "width: 75%"),
      div(sliderInput("alphabet", "alphabet", min = 1, max = 10, value = 5), style = "width: 75%")
  )
)

body <- dashboardBody(
  fluidRow(               
    column(width = 12,
           box(width = NULL, solidHeader = TRUE,
               h1("Stock", style = "margin-top: 0px; margin-bottom: 10px"),
               div(style = "width: 100%; float: left", showOutput("stockPlot", "nvd3")),
               h2("SAX", style = "margin-top: 0px; margin-bottom: 10px"),               
               h3(textOutput("SAX"), style = "margin-top: 0px; margin-bottom: 10px")
    )
  )
)
)

dashboardPage(
  header,
  sidebar,
  body
)