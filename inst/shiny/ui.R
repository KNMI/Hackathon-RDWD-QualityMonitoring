library(shiny)

# Run using:
# runApp("~/Hackathon-RDWD-QualityMonitoring/R/shiny")

fluidPage(

  #title above navbar
  h1("Inhomogenity detection", align = "center"),

  navbarPage("",
    tabPanel("Alerts",
      fluidRow(
        includeMarkdown("Warning_example.Rmd")
      ),
      fluidRow(
        column(6,
          div(class = "panel panel-danger", 
            div(class="panel-heading", "title"),
            div(class="panel-body", 
              div("Body"),
              div(plotOutput("plot"))
            )
          )
        ),
        column(6,
          h4("quantiles"),
          tableOutput("summary")
        )
      )
    ),
    tabPanel("Overview",
      fluidRow(
        p("table: red/green, plotje on white board")
      )
    ),
    tabPanel("Background information",
      fluidRow(
        includeMarkdown("Background_info_example.Rmd")
      )
    )
  )
)

           