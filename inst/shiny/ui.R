library(shiny)

# Run using:
# runApp("~/Hackathon-RDWD-QualityMonitoring/inst/shiny")

fluidPage(

  #title above navbar
  h1("Inhomogenity detection", align = "center"),

  navbarPage("",
    tabPanel("Alerts",
      fluidRow(
        column(12,
          div(class = "panel panel-danger", 
            div(class="panel-heading", "The systems has detected a Break!"),
            div(class="panel-body", 
              div(includeMarkdown("Warning_example.Rmd")),
              div(actionButton("go", "More details...")),
              div(plotOutput("plot"))
            )
          )
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

           