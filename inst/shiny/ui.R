library(shiny)

# Run using:
# runApp("~/Hackathon-RDWD-QualityMonitoring/inst/shiny")

navbarPage(fluid=TRUE, theme="mqm_style.css", windowTitle="MQM: Inhomogeneity detection",
           title=div(class="alert alert-success", img(src="mqm_logo.svg", width=48), "MQM", tags$small("inhomogeneity detection")),

  #title above navbar
#  h1("Inhomogeneity detection", align = "center"),
  
  #titlePanel(title=div(class="alert alert-success", img(src="mqm_logo.svg", width=48), "MQM", tags$small("inhomogeneity detection"))),

  #navlistPanel("",

    navbarMenu("",

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

           