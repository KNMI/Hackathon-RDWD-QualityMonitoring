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

           