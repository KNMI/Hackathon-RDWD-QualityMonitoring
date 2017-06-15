library(shiny)

# Run using:
# runApp("~/Hackathon-RDWD-QualityMonitoring/inst/shiny")

fluidPage(
  title = "MQM - Inhomogeneity detection",
  theme = "mqm_style.css",
  
  tags$header(class = "Header row",
              tags$nav(
                role = "navigation", class = "navbar",
                div(class = "row",
                    div(class = "col-auto",
                        div(
                          class = "navbar-brand",
                          a(
                            class = "breadcrumb-active",
                            href = "#",
                            img(class = "logo", src = "mqm_logo.svg", width =
                                  36),
                            span("MQM - ", tags$small("inhomogeneity detection"))
                          )
                        )))
              )),
  div(class = "MainSection row",
      navbarPage(
        "",
        tabPanel("Alerts",
                 div(class = "row",
                     div(
                       class = "col-xs-3",
                       div(
                         class = "panel panel-danger",
                         div(class = "panel-heading", 
                             icon("warning", "fa-2x", lib = "font-awesome"),
                             "Break detected: Rain Gauges versus AWS"),
                         div(
                           class = "panel-body",
                           textOutput("datetime"),
                           div(class="includeDivider", includeMarkdown("Warning_example.Rmd")),
                           actionButton(class="btn-danger", "details", "» details...")
                           #,
                           #div(plotOutput("plot"))
                         )
                       )
                     )),
                 div(class = "row", "")),
        tabPanel("Overview",
                 div(class = "row",
                      div(
                        class = "col-xs-8",
                   tags$table(class="table table-striped",
                     tags$thead(
                       tags$tr(
                         tags$th("Station(s)"),
                         tags$th("Annual"),
                         tags$th("DJF"),
                         tags$th("MAM"),
                         tags$th("JJA"),
                         tags$th("SON")
                      )),
                     tags$tbody(
                       tags$tr(
                         tags$td("NL"),
                         tags$td(class="alert alert-danger",
                            tags$strong("Break!"), 
                            a(href="#", "» details...")
                          ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         )
                        ),
                       tags$tr(
                         tags$td("260"),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok!"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         )
                       ),
                       tags$tr(
                         tags$td("280"),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok!"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-danger",
                                 tags$strong("Break!"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         ),
                         tags$td(class="alert alert-success",
                                 tags$strong("Ok"), 
                                 a(href="#", "» details...")
                         )
                       )
                      )
                   )
                 ))),
        tabPanel("Background information",
                 fluidRow(
                   includeMarkdown("Background_info_example.Rmd")
                 ))
      ))
  
)