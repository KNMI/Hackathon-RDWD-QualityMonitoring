library(shiny)
library(leaflet)

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
  div(
    class = "MainSection row",
    conditionalPanel(
      class="creedy",
      condition = "output.showDetails != 'true'",
      navbarPage(
        "",
        tabPanel("Alerts",
                 div(
                   class = "row",
                   div(class = "col-xs-2",
                       div(
                         class = "panel panel-danger",
                         div(
                           class = "panel-heading",
                           icon("warning", "fa-2x", lib = "font-awesome"),
                           p(
                             "Break detected: ",
                             tags$br(),
                             tags$strong("Rain Gauges versus AWS")
                           )
                         ),
                         div(
                           class = "panel-body",
                           textOutput("datetime"),
                           div(class = "includeDivider", includeMarkdown("Warning_example.Rmd")),
                           actionButton(
                             class = "btn-danger",
                             "showDetailsNL",
                             icon = icon("arrow-right", lib = "font-awesome"),
                             "details..."
                           )
                         )
                       )),
                   div(class = "col-xs-2",
                       div(
                         class = "panel panel-danger",
                         div(
                           class = "panel-heading",
                           icon("warning", "fa-2x", lib = "font-awesome"),
                           p(
                             "Break detected: ",
                             tags$br(),
                             tags$strong("Rain Gauges versus AWS")
                           )
                         ),
                         div(
                           class = "panel-body",
                           textOutput("datetime2"),
                           div(class = "includeDivider", includeMarkdown("Warning_example.Rmd")),
                           actionButton(
                             class = "btn-danger",
                             "showDetails280",
                             icon = icon("arrow-right", lib = "font-awesome"),
                             "details..."
                           )
                         )
                       ))
                 ),
                 div(class = "row", "")),
        tabPanel("Overview",
                 div(
                   class = "row",
                   div(
                     class = "col-xs-8",
                     tags$table(
                       class = "table table-striped",
                       tags$thead(
                         tags$tr(
                           tags$th("Station(s)"),
                           tags$th("Annual"),
                           tags$th("DJF"),
                           tags$th("MAM"),
                           tags$th("JJA"),
                           tags$th("SON")
                         )
                       ),
                       tags$tbody(
                         tags$tr(
                           tags$td("NL",
                                   actionLink(
                                     "showDetailsNLa",
                                     "details...",
                                     icon("arrow-right", lib = "font-awesome")
                                   )),
                           tags$td(class = "alert alert-danger",
                                   tags$strong("Break!")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok"))
                         ),
                         tags$tr(
                           tags$td("260",
                                   actionLink(
                                     "showDetails260a",
                                     "details...",
                                     icon("arrow-right", lib = "font-awesome")
                                   )),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok!")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok"))
                         ),
                         tags$tr(
                           tags$td("280",
                                   actionLink(
                                     "showDetails280a",
                                     "details...",
                                     icon("arrow-right", lib = "font-awesome")
                                   )),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok!")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok")),
                           tags$td(class = "alert alert-danger",
                                   tags$strong("Break!")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok")),
                           tags$td(class = "alert alert-success",
                                   tags$strong("Ok"))
                         )
                       )
                     )
                   )
                 )),
        tabPanel("Background information",
                 fluidRow(
                   includeMarkdown("Background_info_example.Rmd")
                 )),
        tabPanel("Map experiments", class="creedy",
                 fluidRow(
                   #class="creedy",
                   #div(class = "col-xs-offset-2 col-xs-8",
                       leafletOutput("map")
                       ),
                 fluidRow(
                   div(class = "col-xs-offset-2 col-xs-8",
                       textOutput("clickedMarker"))),
                fluidRow(
                   div(class = "col-xs-offset-2 col-xs-8",
                       checkboxGroupInput("Type","Station Type",
                                                         c("AWS"=2,
                                                           "Manual"=1
                                                           ),
                                          selected = c(1,2)),
                       sliderInput("Radius","Radius",0,100,value=30),
                       sliderInput("nr","Number",0,10,value=3))
                 ),
                 fluidRow(
                   div(class = "col-xs-offset-2 col-xs-8",
                       tableOutput("clickedDistance"))

                 ),
                 fluidRow(
                   div(class = "col-xs-offset-2 col-xs-8",
                       tableOutput("clickedNumber"))
                 ),
                 fluidRow(
                   div(class = "col-xs-offset-2 col-xs-8",
                       tableOutput("stationsNearby")
                       )
                 )
                 )
      )
    )
  ),
  conditionalPanel(condition = "output.showDetails == 'true'",
                   navbarPage(
                     actionLink(class = "", "hideDetails",
                                icon("arrow-left", lib = "font-awesome")),
                     tabPanel("Details", div(class = "imgWrapper",
                                             div(
                                               class = "row",
                                               div(
                                                 class = "col-xs-4",
                                                 # textOutput("stationId"),
                                                 #       tags$img(src="figures/hackathon_NL_AWSvsMAN_y.png", maxHeight=100))
                                                 #   ),
                                                 # div(
                                                 #   class = "row",
                                                 #   div(class = "col-xs-4",
                                                 #       # textOutput("stationId"),
                                                 #       tags$img(src="figures/hackathon_NL_AWSvsMAN_djf.png")),
                                                 #   div(class = "col-xs-4",
                                                 #       # textOutput("stationId"),
                                                 #       tags$img(src="figures/hackathon_NL_AWSvsMAN_mam.png"))),
                                                 #   div(
                                                 #     class = "row",
                                                 #     div(class = "col-xs-4",
                                                 #         # textOutput("stationId"),
                                                 #         tags$img(src="figures/hackathon_NL_AWSvsMAN_jja.png")),
                                                 #     div(class = "col-xs-4",
                                                 #         # textOutput("stationId"),
                                                 #         tags$img(src="figures/hackathon_NL_AWSvsMAN_son.png"))
                                                 # )))
                                                 textOutput("stationId"),
                                                 includeMarkdown("Details.Rmd")
                                               )
                                             )))
                   ))
)

