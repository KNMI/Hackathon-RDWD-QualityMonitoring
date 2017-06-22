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
      class = "greedy",
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
        tabPanel("Station analysis",
          class = "greedy",
          fluidRow(
            class = "greedy",
            div(class = "col-xs-5 col-inner",
                leafletOutput("map", width="100%", height="100%")),
            div(
              class = "col-xs-7 col-right",
              div(
                class = "row",
                tags$label(class="control-label", "1. Which station do you want to analyse?")),
              div(
                class = "row direction",
                div(class = "col-xs-offset-1 col-xs-auto",
                    icon("arrow-right", lib = "font-awesome")),
                div(class = "col-xs-auto",
                    textOutput("clickedStation"))
              ),
              div(
                class = "row heading",
                tags$label(class="control-label", "2. Which surrounding stations do you want to compare it with?")),
              div(
                class = "row",
                div(class = "col-xs-offset-1 col-xs-11",
                  checkboxGroupInput("Type", "a. Filter by type",
                                   c("AWS" = 2,
                                     "Manual" = 1),
                                   selected = c(1, 2)))
              ),
              div(
                class = "row",
                div(class = "col-xs-offset-1 col-xs-10",
                  sliderInput("dateRange", "b. Filter by data availability time period",
                            min = as.Date("1981-01-01"),
                            max = Sys.Date(), value = c(as.Date("2000-01-01"),as.Date("2010-01-01"))))
              ),
              div(
                class = "row",
                div(class = "col-xs-offset-1 col-xs-11",
                tags$label(class="control-label", "c. Filter by location"))),
              div(id="stationTables",
                class = "row greedy",
                div(class = "col-xs-offset-1 col-xs-11",
                tabsetPanel(
                  tabPanel("Limit by distance",
                           div(id="radiusTabInput", class = "row heading",
                               div(class = "col-xs-10",
                               sliderInput("Radius", "Stations within a radius (km)", 0, 100, value =
                                             30))),
                          div(class = "row heading",
                              div(class = "col-xs-12",                  
                               tableOutput("clickedDistance")))),
                  tabPanel("Limit by number",
                           div(id="numberTabInput", class = "row heading",
                               div(class = "col-xs-10",
                               sliderInput("nr", "Number of nearest stations (#)", 0, 10, value =
                                             3))),
                           div(class = "row heading",
                               div(class = "col-xs-12", 
                               tableOutput("clickedNumber")))),
                  tabPanel("Limit by associated stations",
                    div(class = "col-xs-8 heading",
                        tableOutput("stationsNearby"))
                  )
                )
              )),
              div(
                class = "row",
                tags$label(class="control-label", "3. Start the analysis")),
              div(
                class = "row",
                div(class = "col-xs-offset-1 col-xs-6",
                    actionButton("actionAnalyse", "Analyse...", class="btn-primary")))
            )
          )
        ),
        tabPanel("Background information",
                 fluidRow(
                   includeMarkdown("Background_info_example.Rmd")
                 ))
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
