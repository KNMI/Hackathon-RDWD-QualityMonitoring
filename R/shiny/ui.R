library(shiny)

#title above navbar


navbarPage("Inhomogenity detection",
           titlePanel("Hello Shiny!"),
           tabPanel("Introduction",
                    fluidRow(
                      h1('Alerts'),
                      p('Alarm if there is a break')
                      ),
                    
                    fluidRow(
                      column(6,plotOutput("plot")),
                      column(6,h4("quantiles"),
                             tableOutput("summary"))
                    )),
                    tabPanel("Background information",
                    fluidRow(
                      column(6,h1('There are large differences!'),
                             text('Alert! What is going on? Parameters, Meetnetwerken, Significant? What to do? Some basic info
                                  Extra: What is the process behind this alert?')
                        ))
           )

           )

           