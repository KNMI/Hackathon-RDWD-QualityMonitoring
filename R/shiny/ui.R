library(shiny)

navbarPage("Break Detection",
           tabPanel("Introduction",
                    fluidRow(
                      h1('Introduction'),
                      p('Alarm if there is a break')
                      ),
                    
                    fluidRow(
                      column(6,plotOutput("plot")),
                      column(6,h4("quantiles"),
                             tableOutput("summary"))
                    )),
                    tabPanel("Differences",
                    fluidRow(
                      column(6,h1('There are large differences!')
                        ))
           ),
           tabPanel("Something else",
                    fluidRow(
                      column(5,h1('Another Panel'))

           )
))
           