library(shiny)

#title above navbar

fluidPage(

  h1("Inhomogenity detection", align = "center"),

navbarPage("",
           tabPanel("Alerts",
                    fluidRow(
                      
                      includeMarkdown("Warning_example.Rmd")
                      ),
                    
                    
                    
                    fluidRow(
                      
                      column(6,class = "well",style = "background-color:red",plotOutput("plot")),
                      column(6,h4("quantiles"),
                             tableOutput("summary"))
                    )),
                    tabPanel("Background information",
                    fluidRow(
                      
                        includeMarkdown("Background_info_example.Rmd")
                        )
           )

           )

)

           