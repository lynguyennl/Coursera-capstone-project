library(shiny)
library(dplyr)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(markdown)

shinyUI(fluidPage(
    #theme
    theme = shinytheme("cosmo"),
    # Application title
    titlePanel("Text Prediction Model"),
    
    #tabs
    navbarPage("Tabs",
               
               
               tabPanel("Text Prediction",
                        
                        
                        fluidRow(
                            column(2),
                            column(8,
                                   tags$div(textInput("text", 
                                                      label = h3("Put in an english phrase: ")),
                                            br(),
                                            
                                            h3("Predicted Next Word:"),
                                            tags$span(style="color:indigo",
                                                      tags$strong(tags$h3(textOutput("guess_1")))),
                                            br(),
                                            
                                            h4("Second Guess:"),
                                            tags$span(style="color:grey",
                                                      tags$strong(tags$h4(textOutput("guess_2")))),
                                            br(),
                                            h4("Third Guess:"),
                                            tags$span(style="color:grey",
                                                      tags$strong(tags$h4(textOutput("guess_3")))),
                                            br(),
                                            align="center")
                            ),
                            column(2)
                        )
               ),         
               tabPanel("Plot of n-grams",
                        
                        fluidRow(
                            column(width = 5,
                                   uiOutput("ngramP")
                            ),
                            column(width = 5, align="center",
                                   sliderInput("n_terms",
                                               "Select number of n-grams to view:",
                                               min = 5,
                                               max = 50,
                                               value = 20)
                            )
                        ),
                        plotlyOutput("ngramPlot", height=600, width = 600)
                        
               ),
               
               tabPanel("Data table of n-grams",
                        h2("All sampled n-grams"),
                        fluidRow(
                            column(width = 3,
                                   uiOutput("ngramT")
                            )
                        ),
                        DT::dataTableOutput("ngramtable")
               )
    )
))