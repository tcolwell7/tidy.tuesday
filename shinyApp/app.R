library(shiny)
library(shinythemes)
library(shinydashboard)
library(bslib)


source("plotFunctions.R")

# Define UI for application
ui <- 
    fluidPage(
      titlePanel("R shiny dashbaord to host exploritory plots utilising new packages and techniques"),
      # sidebarPanel(
      #   h3(""),
      #   actionButton("do_move", "Do Move", icon = icon("play")),
      #   actionButton("new_game", "New game", icon = icon("refresh")),
      #   width = 3
      # ),
      navbarPage(
        title = "Select tab to explore plots", theme = shinytheme("cosmo"), # use bs_theme for the theme
        collapsible = TRUE, # allow navbar to collapse on smaller screens
        # create navbar with drop down menu
        tabPanel(icon("home")),
        tabPanel(
            "Tab1",
            "first plot test",
            ggiraphOutput("plot1"),
            ),
        tabPanel(
          "Tab2",
          "Second plot text",
          fluidRow(
            column(width=6,ggiraphOutput("plot2")),
            column(width=6,plotOutput("plot3"))
            )
          
          # fluidRow(
          #   box(
          #     width = 6, status = "info",
          #     ggiraphOutput("plot2")
          #     ),
          #   box(
          #     width = 6, status = "primary",
          #     plotOutput("plot3")
          #    )
          #   ),
          ),
        tabPanel("Tab3"),
        navbarMenu("More", icon = icon("info-circle"),
               tabPanel("School Types & Rankings", fluid = TRUE),
               tabPanel("About", fluid = TRUE)
        ),
        navbarMenu("Nav-bar-menu", #icon = icon("chart-bar"),
          tabPanel("drop down 1", fluid = TRUE, h1("TEST")),
          tabPanel("drop down 2", fluid = TRUE, h3("Test text"))
          )

        # 
        # navbarPage("NCAA Swimming", theme = shinytheme("lumen"),
        #            tabPanel("Program Finder", fluid = TRUE, icon = icon("globe-americas")),
        #            
        #            
        #            tabPanel("Program Comparisons", fluid = TRUE, icon = icon("swimmer")),
        #            
        #            
        #            navbarMenu("Divisions Comparisons", icon = icon("chart-bar"),
        #                       tabPanel("Times Comparision Between Divisions", fluid = TRUE),
        #                       
        #                       tabPanel("NCAA Regulation Differences By Division", fluid = TRUE)),
        #            
        #            
        #            navbarMenu("More", icon = icon("info-circle"),
        #                       tabPanel("School Types & Rankings", fluid = TRUE),
        #                       
        #                       tabPanel("About", fluid = TRUE)
        #                       
        #            )
        
      
  ) # end navBar
) # end fluid Page

# Define server ----------


server <- function(input, output) {
    
  
  output$plot1 <- renderggiraph({
    
    plot1()
    
  })
  
  
  output$plot2 <- renderggiraph({
    
    plot2()
  })
  
  
  output$plot3 <- renderPlot({
    
    plot3()
      
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
