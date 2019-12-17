#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#need to remove data that is at the bottom of the well 

library(shiny)
#library(dplyr)
library(tidyverse)
library(lubridate)

# Define UI for application that plots wells for a specific time period
ui <- fluidPage(

    # Application title
    titlePanel("Watershed 3 well visualization"),
    
    #dateRangeInput("date", strong("Date range"), start = "2010-08-01", end = "2013-01-10",
                   #min = "2010-08-01", max = "2019-10-10"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("wells", "enter well names with spaces in between",
                      value = "A5")

        ),

        # Show a plot of the water level data
        fluidRow(
          column(width = 10, offset = 1, class = "well",
                 h4("To zoom: Click and drag box, then double click. Double click plot to zoom back out."),
           plotOutput("wellplot", 
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )))
        )
    )
)

# Define server logic required to draw a line plot 
server <- function(input, output) {
  
  #setwd('C:/Users/KT/Documents/R/HBwellStart/Data')
      
    welldata <- read_csv("welldatahourly.csv")
    
    ranges <- reactiveValues(x = as.POSIXct(c(start = "2010-08-01", end = "2013-01-10")))
    maxrange <- reactiveValues(x = as.POSIXct(c(start = "2010-08-01", end = "2013-01-10")))
    
    output$wellplot <- renderPlot({
      ID <- strsplit(input$wells, " ")[[1]]
      
      #start <- input$date[1]
      
      #end <- input$date[2]
      
      wells <- filter(welldata, Well == ID) #, date >= start, date <= end)
      
      #xmin <- input$plot_brush$xmin
      
      #xmax <- input$plot_brush$xmax
      
      ggplot(data = wells, mapping = aes(x = date, y = wtdepth, color = Well))+
        geom_line()+
        scale_y_reverse()+
        ylab("Water Table Depth (cm)")+
        coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+
        #xlim(ranges$x)+
        theme_classic()
    })
    
   # observeEvent(input$plot1_dblclick,
    observeEvent(input$plot1_dblclick,
      {
      brush <- input$plot1_brush
      if (!is.null(brush)) 
        {
        ranges$x <- c(brush$xmin, brush$xmax)
        
      } else {
        #ranges$x <- NULL
        ranges$x <- maxrange$x
        
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
