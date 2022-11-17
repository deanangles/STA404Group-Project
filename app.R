#
# Dean, Eric, Patricia
# STA 404
# Pei Wang
# 
#
##============================================================= Libraries
library(shiny)
library(tidyverse)
library(readxl)
##============================================================= Data
#perGameStats <- read_xlsx("C:\\STA 404\\nbaData2021.xlsx")
perSeasonStats <- read_xlsx("SeasonsData.xlsx",skip=1)
perSeasonStats<-perSeasonStats%>%
  mutate(Season=as.Date(paste(substr(Season,1,4),"01","01",sep="-")))%>%
  mutate(Season=year(Season))
##============================================================= Shiny
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "stat",
                  label = "Choose the stat to measure:",
                   choices = c("PTS",
                               "AST",
                               "TRB"),
                  selected = "PTS"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$linePlot <- renderPlot({
    ggplot(data=perSeasonStats)+
      geom_line(aes_string(x="Season",y=input$stat))+
      theme_bw()+
      labs(title = paste("The line plot of",input$stat,"over NBA Seasons"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
