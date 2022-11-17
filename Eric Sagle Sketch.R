#
# Dean, Eric, Patricia
# STA 404
# Pei Wang
# 
#
##============================================================= Libraries
library(tidyverse)
library(shiny)
library(lubridate)
library(ggthemes)
library(readxl)
library(tidyverse)
library(plotly)
library(GGally)
library(ggplot2)
##============================================================= Data
#perGameStats <- read_xlsx("C:\\STA 404\\nbaData2021.xlsx")
perSeasonStats <- read_xlsx("SeasonsData.xlsx",skip=1)
perSeasonStats<-perSeasonStats%>%
  mutate(Season=as.Date(paste(substr(Season,1,4),"01","01",sep="-")))%>%
  mutate(Season=year(Season))
gamedata = read_xlsx("PerGameData.xlsx")
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
      plotOutput("linePlot"),
      plotlyOutput('plot1')
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
  
  output$plot1 = renderPlotly({
   
    scatter = ggplot(data = gamedata) +
      geom_point(aes(color = Team, y=PTS, x= TOV, size = FG , label= `FG%`,
                     text=paste0("Made ", FG, " of ", FGA, " shots on average.")),
                 alpha=0.5)+
      labs(y="Average Points per Game", x="Average Turnovers per Game") +
      scale_y_continuous() + 
      scale_color_discrete() +
      theme_bw() + 
      theme(legend.position = "none")
    
    ggplotly(scatter, type = 'scatter', mode = 'markers', tooltip = c("Team","label", "y" , "x", "text"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
