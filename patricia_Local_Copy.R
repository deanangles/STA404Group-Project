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
perSeasonStats <- read_xlsx("C:\\STA 404\\SeasonsData.xlsx",skip=1)
perSeasonStats<-perSeasonStats%>%
  mutate(Season=as.Date(paste(substr(Season,1,4),"01","01",sep="-")))%>%
  mutate(Season=year(Season))
gamedata = read_xlsx("C:\\STA 404\\PerGameData.xlsx")
teamRatings <- read_xlsx("C:\\STA 404\\teamRatings.xlsx")

ratingAndStats <- merge(gamedata, teamRatings, by="Team")

ratingAndStats <- ratingAndStats %>%
  rename("FieldGoalPercentage" = `FG%`,
         "ThreePointPercentage" = `3P%`,
         "TwoPointPercentage"   = `2P%`)

percentData <- c("FieldGoalPercentage", "ThreePointPercentage", "TwoPointPercentage")

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
      selectInput(inputId = "compStat1",
                  label = "Choose the First Percentage:",
                  choices = percentData, 
                  selected = "FG%"),
      uiOutput(outputId = "compStat2"),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("linePlot"),
      plotlyOutput('plot1'),
      plotlyOutput("scatterPlot")
    )
  )
)

# Define server logic required to draw plots
server <- function(input, output) {
  
  output$compStat2 <- renderUI({
    selectInput(inputId = "compStat2",
                label = "Choose the Second Percentage:",
                choices = percentData[percentData!=input$compStat1])
  })
  output$scatterPlot <- renderPlotly({
    ggplotly(
      ggplot(ratingAndStats)+
        geom_point(aes_string(x = input$compStat1, 
                              y = input$compStat2))+
        labs(x = names(percentData)[percentData == input$compStat1],
             y = names(percentData)[percentData == input$compStat2],
             title = paste("The scatterplot of", input$compStat1,"and",
                           input$compStat2, "in the 2021 NBA Season"))+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5)
        ))
    
  })
  
  
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
