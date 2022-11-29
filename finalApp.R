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
perSeasonStats<-perSeasonStats%>%
  mutate(Ht=as.numeric(72+day(Ht)))
colnames(perSeasonStats)[11]<-"ThreePointers"
colnames(perSeasonStats)[12]<-"ThreePointersAttempted"
colnames(perSeasonStats)[24]<-"FieldGoalPercentage"
colnames(perSeasonStats)[25]<-"ThreePointPercentage"
colnames(perSeasonStats)[26]<-"FreeThrowPercentage"
perSeasonVar <- c("Average Points Per Game"="PTS",
                  "Average Assists Per Game" ="AST",
                  "Average Total Rebounds Per Game"="TRB",
                  "Average Steals Per Game"="STL",
                  "Average Bloacks Per Game"="BLK",
                  "Average Turnovers Per Game"="TOV",
                  "Average Age"="Age",
                  "Average Height (In)"="Ht",
                  "Average Weight (Lbs)"="Wt",
                  "Average Made Field Goals Per Game"="FG",
                  "Average Field Goals Attempted Per Game"="FGA",
                  "Average Field Goal Percentage"="FieldGoalPercentage",
                  "Average Made Three Pointers Per Game"="ThreePointers",
                  "Average Three Pointers Attempted Per Game"="ThreePointersAttempted",
                  "Average Three Point Percentage"="ThreePointPercentage",
                  "Average Made Free Throw Per Game"="FT",
                  "Average Free Throws Attempted Per Game"="FTA",
                  "Average Free Throw Percentage"="FreeThrowPercentage")

gamedata = read_xlsx("PerGameData.xlsx")
teamRatings <- read_xlsx("teamRatings.xlsx")

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
                  choices=perSeasonVar,
                  selected = "PTS"),
      sliderInput("Year","Year",min = 1946, max = 2022, value = c(1946, 2022)),
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
    perSeasonStatsLimited <- perSeasonStats %>% filter(Season>=input$Year[1]&Season<=input$Year)
    
    ggplot(data=perSeasonStatsLimited)+
      geom_line(aes_string(x="Season",y=input$stat))+
      theme_bw()+
      labs(title = paste("The line plot of",names(perSeasonVar)[perSeasonVar==input$stat],"over NBA Seasons"),
           x=names(perSeasonVar)[perSeasonVar==input$stat])
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