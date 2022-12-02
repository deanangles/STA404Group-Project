#
# Dean, Eric, Patricia
# STA 404
# Pei Wang
# 
#
##============================================================= Libraries
library(readxl)
##============================================================= Data
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

gamedata = read_xlsx("PerGameData.xlsx")

teamRatings <- read_xlsx("teamRatings.xlsx")

ratingAndStats <- merge(gamedata, teamRatings, by="Team")

ratingAndStats <- ratingAndStats %>%
  rename("FieldGoalPercentage" = `FG%`,
         "ThreePointPercentage" = `3P%`,
         "TwoPointPercentage"   = `2P%`)

save(perSeasonStats,teamRatings,ratingAndStats,file="basketballData.Rdata")