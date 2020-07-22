#Cleaning the environment 
rm(list=ls())

#Import Libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(fmsb)

#Read the csv
data <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'

#Check the data
str(data)
colnames(data)
theme_set(theme_minimal())
head(data)

###############
# Female Enrollment Vs Space Walks

data.gender <- select(data, Year, Gender,Space.Flight..hr.,Space.Walks..hr.)
data.gender <- filter(data.gender, Year !=  'NA')
data.gender <- data.gender[order(data.gender$Year),]

TotalRecruit <- as.data.frame(table(data.gender[,1:2]))
colnames(TotalRecruit) <- c('Year','Gender','Total')
TotalRecruit$Year <- as.numeric(as.character(TotalRecruit$Year))
TotalRecruit <- TotalRecruit[order(TotalRecruit[,1],TotalRecruit[,2]),]
Recruit <- as.data.frame((table(data.gender$Year)))
colnames(Recruit) <- c('Year','Total')

for (row in 1:nrow(Recruit)) {
  year <- Recruit[row,1]
  total <- Recruit[row,2]
  
  for (i in 1:nrow(TotalRecruit)){
    if(year == TotalRecruit[i,1]){
      TotalRecruit[i,4] <- total
      TotalRecruit[i,5] <- TotalRecruit[i,3] / total
    }
  }
}
colnames(TotalRecruit) <- c('Year','Gender','Total_Gender_Enroll','Total_Enroll_Year','Proportion_Enroll')

TotalWalkHours <- data.gender %>% 
  group_by(Year) %>% 
  summarise_at(vars(Space.Walks..hr.),
               list(TotalFlight=sum))

data.hourGender <- data.gender %>% 
  group_by(Year,Gender) %>% 
  summarise_at(vars(Space.Walks..hr.),
               list(TotalWalk=sum))

for (row in 1:nrow(TotalWalkHours)) {
  year <- TotalWalkHours[row,1]
  total <- TotalWalkHours[row,2]
  
  for (i in 1:nrow(data.hourGender)){
    if(year == data.hourGender[i,1]){
      data.hourGender[i,4] <- (data.hourGender[i,3] / total)
    }
  }
}
colnames(data.hourGender) <- c('Year','Gender','TotalWalk',"Proportion_Walk")

#Take only the Females
TotalProportions <- merge(TotalRecruit,data.hourGender,by=c("Year",'Gender'))
FemaleProportions <- TotalProportions[TotalProportions$Gender == "Female", ]
FemaleProportions <- FemaleProportions[,-c(3,4,6)]

#Pivot the table to create the radar
piv1 = dcast(FemaleProportions, Gender ~ Year, value.var = c("Proportion_Enroll"))
piv2 = dcast(FemaleProportions, Gender ~ Year, value.var = c("Proportion_Walk"))
PivotData <- rbind(piv1,piv2)
PivotData[is.na(PivotData)] <- 0
PivotData = PivotData[2:ncol(PivotData)]
rownames(PivotData) <- c("Enroll", "Walk")

Min = 0
Max = 1
PivotData <- rbind(rep(Max,20) , rep(Min,20) , PivotData)

#Colors defined for Flight and enroll
colors_border=c( '#1B9E77E6','#D95F02E6' )
colors_in=c( '#1B9E7766','#D95F0266')

#Radarchart Plot
radarchart( PivotData  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,1), cglwd=0.6,
            #custom labels
            vlcex=0.8 ,title=paste("Female Enrollment Vs Space Walks"),
)

# Add a legend
legend(x=1.2, y=1.2, legend = rownames(PivotData[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=2.0, pt.cex=3)


###############
# Female Enrollment Vs Space Flights

data.gender <- select(data, Year, Gender,Space.Flight..hr.,Space.Walks..hr.)
data.gender <- filter(data.gender, Year !=  'NA')
data.gender <- data.gender[order(data.gender$Year),]

TotalRecruit <- as.data.frame(table(data.gender[,1:2]))
colnames(TotalRecruit) <- c('Year','Gender','Total')
TotalRecruit$Year <- as.numeric(as.character(TotalRecruit$Year))
TotalRecruit <- TotalRecruit[order(TotalRecruit[,1],TotalRecruit[,2]),]
Recruit <- as.data.frame((table(data.gender$Year)))
colnames(Recruit) <- c('Year','Total')

for (row in 1:nrow(Recruit)) {
  year <- Recruit[row,1]
  total <- Recruit[row,2]
  
  for (i in 1:nrow(TotalRecruit)){
    if(year == TotalRecruit[i,1]){
      TotalRecruit[i,4] <- total
      TotalRecruit[i,5] <- TotalRecruit[i,3] / total
    }
  }
}
colnames(TotalRecruit) <- c('Year','Gender','Total_Gender_Enroll','Total_Enroll_Year','Proportion_Enroll')

TotalFlightHours <- data.gender %>% 
  group_by(Year) %>% 
  summarise_at(vars(Space.Flight..hr.),
               list(TotalFlight=sum))

data.hourGender <- data.gender %>% 
  group_by(Year,Gender) %>% 
  summarise_at(vars(Space.Flight..hr.),
               list(TotalFlight=sum))

for (row in 1:nrow(TotalFlightHours)) {
  year <- TotalFlightHours[row,1]
  total <- TotalFlightHours[row,2]
  
  for (i in 1:nrow(data.hourGender)){
    if(year == data.hourGender[i,1]){
      data.hourGender[i,4] <- (data.hourGender[i,3] / total)
    }
  }
}
colnames(data.hourGender) <- c('Year','Gender','TotalWalk',"Proportion_Flight")


#Select onlye Female
TotalProportions <- merge(TotalRecruit,data.hourGender,by=c("Year",'Gender'))
FemaleProportions <- TotalProportions[TotalProportions$Gender == "Female", ]
FemaleProportions <- FemaleProportions[,-c(3,4,6)]

#Pivot the data in order to create the radarchart
piv1 = dcast(FemaleProportions, Gender ~ Year, value.var = c("Proportion_Enroll"))
piv2 = dcast(FemaleProportions, Gender ~ Year, value.var = c("Proportion_Flight"))
PivotData <- rbind(piv1,piv2)
PivotData[is.na(PivotData)] <- 0
PivotData = PivotData[2:ncol(PivotData)]
rownames(PivotData) <- c("Enroll", "Flight")

Min = 0
Max = 1

PivotData <- rbind(rep(Max,20) , rep(Min,20) , PivotData)

#Colors defined for Flight and enroll
colors_border=c( '#1B9E77E6','#D95F02E6' )
colors_in=c( '#1B9E7766','#D95F0266')

#Radarchart Plot
radarchart( PivotData  , axistype=1,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
            #custom the grid
            cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,1,1), cglwd=0.6,
            #custom labels
            vlcex=0.8 ,title=paste("Female Enrollment Vs Space Flights"),
)

# Add a legend
legend(x=1.2, y=1.2, legend = rownames(PivotData[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=2.0, pt.cex=3)

