#Start cleaning everything
rm(list=ls())

#Read the csv
data <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'

#Check the data
str(data)
colnames(data)

###### Libs 

library(fmsb)
library(ggplot2)
library(dplyr)
library(tidyr)
library(packcircles)
library(latticeExtra)


theme_set(theme_minimal())
# Lets try to plot 'Space.Flight..hr.' and 'Space.Walks..hr.' by 'Year' the  dataset
head(data)

# Try 1
# Check the data for how many number of flights per year
# First only take the 2 columns that i need and remove the NA
selected_col <- select(data, Year, Space.Flights)
head(selected_col)

NotNA <- filter(selected_col, Year !=  'NA')
head(NotNA)


TotalFlightsRecords <- NotNA %>% group_by(Year) %>% tally() #Test

TotalFlights <- NotNA %>% group_by(Year) %>% summarize(Total = sum(Space.Flights, na.rm = TRUE))

NoSpaceFlights <- merge(TotalFlights,TotalFlightsRecords,by="Year")

#Basic graph about number of flights 
ggplot(data = NoSpaceFlights, aes(x = Year, y = Total))+
  geom_line(color = "#00AFBB", size = 2)

###############
# Try 2
# Try to compare the flight hours with the walk hours by year 



selected_col <- select(data, Year, Space.Flight..hr.,Space.Walks..hr.)
head(selected_col)

NotNA <- filter(selected_col, Year !=  'NA')
head(NotNA)

gby1 <- NotNA %>% group_by(Year) %>% summarize(TotalHrFlights = sum(Space.Flight..hr., na.rm = TRUE))
gby2 <- NotNA %>% group_by(Year) %>% summarize(TotalHrWalks = sum(Space.Walks..hr., na.rm = TRUE))

NewData <- merge(gby1,gby2,by="Year")
NewData = transform(NewData, TotalHrFlights = as.numeric(TotalHrFlights))
NewData[2:3] <- lapply(NewData[2:3], function(x) c(scale(x)))


# usual line chart
xyplot(TotalHrFlights + TotalHrWalks ~ Year, NewData, type = "l", col=c("steelblue", "#69b3a2") , lwd=2)

###############
# Try 3 

selected_col <- select(data, Year, Space.Flight..hr.,Space.Walks..hr.)
NotNA <- filter(selected_col, Year !=  'NA')

gby1 <- NotNA %>% group_by(Year) %>% summarize(TotalHrFlights = sum(Space.Flight..hr., na.rm = TRUE))
gby2 <- NotNA %>% group_by(Year) %>% summarize(TotalHrWalks = sum(Space.Walks..hr., na.rm = TRUE))

NewData <- merge(gby1,gby2,by="Year")
NewData = transform(NewData, TotalHrFlights = as.numeric(TotalHrFlights))

packing <- circleProgressiveLayout(NewData$TotalHrFlights, sizetype='area')
dataPlot <- cbind(NewData, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

ggplot() + 
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  # Add text in the center of each bubble + control its size
  geom_text(data = dataPlot, aes(x, y, size=TotalHrFlights, label = Year)) +
  scale_size_continuous(range = c(1,4)) +
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal() +
  ggtitle('Total Hours Flights by Year')

###############
# Try 4

selected_col <- select(data, Year, Gender,Space.Flight..hr.,Space.Walks..hr.)
NotNA <- filter(selected_col, Year !=  'NA')
gby1 <- NotNA %>% group_by(Year,Gender) %>% summarize(TotalHrFlights = sum(Space.Flight..hr., na.rm = TRUE))
gby2 <- NotNA %>% group_by(Year,Gender) %>% summarize(TotalHrWalks = sum(Space.Walks..hr., na.rm = TRUE))
NewData <- merge(gby1,gby2,by=c("Year","Gender"))
PivotData = dcast(NewData, Gender ~ Year, value.var = "TotalHrFlights")

Female = PivotData[1,2:ncol(PivotData)]
Male = PivotData[2,2:ncol(PivotData)]

Min = min(NewData[3])
Max = max(NewData[3])

Female <- rbind(rep(Max,20) , rep(Min,20) , Female)

radarchart(Female)

Male <- rbind(rep(Max,20) , rep(Min,20) , Male)

radarchart(Male)


radarchart( Female  , axistype=1 , 
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)


radarchart( Male  , axistype=1 , 
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

###############
# Try 5

selected_col <- select(data, Year, Gender,Space.Flight..hr.,Space.Walks..hr.)
NotNA <- filter(selected_col, Year !=  'NA')
gby1 <- NotNA %>% group_by(Year,Gender) %>% summarize(TotalHrFlights = sum(Space.Flight..hr., na.rm = TRUE))
gby2 <- NotNA %>% group_by(Year,Gender) %>% summarize(TotalHrWalks = sum(Space.Walks..hr., na.rm = TRUE))
NewData <- merge(gby1,gby2,by=c("Year","Gender"))
PivotData = dcast(NewData, Gender ~ Year, value.var = "TotalHrFlights")
PivotData[is.na(PivotData)] <- 0

data = PivotData[2:ncol(PivotData)]
rownames(data) <- c("Female", "Male")

Min = min(NewData[3])
Max = max(NewData[3])

data <- rbind(rep(Max,20) , rep(Min,20) , data)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
