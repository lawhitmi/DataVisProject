###### Libs 

library(fmsb)
library(ggplot2)
library(dplyr)
library(tidyr)
library(packcircles)
library(latticeExtra)
library(reshape2)

#Start cleaning the environment 
rm(list=ls())

#Read the csv
data <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'

#Check the data
str(data)
colnames(data)
theme_set(theme_minimal())
head(data)

###############
# Graph No.1 "How many number of flights per year"
# First only take the 2 columns that i need and remove the NA

data.flights <- select(data, Year, Space.Flights)
data.flightsNotNA <- filter(data.flights, Year !=  'NA')

TotalFlights <- aggregate(x = data.flightsNotNA$Space.Flights,by = list(data.flightsNotNA$Year),FUN = sum)              

ggplot(data = TotalFlights, aes(x = Group.1, y = x))+
  geom_line(color = "#00AFBB", size = 2)

###############
# Graph No.2 "How many hours they spend flying in space compare to the walking hours in space"  

data.flights <- select(data, Year, Space.Flight..hr.,Space.Walks..hr.)
data.flightsNotNA <- filter(data.flights, Year !=  'NA')


gby1 <- data.flightsNotNA %>% 
          group_by(Year) %>% 
            summarise_at(vars(Space.Flight..hr.),
                          list(TotalFlight=sum))
    
gby2 <- data.flightsNotNA %>% 
          group_by(Year) %>% 
            summarise_at(vars(Space.Walks..hr.),
                          list(TotalWalking=sum))

data.hours <- merge(gby1,gby2,by="Year")
data.hours[2:3] <- lapply(data.hours[2:3], function(x) c(scale(x)))


xyplot(TotalFlight + TotalWalking ~ Year, data.hours, type = "l", col=c("steelblue", "#69b3a2") , lwd=2)

###############
# Graph No.3 "How many hours they spend flying in space"
# Look for a specific reason for 1996

data.hours <- merge(gby1,gby2,by="Year")

packing <- circleProgressiveLayout(data.hours$TotalFlight, sizetype='area')
dataPlot <- cbind(data.hours, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

ggplot() + 
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  # Add text in the center of each bubble + control its size
  geom_text(data = dataPlot, aes(x, y, size=TotalFlight, label = Year)) +
  scale_size_continuous(range = c(1,4)) +
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal() +
  ggtitle('Total Hours Flights by Year')

###############
# Graph No.4 "How is the proportion of women spending space flying hours comparing to male" 

data.gender <- select(data, Year, Gender,Space.Flight..hr.,Space.Walks..hr.)
data.genderNotNA <- filter(data.gender, Year !=  'NA')


gby1 <- data.genderNotNA %>% 
          group_by(Year,Gender) %>% 
                summarise_at(vars(Space.Flight..hr.),
                    list(TotalFlight=sum))

gby2 <- data.genderNotNA %>% 
              group_by(Year,Gender) %>% 
                summarise_at(vars(Space.Walks..hr.),
                             list(TotalWalk=sum))


data.hourGender <- merge(gby1,gby2,by=c("Year","Gender"))

data.hourGender[3:4] <- lapply(data.hourGender[3:4], function(x) c(scale(x)))

PivotData = dcast(data.hourGender, Gender ~ Year, value.var = "TotalWalk")

PivotData[is.na(PivotData)] <- 0

PivotData = PivotData[2:ncol(PivotData)]
rownames(PivotData) <- c("Female", "Male")

Min = min(data.hourGender[4])
Max = max(data.hourGender[4])

PivotData <- rbind(rep(Max,20) , rep(Min,20) , PivotData)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

radarchart( PivotData  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 ,title=paste("Space Walk Hours. Male Vs Female "),
)

# Add a legend
legend(x=1, y=1, legend = rownames(PivotData[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.0, pt.cex=3)




###### Graph 5.

theme_set(theme_minimal())
data <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'
data.gender <- select(data, Year, Gender,Space.Flight..hr.,Space.Walks..hr.)
data.genderNotNA <- filter(data.gender, Year !=  'NA')


gby1 <- data.genderNotNA %>% 
  group_by(Year,Gender) %>% 
  summarise_at(vars(Space.Flight..hr.),
               list(TotalFlight=sum))

gby2 <- data.genderNotNA %>% 
  group_by(Year,Gender) %>% 
  summarise_at(vars(Space.Walks..hr.),
               list(TotalWalk=sum))


data.hourGender <- merge(gby1,gby2,by=c("Year","Gender"))

uniqueValues <- unique(data.hourGender[,1])

from <- 'Mision'
to <- 'Mision.Year'
edgesNasa <- data.frame(from,to)

name <- c('Mision','Mision.Year')
size <- c(0,0)
shortname <-  c('Mision','Mision.Year')
verticesNasa <- data.frame(name,size,shortname)


for (row in 1:length(uniqueValues)) {
  from <- 'Mision.Year'
  to <- paste('Mision.Year',toString(uniqueValues[row]), sep='.')
  df1 <- data.frame(from,to)
  edgesNasa <- rbind(edgesNasa,df1)
  
  from <- paste('Mision.Year',toString(uniqueValues[row]), sep='.')
  to <- paste('Mision.Year',toString(uniqueValues[row]),'Male.Hours', sep='.')
  df2 <- data.frame(from,to)
  edgesNasa <- rbind(edgesNasa,df2)
  
  
  to <- paste('Mision.Year',toString(uniqueValues[row]),'Female.Hours', sep='.')
  df3 <- data.frame(from,to)
  edgesNasa <- rbind(edgesNasa,df3)
  
  
  ### Other DataFrame
  name <- paste('Mision.Year',toString(uniqueValues[row]), sep='.')
  size <- 0
  shortname <- paste('Mision.Year',toString(uniqueValues[row]), sep='.')
  df1 <- data.frame(name,size, shortname)
  verticesNasa <- rbind(verticesNasa,df1)
  
  name <- paste('Mision.Year',toString(uniqueValues[row]),'Male.Hours', sep='.')
  size <- 0
  shortname <- paste('Male',toString(uniqueValues[row]), sep='.')
  df2 <- data.frame(name,size, shortname)
  verticesNasa <- rbind(verticesNasa,df2)
  
  name <- paste('Mision.Year',toString(uniqueValues[row]),'Female.Hours', sep='.')
  size <- 0
  shortname <- paste('Female',toString(uniqueValues[row]), sep='.')
  df3 <- data.frame(name,size, shortname)
  verticesNasa <- rbind(verticesNasa,df3)
}

for (row in 1:nrow(data.hourGender)) {
  year <- data.hourGender[row,1]
  gender <- data.hourGender[row,2]
  total <- data.hourGender[row,3]
  nameString <- paste('Mision.Year',toString(year),toString(gender),'Hours', sep='.')
  for (i in 1:nrow(verticesNasa)){
    if(nameString == verticesNasa[i,1]){
      print(paste('lo encontro en la posicion',toString(i)))
      verticesNasa[i,2] <- total
    }
  }
}

verticesNasa <- rbind(verticesNasa[1:2,],verticesNasa[24:nrow(verticesNasa),])
edgesNasa <- rbind(edgesNasa[1,],edgesNasa[23:nrow(edgesNasa),])

mygraph <- graph_from_data_frame( edgesNasa, vertices=verticesNasa )

ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle() +
  theme_void()

# Left: color depends of depth
p <- ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle(aes(fill = depth)) +
  theme_void() + 
  theme(legend.position="FALSE")
p
# Adjust color palette: viridis
p + scale_fill_viridis()
# Adjust color palette: colorBrewer
p + scale_fill_distiller(palette = "RdPu") 


# Rebuild the graph object
mygraph <- graph_from_data_frame( edgesNasa, vertices=verticesNasa )

# left
ggraph(mygraph, layout = 'circlepack', weight=size ) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_text( aes(label=shortname, filter=leaf, fill=depth, size=size)) +
  theme_void() + 
  theme(legend.position="FALSE") + 
  scale_fill_viridis()

ggraph(mygraph, layout = 'circlepack', weight=size ) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_label( aes(label=shortname, filter=leaf, size=size)) +
  theme_void() + 
  theme(legend.position="FALSE") + 
  scale_fill_viridis() +
  ggtitle('Space Flying Hours.', subtitle = 'Male Vs Female')




