
## Datacamp
## Federico Rueda DatVis project
## NASA Astronauts, 1959-Present (as of April 2013)
## Which American astronaut has spent the most time in space?
## https://www.kaggle.com/nasa/astronaut-yearbook/data?select=astronauts.csv

## Group Report
## What are the questions my map will answer? 
## What things can I learn from the data?
## What do I want to communicate? Tell a story.

## Individual Report
## Why do you use this particular vis.? Colors? Instead of another option?
## Shapes? linetypes? 3D or not?
## The code is not going to be graded, think more in the output, why? message? etc.

library(stringr)
library(usmap)
library(ggplot2)
library(tibble)
library(eeptools)
library(dplyr)
library(gridExtra)
library(scales)
library(wesanderson)
library(RColorBrewer)
library(viridis)
library(reshape2)
library(plyr)
library(dplyr)
library(viridis)

rm(list=ls())
dat <- read.csv("datasets_934_1711_astronauts.csv")
str(dat) ## check structure

## PRE-PROCESSING

## Take a look at our variables of interest in terms of NA ,remove examples without Year value
table(is.na(dat$Year))
dat.complete <- dat[!is.na(dat$Year),]; #27 examples removed
head(dat.complete[,c("Year","Gender","Missions")])

## Calculate age at time of mission and convert Year to factor
head(dat.complete$Birth.Date)
head(dat.complete$Year)
dat.complete$AgeAt <- floor(age_calc(as.Date(dat.complete$Birth.Date,format='%m/%d/%Y'), 
                                     as.Date(paste(dat.complete$Year, 04, 30, sep = "-")),
                                     units = "years")) ## Create AgeAt time of mission
head(dat.complete$AgeAt)

# Create a dataframe to use for plotting in US Map
dat.plot <- dat.complete
str(dat.plot$Birth.Place) #check structure in order to extract using regex the state code
str_locate("Inglewood, CA", "[A-Z]{2}") #example
dat.plot$state <- str_extract(dat.plot$Birth.Place, "[A-Z]{2}") #add a column with state code

#Create dataframe with only female examples and other with male
dat.plot.female <- dat.plot[dat.plot$Gender=="Female",]
dat.plot.male <- dat.plot[dat.plot$Gender=="Male",]

#Create df with the count by state of female astronauts
dat.fem.sta<-as.data.frame(table(dat.plot.female$state))
colnames(dat.fem.sta)<-c("state","freq")
dat.fem.sta$freq <- as.factor(dat.fem.sta$freq)
head(dat.fem.sta)

# Create df with the count by state of male astronauts
dat.male.sta<-as.data.frame(table(dat.plot.male$state))
colnames(dat.male.sta)<-c("state","freq")
dat.male.sta$freq <- as.numeric(dat.male.sta$freq)
head(dat.male.sta)

# Pre-processing for proportion of male/female by NASA program (Not mission)
prog <- "Atlantis|Soyuz|Columbia|Endeavor|Discovery|Challenger"
dat.plot.female$Program <- str_extract(dat.plot.female$Missions, prog)
dat.plot.male$Program <- str_extract(dat.plot.male$Missions, prog)
dat.plot.program <- rbind(dat.plot.female,dat.plot.male)

## PLOTING

## INCLUDE: Distribution of Astronauts by Gender
p0 <- ggplot(data=dat.complete, aes(x=Gender,fill=Gender)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.35) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_minimal() + 
  xlab("Astronauts") + ylab("Count") +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="United States Astronauts from 1959 to 2013", 
       subtitle = "NASA selection by Gender", 
       caption = "Data Source: Kaggle NASA Astronauts"); p0

## INCLUDE: Proportion of Gender by year (BarPlot)
p1 <- ggplot(data=dat.complete, mapping=aes(x=as.factor(Year), fill=Gender)) +
  geom_bar(position=position_dodge2(preserve = "single"),
           mapping = aes(y = ..count.., group = Gender)) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_minimal() +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("NASA's Selection Year") +
  labs(title="United States Astronauts from 1959 to 2013", 
       subtitle = "NASA selection by year", 
       caption = "Data Source: Kaggle NASA Astronauts"); p1

grid.arrange(p0, p1, nrow = 1,bottom = "Data Source: Kaggle NASA Astronauts")

## TO INCLUDE: Age vs. Year colored by gender
ggplot(data=dat.complete, aes(x=as.factor(Year), y=AgeAt,col=Gender)) +
  geom_point(alpha=0.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_minimal() +
  ylab("Age") + xlab("NASA's Selection Year") +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="United States Astronauts from 1959 to 2013", 
       subtitle = "Age's distribution (at mission) by year and gender", 
       caption = "Data Source: Kaggle NASA Astronauts")

## TO INCLUDE: States with more female space flight hours
plot_usmap(labels=TRUE, data=dat.fem.sta, values="freq", 
                 color = "black", alpha=0.8) + 
  theme(legend.position = "right",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="United States Astronauts from 1959 to 2013", 
       subtitle = "Female Astronauts by State", 
       caption = "Data Source: Kaggle NASA Astronauts")+
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C",name="Count",
                     guide = guide_legend(reverse = TRUE))

## TO INCLUDE: proportion stacked bars Program by Gender
sub_df <- subset(dat.plot.program, Program==c("Atlantis","Soyuz","Columbia","Endeavor",
                                              "Discovery","Challenger"))

ggplot(data=sub_df, aes(x=Program,y=..count..,fill=Gender)) +
  geom_bar(position = "fill") +
  scale_color_viridis(discrete = TRUE, option = "C")+
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_minimal() +
  ylab("Proportion") + xlab("NASA Program") +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="United States Astronauts from 1959 to 2013", 
       subtitle = "Gender Proportion by NASA Program", 
       caption = "Data Source: Kaggle NASA Astronauts") +
  coord_flip ()
