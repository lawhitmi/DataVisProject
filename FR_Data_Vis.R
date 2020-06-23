
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
## All technical details, why selected plots, decisions, aesthetics, etc.

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
                                     units = "years")) ## Create AgeAt time of NASA selection
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
dat.plot$Program <- str_extract(dat.plot$Missions, prog)
dat.plot <- dat.plot[complete.cases(dat.plot$Program),] # remove those without program
positions <- c("Challenger","Atlantis","Columbia","Soyuz","Endeavor","Discovery") # most female percentage by program

## PLOTING

## INCLUDE: Gender distribution of NASA's astronauts - BAR CHART
p0 <- ggplot(data=dat.complete, aes(x=Gender,fill=Gender)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..),position=position_dodge(width=0.9), 
            vjust=-0.35, size = 4.5) + # add count values above each bar
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_minimal() + 
  theme(legend.position = "none", #remove all legend element, no need 
        plot.title = element_text(color = "black", size = 16, face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title.x = element_blank(), # no need of x title it is obvious
        axis.text.x= element_text(size = 12),
        axis.text.y= element_text(size = 12),
        axis.title.y = element_text(size=12)) +
  labs(title="Gender distribution of NASA's astronauts (1959 - 2013)", 
       subtitle = "Almost 6 times more male astronauts", 
       caption = "Data Source: Kaggle NASA Astronauts") +
  ylab("Count"); p0 # Omit gender xlab (it is obvious female and male colors)

# legend.position = "bottom", legend.text = element_text(size = 12), OMITTED
# legend.title = element_text(size = 12),

## INCLUDE: Gender distribution by year of selection - BAR CHART
p1 <- ggplot(data=dat.complete, mapping=aes(x=as.factor(Year), fill=Gender)) +
  geom_bar(position=position_dodge2(preserve = "single"),
           mapping = aes(y = ..count.., group = Gender)) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold",
                     hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title.x = element_blank(), # no need of x title it is obvious from subtitle
        axis.text.x= element_text(size = 12),
        axis.text.y= element_text(size = 12),
        axis.title.y = element_text(size=12)) +
  labs(title="Gender distribution by year of selection (1959 - 2013)", 
       subtitle = "Female astronauts presence since 1978", 
       caption = "Data Source: Kaggle NASA Astronauts") +
  ylab("Count"); p1

grid.arrange(p0, p1, nrow = 1) # option for plotting in the same device, similar to par()

## TO INCLUDE: Age's distribution by year of selection and gender  STRIP CHART
ggplot(data=dat.complete, aes(x=as.factor(Year), y=AgeAt,col=Gender)) +
  geom_jitter(alpha=0.65, position = position_jitter(width = 0.2, height = 0.2)) +  # points have been jittered along 
                                                                                # the x axis to better show the density
  stat_summary(aes(y = AgeAt, group=1), geom="line", fun = mean, 
               colour="black", linetype = "dashed") +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_minimal() +
  ylab("Age") + xlab("NASA's Selection Year") +
  theme(legend.position = "bottom", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title.x = element_blank(), # no need of x title it is obvious from subtitle
        axis.text.x= element_text(size = 12),
        axis.text.y= element_text(size = 12),
        axis.title.y = element_text(size=12)) +
  labs(title="Age's distribution by year of NASA's selection (1959 - 2013)", 
       subtitle = "Female astronauts below average dashed line", 
       caption = "Data Source: Kaggle NASA Astronauts")

# stat_summary(aes(y = AgeAt, group=1), geom="line", fun = mean, colour="grey"), 
# group=1, linetype = "dashed") + # Age average by year
# geom_violin(aes(x=as.factor(Year), col = Gender), trim = FALSE, alpha = 0.65, position="identity") + # 

## TO INCLUDE: States with more female space flight hours
plot_usmap(labels=TRUE, data=dat.fem.sta, values="freq", 
                 color = "black", alpha=0.8) + 
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic")) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C", element_blank(),
                     guide = guide_legend(reverse = TRUE)) +
  labs(title="Female astronauts distribution by U.S. sates (1959 - 2013)", 
       subtitle = "Most of bithplaces in the East region", 
       caption = "Data Source: Kaggle NASA Astronauts")

## TO INCLUDE: proportion stacked bars Program by Gender
ggplot(data=dat.plot, aes(x=Program,y=..count..,fill=Gender)) +
  geom_bar(position = "fill") +
  scale_color_viridis(discrete = TRUE, option = "C")+
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_minimal() +
  ylab("Proportion") + xlab("NASA's Programs") +
  theme(legend.position = "bottom", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title.x = element_blank(), # no need of x title it is obvious from subtitle
        axis.text.x= element_text(size = 12),
        axis.text.y= element_text(size = 12),
        axis.title.y = element_text(size=12)) +
  labs(title="Gender proportion by NASA's programs (1959 -2013)", 
       subtitle = "Half of astronauts in Soyuz program were women", 
       caption = "Data Source: Kaggle NASA Astronauts") +
  coord_flip () +
  scale_x_discrete(limits = positions) # order by higer percentage of female astronauts

####### EXTRA ########

# Soyuz       1975
# Columbia    1981
# Challenger  1983
# Discovery   1984
# Atlantis    1985
# Endeavor    1992

# Mercury program	1959	
# Gemini program	1963	
# Apollo program	1961	
# Skylab	1973	
# Apollo–Soyuz Test Project	1975	
# Space Shuttle	1981	
# Shuttle-Mir Program	1995	
# International Space Station	1998	
# Project Constellation	2003	
# Artemis program	2017	
