
## Federico Rueda DatVis project (individual report)
## NASA Astronauts, 1959-Present (as of April 2013)
## https://www.kaggle.com/nasa/astronaut-yearbook/data?select=astronauts.csv

library(stringr)
library(usmap)
library(ggplot2)
library(tibble)
library(gridExtra)
library(scales)
library(wesanderson)
library(RColorBrewer)
library(viridis)
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)

rm(list=ls())

## read dataset and inspect structure
dat <- read.csv("datasets_934_1711_astronauts.csv")
names(dat)
str(dat)

## Pre-processing stage
## Take a look at our variables of interest in terms of NA, remove examples without a "Year" value
table(is.na(dat$Year))
dat.complete <- dat[!is.na(dat$Year),]; #27 examples removed
head(dat.complete[,c("Year","Gender","Missions")])

## Convert Year into YY/MM/DD (We just hve the Year, add April 30)
Year <- paste0(as.character(dat.complete$Year),"/04/30")

## Convert Brith.Date to format YY/MM/DD
Aux <- as.Date(dat.complete$Birth.Date,format='%m/%d/%Y')
Birth <- format(Aux, "%Y-%m-%d")

## define function to calculate AgeAt based on Birthdate and NASA's Year of election
calc_age <- function(birthDate, refDate) {
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
} 

## Calculate AgeAt column (Age at year of selection)
dat.complete$AgeAt <- calc_age(Birth,Year)

## Create a dataframe to use for plotting using US Map library
dat.plot <- dat.complete
str(dat.plot$Birth.Place) ## check structure in order to extract using regex the state code
## this is an example: str_locate("Inglewood, CA", "[A-Z]{2}")
dat.plot$state <- str_extract(dat.plot$Birth.Place, "[A-Z]{2}") ## add "a column with state code"state" column

## Create dataframe with only female examples and other with male
dat.plot.female <- dat.plot[dat.plot$Gender=="Female",]
dat.plot.male <- dat.plot[dat.plot$Gender=="Male",]

## Create df with the count by state of female astronauts
dat.fem.sta<-as.data.frame(table(dat.plot.female$state))
colnames(dat.fem.sta)<-c("state","freq")
dat.fem.sta$freq <- as.factor(dat.fem.sta$freq)
head(dat.fem.sta)

## Create df with the count by state of male astronauts
dat.male.sta<-as.data.frame(table(dat.plot.male$state))
colnames(dat.male.sta)<-c("state","freq")
dat.male.sta$freq <- as.numeric(dat.male.sta$freq)
head(dat.male.sta)

## Create df for plotting which states have more bitrthplaces of astronauts
dat.all.sta <- as.data.frame(table(dat.plot$state))
colnames(dat.all.sta)<-c("state","freq")
head(dat.all.sta)

## Calculate proportion of male/female by NASA program (Not mission)
prog <- "Atlantis|Soyuz|Columbia|Endeavor|Discovery|Challenger"
dat.plot$Program <- str_extract(dat.plot$Missions, prog)
dat.plot <- dat.plot[complete.cases(dat.plot$Program),] # remove those without program
positions <- c("Challenger","Atlantis","Columbia","Soyuz","Endeavor","Discovery") # most female percentage by program

## Code for answering some questions and also confirm data
filter(dat.complete,AgeAt==20) 
filter(dat.complete,AgeAt==24) 
filter(dat.complete,AgeAt>50)
filter(dat.complete,Gender=="Female" & Year==1978)
filter(dat.complete,Year==1985 & Gender=="Female")[,c("Name","AgeAt")]

## Calculate number of astronauts per million inhabitants, import column with calculation
dat_pop <- read.csv2("US_Pop.csv",sep = ";")
if (dat.all.sta$state == dat_pop$State_Abb) dat.all.sta$ast_mill <- dat_pop$Ast_mill ## Copy data to actual df
head(dat.all.sta) ## now we have the astronauts per million inhabitants by state

## ----------------------------------------
## Generating the visualizations

## Gender distribution of NASA's astronauts - BAR CHART
p1 <- ggplot(data=dat.complete, aes(x=Gender,fill=Gender)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..),position=position_dodge(width=0.9), 
            vjust=-0.35, size = 4.5) + # add count values above each bar
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
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
  ylab("Count"); p1 # Omit gender xlab (it is obvious female and male colors)

## Gender distribution by year of selection (1959 - 2013)
p2 <- ggplot(data=dat.complete, mapping=aes(x=as.factor(Year), fill=Gender)) +
  geom_bar(position=position_dodge2(preserve = "single"),
           mapping = aes(y = ..count.., group = Gender)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
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
  ylab("Count"); p2

# Gender distribution by year of selection (1959 - 2013) - with blank years
p3 <- ggplot(data=dat.complete, mapping=aes(x=Year, fill=Gender)) +
  geom_bar(position=position_dodge2(preserve = "single"),
           mapping = aes(y = ..count.., group = Gender)) +
  scale_x_continuous(labels = c("'59","'62","'63","'65","'66","'67","'69","'78","'80","'84","'85",
                                "'87","'90","'92","'95","'96","'98","'00","'04","'09"),
                     breaks=c(1959,1962,1963,1965,1966,1967,1969,1978,1980,1984,1985,1987,1990,
                              1992,1995,1996,1998,2000,2004,2009)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title.x = element_blank(), # no need of x title it is obvious from subtitle
        axis.text.x= element_text(size = 10),
        axis.text.y= element_text(size = 12),
        axis.title.y = element_text(size=12)
        ) +
  labs(title="Gender distribution by year of selection (1959 - 2013)", 
       subtitle = "Female astronauts presence since 1978", 
       caption = "Data Source: Kaggle NASA Astronauts") +
  ylab("Count"); p3

## NASA Astronaut Selection (1959 - 2013) - Age's distribution by year
p4 <- ggplot(data=dat.complete, aes(x=as.factor(Year), y=AgeAt,col=Gender)) +
  geom_jitter(alpha=1, position = position_jitter(width = 0.2, height = 0.2)) + ## jittered points showing better density
  stat_summary(aes(y = AgeAt, group=1), geom="line", fun.data = "mean_se", 
               colour="black", linetype = "dashed") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  ylab("Age") + xlab("NASA's Selection Year") +
  theme(legend.position = "bottom", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12, face = "italic"),
        axis.title.x = element_blank(), # no need of x title it is obvious from subtitle
        axis.text.x= element_text(size = 12),
        axis.text.y= element_text(size = 12),
        axis.title.y = element_text(size=12)) +
  labs(title="NASA Astronaut Selection (1959 - 2013) - Age's distribution by year", 
       subtitle = "Female astronauts below average dashed line", 
       caption = "Data Source: Kaggle NASA Astronauts"); p4

## Astronauts distribution by birthplaces Frequency (1959 - 2013)
p5 <- plot_usmap(labels=TRUE, data=dat.all.sta, values="freq", 
           color = "black", alpha=0.8) + 
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 14, face = "italic")) +
  labs(title="Astronauts distribution by birthplaces (1959 - 2013)", 
       subtitle = "Most of birthplaces in the East region; California stands out in West", 
       caption = "Data Source: Kaggle NASA Astronauts",
       fill="Frequency") +
  scale_fill_gradient(low="white",high="darkgreen"); p5

# Astronauts distribution by birthplaces Proportion (1959 - 2013)
p6 <- plot_usmap(labels=TRUE, data=dat.all.sta, values="ast_mill", 
           color = "black", alpha=0.8) + 
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 14, face = "italic")) +
  labs(title="Astronauts distribution by birthplaces (1959 - 2013)", 
       subtitle = "Considerably more astronauts per mill. in the East region", 
       caption = "Data Source: Kaggle NASA Astronauts",
       fill="Astronauts/million") +
  scale_fill_gradient(low="white",high="darkred"); p6

## Gender proportion by NASA's programs (1959 -2013)
p7 <- ggplot(data=dat.plot, aes(x=Program,y=..count..,fill=Gender)) +
  geom_bar(position = "fill") +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2") +
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
       subtitle = "Discovery was the program with the highest female astronauts participation", 
       caption = "Data Source: Kaggle NASA Astronauts") +
  coord_flip () +
  scale_x_discrete(limits = positions); p7 # order by higer percentage of female astronauts

