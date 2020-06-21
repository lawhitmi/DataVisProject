## Lucas Visualization ##
dat <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'

library(networkD3) # for sankeyNetwork call
library(dplyr)
library(tidyr) #separate
library(RColorBrewer)
library(stringr) # str_extract

### EXPLORE DATA ###
checkNA <- function (x) {
  return(sum(is.na(x)))
}
str(dat)
checkNA(dat$Birth.Place) # no missing values in birthplace
checkNA(dat$Year) # 27 missing alum years
checkNA(dat$Alma.Mater) # 1 missing Alma Mater
unique(dat$Alma.Mater) # 280 Universities 
# Note that multiple universities can be listed for graduate/undergraduate. Recommend splitting this to reduce levels for each education level
checkNA(dat$Graduate.Major) # 59 missing Graduate degrees
unique(dat$Graduate.Major)
checkNA(dat$Undergraduate.Major) # 22 missing undergraduate degrees
checkNA(dat$Military.Rank) # 150 missing. Note that a NA probably means no military services

# Look further at the Majors
sort(table(dat$Undergraduate.Major),decreasing=TRUE)
cumsum(sort(prop.table(table(dat$Undergraduate.Major)),decreasing=TRUE))
par(las=2,mar=c(12,4,1,1))
plot(sort(table(dat$Undergraduate.Major),decreasing=TRUE),cex.axis=0.75,main="Frequency of Undergraduate Majors (Sorted)")
levels(dat$Undergraduate.Major) #83 unique values

sort(table(dat$Graduate.Major),decreasing=TRUE)
cumsum(sort(prop.table(table(dat$Graduate.Major)),decreasing=TRUE))
par(las=2,mar=c(12,4,1,1))
plot(sort(table(dat$Graduate.Major),decreasing=TRUE),cex.axis=0.75,main="Frequency of Graduate Majors (Sorted)")
levels(dat$Graduate.Major) # 143 unique values

# Alma Mater
sort(table(dat$Alma.Mater),decreasing=TRUE)
cumsum(sort(prop.table(table(dat$Alma.Mater)),decreasing=TRUE))
par(las=2,mar=c(12,4,1,1))
plot(sort(table(dat$Alma.Mater),decreasing=TRUE),cex.axis=0.75,main="Frequency of Alma Mater (Sorted)")
levels(dat$Alma.Mater) # 280 unique values

investigate <- dat %>% separate("Alma.Mater",c("AM1","AM2","AM3","AM4","AM5","AM6"),sep=";")
sum(!is.na(investigate$AM3)) # 49 values in third spot
sum(!is.na(investigate$AM4)) # 5 values in 4th spot
sum(!is.na(investigate$AM5)) # 2 values in 5th spot
sum(!is.na(investigate$AM6)) # 1 value in 6th spot

# Military Branch
levels(dat$Military.Branch)
# Need to remove 'Reserves' and "(Retired)" as these aren't helpful to the sankey plot.


### PREPROCESSING ###
#Create a new column with two levels: USBorn or BornAbroad

dat <- dat %>% separate("Birth.Place", c("Birth.City", "Birth.Province"),sep=",")

assignBirthplace <- function (x) {
  print(trimws(x)[1])
  if (is.na(x)) {
    return("BornAbroad")
  }
  else if((nchar(trimws(x)[1])==2)){ #trimws is 'trim white space'
    return("USBorn")
  }
  else {
    return("BornAbroad")
  }
}

USborn <- lapply(dat$Birth.Province,FUN=assignBirthplace) # this command creates a 'list' which is a useless structure.
# Before adding this to the dataframe, the values must be 'unlisted' and converted to a factor type.
dat$USborn <- as.factor(unlist(USborn))

# Cleaning up the Alma Mater
dat <- dat %>% separate("Alma.Mater",c("AM1","AM2","AM3"),sep=";")
dat$AM1 <- as.factor(dat$AM1)
dat$AM2 <- as.factor(dat$AM2)
dat$AM3 <- as.factor(dat$AM3)
levels(dat$AM1)
levels(dat$AM2)
levels(dat$AM3)

# Cleansing the Military Branch
str_extract(dat$Military.Branch,"(\\s\\(.+\\))|(\\sReserves)")
dat$Military.Branch <- gsub("(\\s\\(.+\\))|(\\sReserves)","",dat$Military.Branch)
dat$Military.Branch <- gsub("Naval","Navy",dat$Military.Branch)
dat$Military.Branch <- as.factor(dat$Military.Branch)
levels(dat$Military.Branch)
str(dat$Military.Branch)

##### Sankey Plot #####

# EXAMPLE SANKEY PLOT ####
# Load energy projection data
# URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
# Energy <- jsonlite::fromJSON(URL)
# 
# # Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value), and a 'nodes' data frame that gives the name of each node.
# 
# # Thus we can plot it
# sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
#               Target = "target", Value = "value", NodeID = "name",
#               units = "TWh", fontSize = 12, nodeWidth = 30)

nodes <- c(levels(dat$USborn),levels(dat$Military.Branch),) # need to add nodes for majors, alma mater but these need to be paired down.


###### Conditional Density Plot ######

#Gender split of each Astronaut class
cdplot(Gender~Year,data=dat,col=brewer.pal(3,"Set3"))

#Foreign Born/US Born for each Astronaut class
cdplot(USborn~Year,data=dat,col=brewer.pal(3,"Set3"))

#Graduate Degree for each Astronaut class
cdplot(Graduate.Major~Year,data=dat,col=brewer.pal(12,"Set3"))

#Military Branch for each Astronaut class
cdplot(Military.Branch~Year,data=dat,col=brewer.pal(12,"Set3"))

#Alma Mater (Undergrad)
cdplot(AM1~Year,data=dat,col=brewer.pal(12,"Set3"))


