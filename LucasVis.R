## Lucas Visualization ##
dat <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'

library(networkD3) # for sankeyNetwork call
library(dplyr)
library(tidyr)

#Explore Data
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

#Create a new column with two levels: USBorn: True or False
splitCity <- function (x) {
  return(strsplit(as.character(x),",")[1])
}
birthplace.area <- lapply(as.list(dat$Birth.Place), FUN=splitCity)

dat <- dat %>% separate("Birth.Place", c("Birth.City", "Birth.Province"),sep=",")

assignBirthplace <- function (x) {
  print(trimws(x)[1])
  if (is.na(x)) {
    return(FALSE)
  }
  else if((nchar(trimws(x)[1])==2)){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

USborn <- lapply(dat$Birth.Province,FUN=assignBirthplace) # this command creates a 'list' which is a useless structure.
# Before adding this to the dataframe, the values must be 'unlisted' and converted to a factor type.
dat$USborn <- as.factor(unlist(USborn))


##### Sankey Plot #####

# EXAMPLE SANKEY PLOT ####
# # Load energy projection data
# URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
# Energy <- jsonlite::fromJSON(URL)
# 
# # Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value), and a 'nodes' data frame that gives the name of each node.
# 
# # Thus we can plot it
# sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
#               Target = "target", Value = "value", NodeID = "name",
#               units = "TWh", fontSize = 12, nodeWidth = 30)




###### Conditional Density Plot ######

#Gender split of each Astronaut class
cdplot(Gender~Year,data=dat,col=blues9)

cdplot(USborn~Year,data=dat,col=blues9)

