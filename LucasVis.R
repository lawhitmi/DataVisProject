## Lucas Visualization ##

library(networkD3) # for sankeyNetwork call
library(dplyr)
library(htmltools)
library(tidyr) #separate
library(RColorBrewer)
library(stringr) # str_extract

#### LOAD DATA ####
dat <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'


#### PREPROCESSING ####
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

# Cleansing Undergrad Majors
dat$Undergraduate.Major <- sapply(dat$Undergraduate.Major,as.character)
dat$Undergraduate.Major[is.na(dat$Undergraduate.Major)] <- "NoUndergradDeg"

unique(dat$Undergraduate.Major) #84

nrow(dat[dat$Undergraduate.Major=="Physics",]) #35
dat[grepl("Physics",dat$Undergraduate.Major),]$Undergraduate.Major <- "Physics"
nrow(dat[dat$Undergraduate.Major=="Physics",]) #54

nrow(dat[dat$Undergraduate.Major=="Aerospace Engineering",]) #33
dat[grepl("Aerospace",dat$Undergraduate.Major),]$Undergraduate.Major <- "Aerospace"
nrow(dat[dat$Undergraduate.Major=="Aerospace",]) #34

nrow(dat[dat$Undergraduate.Major=="Mechanical Engineering",]) #30
dat[grepl("Mechanic",dat$Undergraduate.Major),]$Undergraduate.Major <- "Mechanical Engineering"
nrow(dat[dat$Undergraduate.Major=="Mechanical Engineering",]) #34

nrow(dat[dat$Undergraduate.Major=="Aeronautical Engineering",]) #28
dat[grepl("Aeronautic",dat$Undergraduate.Major),]$Undergraduate.Major <- "Aeronautics"
nrow(dat[dat$Undergraduate.Major=="Aeronautics",]) #39

nrow(dat[dat$Undergraduate.Major=="Electrical Engineering",]) #23
dat[grepl("Electric",dat$Undergraduate.Major),]$Undergraduate.Major <- "Electrical Engineering"
nrow(dat[dat$Undergraduate.Major=="Electrical Engineering",]) #26

unique(dat$Undergraduate.Major) #64

dat$Undergraduate.Major <- ifelse(dat$Undergraduate.Major %in% c("Physics","Aerospace","Mechanical Engineering","Aeronautics","Electrical Engineering","NoUndergradDeg"), dat$Undergraduate.Major, "Other")
unique(dat$Undergraduate.Major) #7

# Cleansing Graduate Majors
dat$Graduate.Major <- sapply(dat$Graduate.Major,as.character)
dat$Graduate.Major[is.na(dat$Graduate.Major)] <- "NoGraduateDeg"

unique(dat$Graduate.Major) #144

nrow(dat[dat$Graduate.Major=="Physics",]) #15
dat[grepl("Physics",dat$Graduate.Major),]$Graduate.Major <- "Physics"
nrow(dat[dat$Graduate.Major=="Physics",]) #29

nrow(dat[dat$Graduate.Major=="Aerospace Engineering",]) #21
dat[grepl("Aerospace",dat$Graduate.Major),]$Graduate.Major <- "Aerospace"
nrow(dat[dat$Graduate.Major=="Aerospace",]) #35

nrow(dat[dat$Graduate.Major=="Mechanical Engineering",]) #13
dat[grepl("Mechanic",dat$Graduate.Major),]$Graduate.Major <- "Mechanical Engineering"
nrow(dat[dat$Graduate.Major=="Mechanical Engineering",]) #27

nrow(dat[dat$Graduate.Major=="Aeronautical Engineering",]) #27
dat[grepl("Aeronautic",dat$Graduate.Major),]$Graduate.Major <- "Aeronautics"
nrow(dat[dat$Graduate.Major=="Aeronautics",]) #58

nrow(dat[dat$Graduate.Major=="Medicine",]) #16
dat[grepl("Medicin",dat$Graduate.Major),]$Graduate.Major <- "Medicine"
nrow(dat[dat$Graduate.Major=="Medicine",]) #29

unique(dat$Graduate.Major) #79

dat$Graduate.Major <- ifelse(dat$Graduate.Major %in% c("Physics","Aerospace","Mechanical Engineering","Aeronautics","Medicine","NoGraduateDeg"), dat$Graduate.Major, "Other")
unique(dat$Graduate.Major) #7

# Cleansing the Military Branch
str_extract(dat$Military.Branch,"(\\s\\(.+\\))|(\\sReserves)")
dat$Military.Branch <- gsub("(\\s\\(.+\\))|(\\sReserves)","",dat$Military.Branch)
dat$Military.Branch <- gsub("Naval","Navy",dat$Military.Branch)
dat$Military.Branch <- as.factor(dat$Military.Branch)
dat$Military.Branch <- as.character((dat$Military.Branch))
dat[is.na(dat$Military.Branch),]$Military.Branch <- "None"
dat$Military.Branch <- as.factor(dat$Military.Branch)
levels(dat$Military.Branch)
str(dat$Military.Branch)


# EXAMPLE SANKEY PLOT ####
# Load energy projection data
URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)

# Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value),
# and a 'nodes' data frame that gives the name of each node.

# Thus we can plot it
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)
# Colour links
Energy$links$energy_type <- sub(' .*', '',
                                Energy$nodes[Energy$links$source + 1, 'name'])

sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              LinkGroup = 'energy_type', NodeGroup = NULL)

#### SANKEY PLOT ####
# Generate Node Names df
nodes <- data.frame(c(levels(dat$Gender), levels(dat$USborn), levels(dat$Military.Branch), unique(dat$Undergraduate.Major), unique(dat$Graduate.Major))) 
names(nodes) <- c("nodes")
nodes$index <- 1:nrow(nodes)
nodes$concat <- paste0(nodes$index,",",nodes$nodes)

# Generate Link df
end1 <- length(levels(dat$Gender))
end2 <- end1+length(levels(dat$USborn))
end3 <- end2+length(levels(dat$Military.Branch))
end4 <- end3+length(unique(dat$Undergraduate.Major))
end5 <- end4+length(unique(dat$Graduate.Major))
level1 <- crossing(nodes[1:end1,"concat"], nodes[(end1+1):end2,"concat"])
level2 <- crossing(nodes[(end1+1):end2,"concat"], nodes[(end2+1):end3,"concat"])
level3 <- crossing(nodes[(end2+1):end3,"concat"], nodes[(end3+1):end4,"concat"])
level4 <- crossing(nodes[(end3+1):end4,"concat"], nodes[(end4+1):end5,"concat"])
level1$level <- 1
level2$level <- 2
level3$level <- 3
level4$level <- 4

names(level1) <- c("sourcename","targetname","level")
names(level2) <- c("sourcename","targetname","level")
names(level3) <- c("sourcename","targetname","level")
names(level4) <- c("sourcename","targetname","level")

links <- rbind(level1,level2,level3,level4)
links <- links %>% separate("targetname",c("target","targetname"),sep=",")
links <- links %>% separate("sourcename",c("source", "sourcename"), sep=",")

# Calculate values
level1.calc <- function (x,y) {
  return(nrow(dat[dat$Gender==x & dat$USborn==y,]))
} 
level2.calc <- function (x,y) {
  return(nrow(dat[dat$USborn==x & dat$Military.Branch==y,]))
}
level3.calc <- function (x,y) {
  return(nrow(dat[dat$Military.Branch==x & dat$Undergraduate.Major==y,]))
}
level4.calc <- function (x,y) {
  return(nrow(dat[dat$Undergraduate.Major==x & dat$Graduate.Major==y,]))
}
valueCalc <- function (x,y,level) {
  if (level==1) return(level1.calc(x,y))
  if (level==2) return(level2.calc(x,y))
  if (level==3) return(level3.calc(x,y))
  if (level==4) return(level4.calc(x,y))
}
level.values <- c(1:nrow(links))
for (i in 1:nrow(links)) {
  level.values[i] <- valueCalc(links[i,]$sourcename,links[i,]$targetname,links[i,]$level)
}

links$values <- level.values
links$source <- as.numeric(links$source) - 1 # must be zero indexed
links$target <- as.numeric(links$target) - 1 # must be zero indexed

# Plot Sankey

# Source and Target in links df must be integer indices in the nodes df 
# The df must also be zero-indexed
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
            Target = "target",  NodeID = "nodes", Value = "values",
            fontSize = 12, nodeWidth = 10) 


# trying out some options
#NodeGroup = NULL - all nodes are grey
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target",  NodeID = "nodes", Value = "values",
              fontSize = 12, nodeWidth = 10, NodeGroup=NULL)

#LinkGroup = "level"
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target",  NodeID = "nodes", Value = "values",
              fontSize = 12, nodeWidth = 10, NodeGroup = Null)



#### CONDITIONAL DENSITY PLOTS ####

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


