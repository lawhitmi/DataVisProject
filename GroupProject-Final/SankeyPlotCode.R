## Lucas Visualization - Sankey Plot##

library(networkD3) # for sankeyNetwork call
library(dplyr)
library(htmltools)
library(tidyr) #separate
library(RColorBrewer)
library(ggplot2)

#### LOAD DATA ####
dat <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'


#### PREPROCESSING ####
#Create a new column with two levels: USBorn or BornAbroad
cleanBirth <- function(dat) {
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
  return(dat)
}
# Cleansing Undergrad Majors
cleanUndergrad <- function(dat) {
  dat$Undergraduate.Major <- sapply(dat$Undergraduate.Major,as.character)
  dat$Undergraduate.Major[is.na(dat$Undergraduate.Major)] <- "NoUndergradDeg"
  dat$Undergraduate.Orig <- dat$Undergraduate.Major
  unique(dat$Undergraduate.Major) #84
  
  nrow(dat[dat$Undergraduate.Major=="Physics",]) #35
  dat[grepl("Physics",dat$Undergraduate.Major),]$Undergraduate.Major <- "Physics/Maths"
  dat[grepl("Math",dat$Undergraduate.Major),]$Undergraduate.Major <- "Physics/Maths"
  nrow(dat[dat$Undergraduate.Major=="Physics/Maths",]) #54
  
  nrow(dat[dat$Undergraduate.Major=="Aerospace Engineering",]) #33
  dat[grepl("Aerospace",dat$Undergraduate.Major),]$Undergraduate.Major <- "Aerospace"
  dat[grepl("Aeronautic",dat$Undergraduate.Major),]$Undergraduate.Major <- "Aerospace"
  dat[grepl("Astronaut",dat$Undergraduate.Major),]$Undergraduate.Major <- "Aerospace"
  nrow(dat[dat$Undergraduate.Major=="Aerospace",]) #34
  
  # nrow(dat[dat$Undergraduate.Major=="Mechanical Engineering",]) #30
  # dat[grepl("Mechanic",dat$Undergraduate.Major),]$Undergraduate.Major <- "Mechanical Engr"
  # nrow(dat[dat$Undergraduate.Major=="Mechanical Engr",]) #34
  # 
  # nrow(dat[dat$Undergraduate.Major=="Electrical Engineering",]) #23
  # dat[grepl("Electric",dat$Undergraduate.Major),]$Undergraduate.Major <- "Electrical Engr"
  # nrow(dat[dat$Undergraduate.Major=="Electrical Engr",]) #26
  
  dat[grepl("ology",dat$Undergraduate.Major),]$Undergraduate.Major <- "Sciences"
  dat[grepl("osophy",dat$Undergraduate.Major),]$Undergraduate.Major <- "Sciences"
  dat[grepl("Science",dat$Undergraduate.Major),]$Undergraduate.Major <- "Sciences"
  dat[grepl("science",dat$Undergraduate.Major),]$Undergraduate.Major <- "Sciences"
  dat[grepl("istry",dat$Undergraduate.Major),]$Undergraduate.Major <- "Sciences"
  dat[grepl("onomy",dat$Undergraduate.Major),]$Undergraduate.Major <- "Sciences"
  dat[grepl("Nutrition",dat$Undergraduate.Major),]$Undergraduate.Major <- "Sciences"
  
  dat[grepl("Engineer",dat$Undergraduate.Major),]$Undergraduate.Major <-"Engineering"
  
  
  unique(dat$Undergraduate.Major) #64
  
  dat$Undergraduate.Major <- ifelse(dat$Undergraduate.Major %in% c("Physics/Maths","Aerospace","Engineering","NoUndergradDeg","Sciences"), dat$Undergraduate.Major, "Other")
  unique(dat$Undergraduate.Major) #7
  return(dat)
}
# Cleansing Graduate Majors
cleanGrad <- function(dat) {
  dat$Graduate.Major <- sapply(dat$Graduate.Major,as.character)
  dat$Graduate.Major[is.na(dat$Graduate.Major)] <- "NoGraduateDeg"
  
  unique(dat$Graduate.Major) #144
  
  nrow(dat[dat$Graduate.Major=="Physics",]) #15
  dat[grepl("Physics",dat$Graduate.Major),]$Graduate.Major <- "Physics/Maths"
  dat[grepl("Math",dat$Graduate.Major),]$Graduate.Major <- "Physics/Maths"
  nrow(dat[dat$Graduate.Major=="Physics",]) #29
  
  nrow(dat[dat$Graduate.Major=="Aerospace Engineering",]) #21
  dat[grepl("Aerospace",dat$Graduate.Major),]$Graduate.Major <- "Aerospace"
  dat[grepl("Aeronautic",dat$Graduate.Major),]$Graduate.Major <- "Aerospace"
  dat[grepl("Astronaut",dat$Graduate.Major),]$Graduate.Major <- "Aerospace"
  nrow(dat[dat$Graduate.Major=="Aerospace",]) #35
  
  nrow(dat[dat$Graduate.Major=="Mechanical Engineering",]) #13
  dat[grepl("Engineer",dat$Graduate.Major),]$Graduate.Major <- "Engineering"
  dat[grepl("engineer",dat$Graduate.Major),]$Graduate.Major <- "Engineering"
  
  nrow(dat[dat$Graduate.Major=="Engineering",]) #27
  
  dat[grepl("Medicin",dat$Graduate.Major),]$Graduate.Major <- "Medicine"
  dat[grepl("ology",dat$Graduate.Major),]$Graduate.Major <- "Sciences"
  dat[grepl("Science",dat$Graduate.Major),]$Graduate.Major <- "Sciences"
  dat[grepl("istry",dat$Graduate.Major),]$Graduate.Major <- "Sciences"
  dat[grepl("onomy",dat$Graduate.Major),]$Graduate.Major <- "Sciences"
  dat[grepl("science",dat$Graduate.Major),]$Graduate.Major <- "Sciences"
  dat[grepl("graphy",dat$Graduate.Major),]$Graduate.Major <- "Sciences"
  
  unique(dat$Graduate.Major) #79
  
  dat$Graduate.Major <- ifelse(dat$Graduate.Major %in% c("Physics/Maths","Aerospace","Engineering","Medicine","Sciences","NoGraduateDeg"), dat$Graduate.Major, "Other")
  unique(dat$Graduate.Major) #7
  return(dat)
}
# Cleansing the Military Branch
cleanMil <- function(dat) {
  dat$Military.Branch <- as.factor(dat$Military.Branch)
  dat$Military.Branch <- as.character((dat$Military.Branch))
  dat[!is.na(dat$Military.Branch),]$Military.Branch <- "Military"
  dat[is.na(dat$Military.Branch),]$Military.Branch <- "Civilian"
  dat$Military.Branch <- as.factor(dat$Military.Branch)
  levels(dat$Military.Branch)
  str(dat$Military.Branch)
  return(dat)
} # Bin into "Military" and "Civilian"

dat <- cleanMil(cleanGrad(cleanUndergrad(cleanBirth(dat))))

#### PLOT FOR MAJOR GROUPINGS ####
groups=data.frame(table(dat$Undergraduate.Orig,dat$Undergraduate.Major))
groups <- groups[groups$Freq!=0,]
ggplot(groups, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="stack",stat="identity")+
  theme(legend.position = "none")+
  scale_color_brewer(palette="Set3")

#### SANKEY PLOT ####
# Generate Node and Link dataframes
sankeyPrep1 <- function(dat) {
  nodes <- data.frame(c(levels(dat$Gender), levels(dat$USborn), "Military",levels(dat$Military.Branch), unique(dat$Undergraduate.Major), unique(dat$Graduate.Major))) 
  names(nodes) <- c("nodes")
  nodes$index <- 1:nrow(nodes)
  nodes$concat <- paste0(nodes$index,",",nodes$nodes)
  
  # Generate Link df
  end1 <- length(levels(dat$Gender))
  end2 <- end1+length(levels(dat$USborn))
  end3 <- end2+2
  end4 <- end3+length(levels(dat$Military.Branch))-1
  end5 <- end4+length(unique(dat$Undergraduate.Major))
  end6 <- end5+length(unique(dat$Graduate.Major))
  level1 <- crossing(nodes[1:end1,"concat"], nodes[(end1+1):end2,"concat"])
  level2 <- crossing(nodes[(end1+1):end2,"concat"], nodes[(end2+1):end3,"concat"])
  level3 <- crossing(nodes[end2+1,"concat"],nodes[(end3+1):end4,"concat"])
  level4 <- crossing(nodes[end2+2,"concat"],nodes[(end4+1):end5,"concat"])
  level5 <- crossing(nodes[(end3+1):end4,"concat"],nodes[(end4+1):end5,"concat"])
  level6 <- crossing(nodes[(end4+1):end5,"concat"], nodes[(end5+1):end6,"concat"])
  level1$level <- 1
  level2$level <- 2
  level3$level <- 3
  level4$level <- 4
  level5$level <- 5
  level6$level <- 6
  
  names(level1) <- c("sourcename","targetname","level")
  names(level2) <- c("sourcename","targetname","level")
  names(level3) <- c("sourcename","targetname","level")
  names(level4) <- c("sourcename","targetname","level")
  names(level5) <- c("sourcename","targetname","level")
  names(level6) <- c("sourcename","targetname","level")
  
  links <- rbind(level1,level2,level3,level4,level5,level6)
  links <- links %>% separate("targetname",c("target","targetname"),sep=",")
  links <- links %>% separate("sourcename",c("source", "sourcename"), sep=",")
  
  # Calculate values
  level1.calc <- function (x,y) {
    return(nrow(dat[dat$Gender==x & dat$USborn==y,]))
  } 
  level2.calc <- function (x,y) {
    if (y=='Military') {
      return(nrow(dat[dat$USborn==x & dat$Military.Branch %in% c("US Air Force","US Navy","US Marine Corps","US Army","US Coast Guard"),]))
    }
    else {
      return(nrow(dat[dat$USborn==x & dat$Military.Branch==y,]))
    }
  }
  level3.calc <- function (y) {
    return(nrow(dat[dat$Military.Branch==y,]))
  }
  level45.calc <- function (x,y) {
    return(nrow(dat[dat$Military.Branch==x & dat$Undergraduate.Major==y,]))
  }
  level6.calc <- function (x,y) {
    return(nrow(dat[dat$Undergraduate.Major==x & dat$Graduate.Major==y,]))
  }
  valueCalc <- function (x,y,level) {
    if (level==1) return(level1.calc(x,y))
    if (level==2) return(level2.calc(x,y))
    if (level==3) return(level3.calc(y))
    if (level==4) return(level45.calc(x,y))
    if (level==5) return(level45.calc(x,y))
    if (level==6) return(level6.calc(x,y))
  }
  level.values <- c(1:nrow(links))
  for (i in 1:nrow(links)) {
    level.values[i] <- valueCalc(links[i,]$sourcename,links[i,]$targetname,links[i,]$level)
  }
  
  links$values <- level.values
  links$source <- as.numeric(links$source) - 1 # must be zero indexed
  links$target <- as.numeric(links$target) - 1 # must be zero indexed
  return(list(links=links,nodes=nodes))
} # Adds breakout layer for military branches
sankeyPrep <- function(dat) {
  nodes <- data.frame(c(levels(dat$USborn), levels(dat$Gender), levels(dat$Military.Branch), unique(dat$Undergraduate.Major), unique(dat$Graduate.Major))) 
  names(nodes) <- c("nodes")
  nodes <- data.frame(nodes[nodes$nodes!="NoUndergradDeg",]) #Remove this here as it is filtered out with the threshold later
  names(nodes) <- c("nodes")
  nodes$index <- 1:nrow(nodes)
  nodes$concat <- paste0(nodes$index,",",nodes$nodes)
  
  # Generate Link df
  end1 <- length(levels(dat$USborn))
  end2 <- end1+length(levels(dat$Gender))
  end3 <- end2+length(levels(dat$Military.Branch))
  end4 <- end3+length(unique(dat$Undergraduate.Major))-1 # -1 is to compensate for removing NoUndergradDeg
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
    return(nrow(dat[dat$USborn==x & dat$Gender==y,]))
  } 
  level2.calc <- function (x,y) {
    return(nrow(dat[dat$Gender==x & dat$Military.Branch==y,]))
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
  return(list(links=links,nodes=nodes))
} # Swaps the gender and birthplace levels - for overall sankey only
sank <- sankeyPrep(dat)

#LinkGroup and Colors 

sank$links <- sank$links[sank$links$values>4,] # remove links with value less than this amount to clean up plot

sank$links$linkgroup <- rep("notpath",nrow(sank$links))
sank$links[c(3,7,13,18),]$linkgroup <- "path" # manually set desired path to be highlighted

my_color <- 'd3.scaleOrdinal() .domain(["path", "notpath","Male","Female","USBorn","BornAbroad"]) 
    .range(["#FB8072", "#C3C2C4","#D95F02", "#1B9E77", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F","#7570B3" ,"#E7298A", "#66A61E", "#E6AB02", "#A6761D" ,"#666666"])'

sn <- sankeyNetwork(Links = sank$links, Nodes = sank$nodes, Source = "source",
                    Target = "target",  NodeID = "nodes", Value = "values",
                    fontSize = 18, nodeWidth = 10, LinkGroup = "linkgroup",iterations=1000, 
                    colourScale = my_color, nodePadding = 30,sinksRight = FALSE)
sn

# Code below used for splitting into 2 sankey plots, one pre/one post-induction of women.
sankeyPrep3 <- function(dat) {
  nodes <- data.frame(c(levels(dat$USborn), levels(dat$Gender), levels(dat$Military.Branch), unique(dat$Undergraduate.Major), unique(dat$Graduate.Major))) 
  names(nodes) <- c("nodes")
  nodes <- data.frame(nodes[nodes$nodes!="NoUndergradDeg",]) #Remove this here as it is filtered out with the threshold later
  names(nodes) <- c("nodes")
  nodes$index <- 1:nrow(nodes)
  nodes$concat <- paste0(nodes$index,",",nodes$nodes)
  
  # Generate Link df
  end1 <- length(levels(dat$USborn))
  end2 <- end1+length(levels(dat$Gender))
  end3 <- end2+length(levels(dat$Military.Branch))
  end4 <- end3+length(unique(dat$Undergraduate.Major))-1 # -1 is to compensate for removing NoUndergradDeg
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
    return(nrow(dat[dat$USborn==x & dat$Gender==y,]))
  } 
  level2.calc <- function (x,y) {
    return(nrow(dat[dat$Gender==x & dat$Military.Branch==y,]))
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
  return(list(links=links,nodes=nodes))
} # for pre1978
sankeyPrep4 <- function(dat) {
  nodes <- data.frame(c(levels(dat$USborn), levels(dat$Gender), levels(dat$Military.Branch), unique(dat$Undergraduate.Major), unique(dat$Graduate.Major))) 
  names(nodes) <- c("nodes")
  #nodes <- data.frame(nodes[nodes$nodes!="NoUndergradDeg",]) #Remove this here as it is filtered out with the threshold later
  names(nodes) <- c("nodes")
  nodes$index <- 1:nrow(nodes)
  nodes$concat <- paste0(nodes$index,",",nodes$nodes)
  
  # Generate Link df
  end1 <- length(levels(dat$USborn))
  end2 <- end1+length(levels(dat$Gender))
  end3 <- end2+length(levels(dat$Military.Branch))
  end4 <- end3+length(unique(dat$Undergraduate.Major))-1 # -1 is to compensate for removing NoUndergradDeg
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
    return(nrow(dat[dat$USborn==x & dat$Gender==y,]))
  } 
  level2.calc <- function (x,y) {
    return(nrow(dat[dat$Gender==x & dat$Military.Branch==y,]))
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
  return(list(links=links,nodes=nodes))
} # for post1978
datPre1978 <- dat[dat$Year<1978,]
datPost1978 <- dat[dat$Year>=1978,]
datPre1978 <- datPre1978[!is.na(datPre1978$Year),]
datPost1978 <- datPost1978[!is.na(datPost1978$Year),]

sankPre1978 <- sankeyPrep3(datPre1978)
sankPost1978 <- sankeyPrep4(datPost1978)

# Pre 1978 sankey
sankPre1978$links <- sankPre1978$links[sankPre1978$links$values>2,] # remove links with value less than this amount to clean up plot
sankPre1978$links$linkgroup <- rep("notpath",nrow(sankPre1978$links))
sankPre1978$links[c(3,7,13),]$linkgroup <- "path" # manually set desired path to be highlighted
my_color <- 'd3.scaleOrdinal() .domain(["path", "notpath","Male","USBorn","BornAbroad"]) 
    .range(["#FB8072", "#C3C2C4","#D95F02", "#1B9E77", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F","#7570B3" ,"#E7298A", "#66A61E", "#E6AB02", "#A6761D" ,"#666666"])'

sn <- sankeyNetwork(Links = sankPre1978$links, Nodes = sankPre1978$nodes, Source = "source",
                    Target = "target",  NodeID = "nodes", Value = "values",
                    fontSize = 18, nodeWidth = 10, LinkGroup = "linkgroup",iterations=1000, 
                    colourScale = my_color, nodePadding = 30,sinksRight = FALSE)
sn

# Post 1978 sankey
sankPost1978$links <- sankPost1978$links[sankPost1978$links$values>2,] # remove links with value less than this amount to clean up plot
sankPost1978$links$linkgroup <- rep("notpath",nrow(sankPost1978$links))
sankPost1978$links[c(3,7,13,19),]$linkgroup <- "path" # manually set desired path to be highlighted
my_color <- 'd3.scaleOrdinal() .domain(["path", "notpath","Male","Female","USBorn","BornAbroad"]) 
    .range(["#FB8072", "#C3C2C4","#D95F02", "#1B9E77", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F","#7570B3" ,"#E7298A", "#66A61E", "#E6AB02", "#A6761D" ,"#666666"])'

sn <- sankeyNetwork(Links = sankPost1978$links, Nodes = sankPost1978$nodes, Source = "source",
                    Target = "target",  NodeID = "nodes", Value = "values",
                    fontSize = 18, nodeWidth = 10, LinkGroup = "linkgroup",iterations=100, 
                    colourScale = my_color, nodePadding = 30,sinksRight = FALSE)
sn