#### EXPLORE DATA ####

library(tidyr) #separate

dat <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'

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
unique(dat$Graduate.Major) # 143 factors
checkNA(dat$Undergraduate.Major) # 22 missing undergraduate degrees
checkNA(dat$Military.Rank) # 150 missing. Note that a NA probably means no military services

# Look further at the Majors
sort(table(dat$Undergraduate.Major),decreasing=TRUE)
cumsum(sort(prop.table(table(dat$Undergraduate.Major)),decreasing=TRUE))
par(las=2,mar=c(12,4,1,1))
plot(sort(table(dat$Undergraduate.Major),decreasing=TRUE),cex.axis=0.75,main="Frequency of Undergraduate Majors (Sorted)")
levels(dat$Undergraduate.Major) #83 unique values
names(sort(table(dat$Undergraduate.Major),decreasing=TRUE)[1:5])
sort(table(dat[dat$Undergraduate.Major=="Aeronautical Engineering",]$Graduate.Major),decreasing=TRUE)


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
