#### EXPLORE DATA ####

library(tidyr) #separate
library(ggplot2)
dat <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'


checkNA <- function (x) {
  return(sum(is.na(x)))
}
str(dat)
checkNA(dat$Birth.Place) # no missing values in birthplace
unique(dat$Birth.Place) #272
checkNA(dat$Year) # 27 missing alum years
checkNA(dat$Alma.Mater) # 1 missing Alma Mater
unique(dat$Alma.Mater) # 280 Universities 
# Note that multiple universities can be listed for graduate/undergraduate. Recommend splitting this to reduce levels for each education level
checkNA(dat$Graduate.Major) # 58 missing Graduate degrees
unique(dat$Graduate.Major) # 143 factors
checkNA(dat$Undergraduate.Major) # 2 missing undergraduate degrees
checkNA(dat$Military.Rank) # 150 missing. Note that a NA probably means no military services
unique(dat$Undergraduate.Major) #87 factors
# Look further at the Majors
sort(table(dat$Undergraduate.Major),decreasing=TRUE)/nrow(dat)
cumsum(sort(prop.table(table(dat$Undergraduate.Major)),decreasing=TRUE))
plot(cumsum(sort(prop.table(table(dat$Undergraduate.Major)),decreasing=TRUE)))
plot(sort(prop.table(table(dat$Undergraduate.Major)),decreasing=TRUE)[1:5])
par(mfrow=c(2,1),las=1,mar=c(4,4,1,1))
plot(sort(prop.table(table(dat$Undergraduate.Major)),decreasing=TRUE)[1:10],
     cex.axis=0.5,main="Frequency of Undergraduate Majors (Sorted)",
     ylab="Frequency")
points(cumsum(sort(prop.table(table(dat$Undergraduate.Major)),decreasing=TRUE)))
levels(dat$Undergraduate.Major) #83 unique values
names(sort(table(dat$Undergraduate.Major),decreasing=TRUE)[1:5])
sort(table(dat[dat$Undergraduate.Major=="Aeronautical Engineering",]$Graduate.Major),decreasing=TRUE)


sort(table(dat$Graduate.Major),decreasing=TRUE)
cumsum(sort(prop.table(table(dat$Graduate.Major)),decreasing=TRUE))
par(las=2,mar=c(12,4,1,1))
plot(sort(table(dat$Graduate.Major),decreasing=TRUE)[1:10],cex.axis=0.5,main="Frequency of Graduate Majors (Sorted)",ylab="Frequency")
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

#### EDA Plots ####
dev.off()
plot(dat$Gender,main="Gender of Astronauts",col=c("#FDB462", "#80B1D3"))
ggplot(dat, aes(x=factor(Gender)))+geom_bar(fill=c("#D95F02", "#1B9E77"))+ggtitle("Astronaut Gender",)+labs(y= "count", x = "Gender")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  theme(text = element_text(size = 20),axis.text = element_text(size = 18))

hist(dat$Space.Flight..hr.,breaks=100,main="Histogram of Space Flight Time",xlab="Flight Time(hr)",col="#80B1D3")
ggplot(data=dat, aes(Space.Flight..hr.)) + 
  geom_histogram(binwidth=100,fill="#7570B3")+
  ggtitle("Hist of Space Flight Time",)+
  labs(y= "Frequency", x = "Flight Time (hr)")+
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20),axis.text = element_text(size = 18))
