
## DatVis Project
## US Covid-19 dataset from John Hopkins University
rm(list=ls())

#Group Report
#What are the questions my map will answer? 
#What things can I learn from the data?
#What do I want to communicate? Tell a story.

#Individual Report
#Why do you use this particular vis.? Colors? Instead of another option?
#Shapes? linetypes? 3D or not?
#The code is not going to be graded, think more in the output, why? message? etc.

library(usmap)
library(ggplot2)
library(tibble)

dat <- read.csv2("06-08-2020.csv")
dim(dat) ## size of the dataset
pairs(dat) ## take a look at potential relations between variables
names(dat)
str(dat$Province_State) #There are 58 US states
str(dat$Active)     #Active cases = total confirmed - total recovered - total deaths.
str(dat$Confirmed)  #Confirmed: Confirmed cases include presumptive positive cases and probable cases, 
                    #in accordance with CDC guidelines as of April 14.
str(dat$Deaths)     #Deaths: Death totals in the US include confirmed and probable, in accordance with 
                    #CDC guidelines as of April 14.
str(dat$Recovered)  #Recovered: Recovered cases outside China are estimates based on local media 
                    #reports, and state and local reporting when available, and therefore may be 
                    #substantially lower than the true number. US state-level recovered cases are 
                    #from COVID Tracking Project.
str(dat$Incident_Rate)  #Incidence_Rate: Admin2 + Province_State + Country_Region.
#str(dat$??)            #Case-Fatality Ratio (%): = confirmed cases per 100,000 persons.
str(dat$Testing_Rate)   #US Testing Rate: = total test results per 100,000 persons. The "total test 
                        #results" is equal to "Total test results (Positive + Negative)" from COVID 
                        #Tracking Project.
str(dat$Hospitalization_Rate) #US Hospitalization Rate (%): = Total number hospitalized / Number confirmed cases. 
                              #The "Total number hospitalized" is the "Hospitalized – Cumulative" 
                              #count from COVID Tracking Project. The "hospitalization rate" and 
                              #"hospitalized - Cumulative" data is only presented for those states 
                              #which provide cumulative hospital data.

#we will use the plot_usmap library (return an object), applying ggplot also

# US map by states
plot_usmap(regions = "states",labels = TRUE) + 
  labs(title="US Counties", subtitle="by States") + 
  theme(panel.background=element_rect(color="black",fill="lightblue"))

# US map by counties, check for a dataset with counties segmentation
plot_usmap(regions = "county") + 
  labs(title="US Counties", subtitle="by Counties") + 
  theme(panel.background=element_rect(color="black",fill="lightgreen"))

# Plot only certain states
plot_usmap(include = c("CA", "ID", "SA", "OR", "WI","NY","RI")) +
  labs(title = "Western US States",
       subtitle = "These are the states in the Pacific Timezone.")

## We need to filter the states that are available to plot with this function,
## analyze the impact of the information that will not take into account and
## convert the state names into state codes as input to the us_map function


####### FUNCTION #######
US.st <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("Alaska","Alabama","Arkansas","Arizona","California","Colorado",
                     "Connecticut","District of Columbia","Delaware","Florida","Georgia",
                     "Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky",
                     "Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota",
                     "Missouri","Mississippi","Montana","North Carolina","North Dakota",
                     "Nebraska","New Hampshire","New Jersey","New Mexico","Nevada",
                     "New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
                     "Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
                     "Utah","Virginia","Vermont","Washington","Wisconsin",
                     "West Virginia","Wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$state[match(st.x$state,st.codes$full)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
}
####### END OF FUNCTION #######

dat$state <- US.st(dat$Province_State) #now check which regions are not available in the map
dat[is.na(dat$state),c("state","Province_State","Active")] #there are 6 elements not possible to plot
## It will be convenient to discard this data (low impact)
dat.complete <- dat[!is.na(dat$state),] ## dataset with States Code only
#names(dat.complete)[names(dat.complete) == "State_Code"] <- "state"
#names(dat.complete)[names(dat.complete) == "Confirmed"] <- "value"

#now add some data
#A data frame containing values to plot on the map. This parameter should be a data frame 
#consisting of two columns, a fips code (2 characters for state, 5 characters for county) 
#and the value that should be associated with that region. The columns of data must be 
#fips or state and the value of the 'values' parameter.
dat.complete <- as_tibble(dat.complete)
dat.complete$Active <- as.numeric(dat.complete$Active) #Convert Active cases into integer
str(dat.complete$Active) #now it can be plotted. Assign now a variable state to the States Codes

#Plot active cases by selected states 
plot_usmap(labels=TRUE,data = dat.complete, values = "Active", 
           include = c("CA", "ID", "NV", "OR", "WA"), 
           color = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "Active Cases in US", 
                        label = scales::comma) + 
  labs(title = "Western US States", subtitle = "Actives cases by US Region") +
  theme(legend.position = "right")

## Another examples, by mountain region
plot_usmap("states", include = .mountain, labels = TRUE)

# Active Cases by State
plot_usmap(regions="states", labels=TRUE, data = dat.complete, values = "Active") +
  scale_fill_continuous(low = "white", high = "black", name = "Active Cases in US", 
                      label = scales::comma) + 
  labs(title = "US Active cases Map", subtitle = "Updated: June 6, 2020") +
  theme(legend.position = "right")

# Deaths by State
plot_usmap(regions = "states",labels = TRUE, data = dat.complete, values = "Deaths") +
  scale_fill_continuous(low = "white", high = "red", name = "Deaths", 
                        label = scales::comma) + 
  labs(title = "Deaths by State", subtitle = "Updated: June 6, 2020") +
  theme(legend.position = "right")

# Recovered in East North Central
plot_usmap(data = dat.complete, values = "Recovered", include = .east_north_central, 
           labels = TRUE, label_color = "white", color="gray", size=1.5) + 
  scale_fill_continuous(low = "blue", high = "orange", name="Recovered Cases", 
                        label = scales::comma) +
  labs(title = "Recovered Cases in U.S.", subtitle = "East Northe Central region") +
  theme(legend.position = "right")

# Recovered Cases by state
plot_usmap(regions = "states",labels = TRUE, data = dat.complete, values = "Recovered") + 
  scale_fill_continuous(low = "white", high = "lightgreen", name = "Active Cases in US", 
                        label = scales::comma) +
  labs(title="US Recovered cases Map", subtitle="Updated: June 6, 2020") + 
  theme(panel.background=element_rect(color="black",fill="lightblue"),legend.position = "right",
        plot.title = element_text(color = "black", size = 15, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(color = "black", size=12,hjust = 0.5))

# Available regions for inluce/exclude:
# usmap::plot_usmap(include = .mid_atlantic)
# usmap::plot_usmap(include = .east_south_central)
# usmap::plot_usmap(include = .south_region, exclude = .east_south_central)
# .pacific
# .west_north_central
# .west_region
# .west_south_central
# .mid_atlantic
# .south_atlantic
