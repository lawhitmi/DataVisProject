theme_set(theme_minimal())
data <- read.csv("datasets_934_1711_astronauts.csv",na.strings=c(""," ","NA")) #replace blanks with 'NA'
data.gender <- select(data, Year, Gender,Space.Flight..hr.,Space.Walks..hr.)
data.genderNotNA <- filter(data.gender, Year !=  'NA')


gby1 <- data.genderNotNA %>% 
  group_by(Year,Gender) %>% 
  summarise_at(vars(Space.Flight..hr.),
               list(TotalFlight=sum))

gby2 <- data.genderNotNA %>% 
  group_by(Year,Gender) %>% 
  summarise_at(vars(Space.Walks..hr.),
               list(TotalWalk=sum))


data.hourGender <- merge(gby1,gby2,by=c("Year","Gender"))

uniqueValues <- unique(data.hourGender[,1])

from <- 'Mision'
to <- 'Mision.Year'
edgesNasa <- data.frame(from,to)

name <- c('Mision','Mision.Year')
size <- c(0,0)
shortname <-  c('Mision','Mision.Year')
verticesNasa <- data.frame(name,size,shortname)


for (row in 1:length(uniqueValues)) {
  from <- 'Mision.Year'
  to <- paste('Mision.Year',toString(uniqueValues[row]), sep='.')
  df1 <- data.frame(from,to)
  edgesNasa <- rbind(edgesNasa,df1)
  
  from <- paste('Mision.Year',toString(uniqueValues[row]), sep='.')
  to <- paste('Mision.Year',toString(uniqueValues[row]),'Male.Hours', sep='.')
  df2 <- data.frame(from,to)
  edgesNasa <- rbind(edgesNasa,df2)
  
  
  to <- paste('Mision.Year',toString(uniqueValues[row]),'Female.Hours', sep='.')
  df3 <- data.frame(from,to)
  edgesNasa <- rbind(edgesNasa,df3)
  
  
  ### Other DataFrame
  name <- paste('Mision.Year',toString(uniqueValues[row]), sep='.')
  size <- 0
  shortname <- paste('Mision.Year',toString(uniqueValues[row]), sep='.')
  df1 <- data.frame(name,size, shortname)
  verticesNasa <- rbind(verticesNasa,df1)
  
  name <- paste('Mision.Year',toString(uniqueValues[row]),'Male.Hours', sep='.')
  size <- 0
  shortname <- paste('Male',toString(uniqueValues[row]), sep='.')
  df2 <- data.frame(name,size, shortname)
  verticesNasa <- rbind(verticesNasa,df2)
  
  name <- paste('Mision.Year',toString(uniqueValues[row]),'Female.Hours', sep='.')
  size <- 0
  shortname <- paste('Female',toString(uniqueValues[row]), sep='.')
  df3 <- data.frame(name,size, shortname)
  verticesNasa <- rbind(verticesNasa,df3)
}

for (row in 1:nrow(data.hourGender)) {
  year <- data.hourGender[row,1]
  gender <- data.hourGender[row,2]
  total <- data.hourGender[row,3]
  nameString <- paste('Mision.Year',toString(year),toString(gender),'Hours', sep='.')
  for (i in 1:nrow(verticesNasa)){
    if(nameString == verticesNasa[i,1]){
      print(paste('lo encontro en la posicion',toString(i)))
      verticesNasa[i,2] <- total
    }
  }
}

verticesNasa <- rbind(verticesNasa[1:2,],verticesNasa[24:nrow(verticesNasa),])
edgesNasa <- rbind(edgesNasa[1,],edgesNasa[23:nrow(edgesNasa),])

mygraph <- graph_from_data_frame( edgesNasa, vertices=verticesNasa )

ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle() +
  theme_void()

# Left: color depends of depth
p <- ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle(aes(fill = depth)) +
  theme_void() + 
  theme(legend.position="FALSE")
p
# Adjust color palette: viridis
p + scale_fill_viridis()
# Adjust color palette: colorBrewer
p + scale_fill_distiller(palette = "RdPu") 

# Create a subset of the dataset (I remove 1 level)
verticesNasa$size <- runif(nrow(verticesNasa))


# Rebuild the graph object
mygraph <- graph_from_data_frame( edgesNasa, vertices=verticesNasa )

# left
ggraph(mygraph, layout = 'circlepack', weight=size ) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_text( aes(label=shortname, filter=leaf, fill=depth, size=size)) +
  theme_void() + 
  theme(legend.position="FALSE") + 
  scale_fill_viridis()

ggraph(mygraph, layout = 'circlepack', weight=size ) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_label( aes(label=shortname, filter=leaf, size=size)) +
  theme_void() + 
  theme(legend.position="FALSE") + 
  scale_fill_viridis()





