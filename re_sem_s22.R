# Data preparation ----

# loading libraries
library(dplyr)
library(igraph)
library(igraphdata)
library(network)
library(networkD3)
library(sna)
library(intergraph)
library(RColorBrewer)

# loading datasets for wind energy IPC3, inventors and regions
load("res_sem.RData")

#sub stringing the IPC3 from IPC
IPC4 <- substr(joined_IPC_Inv$IPC, start = 1, stop = 4) #extracting the IPC3 from the original sorted dataset
IPC4 <- as.data.frame(IPC4) # defining it as a dataframe
data_final <- cbind(joined_IPC_Inv, IPC4) # combining the newly created IPC3 containing dataframe with the main dataframe
rm(joined_IPC_Inv) & rm(IPC4) # removing the dataset to free memory
data_final <- subset(data_final, select = -c(IPC,ctry_code)) # dropping columns



# Data cleaning ----

# final cleaning by keeping FD03
data <- (data_final %>% 
                 filter(IPC4=="F03D")) # creating subset of Wind Energy Industry (F03D)
rm(data_final)
data <- (data %>% 
           filter(app_year>= 1996 & app_year <= 2016)) # limiting years from 1996 to 2016

# now getting rid of empty city values
data <- (data %>%
           mutate(city = replace(city, city == "", NA)))#making the empty values NA
data <- na.omit(data) #getting rid of NA values

# getting rid of duplicate values
data_main <- data[!duplicated(data),]



# ranking cities according to patents
data_main_city_rank <- (data_main %>% 
                          group_by(city) %>%
                          count(city, sort = TRUE,  name = "patents")) #ranking the cities according to the number of patent applications

data_main_15 <- head(data_main_city_rank,15) # keeping first 15 patent owning cities in ranked format
data_main_15_attr <- (data_main %>%
                     filter(city %in% c("Aurich","Hamburg","Berlin","Salzbergen","Rendsburg","Rheine","Kiel","Aachen","Munchen","Norderstedt","Osnabruck","Bremen","Erlangen","Dresden","Munster"))) # keeping the most 15 patent owning cities with all attributes





# General mapping ----

install.packages("ggmap")
library(ggmap)
library(sf)
library(leaflet)
library(ggplot2)
map_gDE <- get_googlemap("Germany", zoom = 8, maptype = "terrain")

?register_google

## loading shapefile
shape_de <- read_sf("./shape_files_DE/gadm40_DEU_4.shp") # loading shape file for Germany

plot(shape_de, max.plot = 3)
View(head(map_de,10))
colnames(shape_de)[7] <- "city"
map_de <- merge(shape_de, data_main, by="city")

plot(st_geometry(map_de))


# Creating network objects (NETWORK and SNA)----

## A1. Inventors networks for all cities ----
inv_aff_all <- (data_main %>% 
              select(appln_id,person_id))
inv_aff_all_2mode <- table(inv_aff_all)
dim(inv_aff_all_2mode)
inv_aff_all_adj <- inv_aff_all_2mode %*% t(inv_aff_all_2mode)
dim(inv_aff_all_adj)
class(inv_aff_all_adj)
inv_aff_all_nw <-  network(inv_aff_all_adj,
                       matrix.type="adjacency",
                       directed=F)  # convert into 'network' format
print.network(inv_aff_all_nw) # Basic information about the network


## A2. City networks: 15 most innovative cities ----
inv_city <- (data_main_15_attr %>% 
               select(city,appln_id))
inv_city_2mode <-  table(inv_city)   # cross tabulate -> 2-mode sociomatrix
dim(inv_city_2mode)
inv_city_adj <- inv_city_2mode %*% t(inv_city_2mode)
dim(inv_city_adj)
class(inv_city_adj)
inv_city_nw <-  network(inv_city_adj,
                        matrix.type="adjacency",
                        directed=F)  # convert into 'network' format
class(inv_city_nw)

####
get.vertex.attribute(inv_city_nw, "vertex.names") # checking the vertices names
par(mar=c(0,0,0,0))
plot.network(inv_city_nw,
             label = network.vertex.names(inv_city_nw))
network.density(inv_city_nw)

get.vertex.attribute(inv_city_nw, "vertex.names")
####

## A3. Inventor's (person's) network ----

# inventor's data cleaning
inv_person <- (data_main_15_attr %>% 
                 select(inv_name,appln_id)) # sub-setting the data for inventors with patents
inv_per_attr <- (data_main_15_attr %>% 
                   select(appln_id, city))

# parsing the person's network data to have clean and non-duplicated values
inv_person_parsed <- inv_person
length(unique(inv_person_parsed$inv_name))
inv_person_parsed$inv_name_small <- tolower(inv_person_parsed$inv_name)
inv_person_parsed$inv_name_small <- sub(", prof. dr.-ing.","", inv_person_parsed$inv_name_small)
length(unique(inv_person_parsed$inv_name_small))
inv_person_parsed$inv_name_small <- sub(", dr.-ing.","", inv_person_parsed$inv_name_small)
length(unique(inv_person_parsed$inv_name_small))
inv_person_parsed$inv_name_small <- sub(", dipl.-ing.","", inv_person_parsed$inv_name_small)
length(unique(inv_person_parsed$inv_name_small))
inv_person_parsed$inv_name_small <- sub(", -ing.","", inv_person_parsed$inv_name_small)
length(unique(inv_person_parsed$inv_name_small))
inv_person_parsed$inv_name_small <- sub(", dr.","", inv_person_parsed$inv_name_small)
length(unique(inv_person_parsed$inv_name_small))
inv_person_parsed$inv_name_small <- sub(" dr.","", inv_person_parsed$inv_name_small)
length(unique(inv_person_parsed$inv_name_small))

# new network changed size
length(unique(inv_person_parsed$inv_name_small))/length(unique(inv_person_parsed$inv_name))

# getting rid of original inventor names
inv_person_parsed$inv_name <- inv_person_parsed$inv_name_small

# removing third column
inv_person_parsed <- subset(inv_person_parsed, select = -c(inv_name_small))

# creating network with the cleaned data
inv_person_2mode <- table(inv_person_parsed) # making 2 mode sociomatrix
dim(inv_person_2mode)
class(inv_person_2mode)
inv_person_adj <- inv_person_2mode %*% t(inv_person_2mode)
dim(inv_person_adj)
class(inv_person_adj)


inv_person_nw <- network(inv_person_adj,
                         directed = F)
par(mar=c(0,0,0,0))


get.vertex.attribute(inv_person_nw, "vertex.names")


# Converting network objects into igraph object ----
# detaching packages to avoid conflicts between package operations
detach(package:sna)
detach(package:network)


## A1 Converting the network for all the inventors----

inv_aff_ig <- asIgraph(inv_aff_all_nw)
class(inv_aff_all)

### A1 plotting ----
set.seed(100)
par(mar=c(0,0,1,0))
plot(inv_aff_ig,
     layout = layout.auto(inv_aff_ig),
     vertex.size = 2,
     vertex.label = NA,
     main="Network of all the inventors")


## A2. Converting the network package object into igraph object (for 15 cities)----
inv_city_ig <- asIgraph(inv_city_nw)
class(inv_city_ig)

inv_city_ig <- (inv_city_ig %>% 
                  set_vertex_attr(
                    name = "vertex.names",
                    value = c("Aurich","Hamburg","Berlin","Salzbergen","Rendsburg","Rheine","Kiel","Aachen","Munchen","Norderstedt","Osnabruck","Bremen","Erlangen","Dresden","Munster") # modifying vertex names on the igraph object (as city names)
                  ))
V(inv_city_ig)$label <- c("Aurich","Hamburg","Berlin","Salzbergen","Rendsburg","Rheine","Kiel","Aachen","Munchen","Norderstedt","Osnabruck","Bremen","Erlangen","Dresden","Munster") # changing the igraph object's labels also into the city names


### A2 plotting ----
par(mar=c(0,0,2,0))
set.seed(100)
plot(inv_city_ig,
     vertex.label=V(inv_city_ig)$city,
     vertex.color=V(inv_city_ig),
     vertex.size= degree(inv_city_ig)*3,
     vertex.frame.color=NA,
     vertex.label.family= "Cambria",
     vertex.label.font=2,
     main="Network of top 15 cities \n (according to their density)")

#### A2 Centrality measurements----
centr_degree(
  inv_city_ig,
  mode = c("all", "out", "in", "total"),
  loops = F,
  normalized = TRUE)

## A3. Converting inventor's network for the 15 cities ----

inv_person_ig <- asIgraph(inv_person_nw)
class(inv_person_ig)
set_vertex_attr(graph = inv_person_ig, name = "city", value = "city")

### A3 plotting ----
#### the network graph  ----
par(mar=c(0,0,1,0))
set.seed(100)
plot(inv_person_ig,
     vertex.label=NA,
     vertex.frame.color=NA,
     vertex.color=V(inv_person_ig),
     vertex.size=2,
     edge.colour="black",
     main="Inventor's network for top 15 cities",
     sub=("(on edge betweenness of the inventors)"))

#### plot with edge betweenness ----
par(mar=c(0,0,1,0))
plot(inv_person_ig,
     vertex.label=NA,
     vertex.frame.color=NA,
     vertex.color=V(inv_person_ig),
     vertex.size=2,
     edge.curved=.1,
     main="Inventor's network for top 15 cities",
     sub=("(on edge betweenness of the inventors)"),
     edge.width = edge_betweenness(inv_person_ig))
     
plot(inv_person_ig,
     vertex.label = ifelse(degree(inv_person_ig) >= .01, V(inv_person_ig)$label, NA))

##### A3 centrality measurements ----
centr_degree(
  inv_person_ig,
  mode = c("all", "out", "in", "total"),
  loops = F,
  normalized = TRUE)


# Top 5 city's innovation scenario over the time ----
## top 5 city, year 2005 and 2015

detach(package:igraph)
library(network)
library(ndtv)
library(tidyverse)
### Aurich ----
aurich_d_2007 <- (data_main_15_attr %>% 
                    filter(city=="Aurich", app_year==2007) %>% 
                    select(city, appln_id, inv_name))

aurich_d_2016 <- (data_main_15_attr %>%
                    filter(city=="Aurich", app_year==2016) %>%
                    select(inv_name, appln_id))
aurich_d_all <- (data_main_15_attr %>% 
                   filter(city=="Aurich") %>% 
                   select(appln_id, app_year))

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--
#2006 network
aurich_test_data_06 <- (data_main_15_attr %>%
                          filter(city=="Aurich", app_year>=1996 & app_year<=2006) %>%
                          select(inv_name, appln_id))

aurich_test_06 <- table(aurich_test_data_06)
dim(aurich_test_06)
aurich_adj_06 <- aurich_test_06 %*% t(aurich_test_06)
dim(aurich_adj_06)
class(aurich_adj_06)
aurich_nw_06 <-  network(aurich_adj_06,
                      matrix.type="adjacency",
                      directed=F)  # convert into 'network' format


#2016 network
aurich_test_data_16 <- (data_main_15_attr %>%
                          filter(city=="Aurich", app_year>=2007 & app_year<=2016) %>%
                          select(inv_name, appln_id))

aurich_test <- table(aurich_test_data_16)
dim(aurich_test)
aurich_adj <- aurich_test %*% t(aurich_test)
dim(aurich_adj)
class(aurich_adj)
aurich_nw <-  network(aurich_adj,
                           matrix.type="adjacency",
                           directed=F)  # convert into 'network' format


par(mfrow = c(1,2))
par(mar=c(0,0,1,0))

#plot for 1996:2006
plot.network(aurich_nw_06,
             vertex.col="#7570B3",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Aurich 1996:2006",
             pad=4,
             cex.main=.8)
# plot for 2007:2016
plot.network(aurich_nw,
             vertex.col="#7570B3",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Aurich 2007:2016",
             cex.main=.8)



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--

?plot.network

par(mar=c(0,0,0,0))
plot.network(aurich_nw,
             label = network.vertex.names(aurich_nw))



### Hamburg ----

hamburg_data <- (data_main_15_attr %>%
                   filter(city=="Hamburg", app_year==2016) %>%
                   select(inv_name))
hamburg_test <- table(hamburg_data)
dim(hamburg_test)
hamburg_adj <- hamburg_test %*% t(hamburg_test)
dim(hamburg_adj)
class(hamburg_adj)
hamburg_nw <-  network(hamburg_adj,
                      matrix.type="adjacency",
                      directed=T)  # convert into 'network' format
print.network(hamburg_nw)

plot.network(hamburg_nw)

get.vetex.id(hamburg_nw)
?get.edgeIDs()





### TEST Berlin ----





# Old codes ----
## Network measurements

## adjacency matrix
inv_adj <- inv_2mode %*% t(inv_2mode)
dim(inv_adj)
class(inv_adj)



## converting into a network object [network package]
par(mar=c(0,0,0,0))
inv_nw <-  network(inv_adj,matrix.type="adjacency",directed=T)  # convert into 'network' format
print.network(inv_nw)                                               # Basic information about the network
plot.network(inv_nw, displayisolates = T)



network.size(inv_nw)
network.vertex.names(inv_nw)

invp_nw <- network(inv_adj, type = "adjacency", directed = F, ignore.eval = F, names.eval = "strength")
print.network(invp_nw)
plot.network(invp_nw, displayisolates = F, edge.lwd = get.edge.value(invp_nw,"strength"))

gden(inv_city_nw)



# From Dr. Graf :: igraph and igraph data ----

library(intergraph) # Dr. Graf
inv_city_ig <- asIgraph(inv_city_nw) # Dr. Graf
class(inv_city_ig)









