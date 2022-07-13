# Data preparation ----

# loading libraries
suppressMessages(library(dplyr))
suppressMessages(library(igraph))
suppressMessages(library(igraphdata))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(intergraph))
suppressMessages(library(RColorBrewer))
suppressMessages(library(plotly))
suppressMessages(library(ggsci))

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



### exporting the dataset for further visualization
library(writexl)
write_xlsx(data_main_city_rank,"C:\\Study\\FSU MSc\\S22\\PatNet Sem\\Patnet_data_202101\\Patnet_data_202101\\REGPAT_202101\\Research Seminar\\f03d_city_1996-2016.xlsx")
###


data_main_15 <- head(data_main_city_rank,15) # keeping first 15 patent owning cities in ranked format
data_main_15_attr <- (data_main %>%
                     filter(city %in% c("Aurich","Hamburg","Berlin","Salzbergen","Rendsburg","Rheine","Kiel","Aachen","Munchen","Norderstedt","Osnabruck","Bremen","Erlangen","Dresden","Munster"))) # keeping the most 15 patent owning cities with all attributes

## plotting 15 most innovative cities

plot_ly(data_main_15,
        x = data_main_15$city,
        y = data_main_15$patents,
        type = "bar",
        color = data_main_15$city,
        showlegend = FALSE,
        colors=pal_jco()(10)) %>% 
  layout(title = "Top 15 German Cities in Wind Motor Innovation\n1996-2016", #adding this part to make the x axis descending
         xaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~city),
         yaxis = list(title = "Number of patents")
         )


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
map_de_plotted <- merge(shape_de, data_main, by="city")


plot(st_geometry(map_de_plotted))



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

# matching the parsed data with cities
inv_person_parsed$city <- inv_per_attr$city[match(inv_person_parsed$appln_id, inv_person_parsed$appln_id)]

# creating network
inv_person_2mode <- table(inv_person_parsed) # making 2 mode sociomatrix
inv_person_adj <- inv_person_2mode %*% t(inv_person_2mode)

# Converting network objects into igraph object ----
# detaching packages to avoid conflicts between package operations
detach(package:sna)
detach(package:network)
suppressMessages(library(igraph))

## A1 Converting the network for all the inventors----

diag(inv_person_adj) <-0

inv_aff_ig <- graph.adjacency(inv_person_adj, mode = "undirected", weight = TRUE)
vcount(inv_aff_ig)


class(inv_aff_all)
class(inv_aff_ig)

V(inv_aff_ig)$id <- colnames(inv_person_adj)
V(inv_aff_ig)$city_name <- inv_person_parsed$city[match(V(inv_aff_ig)$id, inv_person_parsed$inv_name)]

### A1 plotting ----
# without density
set.seed(100)
par(mar=c(0,0,7,0))
plot(inv_aff_ig,
     vertex.label=NA,
     vertex.size=1.5,
     vertex.frame.color="black",
     vertex.color = V(inv_aff_ig),
     main="Network of all the inventors\n(representing all the cities from 1996-2016)")

# with density
set.seed(100)
par(mar=c(0,0,7,0))
plot(inv_aff_ig,
     vertex.label=NA,
     vertex.size=degree(inv_aff_ig)*1.5,
     vertex.frame.color="black",
     vertex.color = V(inv_aff_ig),
     main="Network of all the inventors\n(representing all the cities from 1996-2016)\nwith degree of the innventors")

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
     main="Network of top 15 cities \n (according to their degree)")

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
     main="Inventor's network for top 15 cities")

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

#2006 network
aurich_data_06 <- (data_main_15_attr %>%
                          filter(city=="Aurich", app_year>=1996 & app_year<=2006) %>%
                          select(inv_name, appln_id))

aurich_data_06 <- table(aurich_data_06)
dim(aurich_data_06)
aurich_adj_06 <- aurich_data_06 %*% t(aurich_data_06)
dim(aurich_adj_06)
class(aurich_adj_06)
aurich_nw_06 <-  network(aurich_adj_06,
                      matrix.type="adjacency",
                      directed=F)  # convert into 'network' format


#2016 network
aurich_data_16 <- (data_main_15_attr %>%
                        filter(city=="Aurich", app_year>=2007 & app_year<=2016) %>%
                        select(inv_name, appln_id))

aurich_data_16 <- table(aurich_data_16)
dim(aurich_data_16)
aurich_adj_16 <- aurich_data_16 %*% t(aurich_data_16)
dim(aurich_adj_16)
class(aurich_adj_16)
aurich_nw_16 <-  network(aurich_adj_16,
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
             frame = T,
             cex.main=.8)
# plot for 2007:2016
plot.network(aurich_nw,
             vertex.col="#7570B3",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Aurich 2007:2016",
             frame = T,
             cex.main=.8)




### Hamburg ----

#2006 network
hamburg_data_06 <- (data_main_15_attr %>%
                          filter(city=="Hamburg", app_year>=1996 & app_year<=2006) %>%
                          select(inv_name, appln_id))

hamburg_data_06 <- table(hamburg_data_06)
dim(hamburg_data_06)
hamburg_adj_06 <- hamburg_data_06 %*% t(hamburg_data_06)
dim(hamburg_adj_06)
class(hamburg_adj_06)
hamburg_nw_06 <-  network(hamburg_adj_06,
                         matrix.type="adjacency",
                         directed=F)  # convert into 'network' format


#2016 network
hamburg_data_16 <- (data_main_15_attr %>%
                          filter(city=="Hamburg", app_year>=2007 & app_year<=2016) %>%
                          select(inv_name, appln_id))

hamburg_data_16 <- table(hamburg_data_16)
dim(hamburg_data_16)
hamburg_adj_16 <- hamburg_data_16 %*% t(hamburg_data_16)
dim(hamburg_adj_16)
class(hamburg_adj_16)
hamburg_nw_16 <-  network(hamburg_adj_16,
                      matrix.type="adjacency",
                      directed=F)  # convert into 'network' format


par(mfrow = c(1,2))
par(mar=c(0,0,1,0))

#plot for 1996:2006
plot.network(hamburg_nw_06,
             vertex.col="#99B898",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Hamburg 1996:2006",
             frame = T,
             pad=4,
             cex.main=.8)
# plot for 2007:2016
plot.network(hamburg_nw_16,
             vertex.col="#99B898",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Hamburg 2007:2016",
             frame = T,
             cex.main=.8)






### Berlin ----

#2006 Network
berlin_data_06 <- (data_main_15_attr %>%
                      filter(city=="Berlin", app_year>=1996 & app_year<=2006) %>%
                      select(inv_name, appln_id))

berlin_data_06 <- table(berlin_data_06)
dim(berlin_data_06)
berlin_adj_06 <- berlin_data_06 %*% t(berlin_data_06)
dim(berlin_adj_06)
class(berlin_adj_06)
berlin_nw_06 <-  network(berlin_adj_06,
                          matrix.type="adjacency",
                          directed=F)  # convert into 'network' format


#2016 network
berlin_data_16 <- (data_main_15_attr %>%
                      filter(city=="Berlin", app_year>=2007 & app_year<=2016) %>%
                      select(inv_name, appln_id))

berlin_data_16 <- table(berlin_data_16)
dim(berlin_data_16)
berlin_adj_16 <- berlin_data_16 %*% t(berlin_data_16)
dim(berlin_adj_16)
class(berlin_adj_16)
berlin_nw_16 <-  network(berlin_adj_16,
                          matrix.type="adjacency",
                          directed=F)  # convert into 'network' format


par(mfrow = c(1,2))
par(mar=c(0,0,1,0))

#plot for 1996:2006
plot.network(berlin_nw_06,
             vertex.col="#F26B38",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Berlin 1996:2006",
             frame = T,
             pad=4,
             cex.main=.8)
# plot for 2007:2016
plot.network(berlin_nw_16,
             vertex.col="#F26B38",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Berlin 2007:2016",
             frame = T,
             cex.main=.8)

## Salzbergen ----

#2006 Network
salzbergen_data_06 <- (data_main_15_attr %>%
                         filter(city=="Salzbergen", app_year>=1996 & app_year<=2006) %>%
                         select(inv_name, appln_id))

salzbergen_data_06 <- table(salzbergen_data_06)
dim(salzbergen_data_06)
salzbergen_adj_06 <- salzbergen_data_06 %*% t(salzbergen_data_06)
dim(salzbergen_adj_06)
class(salzbergen_adj_06)
salzbergen_nw_06 <-  network(salzbergen_adj_06,
                         matrix.type="adjacency",
                         directed=F)  # convert into 'network' format


#2016 network
salzbergen_data_16 <- (data_main_15_attr %>%
                         filter(city=="Salzbergen", app_year>=2007 & app_year<=2016) %>%
                         select(inv_name, appln_id))

salzbergen_data_16 <- table(salzbergen_data_16)
dim(salzbergen_data_16)
salzbergen_adj_16 <- salzbergen_data_16 %*% t(salzbergen_data_16)
dim(salzbergen_adj_16)
class(salzbergen_adj_16)
salzbergen_nw_16 <-  network(salzbergen_adj_16,
                             matrix.type="adjacency",
                             directed=F)  # convert into 'network' format


par(mfrow = c(1,2))
par(mar=c(0,0,1,0))

#plot for 1996:2006
plot.network(salzbergen_nw_06,
             vertex.col="#594F4F",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for salzbergen 1996:2006",
             frame = T,
             pad=4,
             cex.main=.8)
# plot for 2007:2016
plot.network(salzbergen_nw_16,
             vertex.col="#594F4F",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for salzbergen 2007:2016",
             frame = T,
             cex.main=.8)

network.size(salzbergen_nw_16)


## Rendsburg ----

#2006 Network

rendsburg_data_06 <- (data_main_15_attr %>%
                         filter(city=="Rendsburg", app_year>=1996 & app_year<=2006) %>%
                         select(inv_name, appln_id))

rendsburg_data_06 <- table(rendsburg_data_06)
dim(rendsburg_data_06)
rendsburg_adj_06 <- rendsburg_data_06 %*% t(rendsburg_data_06)
dim(rendsburg_adj_06)
class(rendsburg_adj_06)
rendsburg_nw_06 <-  network(rendsburg_adj_06,
                             matrix.type="adjacency",
                             directed=F)  # convert into 'network' format


#2016 network
rendsburg_data_16 <- (data_main_15_attr %>%
                         filter(city=="Rendsburg", app_year>=2007 & app_year<=2016) %>%
                         select(inv_name, appln_id))

rendsburg_data_16 <- table(rendsburg_data_16)
dim(rendsburg_data_16)
rendsburg_adj_16 <- rendsburg_data_16 %*% t(rendsburg_data_16)
dim(rendsburg_adj_16)
class(rendsburg_adj_16)
rendsburg_nw_16 <-  network(rendsburg_adj_16,
                             matrix.type="adjacency",
                             directed=F)  # convert into 'network' format


par(mfrow = c(1,2))
par(mar=c(0,0,1,0))

#plot for 1996:2006
plot.network(rendsburg_nw_06,
             vertex.col="#594F4F",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Rendsburg 1996:2006",
             frame = T,
             pad=4,
             cex.main=.8)
# plot for 2007:2016
plot.network(rendsburg_nw_16,
             vertex.col="#594F4F",
             vertex.cex=1,
             vertex.border="black",
             main="Inventors Network for Rendsburg 2007:2016",
             frame = T,
             cex.main=.8)

# Position of the city networks ----
## Aurich ----










# Mapping ----



library(raster)
library(sf)
library(ggplot2)


germany <- getData('GADM', country='DE', level=3)
ggplot(germany,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=id),color="grey30")






# function testing ----

x <- "Bremen"
# fn 1996:2006
create_data_06 <- function(x, data_main_15_attr)
{
  
  (data_main %>%
     filter(city==x, app_year>=1996 & app_year<=2006) %>%
     select(inv_name, appln_id))
}

# fn 2007:2016
create_data_16 <- function(x, data_main_15_attr)
  {
  
  data_06 <-(data_main %>%
               filter(city==x, app_year>=2007 & app_year<=2016) %>%
               select(inv_name, appln_id))
}

create_network_06 <- function(x,x){
  
  data_06 <- table(data_06)
  adj_06 <- data_06 %*% t(data_06)
  dim(adj_06)
  class(adj_06)
  nw_06 <-  network(adj_06,
                    matrix.type="adjacency",
                    directed=F)  # convert into 'network' format
  return(create_network_06(x))
}


create_network_06(x)
















# Old codes ----
## Network measurements

## adjacency matrix



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









