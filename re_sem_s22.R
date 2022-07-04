# Data preparation ----

# loading libraries
library(dplyr)
library(igraph)
library(igraphdata)
library(network)
library(networkD3)
library(sna)
library(intergraph)
library(sp)
library(RColorBrewer)
library(data.table)

# loading datasets for wind energy IPC3, inventors and regions
load("res_sem.RData")

#sub stringing the IPC3 from IPC
IPC4 <- substr(joined_IPC_Inv$IPC, start = 1, stop = 4) #extracting the IPC3 from the original sorted dataset
IPC4 <- as.data.frame(IPC4) # defining it as a dataframe
data_final <- cbind(joined_IPC_Inv, IPC4) # combining the newly created IPC3 containing dataframe with the main dataframe
rm(joined_IPC_Inv) & rm(IPC4) # removing the dataset to free memory
data_final <- subset(data_final, select = -c(IPC,ctry_code)) # dropping columns


colnames(data_final)


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
              select("appln_id","person_id"))
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
               select(appln_id,city))
inv_city_2mode <-  table(inv_city)   # cross tabulate -> 2-mode sociomatrix
dim(inv_city_2mode)
inv_city_adj <- inv_city_2mode %*% t(inv_city_2mode)
dim(inv_city_adj)
class(inv_city_adj)
inv_city_nw <-  network(inv_city_adj,
                        matrix.type="adjacency",
                        directed=F)  # convert into 'network' format
class(inv_city_nw)


## A3. Inventor's (person's) network ----

# inventor's data cleaning
inv_person <- (data_main_15_attr %>% 
                 select(city,inv_name) %>% 
                 group_by(city)) # sub-setting the data for inventors from 15 cities


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

inv_person_adj <- diag.remove(inv_person_adj, remove.val = 0)
inv_person_nw <- network(inv_person_adj,
                         matrix.type="adjacency",
                         directed = FALSE,
                         ignore.eval=FALSE,
                         names.eval="strength")


# Converting network objects into igraph object ----
# detaching packages to avoid conflicts between package operations
detach(package:sna)
detach(package:network)


## A1 converting the network for all the inventors----

inv_aff_ig <- asIgraph(inv_aff_all_nw)
class(inv_aff_all)

### A1 plotting ----
plot(inv_aff_ig,
     layout = layout.auto(inv_aff_ig),
     vertex.size = 4,
     vertex.label = NA)


## A2. Converting the network package object into igraph object (for 15 cities)----
inv_city_ig <- asIgraph(inv_city_nw)
class(inv_city_ig)

inv_city_ig <- (inv_city_ig %>% 
                  set_vertex_attr(
                    name = "vertex.names",
                    value = c("Aurich","Hamburg","Berlin","Salzbergen","Rendsburg","Rheine","Kiel","Aachen","Munchen","Norderstedt","Osnabruck","Bremen","Erlangen","Dresden","Munster")
                  ))
V(inv_city_ig)

### A2 plotting ----
plot(inv_city_ig,
     vertex.color=rainbow(76),
     vertext.size=V(inv_city_ig)*0.4,
     vertex.label.cex = 1,
     layout=layout.kamada.kawai)




#plotting
par(mar=c(0,0,0,0))
inv_city_nw <-  network(inv_city_adj,
                        matrix.type="adjacency",
                        directed=T)  # convert into 'network' format
print.network(inv_city_nw) # Basic information about the network


# plotting
plot.network(inv_city_nw, label = network.vertex.names(inv_city_nw), displayisolates = T)






# igraph and igraph data ----

library(intergraph) # Dr. Graf
inv_city_ig <- asIgraph(inv_city_nw) # Dr. Graf
class(inv_city_ig)




# plotting graph for igraph
igraph.options(vertex.size = 10)
plot(inv_city_ig,
     layout = layout.drl(inv_city_ig),
     vertex.label = inv_city_ig,
     edge.arrow.size=.8,
     edge.color="gray",
     )




city_pat <- data.frame(city = names(diag(inv_city_adj)), pat = diag(inv_city_adj)) # Dr.Graf
city_pat

city_net_extra <- network(inv_cityD3)
plot.network(city_net_extra,
             label = network.vertex.names(inv_city_nw),
             interactive =T)



# NetworkD3 ----


library(networkD3)
par(mar=c(0,0,0,0))
simpleNetwork(inv_cityD3,
              fontSize = 12,
              opacity = 10,
              zoom = TRUE)


##### Network measurements

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


#--------- igraph -------

inv_nw_igraph <- graph_from_data_frame(inv_person, "undirected", F)
igraph.options(vertex.size = 3,
               edge.arrow.size = 0.2,
               vertex.label = NA,
               vertex.color ="black")
plot(inv_nw_igraph)











