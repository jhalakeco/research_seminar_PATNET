##### Data preparation #####

# loading libraries
library(dplyr)
library(igraph)
library(igraphdata)
library(network)
library(sna)
library(intergraph)

# loading datasets for wind energy IPC3, inventors and regions
load("res_sem.RData")

#sub stringing the IPC3 from IPC
IPC4 <- substr(joined_IPC_Inv$IPC, start = 1, stop = 4) #extracting the IPC3 from the original sorted dataset
IPC4 <- as.data.frame(IPC4) # defining it as a dataframe
data_final <- cbind(joined_IPC_Inv, IPC4) # combining the newly created IPC3 containing dataframe with the main dataframe
rm(joined_IPC_Inv) # removing the dataset to free memory
data_final <- subset(data_final, select = -c(IPC,ctry_code)) # dropping columns
colnames(data_final)[colnames(data_final)=="IPC"] <- "IPC4" # changing the column name to IPC3head






##### Data cleaning ####

# final cleaning by keeping FD03
data_final <- (data_final %>% 
                 filter(IPC4=="F03D")) # creating subset of Wind Energy Industry (F03D)

data_final_F03D <- (data_final %>% 
                 filter(app_year>= 1996 & app_year <= 2016)) # limiting years from 1996 to 2016

# now getting rid of empty city values
data_1 <- (data_final_F03D %>%
           mutate(city = replace(city, city == "", NA)))#making the empty values NA
data_2 <- na.omit(data_1) #getting rid of NA values

# getting rid of duplicate values
data_main <- data_2[!duplicated(data_2),]
View(data_main)

# ranking cities according to patents
data_main_city_rank <- (data_main %>%
                          group_by(city) %>% 
                          count(city, sort = TRUE,  name = "patents"))

# keeping first 15 patent owning cities
data_main_15 <- head(data_main_city_rank,15)






#### general mapping ############################################

library(sf)
library(leaflet)
library(ggplot2)



mymap <- st_read ("C:\Users\jhala\Downloads\gadm40_DEU_4.shp", stringsAsFactors =F) # importing the shape file

colnames(mymap)[colnames(mymap)=="NAME_3"] <- "city" # renaming the column name to math with both data frames
data_final_map <- (data_main_city_rank) # selecting only city name and appln_id

map_and_data <- inner_join(mymap, data_final_map)
map_and_data <- (map_and_data %>% 
                   filter(!patents==0))
str(map_and_data)

View(head(map_and_data, 5))

ggplot(map_and_data) +
  geom_sf((aes(fill = appln_id))+
            stat_summary(fun = count, colour ="black"))

# making spatial data objects
coordinates(map_and_data) <- map_and_data$geometry







#### Creating network objects ####

## applications and inventors networks
inv_aff_all <- (data_main %>% 
              select("appln_id","person_id"))
inv_aff_all_2mode <- table(inv_aff)
dim(inv_aff_all_2mode)
View(head(inv_aff_all_2mode,10))
inv_aff_all_adj <- inv_aff_all_2mode %*% t(inv_aff_all_2mode)
dim(inv_aff_all_adj)
class(inv_aff_all_adj)

par(mar=c(0,0,0,0))
inv_aff_all_nw <-  network(inv_aff_all_adj,
                       matrix.type="adjacency",
                       directed=F)  # convert into 'network' format
print.network(inv_aff_all_nw) # Basic information about the network
# plotting
plot.network(inv_aff_all_nw,
             displayisolates = T)

## city networks
inv_city <- (data_main %>% 
               select("city","appln_id"))
inv_city_2mode <-  table(inv_city)   # cross tabulate -> 2-mode sociomatrix
dim(inv_city_2mode)
inv_city_adj <- inv_city_2mode %*% t(inv_city_2mode)
dim(inv_city_adj)
class(inv_city_adj)
View(head(inv_city_2mode,10))
#plotting
par(mar=c(0,0,0,0))
inv_city_nw <-  network(inv_city_adj,
                        matrix.type="adjacency",
                        directed=T)  # convert into 'network' format
print.network(inv_city_nw) # Basic information about the network
# plotting
plot.network(inv_city_nw, label = network.vertex.names(inv_city_nw), displayisolates = T)


##### igraph and igraphdata ####

library(intergraph) # Dr. Graf
inv_city_ig <- asIgraph(inv_city_nw) # Dr. Graf
class(inv_city_ig)

get.adjacency(inv_city_ig)

# plotting graph
igraph.options(vertex.size = 10)
plot(inv_city_ig,
     layout = layout.kamada.kawai(inv_city_ig),
     vertex.label = V(inv_city$city))




city_pat <- data.frame(city = names(diag(inv_city_adj)), pat = diag(inv_city_adj)) # Dr.Graf
city_pat

city_net_extra <- network(inv_cityD3)
plot.network(city_net_extra,
             label = network.vertex.names(inv_city_nw),
             interactive =T)






##### Preparing dataset for Network D3 ######

D3_1996_Berlin <- (data_main %>%
              filter(app_year==1996, city=="Berlin") %>% 
                select(inv_name, appln_id))
D3_2013_Berlin <- (data_main %>%
                     filter(app_year==2012, city=="berlin") %>% 
                     select(inv_name, appln_id))

D3_count <- (data_main %>%
               group_by(app_year) %>%
               summarise(n_appls = n_distinct(appln_id)) %>%
               filter(app_year >= 2000 & app_year <= 2015) %>%
               mutate(app_year = as.factor(app_year)))


plot(D3_count)


?gden


##### NetworkD3 plotings ###########

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











