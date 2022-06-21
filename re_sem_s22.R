#####
#Data preparation

# loading libraries
library(dplyr)
library(igraph)


# loading datasets for wind energy IPC3, inventors and regions
load("res_sem.RData")

#sub stringing the IPC3 from IPC
IPC3 <- substr(joined_IPC_Inv$IPC, start = 1, stop = 4) #extracting the IPC3 from the original sorted dataset
IPC3 <- as.data.frame(IPC3) # defining it as a dataframe
data_final <- cbind(joined_IPC_Inv, IPC3) # combining the newly created IPC3 containing dataframe with the main dataframe
rm(joined_IPC_Inv) # removing the dataset to free memory
data_final <- subset(data_final, select = -c(IPC,ctry_code,reg_code)) # dropping columns
colnames(data_final)[colnames(data_final)=="IPC"] <- "IPC3" # changing the column name to IPC3head

#####

# final cleaning by keeping FD03
data_final <- (data_final %>% 
                 filter(IPC3=="F03D")) # creating subset of Wind Energy Industry (F03D)


data_final_F03D <- (data_final %>% 
                 filter(app_year>= 1996 & app_year <= 2016)) # limiting years from 1996 to 2016

#####
#Cleaning the data, changing Frankfurt am Main to Frankfurt

data <- (data_final_F03D %>%
           mutate(city = replace(city, city == "Frankfurt am Main", "Frankfurt")))

# now getting rid of empty city values
data_1 <- (data %>%
           mutate(city = replace(city, city == "", NA)))#making the empty values NA
data_2 <- na.omit(data_1) #getting rid of NA values

# keeping only 10 cities

new_data <-(data_2 %>% 
              filter(city %in% c("Munich","Berlin","Darmstadt","Hamburg","Stuttgart","Erlangen","Dresden","Frankfurt","Karlsruhe","Aachen")))
new_data <- new_data[!duplicated(new_data),]

#####
load("data_f03D.RData")
View(data_1)


#### general mapping ############################################
plot(mymap)
mymap <- st_read ("C:/Users/jhala/Downloads/gadm40_DEU_shp/gadm40_DEU_4.shp", stringsAsFactors =F) # importing the shape file

colnames(mymap)[colnames(mymap)=="NAME_3"] <- "city" # renaming the column name to math with both data frames
data_final_map <- (data_final %>% 
                     select("appln_id", "city")) # selecting only city name and appln_id

map_and_data <- inner_join(mymap, data_final_map)
str(map_and_data)

View(head(map_and_data,10))

spplot(map_and_data) +
  geom_sf((aes(fill = appln_id))+
            stat_summary(fun = count, colour ="black"))
##########



library(igraph)
library(network)


## applications and city networks
inv_aff <- (new_data %>% 
              select("appln_id","city"))
inv_aff_2mode <- table(inv_aff)
dim(inv_aff_2mode)
View(head(inv_aff_2mode,10))
inv_aff_adj <- inv_aff_2mode %*% t(inv_aff_2mode)
dim(inv_aff_adj)
class(inv_aff_adj)

par(mar=c(0,0,0,0))
inv_aff_nw <-  network(inv_aff_adj,matrix.type="adjacency",directed=F)  # convert into 'network' format
print.network(inv_aff_nw)                                               # Basic information about the network
# plotting
plot.network(inv_aff_nw,
             hyper = T,
             label = network.vertex.names(inv_app_nw),
             displayisolates = T)




## city networks
inv_app <- (inv_aff %>% 
               select("city","appln_id"))
inv_app_2mode <-  table(inv_app)   # cross tabulate -> 2-mode sociomatrix
dim(inv_app_2mode)
inv_app_adj <- inv_app_2mode %*% t(inv_app_2mode)
dim(inv_app_adj)
class(inv_app_adj)
View(head(inv_app_2mode,10))
#plotting
par(mar=c(0,0,0,0))
inv_app_nw <-  network(inv_app_adj,  matrix.type="adjacency",directed=T)  # convert into 'network' format
print.network(inv_app_nw)                                               # Basic information about the network
# plotting
plot.network(inv_app_nw, label = network.vertex.names(inv_app_nw), displayisolates = T)

?plot.network
?network()




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

gden(invp_nw)


#--------- igraph -------

inv_nw_igraph <- graph_from_data_frame(inv_person, "undirected", F)
igraph.options(vertex.size = 3,
               edge.arrow.size = 0.2,
               vertex.label = NA,
               vertex.color ="black")
plot(inv_nw_igraph)











