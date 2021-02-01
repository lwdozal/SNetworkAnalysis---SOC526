# Laura W. Dozal - SOC526
# 1/31/2021
# Practicing nets with R

##############

### Install Packages ###

library(devtools)
install_github("DougLuke/UserNetR")

install.packages("statnet")

library(statnet)
library(UserNetR)

#help(package='UserNetR')
data(Moreno)

### Simple Viz ###

gender <- Moreno %v% "gender"
plot(Moreno, vertex.col = gender +2, vertex.cex = 1.2)

### Turkey's 5 number summary ###

summary(Moreno, print.adj = FALSE)

# SIZE
network.size(Moreno)

# DENSITY
den_by_hand <- 2*46/(33*32)
den_by_hand

gden(Moreno)

# COMPONENTS
components(Moreno)

# DIAMETER
lgc <- component.largest(Moreno, result = "graph")
gd <- geodist(lgc)
max((gd$gdist))

#clustering
gtrans(Moreno, mode = "graph")


### Network Data Object ###
install.packages("network")

# NETWORK WITH ADJACENCY MATRIX
netmat1 <- rbind((c(0,1,1,0,0)),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))

rownames(netmat1) <- c("A","B", "C", "D", "E")
colnames(netmat1) <- c("A","B", "C", "D", "E")

net1 <- network::network(netmat1, matrix.type = "adjacency")

summary(net1)

gplot(net1, vertex.col = 2, displaylabels = TRUE)

# NETWORK WITH EDGE LIST

netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
net2 <- network(netmat2, matrix.type = 'edgelist')
network.vertex.names(net2) <- c("A","B","c","D","E")
summary(net2)

as.sociomatrix(net1)
class(as.sociomatrix(net1))
all(as.matrix(net1) == as.sociomatrix(net1))
as.matrix(net1,matrix.type = "edgelist")


# NODE ATTRIBUTES 
set.vertex.attribute(net1, "gender", c("F", "F", "M",
                                       "F", "M"))
net1 %v% "alldeg" <- degree(net1)
list.vertex.attributes(net1)
summary(net1)

get.vertex.attribute(net1, "gender")
net1 %v% "alldeg"


# TIE ATTRIBUTES
list.edge.attributes(net1)
set.edge.attribute(net1, "rndval", runif(network.size(net1),0,1))
list.edge.attributes(net1)

summary(net1 %e% "rndval")
summary(get.edge.attribute(net1, "rndval"))


### Valued Network ###
netval1 <- rbind(c(0,2,3,0,0),
                 c(0,0,3,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,2,0,0))

#ignore zeros and create a new edge attribute called "like"
netval1 <- network(netval1,matrix.type="adjacency",
                   ignore.eval=FALSE,names.eval="like")

network.vertex.names(netval1) <- c("A","B","C","D","E")

list.edge.attributes(netval1)

get.edge.attribute(netval1, "like")

#restore value mtx
as.sociomatrix(netval1)

as.sociomatrix(netval1,"like")


### CREATING A NETWORK OBJECT IN igraph ###
detach(package:statnet)
library(igraph)

inet1 <- graph.adjacency(netmat1)
class(inet1)
summary(inet1)
str(inet1)

help("summary.igraph")

inet2<- graph.edgelist(netmat2)
summary(inet2)

V(inet2)$name <- c("A","B","C","D","E")
E(inet2)$val <- c(1:6)
summary(inet2)

str(inet2)

### IGRAPH AND STATNET ###
install.packages("intergraph")
library(intergraph)
class(net1)

net1igraph <- asIgraph(net1)
class(net1igraph)
str(net1igraph)


### IMPORTING NETWORK DATA ###
detach("package:igraph", unload = TRUE)
library(statnet)

netmat3 <- rbind(c("A","B"),
                 c("A","C"),
                 c("B","C"),
                 c("B","D"),
                 c("C","B"),
                 c("E","C"))

net.df <- data.frame(netmat3)
net.df

#write.csv(net.df, file = "MyData.csv",
#          row.names = FALSE)
#net.edge <- read.csv(file="MyData.csv")
#net_import <- network(net.edge,
#                      matrix.type="edgelist")
#summary(net.edge)
#gden(net_import)