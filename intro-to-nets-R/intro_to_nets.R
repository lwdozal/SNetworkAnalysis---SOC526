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
