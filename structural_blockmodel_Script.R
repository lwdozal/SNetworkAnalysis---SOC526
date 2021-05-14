# LwDozal
# Concor blockmodels
# 5/13/2021

##Structural Blockmodel
library("tidyverse")
library("network")
library("statnet")
library("UserNetR")
library("sna")
library("corrplot")

load("concorSoc526.RData")
require(concorR)


######### Setup

crime150 <- read_xlsx("C:/Users/LW/Box/Mexico City Police 2020/data/buffer_150_crime3.xlsx")

#crime columns
delito_espesifico <- crime150$delito
delito_vec <- as.array(crime150$categoria_)
delito <- crime150$categoria_
crime_spot <- crime150$calle_hech
crime_neighbrhd <- crime150$colonia_he
crime_region <- crime150$alcaldia_h

#school columns
escuela_todo <- crime150$escuela_nombre
nivel <- (crime150$escuela_nivel)
nombre <- crime150$escuela
school_address <- crime150$domicilio

# length(unique(nivel))
# table(delito_espesifico)
# length(unique(delito_espesifico))

# mtx1 <- matrix(crime150, nrow = length(unique(delito_espesifico)), ncol = length(unique(nivel)))

mtx_crime150 <- data.matrix(crime150)
# mtx_crime150
# glimpse(crime150)
# crime <- model.matrix(~ delito_espesifico + nivel -1)

new_crime <- crime150[, c("categoria_","delito", "escuela_nivel", "calle_hech", "colonia_he", "alcaldia_h", "escuela", "domicilio")]
crime_mtx <- data.matrix(new_crime)
# crime_mtx
summary(crime_mtx, print.adj = FALSE)

mtx <- crime150[, c("delito", "escuela_nivel")]
# glimpse(mtx)
mtx_fin <- na.omit(mtx) #mtx %>% drop_na() #[complete.cases(mtx), ] #drop na's
# glimpse(mtx_fin)



bin_crime <- as.data.frame.matrix(table(mtx_fin))
mtx_full <- as.matrix(bin_crime)
bin_crime <- as.matrix((bin_crime>0) +0)

#############################################

dim(bin_crime)
# dim(t(bin_crime))
# S <- t(bin_crime) %*% bin_crime
# C <- bin_crime  %*% t(bin_crime)
# # dim(S)
# # dim(C)
# stackC <- rbind(C, S)
# stackS <- rbind(S, C)
# dim(stackC)
# dim(stackS)


result <- concorFirst(bin_crime)
names(result)
r1 <- result$r1
# Let's take a look at only the first 5 rows and coluns, rounded to 3 places:
round(r1[1:5,1:5], digits = 3)


runA <- concor(r1, colnumbers = 1:37)
names(runA)
b1 <- runA$b1
b2 <- runA$b2
# For this first split (into 2 blocks), let's look at the list of managers in each block.
# Block 1:
b1
b2



# Now let's split each of these two blocks into two sub-blocks.
#  (Handout, top of page 5).
runB1 <- concor(r1, b1)
block1_1 <- runB1$b1
block1_2 <- runB1$b2
runB2 <- concor(r1, b2)
block2_1 <- runB2$b1
block2_2 <- runB2$b2

# Let's look at the block membership--
block1_1
block1_2
block2_1
block2_2

perm4 <- c(block1_1, block1_2, block2_1, block2_2)
size_of_split <- c(length(block1_1),length(block1_2),length(block2_1),length(block2_2))
size_of_split
perm4
ddis <- density(bin_crime, perm4, size_of_split)
ddis
bin <- (ddis$dens > ddis$overall) +0
bin

# dis_groups <- c(nivel, delito_espesifico)
# dis_groups
# size_group_split <- c(length(nivel),length(delito_espesifico))
# size_group_split
# 
# dis_den_mat <- density(bin_crime, dis_groups, size_group_split)
# 


print("CONCOR BLOCKS")
ddis$dens
ddis$overall
(ddis$dens > ddis$overall) +0


# print("PREDETERMINED 'DISLIKE' GROUPS")
# dis_den_mat$dens
# dis_den_mat$overall
# (dis_den_mat$dens > dis_den_mat$overall) +0


###  plot it

# Let's create a membership vector:
memb <- rep(0, 37)
memb[block1_1] <- 1
memb[block1_2] <- 2
memb[block2_1] <- 3
memb[block2_2] <- 4
# Take a look:
memb

my.blockmodel.dis <- blockmodel(bin_crime, ec = memb, block.content = "density")

# Take a look at the densities within each submatrix:
# my.blockmodel.dis
my.blockmodel.dis$block.model # This object consists only of the density matrix.
# group.blockmodel.dis$block.model # This object consists only of the density matrix.

# To represent these visually, we could use the "corrplot" library. (You may need to
corrplot(my.blockmodel.dis$block.model, labels = rownames(bin_crime), is.corr=FALSE, method="square") + title(sub = "Concor: Crime Densities")
# corrplot(group.blockmodel.dis$block.model, is.corr=FALSE, method="square") + title(sub = "Predetermined Groups: Dislike Densities")
# Plot the blockmodel:
# plot(my.blockmodel.dis)+ title(sub =  "Concor: Crime Densities")
# plot(group.blockmodel.dis) + title(sub = "Predetermined Groups: Dislike Densities")

############################################################################################

