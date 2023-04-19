
#load the required libraries
 library(conflicted)
 library(dplyr)
 library(tidyverse)
#read the eigenvector file
 pca <- read_table("/give/your/path/to/file.eigenvec", col_names = FALSE)
#read the eigenvalue file
 eigenval <- scan("/give/your/path/to/file.eigenval")
#remove the upper cell of the pca file
pca <- pca[,-1]
#give the first column another name (in this case individual)
names(pca)[1] <- "individual"
#give the other columns PC names starting from PC1 to the last PC
names(pca)[2:ncol(pca)] <- paste0("PC", 1:(ncol(pca)-1))
#remove the cell after the first one
pca <- pca[-1,]
#create a column with all locations
Location <- rep(0, length(pca$individual))
Location[grep("1", pca$individual)] <- "loc1"
Location[grep("2", pca$individual)] <- "loc2"
Location[grep("3", pca$individual)] <- "loc3"
Location[grep("4", pca$individual)] <- "loc4"
Location[grep("5", pca$individual)] <- "loc5"
Location[grep("6", pca$individual)] <- "loc6"
Location[grep("7", pca$individual)] <- "loc7"
Location[grep("8", pca$individual)] <- "loc8"
Location[grep("9", pca$individual)] <- "loc9"
Location[grep("10", pca$individual)] <- "loc10"
Location[grep("11", pca$individual)] <- "loc11"
#create a column with all regions
Region <- rep(NA, length(pca$individual))
Region[grep("1", pca$individual)] <- "reg1"
Region[grep("2", pca$individual)] <- "reg1"
Region[grep("3", pca$individual)] <- "reg1"
Region[grep("4", pca$individual)] <- "reg1"
Region[grep("5", pca$individual)] <- "reg1"
Region[grep("6", pca$individual)] <- "reg1"
Region[grep("7", pca$individual)] <- "reg1"
Region[grep("8", pca$individual)] <- "reg2"
Region[grep("9", pca$individual)] <- "reg2"
Region[grep("10", pca$individual)] <- "reg2"
Region[grep("11", pca$individual)] <- "reg3"
#concatenate vectors location and region after convertion
loc_reg <- paste0(Location, "_", Region)
#Coerce pca and vectors  to data frame
pca <- as_tibble(data.frame(pca, Location, Region, loc_reg))
#cretae a data frame with eigenvalues
pve <- data.frame(PC = 1:10, pve = eigenval/sum(eigenval)*100)
#load the required library 
 library("ggplot2")
plot the pc
a <- ggplot(pca, aes(PC1, PC2, col = Location, shape = Region)) + geom_point(size = 1.4)
a <- a + scale_colour_manual(values = c("#99cc00", "#ffcc00", "#9933ff", "#f00000", "#0000ff", "#ff6633", "#00cccc", "#ff99cc", "#666666", "#993300", "#ff00cc"))
a <- a + coord_equal() + theme_classic() + xlim(c(-0.2,0.1)) + ylim(c(-0.15,0.05))   
a + xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) + ylab(paste0("PC2 (", signif(pve$pve[2], 3),"%)"))
#if you cross this foloowing problem (error message)
Error: Discrete value supplied to continuous scale
#check the class of your pcs
class(pca$PC1)
[1] "character"     #if a character convert it to numeric
pca$PC1 <- as.numeric(pca$PC1)
pca$PC2 <- as.numeric(pca$PC2)
pca$PC3 <- as.numeric(pca$PC3)
pca$PC4 <- as.numeric(pca$PC4)
pca$PC5 <- as.numeric(pca$PC5)
pca$PC6 <- as.numeric(pca$PC6)
#check the number of Ns in your data frame
sum(is.na(pca$PC1))        #if not 0 replace all ns with 0

pca$PC1 <- replace(pca$PC1,is.na(pca$PC1),0)
pca$PC2 <- replace(pca$PC2,is.na(pca$PC2),0)
pca$PC3 <- replace(pca$PC3,is.na(pca$PC3),0)
pca$PC4 <- replace(pca$PC4,is.na(pca$PC4),0)
pca$PC5 <- replace(pca$PC5,is.na(pca$PC5),0)
pca$PC6 <- replace(pca$PC6,is.na(pca$PC6),0)

#plot again
b <- ggplot(pca, aes(PC1, PC2, col = Location, shape = Region)) + geom_point(size = 1.4)
b <- b + scale_colour_manual(values = c("#99cc00", "#ffcc00", "#9933ff", "#f00000", "#0000ff", "#ff6633", "#00cccc", "#ff99cc", "#666666", "#993300", "#ff00cc"))
b <- b + coord_equal() + theme_bw()
b + xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) + ylab(paste0("PC2 (", signif(pve$pve[2], 3),"%)"))
#to get more than score plot in one figure, install and load gridExtra and cowplot packages
install.packages("gridExtra")
library("gridExtra")
install.packages("cowplot")
library("cowplot")

#plot all pcs you need in the graph separately 
a <- ggplot(pca, aes(PC1, PC2, col = Location, shape = Region)) + geom_point(size = 1.5)
a <- a + scale_colour_manual(values = c("#99cc00", "#ffcc00", "#9933ff", "#f00000", "#0000ff", "#ff6633", "#00cccc", "#ff99cc", "#666666", "#993300", "#ff00cc"))
a <- a  + theme_bw() + theme(text = element_text(size = 17)) 
a + xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) + ylab(paste0("PC2 (", signif(pve$pve[2], 3),"%)"))
b <- ggplot(pca, aes(PC1, PC3, col = Location, shape = Region)) + geom_point(size = 1.5) 
b <- b + scale_colour_manual(values = c("#99cc00", "#ffcc00", "#9933ff", "#f00000", "#0000ff", "#ff6633", "#00cccc", "#ff99cc", "#666666", "#993300", "#ff00cc"), guide = "none")
b <- b  + theme_bw() + theme(text = element_text(size = 17)) + theme(legend.position = "none")
b + xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) + ylab(paste0("PC3 (", signif(pve$pve[3], 3),"%)"))
c <- ggplot(pca, aes(PC1, PC4, col = Location, shape = Region)) + geom_point(size = 1.5)
c <- c + scale_colour_manual(values = c("#99cc00", "#ffcc00", "#9933ff", "#f00000", "#0000ff", "#ff6633", "#00cccc", "#ff99cc", "#666666", "#993300", "#ff00cc"), guide = "none") 
c <- c  + theme_bw() + theme(text = element_text(size = 17)) + theme(legend.position = "none")
c + xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) + ylab(paste0("PC4 (", signif(pve$pve[4], 3),"%)"))
d <- ggplot(pca, aes(PC1, PC5, col = Location, shape = Region)) + geom_point(size = 1.5)
d <- d + scale_colour_manual(values = c("#99cc00", "#ffcc00", "#9933ff", "#f00000", "#0000ff", "#ff6633", "#00cccc", "#ff99cc", "#666666", "#993300", "#ff00cc"), guide = "none") 
d <- d  + theme_bw() + theme(text = element_text(size = 17)) + theme(legend.position = "none")
d + xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) + ylab(paste0("PC5 (", signif(pve$pve[5], 3),"%)"))
#then run the following function
grid.arrange(d, b, c, f, ncol = 2, 
+              layout_matrix = cbindividual(c(1,1,1), c(2,3,4)))



