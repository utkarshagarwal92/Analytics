## This is what the data looks like, following code will help in doing the Principal Component Analysis for this data.....
# data.frame':	1000 obs. of  10 variables:
# $ perform: int  2 1 2 1 1 2 1 2 2 3 ...
# $ leader : int  4 1 3 6 1 8 1 1 1 1 ...
# $ latest : int  8 4 5 10 5 9 5 7 8 9 ...
# $ fun    : int  8 7 9 8 8 5 7 5 10 8 ...
# $ serious: int  2 1 2 3 1 3 1 2 1 1 ...
# $ bargain: int  9 1 9 4 9 8 5 8 7 3 ...
# $ value  : int  7 1 5 5 9 7 1 7 7 3 ...
# $ trendy : int  4 2 1 2 1 1 1 7 5 4 ...
# $ rebuy  : int  6 2 6 1 1 2 1 1 1 1 ...
# $ brand  : Factor w/ 10 levels "a","b","c","d",..: 1 1 1 1 1 1 1 1 1 1 ...
 
dim(brand.ratings)
brand.sc <- brand.ratings
str(brand.sc)

brand.sc[, 1:9] <- scale(brand.sc[ , 1:9])
str(brand.sc)
dim(brand.sc)
summary(brand.sc)


brand.pc <- prcomp(brand.sc[,1:9])
dim(brand.pc)
summary(brand.pc)
# rotation matrix of varialbe loadings...
# std dev of eigne values...
plot(brand.pc, type = "l")# elbow selection rule...
# So here, you basically look for an elbow in the graph and select #dots above that and leave all the flat ones.
# #Dots counted like that are the prominent number of factors that are explaining a certain Dependent Variable.
pc_var <- (brand.pc$sdev)^2
prop_var <- pc_var/sum(pc_var)
# cummulative scree plot...
plot(cumsum(prop_var), xlab = "principal component...", ylab = "cummulative proprtion of variance explained",
     type = "b")
biplot(brand.pc, cex = c(0.5, 1),) #multi-dimensional plotting...0

# install.packages("factoextra")
library(factoextra)

fviz_eig(brand.pc) # this graph is just like the plot made at #14...

get_eigenvalue(brand.pc)
library(FactoMineR)
res.pca <- PCA(brand.sc, quali.sup = 10)
# ""quali.sup"" vector indicating indexes of supplementary variables... 
# segmentation targeting and positioning...STP analogy for marketing...

# computing means across 10 brands...
brand.mean <- aggregate(brand.sc[,1:9], list(brand.sc[,10]), mean)
brand.mean


brand.mean <- brand.mean[,-1] # excluding brand column...
brand.mean
rownames(brand.mean) <- paste("", letters, sep = "")[1:10]
## with this we have a,b,c,d... instead of 1,2,3,4....for row numbers...
brand.mean

brand.mu.pc <- prcomp(brand.mean, scale = T)
summary(brand.mean)
summary(brand.mu.pc)

biplot(brand.mu.pc, main = "Brand Positioning...", cex = c(1,1))
cor(brand.sc[,1:9])

## MDS by mdscale...

brand.dist <- dist(brand.mean)
(brand.mds <- cmdscale(brand.dist))
# cmdscale: classical multidimensional scaling of a data matrix...

# compute by distances...
plot(brand.mds, type = "n")


rownames(brand.mds) <- paste("", letters, sep = "")[1:10]
brand.mds

text(brand.mds, rownames(brand.mds), cex = 1)



# To import, provide the directory(path)
infiniti <- read.csv("PATH/Infiniti.csv", header = T)
dim(infiniti)
str(infiniti)
infi <- infiniti[,2:16]
infi.dist <- dist(infi)# measure distance matrix between features...
infi.dist
(infi.mds <- cmdscale(infi.dist))
infi.mds
plot(infi.mds, type = "n")
rownames(infi.mds) <- as.character(infiniti[,1]) #changing row names...
infi.mds
text(infi.mds, rownames(infi.mds), cex = 0.7)
infiniti.mu.pc <- prcomp(infi.mds, scale = T)
res.pca <- PCA(infiniti.mu.pc, quali.sup = 1)
res.pca <- PCA(infi.mds, quali.sup = 1)
biplot(infiniti.mu.pc, main = "Brand Positioning...", cex = c(1,1))









