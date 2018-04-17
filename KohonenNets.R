# Unsupervised Kohonen Nets
# Installing Libraries and loading
install.packages("kohonen")
install.packages("RColorBrewer")
install.packages("RCurl")
library("kohonen")
library("RColorBrewer")
library("RCurl")

# Loading the dataset
NBA <- read.csv(text = getURL("https://raw.githubusercontent.com/clarkdatalabs/soms/master/NBA_2016_player_stats_cleaned.csv"), 
                sep = ",", header = T, check.names = FALSE)

# Checking the column names
colnames(NBA)
str(NBA)
# Let us visualize Self Organizing Masp
NBA.measures1 <- c("FTA", "2PA", "3PA")

# Before plotting the SOM, we are normalizing and centering the data
# Read more about scaling and centering over here "http://shahramabyari.com/2015/12/20/data-preparation-for-predictive-modeling-centering-scaling/"
# Defined the grid size as 6 x 4
NBA.SOM1 <- som(scale(NBA[NBA.measures1]), grid = somgrid(6, 4, "rectangular"))
plot(NBA.SOM1)

# Radius of wedge corresponds to the magnitude of a particular dimension
# Patterns are made with players generally clustered by how many of each type of shot they take
# The players are usually clustered with same number of shots are with the shot nos near by

# Each cell above displays a representative vector
# The major drawback from the above is that we cannot visualize how many players are there in each cluster
# To overcome that, we use the heatmap
colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}
plot(NBA.SOM1, type = "counts", palette.name = colors, heatkey = TRUE)

# Alternatively, if we want to plot the points (no of players in a cluster)
# On the left, players are plotted on this map based on how close their stat lines are to these representative vectors
plot(NBA.SOM1, type = "mapping", pchs = 20, main = "Mapping Type SOM")

# Finding the distance between the clusters
plot(NBA.SOM1, type = "dist.neighbours", palette.name = terrain.colors)

# Supervised Kohonen Nets
# Expanded list of Player stats.
NBA.measures2 <- c("FTA", "FT", "2PA", "2P", "3PA", "3P", "AST", "ORB", "DRB", 
                   "TRB", "STL", "BLK", "TOV")
# Randomly divide data to test and train
training_indices <- sample(nrow(NBA), 200)
NBA.training <- scale(NBA[training_indices, NBA.measures2])
# We need to rescale the testing data because we need to scale it according to the training data
NBA.testing <- scale(NBA[-training_indices, NBA.measures2], center = attr(NBA.training, 
                                                                          "scaled:center"), scale = attr(NBA.training, "scaled:scale"))
# Training
NBA.SOM3 <- xyf(NBA.training, classvec2classmat(NBA$Pos[training_indices]), 
                grid = somgrid(6, 4, "rectangular"), rlen = 100)
NBA.SOM3 <- as.matrix(NBA.SOM3)
pos.prediction <- predict(NBA.SOM3, newdata = NBA.testing)

table(NBA[-training_indices, "Pos"], pos.prediction$prediction)


NBA.SOM4 <- xyf(scale(NBA[, NBA.measures2]), classvec2classmat(NBA[, "Pos"]), 
                grid = somgrid(13, 13, "hexagonal"))

par(mfrow = c(1, 2))
plot(NBA.SOM4, type = "codes", main = c("Codes X", "Codes Y"))
NBA.SOM4.hc <- cutree(hclust(dist(NBA.SOM4$codes[[2]])), 5)
add.cluster.boundaries(NBA.SOM4, NBA.SOM4.hc)


