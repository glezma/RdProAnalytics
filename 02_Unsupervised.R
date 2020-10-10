# k-means 
library(tidyverse)
setwd("~/Programming/Analytics")
x<-read_csv("data_clustering.csv")
kmeans(x, centers = 3, nstart = 20)


# Create the k-means model: km.out
km.out <-kmeans(x,centers=3,
                nstart=20)

# Inspect the result
summary(km.out)
# Print the cluster membership component of the model
print(km.out$cluster)

# Print the km.out object
print(km.out)

# ploting 
# Scatter plot of x
plot(x,col=km.out$cluster,main="k-means with 3 clusters",ylab="",xlab="")


# K-means has a random component which is important to understand

# Set up 2 x 3 plotting grid
par(mfrow = c(2, 3))

# Set seed
set.seed(1)

for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(x, centers=3,nstart=1)
  
  # Plot clusters
  plot(x, col = km.out$cluster, 
       main = km.out$tot.withinss, 
       xlab = "", ylab = "")
}

km.out <- kmeans(x, centers=3,nstart=200)

# Plot clusters
plot(x, col = km.out$cluster, 
     main = km.out$tot.withinss, 
     xlab = "", ylab = "")


# GGplot2
ggplot(data = x, aes(x=x1,y=x2,color=factor(km.out$cluster)))+geom_point()+stat_ellipse()


# The elbow
 
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Set k equal to the number of clusters corresponding to the elbow location
k <- 2
Xraw<-read_csv('')
library(Rtsne)
X<-normalize_input(Xraw)
tsne_out<-Rtsne(X, dims = 2)

library(ggplot2)
tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = iris_unique$Species)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col))

