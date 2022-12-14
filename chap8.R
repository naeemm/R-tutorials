#' ---
#' title: "R Code for Chapter 8 of Introduction to Data Mining: Clustering"
#' author: "Michael Hahsler"
#' output:
#'  html_document:
#'    toc: true
#' ---

#' This code covers chapter 8 of _"Introduction to Data Mining"_
#' by Pang-Ning Tan, Michael Steinbach and Vipin Kumar.
#'
#' ![CC](https://i.creativecommons.org/l/by/4.0/88x31.png)
#' This work is licensed under the
#' [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/). For questions please contact
#' [Michael Hahsler](http://michael.hahsler.net).
#'


#' ruspini_scaled data is in package cluster. It is a very simple data set with well separated clusters.
data(ruspini, package="cluster")
#' Shuffle rows
ruspini <- ruspini[sample(1:nrow(ruspini)),]
plot(ruspini)


#' Scale each column in the data to zero mean and unit standard deviation (z-scores). This prevents one attribute with a large range to dominate the others for the distance calculation.
ruspini_scaled <- scale(ruspini)
plot(ruspini_scaled)

#' # Clustering methods
#' ## k-means Clustering
#'
#' Assumes Euclidean distances. We use k=10 clusters and run the algorithm 10 times with random initialized centroids. The best result is returned.
km <- kmeans(ruspini_scaled, centers=4, nstart=10)
km

plot(ruspini_scaled, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID
#' Alternative plot from package cluster (uses principal components analysis for >2 dimensions)
library(cluster)
clusplot(ruspini_scaled, km$cluster)

#' Inspect the centroids (cluster profiles)
km$centers

#+ fig.height=3, fig.width=10
def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(t(1:4)) # 4 plots in one
for(i in 1:4) barplot(km$centers[i,], ylim=c(-2,2), main=paste("Cluster", i))
par(def.par)  #- reset to default

#' Find data for a single cluster
#'
#' All you need is to select the rows corresponding to the cluster. The next
#' example plots all data points of cluster 1
cluster1 <- ruspini_scaled[km$cluster==1,]
head(cluster1)
plot(cluster1, xlim = c(-2,2), ylim = c(-2,2))

#' Try 10 clusters
plot(ruspini_scaled, col=kmeans(ruspini_scaled, centers=10)$cluster)

#' ## Hierarchical Clustering
#'
#' dist defaults to method="Euclidean"
d <- dist(ruspini_scaled)
#' We cluster using complete link
hc <- hclust(d, method="complete")

#' Dendrogram
plot(hc)
rect.hclust(hc, k=4)

plot(as.dendrogram(hc), leaflab="none") # plot dendrogram without leaf labels

#' More plotting options for dendrograms, including plotting
#' parts of large dendrograms can be found [here.](https://rpubs.com/gaston/dendrograms)

cluster_complete <- cutree(hc, k=4)
plot(ruspini_scaled, col=cluster_complete)

#' Try 10 clusters
plot(ruspini_scaled, col=cutree(hc, k=10))

#' Clustering with single link
hc_single <- hclust(d, method="single")
plot(hc_single)
rect.hclust(hc_single, k=4)

cluster_single <- cutree(hc_single, k=4)
plot(ruspini_scaled, col=cluster_single)

#' Try 10 clusters
plot(ruspini_scaled, col=cutree(hc_single, k=10))

#' ## Density-based clustering with DBSCAN

library(dbscan)

#' Parameters: minPts is often chosen as dimensionality of the data +1.
#' Decide on epsilon using the knee in the kNN distance plot
#' (seems to be around eps = .25).
kNNdistplot(ruspini_scaled, k = 3)
abline(h=.25, col="red")

#' run dbscan
db <- dbscan(ruspini_scaled, eps=.25, minPts=3)
db
str(db)
plot(ruspini_scaled, col=db$cluster+1L)
#' __Note:__ 0 is not a color so we add 1 to cluster (0 is black now).
#'
#' Play with eps (neighborhood size) and MinPts (minimum of points needed for core cluster)

#'
#' ## Gaussian Mixture Models
library(mclust)

#' Mclust uses Bayesian Information Criterion (BIC) to find the
#' number of clusters.
m <- Mclust(ruspini_scaled)
summary(m)
plot(m, what = "classification")

#' Rerun with a fixed number of 4 clusters
m <- Mclust(ruspini_scaled, G=4)
summary(m)
plot(m, what = "classification")


#' # Internal Cluster Validation
#'
#' ## Compare the Clustering Quality
#'
#' Look at the within.cluster.ss and the avg.silwidth

#library(fpc)
#' Note: I do not load fpc since the NAMESPACE overwrites dbscan.

fpc::cluster.stats(d, km$cluster)
#cluster.stats(d, cluster_complete)
#cluster.stats(d, cluster_single)

#' Read `? cluster.stats` for an explanation of all the available indices.

sapply(list(
  km=km$cluster,
  hc_compl=cluster_complete,
  hc_single=cluster_single),
       FUN=function(x)
         fpc::cluster.stats(d, x))[c("within.cluster.ss","avg.silwidth"),]

#' ## Silhouette plot
plot(silhouette(km$cluster, d))

#' ## Find Optimal Number of Clusters for k-means
set.seed(1234)
ks <- 2:10

#' ### Within Sum of Squares
#' Use within sum of squares and look for the knee (nstart=5 repeats k-means 5 times and returns the best solution)
WSS <- sapply(ks, FUN=function(k) {
  kmeans(ruspini_scaled, centers=k, nstart=5)$tot.withinss
  })
plot(ks, WSS, type="l")
abline(v=4, col="red", lty=2)

#' ### Average Silhouette Width
#' Use average silhouette width (look for the max)
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(ruspini_scaled, centers=k, nstart=5)$cluster)$avg.silwidth
  })
plot(ks, ASW, type="l")

ks[which.max(ASW)]
abline(v=ks[which.max(ASW)], col="red", lty=2)

#' ### Dunn Index
#' Use Dunn index (another internal measure given by min. separation/ max. diameter)
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(ruspini_scaled, centers=k, nstart=5)$cluster)$dunn
})
plot(ks, DI, type="l")
ks[which.max(DI)]
abline(v=ks[which.max(DI)], col="red", lty=2)


#' ### Gap Statistic
#' see `? clusGap`
library(cluster)
k <- clusGap(ruspini_scaled, kmeans, K.max = 10)
k
plot(k)

#' __Note:__ these methods can also be used for hierarchical clustering.
#'
#' There have been many other methods and indices proposed to determine
#' the number ofvclusters.
#' See, e.g.,  package [NbClust](https://cran.r-project.org/package=NbClust).
#'

#' ## Visualize the Distance Matrix
#'
#' Visualizing the unordered distance matrix does not show much stucutre.
library(seriation)
pimage(d, colorkey=TRUE)

#' Reorder using cluster labels
pimage(d, order=order(km$cluster), colorkey=TRUE)

#' Use dissplot which rearranges clusters, adds cluster labels,
#'  and shows average dissimilarity in the lower half of the plot.
dissplot(d, labels=km$cluster, options=list(main="k-means with k=4"))
dissplot(d, labels=db$cluster+1, options=list(main="DBSCAN"))
#' Spot the problem data points for DBSCAN (we use +1 so the noise is now cluster #1)
#'
#' Misspecified k
dissplot(d, labels=kmeans(ruspini_scaled, centers=3)$cluster)
dissplot(d, labels=kmeans(ruspini_scaled, centers=9)$cluster)


#' # External Cluster Validation
#'
#' External cluster validation uses ground truth information. That is,
#' the user has an idea how the data should be grouped. This could be a know
#' class label not provided to the clustering algorithm.
#'
#' We use an artificial data set with known groups here. First, we need to
#' cluster the new data. We do k-means and hierarchical clustering.

library(mlbench)
shapes <- mlbench.smiley(n=500, sd1 = 0.1, sd2 = 0.05)
plot(shapes)

#' Prepare data
truth <- as.integer(shapes$class)
shapes <- scale(shapes$x)

plot(shapes)

#' Find optimal number of Clusters for k-means
ks <- 2:20

#' Use within sum of squares (look for the knee)
WSS <- sapply(ks, FUN=function(k) {
  kmeans(shapes, centers=k, nstart=10)$tot.withinss
})
plot(ks, WSS, type="l")
#' looks like 6 clusters
km <- kmeans(shapes, centers=6, nstart = 10)
plot(shapes, col=km$cluster)

#' Hierarchical clustering: single-link because of the mouth
d <- dist(shapes)
hc <- hclust(d, method="single")

#' Find optimal number of clusters
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, cutree(hc, k))$avg.silwidth
})
plot(ks, ASW, type="l")
#' 4 clusters
hc_4 <- cutree(hc, 4)
plot(shapes, col=hc_4)

#' Compare with ground truth with the corrected (=adjusted) Rand index (ARI)
#' and the variation of information (VI) index.
cbind(
  kmeans = fpc::cluster.stats(d, km$cluster, truth, compareonly = TRUE),
  hc = fpc::cluster.stats(d, hc_4, truth, compareonly = TRUE)
  )

#' Hierarchical clustering found the perfect clustering.
#'
#' Read `? cluster.stats` for an explanation of all the available indices.


#'
#' # Related Topics
#' ## Outlier Removal
#' It is often useful to remove outliers prior to clustering.
#' A density based method to identify outlier is LOF (Local Outlier Factor).
#' It is related to dbscan. The LOF value for a regular data
#' point is 1. The larger the LOF value gets, the more likely the point is and
#' outlier.
library(dbscan)
lof <- lof(ruspini_scaled, k = 3)
lof
plot(ruspini_scaled, pch = ".", main = "LOF (k=3)")
points(ruspini_scaled, cex = (lof-1)*3, pch = 1, col="red")

#' Find outliers (find the knee)
plot(sort(lof), type = "l")
abline(h = 1.3, col = "red")
plot(ruspini_scaled[lof < 1.3,], main = "Data without outliers")

#' There are many other outlier removal strategies available. See, e.g., package
#' [outliers](https://cran.r-project.org/package=outliers).
#'
#' ## Cluster Tendency
#' Most clustering algorithms will always produce a clustering, even if the
#' data does not contain a cluster structure. It is typically good to check
#' cluster tendency before attempting to cluster the data.
#'
#' We use again the smiley data.
d_shapes <- dist(shapes)

library(seriation)
#' Visual Analysis for Cluster Tendency Assessment (VAT) reorders the
#' objects to show potential clustering tendency as a block structure
#' (dark blocks along the main diagonal).
VAT(d_shapes)

#' iVAT uses largest distances in all possible paths between the objects
#' instead of the distances to make the block structure better visible.
iVAT(d_shapes)

#' Both plots show a strong cluster structure with 5 clusters.
#'
#' Compare with random data.
data_random <- matrix(runif(2*500), ncol=2, dimnames = list(NULL, c("x", "y")))
plot(data_random)

d_random <- dist(data_random)
VAT(d_random)
iVAT(d_random)
#' There is very little structure visible.

