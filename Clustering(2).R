##############
# Clustering #
##############

# Prepared by Jay Simon
# Last updated on 10/24/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of how to perform k-means and hierarchical clustering in R
# The script uses the mtcars data set.

# Packages required: cluster, DMwR, ggdendro


# If you have not installed the "cluster" package, do so before continuing.

# Activate the cluster package by checking it in the bottom right panel of RStudio, or with the following command:
library("cluster", lib.loc="~/R/win-library/3.3")

# Before clustering, we will want to normalize the data set.
# The following command converts the mtcars data set into standardized values (z-scores)
mtcars.norm <- scale(mtcars)

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
# Always run this command immediately before the command that uses randomness
set.seed(12345)

# "kmeans" is the function that applies k-means clustering to a data set
# The following command uses k-means clustering to split the data into 3 clusters
mtcars.kmclusters <- kmeans(mtcars.norm, 3, nstart=10)

# The following command displays the centroids of the 3 clusters (in z-scores)
mtcars.kmclusters$centers

# To convert the centroids back into their original units, we need the unscale function from the DMwR package
# Activate the DMwR package by checking it in the bottom right panel of RStudio, or with the following command:
library("DMwR", lib.loc="~/R/win-library/3.3")

# The following command shows the centroids in original units
unscale(mtcars.kmclusters$centers, mtcars.norm)

# The following command displays the sizes (# of data points) of the 3 clusters
mtcars.kmclusters$size


set.seed(12345)
# A common question in k-means clustering is how to choose the "best" value of k
# The "clusGap" and "maxSE" functions can be used for this, as shown in the following two lines
gaps <- clusGap(mtcars.norm,kmeans,30)
maxSE(gaps$Tab[,"gap"],gaps$Tab[,"SE.sim"],"Tibs2001SEmax")
# The underlying computations are rather complicated, and there are many variations on this approach

# The "gap" represents the improvement over a distribution of points that has no clustering
# We want a value of k for which the gap is large
# However, the error involved with estimating the gap metric increases with k
# The maxSE function determines an appropriate balance between maximizing the gap and minimizing the error in estimating it
# The following command shows the gap metric vs. k:
plot(gaps$Tab[,"gap"])



# "hclust" is the function that applies hierarchical clustering to a data set
# The following two commands will apply hierarchical clustering using the centroid method
distances <- dist(mtcars.norm, method="euclidean")
mtcars.hclusters <- hclust(distances, method="centroid")

# See online documentation for the hclust function for additional methods (e.g. Ward's method).

# We will need the "ggdendro" package to get a good display of the dendrogram
# Install the ggdendro package before proceeding, if you have not already

# Activate the ggdendro package by checking it in the bottom right panel of RStudio, or with the following command:
library("ggdendro", lib.loc="~/R/win-library/3.3")

#The following command creates a dendrogram
#Set labels to FALSE if you do not want the labels for each data point to appear
ggdendrogram(mtcars.hclusters, labels=TRUE)

#Click "Zoom" above the plot to show the dendrogram in a new window for a better display
#Use the "Export" options above the plot to transfer the dendrogram outside of R