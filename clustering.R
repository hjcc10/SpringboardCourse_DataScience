#  
#
############################################################################
#
# Mini-Project: K-means (HC March 2017)
#
############################################################################
#
#
# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle", "NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)
#

#
# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
#
# Scaling the dataset and removing the 1st columd (Type)
df_wine <- scale(wine[-1])
#
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.
#
# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 
#
# Defining wssplot function
wssplot <- function(data, nc=15, seed=1234) {
	wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
    for (i in 2:nc) {
		set.seed(seed)
	    wss[i] <- sum(kmeans(data, centers=i)$withinss) 
	}
	plot(1:nc, wss, type="b", xlab="Number of Clusters",
	 	ylab="Within groups sum of squares")
}
#
# Running wssplot function
wssplot(df_wine)
#

#
# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works
#
# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.
#
# Loading NbClust library
library(NbClust)
#
# Generating a random number
set.seed(1234)
#
# Running the k-means clustering based on NCClust
nc <- NbClust(df_wine, min.nc = 2, max.nc = 15, method="kmeans")
#
# Plotting the outcome (barplot)
barplot(table(nc$Best.n[1,]),
	xlab = "Number of Clusters", ylab="Number of Criteria",
	main = "Clusters by 26 Criteria",
	las = 2)
#

#
# Exercise 3: How many clusters does this method suggest?
#
# ### See output... also 3 clusters like the 1st method. ###
#

#
# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
#
# Running k.means clustering using the suggested number of clusters (3), and
# assign the output into a variable named fit.km
fit.km <- kmeans(df_wine, 3, nstart = 25)
#
# Now we want to evaluate how well this clustering does.

#
# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
#
# Using table() function compairing fit.km$clusters and the actual wine tipes
table(x=wine$Type, y=fit.km$cluster)
#
#
# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
#
# Loadind cluster library
library(cluster)
#
# Running clusplot() function to visualize the clusters
mydata_wine <- data.frame(df_wine, cluster = fit.km$cluster)
clusplot(mydata_wine, fit.km$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)
