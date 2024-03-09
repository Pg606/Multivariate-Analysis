library(cluster)
library(readr)
library(factoextra)
library(magrittr)
library(NbClust)

# With made up data. 
l_d <- read.csv("/Users/Prateekg/Downloads/data1.csv",row.names=1)
l_d
matstd.l_d <- scale(l_d)
l_d_scaled <- scale(l_d)
attach(l_d)
dim(l_d)
str(l_d)
#lung$LUNG_CANCER <- as.factor(lung$LUNG_CANCER)
str(l_d)
matstd.l_d <- scale(l_d)

# Creating a (Euclidean) distance matrix of the standardized data 
dist.l_d <- dist(matstd.l_d, method="euclidean")
dist.l_d

# Invoking hclust command (cluster analysis by single linkage method)      
clusl_d.nn <- hclust(dist.l_d, method = "single") 
plot(clusl_d.nn, hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Nearest neighbor linkage")

#Default - Complete
clusl_d.fn <- hclust(dist.l_d)
plot(clusl_d.fn,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Farthest neighbor linkage")

#Average
clusl_d.avl <- hclust(dist.l_d,method="average")
plot(clusl_d.avl,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Group average linkage")

# Plotting vertical dendrogram      
plot(as.dendrogram(clusl_d.nn),ylab="Distance between Company Layoffs",ylim=c(0,2.5),main="Dendrogram of six company")

plot(as.dendrogram(clusl_d.nn),ylab="Distance between Company Layoffs",ylim=c(0,6),
     main="Dendrogram. People layoff")


plot(as.dendrogram(clusl_d.nn), xlab= "Distance between CVompany Layoffs", xlim=c(6,0),
     horiz = TRUE,main="Dendrogram. People layoff")

# We will use agnes function as it allows us to select option for data standardization, the distance measure and clustering algorithm in one single function

(agn.l_d <- agnes(l_d, metric="euclidean", stand=TRUE, method = "single"))



#  Description of cluster merging
agn.l_d$merge

#Dendogram
plot(as.dendrogram(agn.l_d), xlab= "Distance between Countries",xlim=c(8,0),
     horiz = TRUE,main="Dendrogram of company layoffs")

#Interactive Plots
#plot(agn.employ,ask=TRUE)
plot(agn.l_d, which.plots=1)
plot(agn.l_d, which.plots=2)
plot(agn.l_d, which.plots=3) # doesn't matter

# K-Means Clustering

matstd.l_d <- scale(l_d[,1:6])
# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen
(kmeans2.l_d <- kmeans(matstd.l_d,2,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.l_d$betweenss/kmeans2.l_d$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Computing the percentage of variation accounted for. Three clusters
(kmeans3.l_d <- kmeans(matstd.l_d,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.l_d$betweenss/kmeans3.l_d$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Computing the percentage of variation accounted for. Four clusters
(kmeans4.l_d <- kmeans(matstd.l_d,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.l_d$betweenss/kmeans4.l_d$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

# Computing the percentage of variation accounted for. Five clusters
(kmeans5.l_d <- kmeans(matstd.l_d,5,nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5.l_d$betweenss/kmeans5.l_d$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5
(kmeans6.l_d <- kmeans(matstd.l_d,6,nstart = 10))

# Computing the percentage of variation accounted for. Six clusters
perc.var.6 <- round(100*(1 - kmeans6.l_d$betweenss/kmeans6.l_d$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6
attributes(perc.var.6)
Variance_List <- c(perc.var.2,perc.var.3,perc.var.4,perc.var.5,perc.var.6)

Variance_List
plot(Variance_List)
#
# Saving four k-means clusters in a list
clus1 <- matrix(names(kmeans4.l_d$cluster[kmeans4.l_d$cluster == 1]), 
                ncol=1, nrow=length(kmeans4.l_d$cluster[kmeans4.l_d$cluster == 1]))
colnames(clus1) <- "Cluster 1"
clus2 <- matrix(names(kmeans4.l_d$cluster[kmeans4.l_d$cluster == 2]), 
                ncol=1, nrow=length(kmeans4.l_d$cluster[kmeans4.l_d$cluster == 2]))
colnames(clus2) <- "Cluster 2"
clus3 <- matrix(names(kmeans4.l_d$cluster[kmeans4.l_d$cluster == 3]), 
                ncol=1, nrow=length(kmeans4.l_d$cluster[kmeans4.l_d$cluster == 3]))
colnames(clus3) <- "Cluster 3"
clus4 <- matrix(names(kmeans4.l_d$cluster[kmeans4.l_d$cluster == 4]), 
                ncol=1, nrow=length(kmeans4.l_d$cluster[kmeans4.l_d$cluster == 4]))
colnames(clus4) <- "Cluster 4"
list(clus1,clus2,clus3,clus4)


# gg Visualizations with Dataset
res.dist <- get_dist(matstd.l_d, stand = TRUE, method = "pearson")

# Understand the Distance Between States
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Lets Try to Find the Optimal Distance
fviz_nbclust(matstd.l_d, kmeans, method = "gap_stat")

set.seed(123)
km.res <- kmeans(matstd.l_d, 3, nstart = 25)
# Visualize
fviz_cluster(km.res, data = matstd.l_d, ellipse.type = "convex",palette = "jco",ggtheme = theme_minimal())

# If your data has outliears , use PAM method
pam.res <- pam(matstd.l_d, 3)
# Visualize
fviz_cluster(pam.res)

# Hierarchial Clusiering
res.hc <- matstd.l_d %>% scale() %>% dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
# Lets see what the optimal numbers of clusers are
# Compute

res.nbclust <- l_d %>% scale() %>% NbClust(distance = "euclidean", min.nc = 2, max.nc = 19, method = "complete", index ="all") 

# Visualize
library(factoextra)

# Compute NbClust
res.nbclust <- l_d %>% scale() %>% NbClust(distance = "euclidean", min.nc = 2, max.nc = 19, method = "complete", index ="all")

# Visualize NbClust results
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

# Quality of Clustering

set.seed(123)
# Enhanced hierarchical clustering, cut in 3 groups
res.hc <- l_d[, -1] %>% scale() %>%
  eclust("hclust", k = 2, graph = FALSE)

# Visualize with factoextra
fviz_dend(res.hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)

#Inspect the silhouette plot:
fviz_silhouette(res.hc)

# Silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]
