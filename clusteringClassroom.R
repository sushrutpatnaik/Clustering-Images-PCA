require(jpeg)
require(RCurl)
url <-"https://parkers-images.bauersecure.com/pagefiles/203321/cut-out/600x400/header_488gtb_260716(19).jpg"
readImage <- readJPEG(getURLContent(url, binary=TRUE))
dm <- dim(readImage)
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

plot(y ~ x, data=rgbImage, main="Ferrari",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")


#1 Perform K-means clustering
#perform clustering using k=5
kColors <- 5  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="Ferrari",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 5 colours")


#perform clustering using k=4
kColors <- 4  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="Ferrari",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 4 colours")

#perform clustering using k=6
kColors <- 6  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="Ferrari",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 6 colours")

#perform clustering using k=3
kColors <- 2  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="Ferrari",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 2 colours")

#perform clustering using k=3
kColors <- 3  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="Ferrari",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 3 colours")


#WCSS Plot as a evaluation metric for clustering
wssplot <- function(rgbImage, nc=15, seed=1234){
  wss <- (nrow(rgbImage)-1)*sum(apply(rgbImage,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(rgbImage, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#plot of WCSS vs k
wssplot(rgbImage, nc=6) 

#from the plot it looks like k=2 is the preferred value for k



#2 Peform PCA using dimension reduction
library(jpeg)

#Extract the individual color value matrices to perform PCA
r <- readImage[,,1]
g <- readImage[,,2]
b <- readImage[,,3]

#PCA is performed on each color value matrix.
r.pca <- prcomp(r, center = FALSE)
g.pca <- prcomp(g, center = FALSE)
b.pca <- prcomp(b, center = FALSE)

#collect PCA objects into a list
rgb.pca <- list(r.pca, g.pca, b.pca)


#reconstructing the image using projections of the data using increasing amounts of Principal components
for (i in seq.int(3, round(nrow(readImage) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('compressed_', round(i,0), '_components.jpg', sep = ''))
}