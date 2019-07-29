iris
iris$Species <- NULL
k<-3
iris.scaled <- scale(iris)
cluster <- kmeans(iris.scaled,k,nstart = 20)

library(factoextra)

fviz_cluster(kmeans,data=iris.scaled,type.frame="norm", ellipse="norm")
fviz_nbcluster(data.iris.scaled,kmeans,ellipse.type="norm")

k.max <- 20
data = iris
wss <- sapply(1:k.max, function(k){kmeans(data,k)$tot.withinss})

plot(1:k.max,wss)
abline(v=3,lty=2)




