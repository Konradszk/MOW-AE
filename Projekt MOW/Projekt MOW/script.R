#set.seed(542)
#wymiar_problemu = 10
#func.num <- 15

#fitne <- function(a) cec2013(func.num, rep(5, 30))


#fitne(5)

#summary(GA)
#plot(GA)
#func.num <- 8
#D <- 10
#plot(cec2013(func.num, rep(15, D)))
if (!"GA" %in% row.names(installed.packages()))
    install.packages("GA")
library(GA)
if (!"cluster" %in% row.names(installed.packages()))
    install.packages("cluster")
library(cluster)
if (!"stats" %in% row.names(installed.packages()))
    install.packages("stats")
library(stats)
if (!"doRNG" %in% row.names(installed.packages()))
    install.packages("doRNG")
library(doRNG)
if (!"cec2103" %in% row.names(installed.packages()))
    install.packages("cec2013")
library(cec2013)
if (!"dbscan" %in% row.names(installed.packages()))
    install.packages("dbscan")
library(dbscan)

#------------------------------------------------------------------------
fx <- function(x) {
    return(cec2013(8, x))
}
srednia_kmean <- function(GA, GA.size) {
    sr <- rep(0, GA.size)
    for (i in GA@popSize)
        sr <- sr + GA@population[i,]
    sr <- sr / GA.size
    return(sr)

}
B_W_pam = function(pop, gpam) {
    v.pred <- as.integer(gpam$clustering)
    cls.attr <- cls.attrib(pop, v.pred)
    center <- cls.attr$cluster.center
    size <- cls.attr$cluster.size
    mean <- cls.attr$mean

    W.matrix <- wcls.matrix(pop, v.pred, center)
    B.matrix <- bcls.matrix(center, size, mean)
    sum(diag(B.matrix)) / sum(diag(W.matrix))
}
B_W_kmean = function(pop, gKmean, GAx, GAx.size) {
    v.pred <- as.integer(gKmean$cluster)

    W.matrix <- wcls.matrix(pop, v.pred, gKmean$center)
    B.matrix <- bcls.matrix(gKmean$centers, gKmean$size, srednia_kmean(GAx, GAx.size))
    sum(diag(B.matrix)) / sum(diag(W.matrix))
}
silhouette_kmean <- function(gkmean, GAx) {
    # Silhouette coefficient of observations
    sil3 <- silhouette(gkmean$cluster, dist(GAx@population))
    # Summary of silhouette analysis
    si.sum <- summary(sil3)
    # Average silhouette width of each cluster
    si.sum$clus.avg.widths
    #The total average(mean of all individual silhouette widths)
    si.sum$avg.width

    # The size of each clusters
}
B_W_dbscam_size <- function(db, GAx) {
    n.clust <- max(db$cluster + 1)
    n.in.clust <- rep(0, n.clust)
    for (i in db$cluster)
        n.in.clust[i + 1] <- n.in.clust[i + 1] + 1
    return(n.in.clust[2:n.clust]) #zwraca bez szumu
}
B_W_dbscam_center <- function(db, GAx) { #trzeba uwzglednic szum bo WCV wymaga :(
    n.clust <- max(db$cluster+1)
    m.maatrix <- length(colMeans(GA10@population[GA10.db$cluster == n.clust-1,]))
    A = matrix(nrow = n.clust, ncol = m.maatrix)
    for (i in c(0:n.clust-1))
        A[i+1,] = colMeans(GAx@population[db$cluster == i,])
    return(A)

}

GA10 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 10), max = rep(100, 10), popSize = 100, seed = 1234, monitor = NULL, maxiter = 100, pmutation = 0.1)

GA30 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 30), max = rep(100, 30), popSize = 300, seed = 1234, monitor = NULL, maxiter = 300, pmutation = 0.1)

GA50 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 50), max = rep(100, 50), popSize = 500, seed = 1234, monitor = NULL, maxiter = 500, pmutation = 0.1)

GA10.data <- data.frame(GA10@population, stringsAsFactors = TRUE)
GA30.data <- data.frame(GA30@population, stringsAsFactors = TRUE)
GA50.data <- data.frame(GA50@population, stringsAsFactors = TRUE)

write.table(format(GA10.data, digits = 2.0), file = "GA10population")
write.table(format(GA30.data, digits = 2.0), file = "GA30population")
write.table(format(GA50.data, digits = 2.0), file = "GA50population")
#plot(GA10)
#plot(GA30)
#plot(GA50@fitness)
#summary(GA10)
#summary(GA30)
#summary(GA50)
#------------------------------------------------------------------------KMEANS------------------------------------------------------



GA10.km3 <- kmeans(GA10@population, 3, iter.max = 100, nstart = 10) #klastry 3, iter 10, poczatkow 10 pkt 
GA10.km6 <- kmeans(GA10@population, 6, iter.max = 100, nstart = 10)
GA10.km10 <- kmeans(GA10@population, 10, iter.max = 100, nstart = 30)
GA10.km30 <- kmeans(GA10@population, 30, iter.max = 100, nstart = 30)
GA30.km3 <- kmeans(GA30@population, 3, iter.max = 100, nstart = 10)
GA30.km6 <- kmeans(GA30@population, 6, iter.max = 100, nstart = 10)
GA30.km10 <- kmeans(GA30@population, 10, iter.max = 10, nstart = 20)

GA50.km3 <- kmeans(GA50@population, 3, iter.max = 100, nstart = 10)
GA50.km6 <- kmeans(GA50@population, 6, iter.max = 100, nstart = 10)
GA50.km10 <- kmeans(GA50@population, 10, iter.max = 10, nstart = 20)

#plot(GA10@population, col = GA10.km10$centers)
#--------------------KMEDOID--------------

GA10.kmed3 <- pam(GA10@population, 3, medoids = 3:1)
GA10.kmed6 <- pam(GA10@population, 6, medoids = 6:1)
GA10.kmed10 <- pam(GA10@population, 10, medoids = 10:1)

GA30.kmed3 <- pam(GA30@population, 3, medoids = 3:1)
GA30.kmed6 <- pam(GA30@population, 6, medoids = 6:1)
GA30.kmed10 <- pam(GA30@population, 10, medoids = 10:1)

GA50.kmed3 <- pam(GA50@population, 3, medoids = 3:1)
GA50.kmed6 <- pam(GA50@population, 6, medoids = 6:1)
GA50.kmed10 <- pam(GA50@population, 10, medoids = 10:1)

#--------------------------AGNES-----------------

GA10.agn.ave <- agnes(GA10@population, diss = FALSE, metric = "euclidean", method = "average")
GA10.agn.sgl <- agnes(GA10@population, diss = FALSE, metric = "euclidean", method = "single")
GA10.agn.cmpl <- agnes(GA10@population, diss = FALSE, metric = "euclidean", method = "complete")

GA30.agn.ave <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "average")
GA30.agn.sgl <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "single")
GA30.agn.cmpl <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "complete")

GA50.agn.ave <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "average")
GA50.agn.sgl <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "single")
GA50.agn.cmpl <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "complete")



#--------------------------HCLUST-----------------

GA10.hcl.cmpl <- hclust(dist(GA10@population), method = "complete")
GA10.hcl.avr <- hclust(dist(GA10@population), method = "average")
GA10.hcl.sgl <- hclust(dist(GA10@population), method = "single")

GA30.hcl.cmpl <- hclust(dist(GA30@population), method = "complete")
GA30.hcl.avr <- hclust(dist(GA30@population), method = "average")
GA30.hcl.sgl <- hclust(dist(GA30@population), method = "single")

GA50.hcl.cmpl <- hclust(dist(GA50@population), method = "complete")
GA50.hcl.avr <- hclust(dist(GA50@population), method = "average")
GA50.hcl.sgl <- hclust(dist(GA50@population), method = "single")



#--------------------------DBSCAN-----------------

GA10.db <- fpc::dbscan(GA10@population, eps = 50, MinPts = 5)

GA30.db <- fpc::dbscan(GA30@population, eps = 70, MinPts = 5)

GA50.db <- fpc::dbscan(GA50@population, eps = 70, MinPts = 5)


#----------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------OCENIANIE----------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------
#w<-wcls.matrix(data = GA10@population,clust = GA10.km3$cluster,cluster.center = GA10.km3$centers)
#b <- bcls.matrix(mean = GA10.km3$withinss, clust = GA10.km3$cluster, cluster.center = GA10.km3$centers)
#bcls.matrix()
#ss <- sum(diag(w))
#GA10.km3$centers



B_W(GA50@population, GA50.kmed10)

summary(GA10.kmed10)

B_W(GA50@population, GA50)

GA10.km3$iter

GA10.agn.ave$ac
GA50.km10[4]
cec2013(8, GA10.km10$centers[1,])
summary(GA10.km10)
summary(GA10.hcl.avr)
GA10.km10$withinss
GA10.kmed10
GA10.hcl.avr$
GA10.db
GA10.km3$iter

GA50@population

as.vector(GA50@solution)

GA30.km3

length(GA10.db$cluster)
GA10.db$cluster
#B_W_kmean(GA10@population,GA10.km3,GA10,10 <----- przyladowe zuycie wazne!
#-----------------do kmenas ---- silhouette

GA10.db
B_W_dbscam(GA10.db,GA10)
silhouette_kmean(GA10.km30, GA10)
wc
B_W_dbscam = function(db, GAx, GAx.size) {
    v.pred <- as.integer(db$cluster)

    W.matrix <- wcls.matrix(GAx@population, v.pred, as.data.frame.ts( db$cluster))
    #B.matrix <- bcls.matrix(B_W_dbscam_center(db, GAx), B_W_dbscam_size(db,GAx), srednia_kmean(GAx, GAx.size))
   #sum(diag(B.matrix)) / sum(diag(W.matrix))
}
B_W_dbscam(GA10.db, GA10, 10)
B_W_kmean(GA10@population, GA10.km3, GA10, 10)
as.vector(B_W_dbscam_center(GA10.db, GA10))
GA10.db$cluster
GA10.db$cluster
GA10.km3$center
