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
fx <- function(x)
     {
    return(cec2013(8, x))
     }

GA10 <- ga(type = "real-valued",fitness = fx, min = rep(-100, 10),  max = rep(100, 10),  popSize = 100,   seed = 1234, monitor = NULL, maxiter = 100,pmutation = 0.1)

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



GA10.km3<-kmeans(GA10@population, 3, iter.max = 100, nstart = 10) #klastry 3, iter 10, poczatkow 10 pkt 
GA10.km6 <- kmeans(GA10@population, 6, iter.max = 100, nstart = 10)
GA10.km10 <- kmeans(GA10@population, 10, iter.max = 100, nstart = 30)

GA30.km3 <- kmeans(GA30@population, 3, iter.max = 100, nstart = 10)  
GA30.km6 <- kmeans(GA30@population, 6, iter.max = 100, nstart = 10)
GA30.km10 <- kmeans(GA30@population, 10, iter.max = 10, nstart = 20)

GA50.km3 <- kmeans(GA50@population, 3, iter.max = 100, nstart = 10) 
GA50.km6 <- kmeans(GA50@population, 6, iter.max = 100, nstart = 10)
GA50.km10 <- kmeans(GA50@population, 10, iter.max = 10, nstart = 20)

plot(GA10@population, col = GA10.km10$centers)
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
GA10.agn.sgl <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "single")
GA10.agn.cmpl <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "complete")

GA30.agn.ave <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "average")
GA30.agn.sgl <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "single")
GA30.agn.cmpl <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "complete")

GA50.agn.ave <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "average")
GA50.agn.sgl <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "single")
GA50.agn.cmpl <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "complete")



#--------------------------HCLUST-----------------

GA10.hcl.cmpl<-hclust(dist(GA10@population), method = "complete")
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
w<-wcls.matrix(data = GA10@population,clust = GA10.km3$cluster,cluster.center = GA10.km3$centers)
b <- bcls.matrix(mean = GA10.km3$withinss, clust = GA10.km3$cluster, cluster.center = GA10.km3$centers)
bcls.matrix()
ss = sum(diag(w))
GA10.km3$centers


