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

GA10 <- ga(type = "real-valued",fitness = fx, min = rep(-100, 10),  max = rep(100, 10),  popSize = 100,   seed = 1234, monitor = NULL, maxiter = 100,pmutation = 0.5)

GA30 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 30), max = rep(100, 30), popSize = 300, seed = 1234, monitor = NULL, maxiter = 300, pmutation = 0.5)

GA50 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 50), max = rep(100, 50), popSize = 500, seed = 1234, monitor = NULL, maxiter = 500, pmutation = 0.5)

plot(GA10)
plot(GA30)
plot(GA50)
#summary(GA10)
#summary(GA30)
#summary(GA50)
#------------------------------------------------------------------------KMEANS------------------------------------------------------



GA10.km4<-kmeans(GA10@population, 3, iter.max = 100, nstart = 10) #klastry 3, iter 10, poczatkow 10 pkt 
GA10.km5 <- kmeans(GA10@population, 6, iter.max = 100, nstart = 10)
GA10.km10 <- kmeans(GA10@population, 10, iter.max = 10, nstart = 20)

GA30.km4 <- kmeans(GA30@population, 3, iter.max = 100, nstart = 10)  
GA30.km5 <- kmeans(GA30@population, 6, iter.max = 100, nstart = 10)
GA30.km10 <- kmeans(GA30@population, 10, iter.max = 10, nstart = 20)

GA50.km4 <- kmeans(GA50@population, 3, iter.max = 100, nstart = 10) 
GA50.km5 <- kmeans(GA50@population, 6, iter.max = 100, nstart = 10)
GA50.km10 <- kmeans(GA50@population, 10, iter.max = 10, nstart = 20)

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
GA30.agn.sgl <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "single")
GA50.agn.cmpl <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "complete")

GA10.agn.ave <- agnes(GA10@population, diss = FALSE, metric = "euclidean", method = "average")
GA30.agn.sgl <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "single")
GA50.agn.cmpl <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "complete")

GA10.agn.ave <- agnes(GA10@population, diss = FALSE, metric = "euclidean", method = "average")
GA30.agn.sgl <- agnes(GA30@population, diss = FALSE, metric = "euclidean", method = "single")
GA50.agn.cmpl <- agnes(GA50@population, diss = FALSE, metric = "euclidean", method = "complete")

plot(GA10.agn.cmpl)

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

db <- dbscan(GA10@population, eps = 70, minPts =5)


print(db)
Summary(GA10)
plot(GA10@population[,])
 plot(db, GA10@population, main = "dbsan", frame = false)