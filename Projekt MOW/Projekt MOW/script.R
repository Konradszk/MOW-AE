
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
if (!"clv" %in% row.names(installed.packages()))
    install.packages("clv")
library(clv)

#------------------------------------------------------------------------
fx <- function(x) {
    return(cec2013(8, x))
}

srednia_kmean <- function(GAx, GA.size) {
    sr <- rep(0, GA.size)
    for (i in 1:GAx@popSize)
        sr <- sr + GAx@population[i,]
    sr <- sr / GAx@popSize

  return(sr)
}
srednia_dbscam <- function(pop_matrix) {
    sr <- vector(length = length(pop_matrix[1,]), mode = "integer")
    for (i in 1:length(pop_matrix[, 1]))
        sr <- sr + pop_matrix[i,]

    sr <- sr / length(pop_matrix[, 1])
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

B_W_dbscam_size <- function(db, GAx) {
    n.clust <- max(db$cluster + 1)
    n.in.clust <- rep(0, n.clust)
    for (i in db$cluster)
        n.in.clust[i + 1] <- n.in.clust[i + 1] + 1
    return(n.in.clust[2:n.clust]) #zwraca z szukem {1:n}, bez {2:n}
}
B_W_dbscam_center <- function(db, GAx) { #trzeba uwzglednic szum bo WCV wymaga :(
    n.clust <- max(db$cluster)
    m.maatrix <- length(colMeans(GAx@population[db$cluster == n.clust,]))
    A = matrix(nrow = n.clust, ncol = m.maatrix)
    for (i in c(0:(n.clust-1)))
        A[i+1,] = colMeans(GAx@population[db$cluster == i+1,])
    return(A)

}
B_W_dbscam_pop <- function(pop, cluster, sizePop) {
    N <- 0
    for (i in cluster)
        if (i > 0) { N <- N + 1 }

    M <- matrix(ncol = sizePop, nrow = N)
    j <- 1
    for (i in 1:length(cluster)) {

        if (cluster[i] > 0) {
            M[j,] = pop[i,]
            j <- j + 1
        }
    }
    return(M)
}
B_W_dbscam = function(db, GAx, GAx.size) {
    v.pred <- as.integer(B_W_dbscam_clust(db$clust))

    W.matrix <- wcls.matrix(B_W_dbscam_pop(GAx@population, db$cluster, GAx.size), v.pred, B_W_dbscam_center(db, GAx))
    B.matrix <- bcls.matrix(B_W_dbscam_center(db, GAx), B_W_dbscam_size(db, GAx), srednia_dbscam(B_W_dbscam_pop(GAx@population, db$cluster, GAx.size)))
     sum(diag(B.matrix)) / sum(diag(W.matrix))
   

}
B_W_dbscam_clust <- function(cluster) {
    N <- 0
    for (i in cluster)
        if (i > 0) { N <- N + 1 }

    j <- 1
    v <- vector(length = N, mode = "integer")
    for (i in 1:length(cluster)) {
        if (cluster[i] > 0) {
            v[j] <- cluster[i]
            j <- j + 1
        }
    }

    return(v)
}
B_W_tree <- function(tree, GAx, kcut) {

    v_clust <- cutree(tree, kcut)
    pop <- GAx@population
    cls.attr <- cls.attrib(pop, as.integer(v_clust))

    W.matrix <- wcls.matrix(pop, v_clust, cls.attr$cluster.center)
    B.matrix <- bcls.matrix(cls.attr$cluster.center, cls.attr$cluster.size, cls.attr$mean)
    sum(diag(B.matrix)) / sum(diag(W.matrix))

}

Own_valuate_pam <- function(gpam) {
    v_clust <- gpam$clustering
    n_clust <- max(gpam$clustering)
    v_return <- vector(length = n_clust)
    for (i in 1:n_clust) {
        n_rows <- 0
        actual_row <- 1
        best_val <- 0
        for (j in 1:length(gpam$clustering))
            if (i == v_clust[j]) { n_rows <- n_rows + 1 }

        ma <- matrix(ncol = length(gpam$data[1,]), nrow = n_rows)

        for (k in 1:length(gpam$clustering)) {
            if (i == v_clust[k]) {
                ma[actual_row,] <- gpam$data[k,]
                actual_row <- actual_row + 1
            }

        }
        for (m in 1:n_rows) {

            if (best_val > fx(ma[m, ])) {


                best_val <- fx(ma[m,])
            }
        }

        if (best_val >= fx(gpam$medoids[i, ]) && cls.attr$cluster.size[i] != 1) { v_return[i] <- TRUE }

    }
    return(v_return)
}
Own_valuate_kmeans <- function(gkeams, pop) {
    v_clust <- gkeams$cluster
    n_clust <- max(gkeams$cluster)
    cls.attr <- cls.attrib(pop, as.integer(gkeams$cluster))
    v_return <- vector(length = n_clust)
    for (i in 1:n_clust) {
        n_rows <- 0
        actual_row <- 1
        best_val <- 0
        for (j in 1:length(gkeams$cluster))
            if (i == v_clust[j]) { n_rows <- n_rows + 1 }

        ma <- matrix(ncol = length(pop[1,]), nrow = n_rows)

        for (k in 1:length(gkeams$cluster)) {
            if (i == v_clust[k]) {
                ma[actual_row,] <- pop[k,]
                actual_row <- actual_row + 1
            }

        }
        for (m in 1:n_rows) {

            if (best_val > fx(ma[m, ])) {


                best_val <- fx(ma[m,])
            }
        }

        if (best_val >= fx(cls.attr$cluster.center[i, ]) && cls.attr$cluster.size[i] != 1) { v_return[i] <- TRUE }

    }
    return(v_return)
}
Own_valuate_dbscam <- function(db, GAx) {
    v_clust <- B_W_dbscam_clust(db$cluster)
    n_clust <- max(db$cluster)
    pop <- B_W_dbscam_pop(GAx@population, v_clust, length(GAx@solution))
    cls.attr <- cls.attrib(pop, as.integer(v_clust))
    v_return <- vector(length = n_clust)
    for (i in 1:n_clust) {
        n_rows <- 0
        actual_row <- 1
        best_val <- 0
        for (j in 1:length(v_clust))
            if (i == v_clust[j]) { n_rows <- n_rows + 1 }

        ma <- matrix(ncol = length(pop[1,]), nrow = n_rows)

        for (k in 1:length(v_clust)) {
            if (i == v_clust[k]) {
                ma[actual_row,] <- pop[k,]
                actual_row <- actual_row + 1
            }

        }
        for (m in 1:n_rows) {

            if (best_val > fx(ma[m, ])) {


                best_val <- fx(ma[m,])
            }
        }

        if (best_val >= fx(cls.attr$cluster.center[i, ]) && cls.attr$cluster.size[i] != 1) { v_return[i] <- TRUE }

    }
    return(v_return)
}
Own_valuate_tree <- function(tree, GAx, kcut) {
    v_clust <- cutree(tree, kcut)
    n_clust <- max(v_clust)
    pop <- GAx@population
    pop_in_cluster <-0
    cls.attr <- cls.attrib(pop, as.integer(v_clust))
    v_return <- vector(length = n_clust)
    for (i in 1:n_clust) {
        n_rows <- 0
        actual_row <- 1
        best_val <- 0
        for (j in 1:length(v_clust))
            if (i == v_clust[j]) { n_rows <- n_rows + 1 }

        ma <- matrix(ncol = length(pop[1,]), nrow = n_rows)

        for (k in 1:length(v_clust)) {
            if (i == v_clust[k]) {
                ma[actual_row,] <- pop[k,]
                actual_row <- actual_row + 1
            }

        }
        for (m in 1:n_rows) {

            if (best_val > fx(ma[m, ])) {


                best_val <- fx(ma[m,])
            }
        }

        if (best_val >= fx(cls.attr$cluster.center[i, ]) && cls.attr$cluster.size[i]!=1) { v_return[i] <- TRUE }

    }
    return(v_return)
}

B_W_dbscan_serially <- function(GAx, GAx.size, minPts_t) {
    v_int <- 20:100
    v_value <- vector(mode = "double", length = 80)
    for (i in 20:100) {

        db <- dbscan(GAx@population, eps = i, minPts = minPts_t)
        v_value[i - 19] <- round(B_W_dbscam(db, GAx, GAx.size), 2)

    }
    d_frame <- data.frame(v_int, v_value)
    return(d_frame)
}
B_W_kmean_serially <- function(GAx, GAx.size, max_iter) {

    v_value <- vector(mode = "double", length = max_iter)
    v_value[1] <- 0
    for (i in 2:max_iter) {
        gkmean <- kmeans(GAx@population, max_iter, iter.max = 100, nstart = 10)
        v_value[i] <- round(B_W_kmean(GAx@population, gkmean, GAx, GAx.size), 2)
    }
    return(data.frame(v_value))
}
B_W_pam_serially <- function(GAx, max_iter) {

    v_value <- vector(mode = "double", length = max_iter)
    v_value[1] <- 0
    for (i in 2:max_iter) {
        gpam <- pam(GAx@population, i)
        v_value[i] <- round(B_W_pam(GAx@population, gpam), 2)
    }
    return(data.frame(v_value))
}
B_W_hclust_serially <- function(GAx, max_iter, method_t) {

    v_value <- vector(mode = "double", length = max_iter)
    v_value[1] <- 0
    for (i in 2:max_iter) {
        hclust <- hclust(dist(GAx@population), method = method_t)
        v_value[i] <- round(B_W_tree(hclust, GAx, i), 2)
    }
    return(data.frame(v_value))
}
B_W_agnes_serially <- function(GAx, max_iter, method_t) {

    v_value <- vector(mode = "double", length = max_iter)
    v_value[1] <- 0
    for (i in 2:max_iter) {
        agn <- agnes(GAx@population, diss = FALSE, method = method_t)
        v_value[i] <- round(B_W_tree(agn, GAx, i), 2)
    }
    return(data.frame(v_value))
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
silhouette_agnes <- function(GAx_opcja, kClust, GAx) {
    sil4agnes <- silhouette(cutree(GAx_opcja, kClust), daisy(GAx@population))
    sss <- summary(sil4agnes)
    aswec <- sss$clus.avg.widths
    ta <- sss$avg.width
    ta

}
silhouette_dbscan <- function(GAx.dbx, GAx) {
    dbbb <- silhouette(GAx.dbx$cluster, dist(GAx@population))
    dbbb.sum <- summary(dbbb)
    summary(dbbb)
    aswec.db <- dbbb.sum$clus.avg.widths
    aswec.db
    ta.db <- dbbb.sum$avg.width
    ta.db
}

silhoDlaROznychK <- function(GAx) {
    best <- 0
    iter <- vector(mode = "double", length = 100)
    v <- vector(mode = "double", length = 100)
    j <- 2
    for (i in 2:90) {

        GAx.kmFunkcja <- kmeans(GAx@population, i, iter.max = 100, nstart = 10)
        if (silhouette_kmean(GAx.kmFunkcja, GAx) > best) {
            best <- silhouette_kmean(GAx.kmFunkcja, GAx)
        }
        iter[i] <- j
        j <- j + 1
        v[i] <- silhouette_kmean(GAx.kmFunkcja, GAx)
    }
    wynik <- data.frame(v, i)
    wynik
    best
}
silPAMrozneK <- function(GAx) {
    best <- 0
    #iter <- vector(mode = "double", length = 100)
    v <- vector(mode = "double", length = 100)
    j <- 2
    for (i in 2:90) {
        GAx.kmedFunkcja <- pam(GAx@population, i, medoids = i:1)

        if (silhouette_kmean(GAx.kmedFunkcja, GAx) > best) {
            best <- silhouette_kmean(GAx.kmedFunkcja, GAx)
        }
        #iter[i] <- j
        #j <- j + 1
        v[i] <- silhouette_kmean(GAx.kmedFunkcja, GAx)
    }
    wynik <- data.frame(v, i)
    wynik
    #best
}
silDlaHclustCuttreeK <- function(GAx, metoda) {
    best <- 0
    v <- vector(mode = "double", length = 100)
    GAx.hclFunkcja <- hclust(dist(GAx@population), method = metoda)

    for (i in 2:50) {

        if (silhouette_agnes(GAx.hclFunkcja, i, GAx) > best) {
            best <- silhouette_agnes(GAx.hclFunkcja, i, GAx)
        }

        v[i] <- silhouette_agnes(GAx.hclFunkcja, i, GAx)
    }
    wynik <- data.frame(v, i)
    wynik
    # best
}
silDlaAgnesCuttreeK <- function(GAx, metoda) {
    best <- 0
    v <- vector(mode = "double", length = 100)
    GAx.hclFunkcja <- agnes(GAx@population, diss = FALSE, metric = "euclidean", method = metoda)

    for (i in 2:70) {

        if (silhouette_agnes(GAx.hclFunkcja, i, GAx) > best) {
            best <- silhouette_agnes(GAx.hclFunkcja, i, GAx)
        }

        v[i] <- silhouette_agnes(GAx.hclFunkcja, i, GAx)
    }
    wynik <- data.frame(v, i)
    wynik
    # best
}

Wlasna_km_rozneK <- function(GAx, ile) {
    #best <- 0

    ff <- vector(mode = "integer", length = ile + 10)
    tt <- vector(mode = "integer", length = ile + 10)
    ss <- vector(mode = "double", length = ile + 10)

    for (i in 2:ile) {
        GAx.kmFunkcja <- kmeans(GAx@population, i, iter.max = 100, nstart = 10)
        v <- Own_valuate_kmeans(GAx.kmFunkcja, GAx@population)

        t <- 0
        f <- 0
        #stosunek <- 0
        for (l in 1:length(v)) {

            if (v[l] == TRUE) {
                t <- t + 1
            }
            if (v[l] == FALSE) {
                f <- f + 1
            }
        }
        tt[i] <- t
        ff[i] <- f
        ss[i] <- t / (f + t) * 100
    }
    wynik <- data.frame(tt, ff, ss)
    wynik
    #best
}
Wlasna_pam_rozneK <- function(GAx, ile) {
    #best <- 0

    ff <- vector(mode = "integer", length = ile + 10)
    tt <- vector(mode = "integer", length = ile + 10)
    ss <- vector(mode = "double", length = ile + 10)

    for (i in 2:ile) {
        GAx.kmedFunkcja <- pam(GAx@population, i, medoids = i:1)
        v <- Own_valuate_pam(GAx.kmedFunkcja)

        t <- 0
        f <- 0
        #stosunek <- 0
        for (l in 1:length(v)) {

            if (v[l] == TRUE) {
                t <- t + 1
            }
            if (v[l] == FALSE) {
                f <- f + 1
            }
        }
        tt[i] <- t
        ff[i] <- f
        ss[i] <- t / (f + t) * 100
    }
    wynik <- data.frame(tt, ff, ss)
    wynik
    #best
}
Wlasna_hclust_rozneK <- function(GAx, metoda, ile) {

    ff <- vector(mode = "integer", length = ile + 10)
    tt <- vector(mode = "integer", length = ile + 10)
    ss <- vector(mode = "double", length = ile + 10)
    GAx.hclFunkcja <- hclust(dist(GAx@population), method = metoda)

    for (i in 2:ile) {
        v <- Own_valuate_tree(GAx.hclFunkcja, GAx, ile)

        t <- 0
        f <- 0
        for (l in 1:length(v)) {

            if (v[l] == TRUE) {
                t <- t + 1
            }
            if (v[l] == FALSE) {
                f <- f + 1
            }
        }
        tt[i] <- t
        ff[i] <- f
        ss[i] <- t / (f + t) * 100
    }
    wynik <- data.frame(tt, ff, ss)
    wynik
}
Wlasna_agnes_rozneK <- function(GAx, metoda, ile) {

    ff <- vector(mode = "integer", length = ile + 10)
    tt <- vector(mode = "integer", length = ile + 10)
    ss <- vector(mode = "double", length = ile + 10)
    GAx.agnFunkcja <- agnes(GAx@population, diss = FALSE, metric = "euclidean", method = metoda)

    for (i in 2:ile) {
        v <- Own_valuate_tree(GAx.agnFunkcja, GAx, i)

        t <- 0
        f <- 0
        for (l in 1:length(v)) {

            if (v[l] == TRUE) {
                t <- t + 1
            }
            if (v[l] == FALSE) {
                f <- f + 1
            }
        }
        tt[i] <- t
        ff[i] <- f
        ss[i] <- t / (f + t) * 100
    }
    wynik <- data.frame(tt, ff, ss)
    wynik
}

GA10 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 10), max = rep(100, 10), popSize = 100, seed = 1234, monitor = NULL, maxiter = 100, pmutation = 0.1)
GA30 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 30), max = rep(100, 30), popSize = 300, seed = 1234, monitor = NULL, maxiter = 300, pmutation = 0.1)
GA50 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 50), max = rep(100, 50), popSize = 500, seed = 1234, monitor = NULL, maxiter = 500, pmutation = 0.1)

GA10.data <- data.frame(data=GA10@population)
GA30.data <- data.frame(data=GA30@population)
GA50.data <- data.frame(data=GA50@population)



#---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------OCENA---------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------



#-------------------KMEANS------------------------------
B_W_kmean_serially(GA10, 50, 930)
B_W_kmean_serially(GA30, 50, 60)
B_W_kmean_serially(GA50, 50, 90)

#--------------------KMEDOID--------------
B_W_pam_serially(GA10, 90)
B_W_pam_serially(GA30, 90)
B_W_pam_serially(GA50, 90)


#--------------------------AGNES-----------------

B_W_agnes_serially(GA10, 30, "complete")
B_W_agnes_serially(GA30, 30, "complete")
B_W_agnes_serially(GA50, 30, "complete")

B_W_agnes_serially(GA10, 30, "single")
B_W_agnes_serially(GA30, 30, "single")
B_W_agnes_serially(GA50, 30, "single")

B_W_agnes_serially(GA10, 30, "average")
B_W_agnes_serially(GA30, 30, "average")
B_W_agnes_serially(GA50, 30, "average")

#--------------------------HCLUST-----------------

B_W_hclust_serially(GA10, 30, "single")
B_W_hclust_serially(GA30, 30, "single")
B_W_hclust_serially(GA50, 30, "single")

B_W_hclust_serially(GA10, 30, "complete")
B_W_hclust_serially(GA30, 30, "complete")
B_W_hclust_serially(GA50, 30, "complete")

B_W_hclust_serially(GA10, 30, "average")
B_W_hclust_serially(GA30, 30, "average")
B_W_hclust_serially(GA50, 30, "average")


#--------------------------DBSCAN-----------------
B_W_dbscan_serially(GA50, 50, 2)
B_W_dbscan_serially(GA50, 50, 3)
B_W_dbscan_serially(GA50, 50, 4)
B_W_dbscan_serially(GA50, 50, 5)

B_W_dbscan_serially(GA10, 50, 2)
B_W_dbscan_serially(GA10, 50, 3)
B_W_dbscan_serially(GA10, 50, 4)
B_W_dbscan_serially(GA10, 50, 5)

B_W_dbscan_serially(GA30, 50, 2)
B_W_dbscan_serially(GA30, 50, 3)
B_W_dbscan_serially(GA30, 50, 4)
B_W_dbscan_serially(GA30, 50, 5)


#----------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------OCENIANIE----------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------
#w<-wcls.matrix(data = GA10@population,clust = GA10.km3$cluster,cluster.center = GA10.km3$centers)
#b <- bcls.matrix(mean = GA10.km3$withinss, clust = GA10.km3$cluster, cluster.center = GA10.km3$centers)
#bcls.matrix()
#ss <- sum(diag(w))
#GA10.km3$centers



#B_W(GA50@population, GA50.kmed10)


#B_W(GA50@population, GA50)











#B_W_kmean(GA10@population,GA10.km3,GA10,10 <----- przyladowe zuycie wazne!
#-----------------do kmenas ---- silhouette


silhouette_kmean(GA10.km3, GA10)


B_W_dbscam(GA30.db, GA30, 30)
B_W_kmean(GA30@population, GA30.km3, GA30, 30)
B_W_pam(GA10@population,GA10.kmed10)
summary(GA10.kmed10)
(B_W_dbscam_center(GA10.db, GA10))
typeof(GA10.km3$center[,])
GA10.km3$center
GA10.km3$center
A<-as.integer(GA10.db$cluster)
A + 1
B_W_dbscam_size(GA10.db, GA10)
B_W_dbscam_center(GA10.db, GA10)


Own_valuate_pam(GA50.kmed6)
Own_valuate_dbscam(GA50.db, GA50)


Own_valuate_tree(GA10.hcl.cmpl, GA10, 5)



