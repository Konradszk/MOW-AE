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


fx <- function(x) {
    return(cec2013(8, x))
}
GA10 <- ga(type = "real-valued", fitness = fx, min = rep(-100,10), max = rep(100,10), popSize = 100, seed = 1234, monitor = NULL, maxiter = 20)

GA30 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 30), max = rep(100, 30), popSize = 300, seed = 1234, monitor = NULL, maxiter = 30)

GA50 <- ga(type = "real-valued", fitness = fx, min = rep(-100, 50), max = rep(100, 50), popSize = 500, seed = 1234, monitor = NULL, maxiter = 40)

plot(GA10)
plot(GA30)
plot(GA50)
summary(GA10)
summary(GA30) 
summary(GA50)
