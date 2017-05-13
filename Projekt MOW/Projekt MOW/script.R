#set.seed(542)
wymiar_problemu = 10
func.num <- 8

fitne <- function(a) cec2013(8, rep(0, 10))

GA <- ga(type = "real-valued", fitness = fitne, min = wymiar_problemu, max = wymiar_problemu, popSize = 10 * wymiar_problemu)

#summary(GA)
#plot(GA)#func.num <- 8
#D <- 10
#plot(cec2013(func.num, rep(15, D)))


