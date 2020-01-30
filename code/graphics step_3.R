# first load all data obtained with the simulations 
#Data in the lIST is ordered in such a way that there are 19 LUI points and within each point 
#there are up to 30 species.  

results_coex <- readRDS("new results/results_coexistence_sps_5000_all.rds")


# plot lui results up to 4 sps.
#results_coex <- readRDS("new results/results_coexistence_4sps.rds")

#1. Plot results all species----

library(ggplot2)
library(gridExtra)

#Lets create a pdf to store all the information.

pdf(file="results_coexistence_LUI.pdf")


#LUI=0 ----

results_coex_0_1 <- results_coex[[1]][[24]]
#results_coex_0_2 <- results_coex[[1]][[29]]
#Tengo que eliminar el numero 1 # ver como hacer en internet
results_coex_0 <- results_coex_0_1
#NOW limited to 25 species and does not include the fesibility to all because there are
# some computational issues I need to check, break in species above 24-29
  
#Omega with LUI=0
omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_0)){
  omega[[i]] <- results_coex_0[[i]][,1]
  theta[[i]] <- results_coex_0[[i]][,2]
  overlap[[i]] <- results_coex_0[[i]][,3]
  differential[[i]] <- results_coex_0[[i]][,4]
  feasibility [[i]] <- results_coex_0[[i]][,5]
  feasibility_pair_all <- results_coex_0[[i]][,6]
  lui_omega[[i]] <-results_coex_0[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_0[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_0 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_0) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_0, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 0", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_0, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_0, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_0, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_0, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

#LUI=0.2 ----

results_coex_0_2 <- results_coex[[2]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_0_2)){
  omega[[i]] <- results_coex_0_2[[i]][,1]
  theta[[i]] <- results_coex_0_2[[i]][,2]
  overlap[[i]] <- results_coex_0_2[[i]][,3]
  differential[[i]] <- results_coex_0_2[[i]][,4]
  feasibility [[i]] <- results_coex_0_2[[i]][,5]
  feasibility_pair_all <- results_coex_0_2[[i]][,6]
  lui_omega[[i]] <-results_coex_0_2[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_0_2[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_0_2 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_0_2) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_0_2, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 0.2", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_0_2, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_0_2, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_0_2, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_0_2, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=0.4 ----

results_coex_0_4 <- results_coex[[3]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_0_4)){
  omega[[i]] <- results_coex_0_4[[i]][,1]
  theta[[i]] <- results_coex_0_4[[i]][,2]
  overlap[[i]] <- results_coex_0_4[[i]][,3]
  differential[[i]] <- results_coex_0_4[[i]][,4]
  feasibility [[i]] <- results_coex_0_4[[i]][,5]
  feasibility_pair_all <- results_coex_0_4[[i]][,6]
  lui_omega[[i]] <-results_coex_0_4[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_0_4[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_0_4 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_0_4) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_0_4, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 0.4", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_0_4, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_0_4, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_0_4, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_0_4, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

#LUI=0.6 ----

results_coex_0_6 <- results_coex[[4]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_0_6)){
  omega[[i]] <- results_coex_0_6[[i]][,1]
  theta[[i]] <- results_coex_0_6[[i]][,2]
  overlap[[i]] <- results_coex_0_6[[i]][,3]
  differential[[i]] <- results_coex_0_6[[i]][,4]
  feasibility [[i]] <- results_coex_0_6[[i]][,5]
  feasibility_pair_all <- results_coex_0_6[[i]][,6]
  lui_omega[[i]] <-results_coex_0_6[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_0_6[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_0_6 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_0_6) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_0_6, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 0.6", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_0_6, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_0_6, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_0_6, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_0_6, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=0.8 ----

results_coex_0_8 <- results_coex[[5]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_0_8)){
  omega[[i]] <- results_coex_0_8[[i]][,1]
  theta[[i]] <- results_coex_0_8[[i]][,2]
  overlap[[i]] <- results_coex_0_8[[i]][,3]
  differential[[i]] <- results_coex_0_8[[i]][,4]
  feasibility [[i]] <- results_coex_0_8[[i]][,5]
  feasibility_pair_all <- results_coex_0_8[[i]][,6]
  lui_omega[[i]] <-results_coex_0_8[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_0_8[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_0_8 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_0_8) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_0_8, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 0.8", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_0_8, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_0_8, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_0_8, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_0_8, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=1.0 ----

results_coex_1_0 <- results_coex[[6]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_1_0)){
  omega[[i]] <- results_coex_1_0[[i]][,1]
  theta[[i]] <- results_coex_1_0[[i]][,2]
  overlap[[i]] <- results_coex_1_0[[i]][,3]
  differential[[i]] <- results_coex_1_0[[i]][,4]
  feasibility [[i]] <- results_coex_1_0[[i]][,5]
  feasibility_pair_all <- results_coex_1_0[[i]][,6]
  lui_omega[[i]] <-results_coex_1_0[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_1_0[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_1_0 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_1_0) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_1_0, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 1.0", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_1_0, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_1_0, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_1_0, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_1_0, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

#LUI=1.2 ----

results_coex_1_2 <- results_coex[[7]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_1_2)){
  omega[[i]] <- results_coex_1_2[[i]][,1]
  theta[[i]] <- results_coex_1_2[[i]][,2]
  overlap[[i]] <- results_coex_1_2[[i]][,3]
  differential[[i]] <- results_coex_1_2[[i]][,4]
  feasibility [[i]] <- results_coex_1_2[[i]][,5]
  feasibility_pair_all <- results_coex_1_2[[i]][,6]
  lui_omega[[i]] <-results_coex_1_2[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_1_2[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_1_2 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_1_2) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_1_2, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 1.2", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_1_2, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_1_2, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_1_2, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_1_2, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

#LUI=1.4 ----

results_coex_1_4 <- results_coex[[8]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_1_4)){
  omega[[i]] <- results_coex_1_4[[i]][,1]
  theta[[i]] <- results_coex_1_4[[i]][,2]
  overlap[[i]] <- results_coex_1_4[[i]][,3]
  differential[[i]] <- results_coex_1_4[[i]][,4]
  feasibility [[i]] <- results_coex_1_4[[i]][,5]
  feasibility_pair_all <- results_coex_1_4[[i]][,6]
  lui_omega[[i]] <-results_coex_1_4[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_1_4[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_1_4 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_1_4) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_1_4, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 1.4", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_1_4, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_1_4, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_1_4, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_1_4, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

#LUI=1.6 ----

results_coex_1_6 <- results_coex[[9]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_1_6)){
  omega[[i]] <- results_coex_1_6[[i]][,1]
  theta[[i]] <- results_coex_1_6[[i]][,2]
  overlap[[i]] <- results_coex_1_6[[i]][,3]
  differential[[i]] <- results_coex_1_6[[i]][,4]
  feasibility [[i]] <- results_coex_1_6[[i]][,5]
  feasibility_pair_all <- results_coex_1_6[[i]][,6]
  lui_omega[[i]] <-results_coex_1_6[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_1_6[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_1_6 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_1_6) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_1_6, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 1.6", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_1_6, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_1_6, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_1_6, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_1_6, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=1.8 ----

results_coex_1_8 <- results_coex[[10]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_1_8)){
  omega[[i]] <- results_coex_1_8[[i]][,1]
  theta[[i]] <- results_coex_1_8[[i]][,2]
  overlap[[i]] <- results_coex_1_8[[i]][,3]
  differential[[i]] <- results_coex_1_8[[i]][,4]
  feasibility [[i]] <- results_coex_1_8[[i]][,5]
  feasibility_pair_all <- results_coex_1_8[[i]][,6]
  lui_omega[[i]] <-results_coex_1_8[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_1_8[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_1_8 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_1_8) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_1_8, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 1.8", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_1_8, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_1_8, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_1_8, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_1_8, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

#LUI=1.8 ----

results_coex_1_8 <- results_coex[[10]][[24]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_1_8)){
  omega[[i]] <- results_coex_1_8[[i]][,1]
  theta[[i]] <- results_coex_1_8[[i]][,2]
  overlap[[i]] <- results_coex_1_8[[i]][,3]
  differential[[i]] <- results_coex_1_8[[i]][,4]
  feasibility [[i]] <- results_coex_1_8[[i]][,5]
  feasibility_pair_all <- results_coex_1_8[[i]][,6]
  lui_omega[[i]] <-results_coex_1_8[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_1_8[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_1_8 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_1_8) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_1_8, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 1.8", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_1_8, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_1_8, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_1_8, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_1_8, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=2.0 ----

results_coex_2_0 <- results_coex[[11]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_2_0)){
  omega[[i]] <- results_coex_2_0[[i]][,1]
  theta[[i]] <- results_coex_2_0[[i]][,2]
  overlap[[i]] <- results_coex_2_0[[i]][,3]
  differential[[i]] <- results_coex_2_0[[i]][,4]
  feasibility [[i]] <- results_coex_2_0[[i]][,5]
  feasibility_pair_all <- results_coex_2_0[[i]][,6]
  lui_omega[[i]] <-results_coex_2_0[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_2_0[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_2_0 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_2_0) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_2_0, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 2.0", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_2_0, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_2_0, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_2_0, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_2_0, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

#LUI=2.3 ----

results_coex_2_3 <- results_coex[[12]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_2_3)){
  omega[[i]] <- results_coex_2_3[[i]][,1]
  theta[[i]] <- results_coex_2_3[[i]][,2]
  overlap[[i]] <- results_coex_2_3[[i]][,3]
  differential[[i]] <- results_coex_2_3[[i]][,4]
  feasibility [[i]] <- results_coex_2_3[[i]][,5]
  feasibility_pair_all <- results_coex_2_3[[i]][,6]
  lui_omega[[i]] <-results_coex_2_3[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_2_3[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_2_3 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_2_3) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_2_3, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 2.3", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_2_3, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_2_3, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_2_3, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_2_3, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=2.6----

results_coex_2_6 <- results_coex[[13]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_2_6)){
  omega[[i]] <- results_coex_2_6[[i]][,1]
  theta[[i]] <- results_coex_2_6[[i]][,2]
  overlap[[i]] <- results_coex_2_6[[i]][,3]
  differential[[i]] <- results_coex_2_6[[i]][,4]
  feasibility [[i]] <- results_coex_2_6[[i]][,5]
  feasibility_pair_all <- results_coex_2_6[[i]][,6]
  lui_omega[[i]] <-results_coex_2_6[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_2_6[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_2_6 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_2_6) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_2_6, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 2.6", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_2_6, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_2_6, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_2_6, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_2_6, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=2.9----

results_coex_2_9 <- results_coex[[14]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_2_9)){
  omega[[i]] <- results_coex_2_9[[i]][,1]
  theta[[i]] <- results_coex_2_9[[i]][,2]
  overlap[[i]] <- results_coex_2_9[[i]][,3]
  differential[[i]] <- results_coex_2_9[[i]][,4]
  feasibility [[i]] <- results_coex_2_9[[i]][,5]
  feasibility_pair_all <- results_coex_2_9[[i]][,6]
  lui_omega[[i]] <-results_coex_2_9[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_2_9[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_2_9 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_2_9) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_2_9, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 2.9", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_2_9, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_2_9, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_2_9, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_2_9, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=3.2----

results_coex_3_2 <- results_coex[[15]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_3_2)){
  omega[[i]] <- results_coex_3_2[[i]][,1]
  theta[[i]] <- results_coex_3_2[[i]][,2]
  overlap[[i]] <- results_coex_3_2[[i]][,3]
  differential[[i]] <- results_coex_3_2[[i]][,4]
  feasibility [[i]] <- results_coex_3_2[[i]][,5]
  feasibility_pair_all <- results_coex_3_2[[i]][,6]
  lui_omega[[i]] <-results_coex_3_2[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_3_2[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_3_2 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_3_2) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_3_2, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 3.2", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_3_2, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_3_2, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_3_2, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_3_2, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

#LUI=3.5----

results_coex_3_5 <- results_coex[[16]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_3_5)){
  omega[[i]] <- results_coex_3_5[[i]][,1]
  theta[[i]] <- results_coex_3_5[[i]][,2]
  overlap[[i]] <- results_coex_3_5[[i]][,3]
  differential[[i]] <- results_coex_3_5[[i]][,4]
  feasibility [[i]] <- results_coex_3_5[[i]][,5]
  feasibility_pair_all <- results_coex_3_5[[i]][,6]
  lui_omega[[i]] <-results_coex_3_5[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_3_5[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_3_5 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_3_5) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_3_5, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 3.5", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_3_5, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_3_5, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_3_5, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_3_5, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=3.8----

results_coex_3_8 <- results_coex[[17]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_3_8)){
  omega[[i]] <- results_coex_3_8[[i]][,1]
  theta[[i]] <- results_coex_3_8[[i]][,2]
  overlap[[i]] <- results_coex_3_8[[i]][,3]
  differential[[i]] <- results_coex_3_8[[i]][,4]
  feasibility [[i]] <- results_coex_3_8[[i]][,5]
  feasibility_pair_all <- results_coex_3_8[[i]][,6]
  lui_omega[[i]] <-results_coex_3_8[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_3_8[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_3_8 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_3_8) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_3_8, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 3.8", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_3_8, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_3_8, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_3_8, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_3_8, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)


#LUI=4.1----

results_coex_4_1 <- results_coex[[18]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_4_1)){
  omega[[i]] <- results_coex_4_1[[i]][,1]
  theta[[i]] <- results_coex_4_1[[i]][,2]
  overlap[[i]] <- results_coex_4_1[[i]][,3]
  differential[[i]] <- results_coex_4_1[[i]][,4]
  feasibility [[i]] <- results_coex_4_1[[i]][,5]
  feasibility_pair_all <- results_coex_4_1[[i]][,6]
  lui_omega[[i]] <-results_coex_4_1[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_4_1[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_4_1 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_4_1) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_4_1, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 4.1", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_4_1, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_4_1, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_4_1, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_4_1, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow = 2, ncol = 3)


#LUI = 4.4----

results_coex_4_4 <- results_coex[[19]]

omega <- list()
theta <- list()
overlap <- list()
differential <- list()
feasibility <- list()
feasibility_pair_all <- list()
lui_omega <-list()
richness <- list()


for(i in 2:length(results_coex_4_4)){
  omega[[i]] <- results_coex_4_4[[i]][,1]
  theta[[i]] <- results_coex_4_4[[i]][,2]
  overlap[[i]] <- results_coex_4_4[[i]][,3]
  differential[[i]] <- results_coex_4_4[[i]][,4]
  feasibility [[i]] <- results_coex_4_4[[i]][,5]
  feasibility_pair_all <- results_coex_4_4[[i]][,6]
  lui_omega[[i]] <-results_coex_4_4[[i]][,7]
  richness[[i]] <-rep(i, times=length(results_coex_4_4[[i]][,1]))
}

omega <- unlist(omega)
theta <- unlist(theta)
overlap <- unlist(overlap)
differential <- unlist(differential)
feasibility <- unlist(feasibility)
feasibility_pair_all <- unlist(feasibility_pair_all)
lui_omega <- unlist(lui_omega)
richness <- unlist(richness)

data_lui_4_4 <- as.data.frame(cbind(omega, theta, overlap, differential, feasibility, lui_omega, richness))
colnames(data_lui_4_4) <- c("niche_diff","fitness_diff","overlap","differential", "feasilibity", "lui", "richness")
gr_1 <- ggplot(data_lui_4_4, aes(x=richness, y=niche_diff)) + ylim(0,10) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Niche Differences", title = "LUI = 4.4", tag = "A") + geom_smooth(method = "lm")
gr_2 <- ggplot(data_lui_4_4, aes(x=richness, y=fitness_diff)) + ylim(0,100) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Structural Fitness Differences", tag = "B") + geom_smooth(method = "lm")
gr_3 <- ggplot(data_lui_4_4, aes(x=richness, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Overlap", tag = "C") + geom_smooth(method = "lm")
gr_4 <- ggplot(data_lui_4_4, aes(x=richness, y=differential)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Differential", tag = "D") + geom_smooth(method = "lm")
gr_5 <- ggplot(data_lui_4_4, aes(x=richness, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "Richness", y= "Feasibility", tag = "E") + geom_smooth(method = "lm")

grid.arrange(gr_1, gr_2, gr_3, gr_4, gr_5, nrow=2, ncol=3)

dev.off()











#Omega with LUI three species
omega <- list()
lui_omega <-list()

for(i in 1:length(results_coex)){
  omega[[i]] <-results_coex[[i]][[3]][,1]
  lui_omega[[i]] <-results_coex[[i]][[3]][,6]
}
omega <- unlist(omega)
lui_omega <- unlist(lui_omega)
omega_3 <- as.data.frame(cbind(omega, lui_omega))
colnames <- c("omega", "lui")
om_3 <- ggplot(omega_3,aes(x=lui_omega, y=omega)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Omega", title = "Structural Niche Diff. 3 sps", tag = "B") + geom_smooth(method = "lm")

#Omega with LUI four species
omega <- list()
lui_omega <-list()

for(i in 1:length(results_coex)){
  omega[[i]] <-results_coex[[i]][[4]][,1]
  lui_omega[[i]] <-results_coex[[i]][[4]][,6]
}
omega <- unlist(omega)
lui_omega <- unlist(lui_omega)
omega_4 <- as.data.frame(cbind(omega, lui_omega))
colnames <- c("omega", "lui")
om_4 <- ggplot(omega_4,aes(x=lui_omega, y=omega)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Omega", title = "Structural Niche Diff. 4 sps", tag = "C") + geom_smooth(method = "lm")

#Theta with LUI two species
theta <- list()
lui_theta <-list()

for(i in 1:length(results_coex)){
  theta[[i]] <-results_coex[[i]][[2]][,2]
  lui_theta[[i]] <-results_coex[[i]][[2]][,6]
}
theta <- log(unlist(theta))
lui_theta <- unlist(lui_theta)
theta_2 <- as.data.frame(cbind(theta, lui_theta))
colnames <- c("theta", "lui")
th_2 <- ggplot(theta_2,aes(x=lui_theta, y=theta)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Log. transf. Theta", title = "Structural Fitness differences. 2 sps", tag = "D") + geom_smooth(method = "lm")

#Theta with LUI three species
theta <- list()
lui_theta <-list()

for(i in 1:length(results_coex)){
  theta[[i]] <-results_coex[[i]][[3]][,2]
  lui_theta[[i]] <-results_coex[[i]][[3]][,6]
}
theta <- log(unlist(theta))
lui_theta <- unlist(lui_theta)
theta_3 <- as.data.frame(cbind(theta, lui_theta))
colnames <- c("theta", "lui")
th_3 <- ggplot(theta_3,aes(x=lui_theta, y=theta)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Log. transf. Theta", title = "Structural Fitness differences. 3 sps", tag = "E") + geom_smooth(method = "lm")

#Theta with LUI four species
theta <- list()
lui_theta <-list()

for(i in 1:length(results_coex)){
  theta[[i]] <-results_coex[[i]][[4]][,2]
  lui_theta[[i]] <-results_coex[[i]][[4]][,6]
}
theta <- log(unlist(theta))
lui_theta <- unlist(lui_theta)
theta_4 <- as.data.frame(cbind(theta, lui_theta))
colnames <- c("theta", "lui")
th_4 <- ggplot(theta_4,aes(x=lui_theta, y=theta)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Log. transf. Theta", title = "Structural Fitness differences. 4 sps", tag = "F") + geom_smooth(method = "lm")

#put all together. 
grid.arrange(om_2, om_3, om_4, th_2, th_3, th_4, nrow=2, ncol=3)

#Plot how community overlap and community differential change with LUI.
#These are metrics of multispecies assemblages, so only for 3 species. 

#Overlap with LUI three species
overlap <- list()
lui_overlap <-list()

for(i in 1:length(results_coex)){
  overlap[[i]] <-results_coex[[i]][[3]][,3]
  lui_overlap[[i]] <-results_coex[[i]][[3]][,6]
}
overlap <- unlist(overlap)
lui_overlap <- unlist(lui_overlap)
overlap_3 <- as.data.frame(cbind(overlap, lui_overlap))
colnames <- c("overlap", "lui")
ov_3 <- ggplot(overlap_3,aes(x=lui_overlap, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Overlap", title = "Community-pair Overlap, 3sps", tag = "A") + geom_smooth(method = "lm")

#Overlap with LUI four species
overlap <- list()
lui_overlap <-list()

for(i in 1:length(results_coex)){
  overlap[[i]] <-results_coex[[i]][[4]][,3]
  lui_overlap[[i]] <-results_coex[[i]][[4]][,6]
}
overlap <- unlist(overlap)
lui_overlap <- unlist(lui_overlap)
overlap_4 <- as.data.frame(cbind(overlap, lui_overlap))
colnames <- c("overlap", "lui")
ov_4 <- ggplot(overlap_4,aes(x=lui_overlap, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Overlap", title = "Community-pair Overlap, 4sps", tag = "B") + geom_smooth(method = "lm")


#Differential with LUI three species
differential <- list()
lui_differential <-list()

for(i in 1:length(results_coex)){
  differential[[i]] <-results_coex[[i]][[3]][,4]
  lui_differential[[i]] <-results_coex[[i]][[3]][,6]
}
differential <- unlist(differential)
lui_differential <- unlist(lui_differential)
differential_3 <- as.data.frame(cbind(differential, lui_differential))
colnames <- c("differential", "lui")
di_3 <- ggplot(differential_3,aes(x=lui_differential, y=differential)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Differential", title = "Community-pair Differential, 3sps", tag = "C") + geom_smooth(method = "lm")

#Differential with LUI four species
differential <- list()
lui_differential <-list()

for(i in 1:length(results_coex)){
  differential[[i]] <-results_coex[[i]][[4]][,4]
  lui_differential[[i]] <-results_coex[[i]][[4]][,6]
}
differential <- unlist(differential)
lui_differential <- unlist(lui_differential)
differential_4 <- as.data.frame(cbind(differential, lui_differential))
colnames <- c("differential", "lui")
di_4 <- ggplot(differential_4,aes(x=lui_differential, y=differential)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Differential", title = "Community-pair Differential, 4sps", tag = "D") + geom_smooth(method = "lm")

#put all together. 
grid.arrange(ov_3, ov_4, di_3, di_4, nrow=2, ncol=2)

#Finally plot the number of species quadruplets, triplets and pairs that coexist across LUI

#Feasibility with LUI two species
feasibility <- list()
lui_feasibility <-list()

for(i in 1:length(results_coex)){
  feasibility[[i]] <-results_coex[[i]][[2]][,5]
  lui_feasibility[[i]] <-results_coex[[i]][[2]][,6]
}
feasibility <- unlist(feasibility)
lui_feasibility <- unlist(lui_feasibility)
feasibility_2 <- as.data.frame(cbind(feasibility, lui_feasibility))
colnames <- c("feasibility", "lui")
fe_2 <- ggplot(feasibility_2,aes(x=lui_feasibility, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Feasibility", title = " Feasible 2 sps", tag = "A") + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)

#Feasibility with LUI three species
feasibility <- list()
lui_feasibility <-list()

for(i in 1:length(results_coex)){
  feasibility[[i]] <-results_coex[[i]][[3]][,5]
  lui_feasibility[[i]] <-results_coex[[i]][[3]][,6]
}
feasibility <- unlist(feasibility)
lui_feasibility <- unlist(lui_feasibility)
feasibility_3 <- as.data.frame(cbind(feasibility, lui_feasibility))
colnames <- c("feasibility", "lui")
fe_3 <- ggplot(feasibility_3,aes(x=lui_feasibility, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Feasibility", title = "Feasible 3 sps", tag = "B") + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)

#Feasibility with LUI four species
feasibility <- list()
lui_feasibility <-list()

for(i in 1:length(results_coex)){
  feasibility[[i]] <-results_coex[[i]][[4]][,5]
  lui_feasibility[[i]] <-results_coex[[i]][[4]][,6]
}
feasibility <- unlist(feasibility)
lui_feasibility <- unlist(lui_feasibility)
feasibility_4 <- as.data.frame(cbind(feasibility, lui_feasibility))
colnames <- c("feasibility", "lui")
fe_4 <- ggplot(feasibility_4,aes(x=lui_feasibility, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Feasibility", title = "Feasible 4 sps", tag = "C") + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)

#put all together. 
grid.arrange(fe_2, fe_3, fe_4, ncol=3)


#2. Plot results common species across LUI----

sps2 <- list(results_coex[[1]][[2]], results_coex[[2]][[2]], results_coex[[3]][[2]], results_coex[[4]][[2]],
             results_coex[[5]][[2]], results_coex[[6]][[2]], results_coex[[7]][[2]], results_coex[[8]][[2]],
             results_coex[[9]][[2]]) 
common_names2 <- Reduce(intersect, lapply(sps2, row.names))
sps2_reduced <- lapply(sps2, function(x) { x[row.names(x) %in% common_names2,] })

sps3 <- list(results_coex[[1]][[3]], results_coex[[2]][[3]], results_coex[[3]][[3]], results_coex[[4]][[3]],
             results_coex[[5]][[3]], results_coex[[6]][[3]], results_coex[[7]][[3]], results_coex[[8]][[3]],
             results_coex[[9]][[3]]) 
common_names3 <- Reduce(intersect, lapply(sps3, row.names))
sps3_reduced <- lapply(sps3, function(x) { x[row.names(x) %in% common_names3,] })

sps4 <- list(results_coex[[1]][[4]], results_coex[[2]][[4]], results_coex[[3]][[4]], results_coex[[4]][[4]],
             results_coex[[5]][[4]], results_coex[[6]][[4]], results_coex[[7]][[4]], results_coex[[8]][[4]],
             results_coex[[9]][[4]]) 
common_names4 <- Reduce(intersect, lapply(sps4, row.names))
sps4_reduced <- lapply(sps4, function(x) { x[row.names(x) %in% common_names4,] })

#Omega with LUI two species
omega <- list()
lui_omega <-list()

for(i in 1:length(sps2_reduced)){
  omega[[i]] <-sps2_reduced[[i]][,1]
  lui_omega[[i]] <-sps2_reduced[[i]][,6]
}
omega <- unlist(omega)
lui_omega <- unlist(lui_omega)
omega_2 <- as.data.frame(cbind(omega, lui_omega))
colnames <- c("omega", "lui")
om_2 <- ggplot(omega_2,aes(x=lui_omega, y=omega)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Omega", title = "Structural Niche Diff. 2 sps", tag = "A") + geom_smooth(method = "lm")

#Omega with LUI three species
omega <- list()
lui_omega <-list()

for(i in 1:length(sps3_reduced)){
  omega[[i]] <-sps3_reduced[[i]][,1]
  lui_omega[[i]] <-sps3_reduced[[i]][,6]
}
omega <- unlist(omega)
lui_omega <- unlist(lui_omega)
omega_3 <- as.data.frame(cbind(omega, lui_omega))
colnames <- c("omega", "lui")
om_3 <- ggplot(omega_3,aes(x=lui_omega, y=omega)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Omega", title = "Structural Niche Diff. 3 sps", tag = "B") + geom_smooth(method = "lm")

#Omega with LUI four species
omega <- list()
lui_omega <-list()

for(i in 1:length(sps4_reduced)){
  omega[[i]] <-sps4_reduced[[i]][,1]
  lui_omega[[i]] <-sps4_reduced[[i]][,6]
}
omega <- unlist(omega)
lui_omega <- unlist(lui_omega)
omega_4 <- as.data.frame(cbind(omega, lui_omega))
colnames <- c("omega", "lui")
om_4 <- ggplot(omega_4,aes(x=lui_omega, y=omega)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Omega", title = "Structural Niche Diff. 4 sps", tag = "C") + geom_smooth(method = "lm")

#Theta with LUI two species
theta <- list()
lui_theta <-list()

for(i in 1:length(sps2_reduced)){
  theta[[i]] <-sps2_reduced[[i]][,2]
  lui_theta[[i]] <-sps2_reduced[[i]][,6]
}
theta <- log(unlist(theta))
lui_theta <- unlist(lui_theta)
theta_2 <- as.data.frame(cbind(theta, lui_theta))
colnames <- c("theta", "lui")
th_2 <- ggplot(theta_2,aes(x=lui_theta, y=theta)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Log. transf. Theta", title = "Structural Fitness differences. 2 sps", tag = "D") + geom_smooth(method = "lm")

#Theta with LUI three species
theta <- list()
lui_theta <-list()

for(i in 1:length(sps3_reduced)){
  theta[[i]] <-sps3_reduced[[i]][,2]
  lui_theta[[i]] <-sps3_reduced[[i]][,6]
}
theta <- log(unlist(theta))
lui_theta <- unlist(lui_theta)
theta_3 <- as.data.frame(cbind(theta, lui_theta))
colnames <- c("theta", "lui")
th_3 <- ggplot(theta_3,aes(x=lui_theta, y=theta)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Log. transf. Theta", title = "Structural Fitness differences. 3 sps", tag = "E") + geom_smooth(method = "lm")

#Theta with LUI four species
theta <- list()
lui_theta <-list()

for(i in 1:length(sps4_reduced)){
  theta[[i]] <-sps4_reduced[[i]][,2]
  lui_theta[[i]] <-sps4_reduced[[i]][,6]
}
theta <- log(unlist(theta))
lui_theta <- unlist(lui_theta)
theta_4 <- as.data.frame(cbind(theta, lui_theta))
colnames <- c("theta", "lui")
th_4 <- ggplot(theta_4,aes(x=lui_theta, y=theta)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Log. transf. Theta", title = "Structural Fitness differences. 4 sps", tag = "F") + geom_smooth(method = "lm")

#put all together. 
grid.arrange(om_2, om_3, om_4, th_2, th_3, th_4, nrow=2, ncol=3)

#Plot how community overlap and community differential change with LUI.
#These are metrics of multispecies assemblages, so only for 3 species. 

#Overlap with LUI three species
overlap <- list()
lui_overlap <-list()

for(i in 1:length(sps3_reduced)){
  overlap[[i]] <-sps3_reduced[[i]][,3]
  lui_overlap[[i]] <-sps3_reduced[[i]][,6]
}
overlap <- unlist(overlap)
lui_overlap <- unlist(lui_overlap)
overlap_3 <- as.data.frame(cbind(overlap, lui_overlap))
colnames <- c("overlap", "lui")
ov_3 <- ggplot(overlap_3,aes(x=lui_overlap, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Overlap", title = "Community-pair Overlap, 3sps", tag = "A") + geom_smooth(method = "lm")

#Overlap with LUI four species
overlap <- list()
lui_overlap <-list()

for(i in 1:length(sps4_reduced)){
  overlap[[i]] <- sps4_reduced[[i]][,3]
  lui_overlap[[i]] <- sps4_reduced[[i]][,6]
}
overlap <- unlist(overlap)
lui_overlap <- unlist(lui_overlap)
overlap_4 <- as.data.frame(cbind(overlap, lui_overlap))
colnames <- c("overlap", "lui")
ov_4 <- ggplot(overlap_4,aes(x=lui_overlap, y=overlap)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Overlap", title = "Community-pair Overlap, 4sps", tag = "B") + geom_smooth(method = "lm")


#Differential with LUI three species
differential <- list()
lui_differential <-list()

for(i in 1:length(sps3_reduced)){
  differential[[i]] <- sps3_reduced[[i]][,4]
  lui_differential[[i]] <- sps3_reduced[[i]][,6]
}
differential <- unlist(differential)
lui_differential <- unlist(lui_differential)
differential_3 <- as.data.frame(cbind(differential, lui_differential))
colnames <- c("differential", "lui")
di_3 <- ggplot(differential_3,aes(x=lui_differential, y=differential)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Differential", title = "Community-pair Differential, 3sps", tag = "C") + geom_smooth(method = "lm")

#Differential with LUI four species
differential <- list()
lui_differential <-list()

for(i in 1:length(sps4_reduced)){
  differential[[i]] <- sps4_reduced[[i]][,4]
  lui_differential[[i]] <- sps4_reduced[[i]][,6]
}
differential <- unlist(differential)
lui_differential <- unlist(lui_differential)
differential_4 <- as.data.frame(cbind(differential, lui_differential))
colnames <- c("differential", "lui")
di_4 <- ggplot(differential_4,aes(x=lui_differential, y=differential)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Differential", title = "Community-pair Differential, 4sps", tag = "D") + geom_smooth(method = "lm")

#put all together. 
grid.arrange(ov_3, ov_4, di_3, di_4, nrow=2, ncol=2)

#Finally plot the number of species quadruplets, triplets and pairs that coexist across LUI

#Feasibility with LUI two species
feasibility <- list()
lui_feasibility <-list()

for(i in 1:length(sps2_reduced)){
  feasibility[[i]] <-sps2_reduced[[i]][,5]
  lui_feasibility[[i]] <-sps2_reduced[[i]][,6]
}
feasibility <- unlist(feasibility)
lui_feasibility <- unlist(lui_feasibility)
feasibility_2 <- as.data.frame(cbind(feasibility, lui_feasibility))
colnames <- c("feasibility", "lui")
fe_2 <- ggplot(feasibility_2,aes(x=lui_feasibility, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Feasibility", title = " Feasible 2 sps", tag = "A") + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)

#Feasibility with LUI three species
feasibility <- list()
lui_feasibility <-list()

for(i in 1:length(sps3_reduced)){
  feasibility[[i]] <-sps3_reduced[[i]][,5]
  lui_feasibility[[i]] <-sps3_reduced[[i]][,6]
}
feasibility <- unlist(feasibility)
lui_feasibility <- unlist(lui_feasibility)
feasibility_3 <- as.data.frame(cbind(feasibility, lui_feasibility))
colnames <- c("feasibility", "lui")
fe_3 <- ggplot(feasibility_3,aes(x=lui_feasibility, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Feasibility", title = "Feasible 3 sps", tag = "B") + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)

#Feasibility with LUI four species
feasibility <- list()
lui_feasibility <-list()

for(i in 1:length(sps4_reduced)){
  feasibility[[i]] <-sps4_reduced[[i]][,5]
  lui_feasibility[[i]] <-sps4_reduced[[i]][,6]
}
feasibility <- unlist(feasibility)
lui_feasibility <- unlist(lui_feasibility)
feasibility_4 <- as.data.frame(cbind(feasibility, lui_feasibility))
colnames <- c("feasibility", "lui")
fe_4 <- ggplot(feasibility_4,aes(x=lui_feasibility, y=feasibility)) + geom_point(alpha = 0.3) + labs(x = "LUI", y= "Feasibility", title = "Feasible 4 sps", tag = "C") + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)

#put all together. 
grid.arrange(fe_2, fe_3, fe_4, ncol=3)



