#this loads the data for 2 and 3 species combinations
coex2 <- read.csv(file = "results/results-LUI_coexistence-2spp.csv")
coex2$feasibility <- as.factor(coex2$feasibility)

coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)

#load random results (takes time)
random <- read.csv("https://www.dropbox.com/s/3m94ec4kc5bq1uo/random_results_LUI.csv?dl=1")
colnames(random)[2] <- "species"

#then perform quantile regression to estimate whether LUI affects changes in median
# as well as changes in the tails of SND and SFD distibutions. 
library(quantreg)

#tau specifies the value of the quantile 0.5 correspond to the median

#2 species
rqn2 <-rq(SND ~ LUI, data = coex2, tau = seq(0.1, 0.9, by = 0.1))
rqn2
rqf2 <- rq(SFD ~ LUI, data = coex2, tau = seq(0.1, 0.9, by = 0.1))
rqf2
#3 species
rqn3 <-rq(SND ~ LUI, data = coex3, tau = seq(0.1, 0.9, by = 0.1))
rqn3
rqf3 <- rq(SFD ~ LUI, data = coex3, tau = seq(0.1, 0.9, by = 0.1))
rqf3

# plotting different quantiles
colors <- c("#ffe6e6", "#ffcccc", "#ff9999", "#ff6666", "#ff3333",
            "#ff0000", "#cc0000", "#b30000", "#800000", "#4d0000", "#000000")
par(mfrow=c(2,2))
plot(SND ~ LUI, data = coex2, pch = 16, main = "Niche diff. vs LUI 2 sp")
for (j in 1:ncol(rqn2$coefficients)) {
  abline(coef(rqn2)[, j], col = colors[j])
}
plot(SND ~ LUI, data = coex3, pch = 16, main = "Niche diff. vs LUI 3 sp")
for (j in 1:ncol(rqn3$coefficients)) {
  abline(coef(rqn3)[, j], col = colors[j])
}
plot(SFD ~ LUI, data = coex2, pch = 16, main = "Fitness diff. vs LUI 2 sp")
for (j in 1:ncol(rqf2$coefficients)) {
  abline(coef(rqf2)[, j], col = colors[j])
}
plot(SFD ~ LUI, data = coex3, pch = 16, main = "Fitness diff. vs LUI 3 sp")
for (j in 1:ncol(rqf3$coefficients)) {
  abline(coef(rqf3)[, j], col = colors[j])
}

#Most of them are significant because of the high sample size. But let's plot
#the estimates and the standard error to check which ones have bigger effects. 

rqn2.summ <- summary(rq(SND ~ LUI, data = coex2, tau = seq(0.1, 0.9, by = 0.1)))
rqf2.summ <- summary(rq(SFD ~ LUI, data = coex2, tau = seq(0.1, 0.9, by = 0.1)))
rqn3.summ <- summary(rq(SND ~ LUI, data = coex3, tau = seq(0.1, 0.9, by = 0.1)))
rqf3.summ <- summary(rq(SFD ~ LUI, data = coex3, tau = seq(0.1, 0.9, by = 0.1)))
rqn.random.summ <- summary(rq(SND ~ LUI, data = random, tau = seq(0.1, 0.9, by = 0.1)))
rqf.random.summ <- summary(rq(SFD ~ LUI, data = random, tau = seq(0.1, 0.9, by = 0.1)))


par(mfrow=c(2,2))

# 2 sp niche
mean <- as.vector(matrix(nrow=1, ncol=length(rqn2.summ), NA))
sd <- as.vector(matrix(nrow=1, ncol=length(rqn2.summ), NA))
for(i in 1:length(rqn2.summ)) {
  mean[i] <- rqn2.summ[[i]]$coefficients[2,1]
  sd[i] <- rqn2.summ[[i]]$coefficients[2,2]
}
plot(seq(0.1, 0.9, by = 0.1), mean, pch = 16, cex=1.5, ylab="Estimate", xlab ="Quantile",
     ylim= c(min(mean)-0.03, max(mean)+0.02), cex.lab=1.3, main="Structural niche differences \n2 species combination")
axis(side=1, at=seq(0.1, 0.9, by = 0.1))
segments(seq(0.1, 0.9, by = 0.1),mean-sd, seq(0.1, 0.9, by = 0.1),mean+sd)
abline(h=0.00639, col="red", lty=2)
text("n.s", x=0.15, y=0.015)

# 3 sp niche
mean <- as.vector(matrix(nrow=1, ncol=length(rqn3.summ), NA))
sd <- as.vector(matrix(nrow=1, ncol=length(rqn3.summ), NA))
for(i in 1:length(rqn3.summ)) {
  mean[i] <- rqn3.summ[[i]]$coefficients[2,1]
  sd[i] <- rqn3.summ[[i]]$coefficients[2,2]
}
plot(seq(0.1, 0.9, by = 0.1), mean, pch = 16, cex=1.5, ylab="Estimate", xlab ="Quantile",
     ylim= c(min(mean)-0.02, max(mean)+0.04), cex.lab=1.3, main="Structural niche differences \n3 species combination")
axis(side=1, at=seq(0.1, 0.9, by = 0.1))
segments(seq(0.1, 0.9, by = 0.1),mean-sd, seq(0.1, 0.9, by = 0.1),mean+sd)
abline(h=0.03, col="red", lty=2)
text("n.s", x=0.15, y=0.05)

# 2 sp fitness
mean <- as.vector(matrix(nrow=1, ncol=length(rqf2.summ), NA))
sd <- as.vector(matrix(nrow=1, ncol=length(rqf2.summ), NA))
for(i in 1:length(rqf2.summ)) {
  mean[i] <- rqf2.summ[[i]]$coefficients[2,1]
  sd[i] <- rqf2.summ[[i]]$coefficients[2,2]
}
plot(seq(0.1, 0.9, by = 0.1), mean, pch = 16, cex=1.5, ylab="Estimate", xlab ="Quantile",
     ylim= c(min(mean)-0.9, max(mean)+0.7), cex.lab=1.3, main="Structural fitness differences \n2 species combination")
axis(side=1, at=seq(0.1, 0.9, by = 0.1))
segments(seq(0.1, 0.9, by = 0.1),mean-sd, seq(0.1, 0.9, by = 0.1),mean+sd)
abline(h=0.85, col="red", lty=2)
text("n.s", x=0.18, y=0.5)

# 3 sp fitness
mean <- as.vector(matrix(nrow=1, ncol=length(rqf3.summ), NA))
sd <- as.vector(matrix(nrow=1, ncol=length(rqf3.summ), NA))
for(i in 1:length(rqf3.summ)) {
  mean[i] <- rqf3.summ[[i]]$coefficients[2,1]
  sd[i] <- rqf3.summ[[i]]$coefficients[2,2]
}
plot(seq(0.1, 0.9, by = 0.1), mean, pch = 16, cex=1.5, ylab="Estimate", xlab ="Quantile",
     ylim= c(min(mean)-0.7, max(mean)+0.7), cex.lab=1.3, main="Structural niche differences \n3 species combination")
axis(side=1, at=seq(0.1, 0.9, by = 0.1))
segments(seq(0.1, 0.9, by = 0.1),mean-sd, seq(0.1, 0.9, by = 0.1),mean+sd)
abline(h=0.20, col="red", lty=2)
text("n.s", x=0.15, y=0)

ggsave(filename = "figures/paper_figures/sup mat quantile regresion.png", device = "png",
       width = 15, height = 10, limitsize = FALSE)

# random niche


# random fitness
