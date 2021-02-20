#this loads the data for 2 and 3 species combinations
coex2 <- read.csv(file = "results/results-LUI_coexistence-2spp.csv")
coex2$feasibility <- as.factor(coex2$feasibility)

coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)


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


#rqn.random.summ <- summary(rq(SND ~ LUI, data = random, tau = seq(0.1, 0.9, by = 0.1)))
#rqf.random.summ <- summary(rq(SFD ~ LUI, data = random, tau = seq(0.1, 0.9, by = 0.1)))


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



#load random results (takes time)
random <- read.csv("https://www.dropbox.com/s/3m94ec4kc5bq1uo/random_results_LUI.csv?dl=1")
colnames(random)[2] <- "species"
random2 <- subset(random, richness==2)
random3 <- subset(random, richness==3)

#2 species
#rqn2 <-rq(SND ~ LUI, data = random2, tau = seq(0.1, 0.9, by = 0.1))
#rqn2
#rqf2 <- rq(SFD ~ LUI, data = random2, tau = seq(0.1, 0.9, by = 0.1))
#rqf2
#3 species
#rqn3 <-rq(SND ~ LUI, data = random3, tau = seq(0.1, 0.9, by = 0.1))
#rqn3
#rqf3 <- rq(SFD ~ LUI, data = random3, tau = seq(0.1, 0.9, by = 0.1))
#rqf3


rqn2.summ <- summary(rq(SND ~ LUI, data = random2, tau = seq(0.1, 0.9, by = 0.1)))
rqf2.summ <- summary(rq(SFD ~ LUI, data = random2, tau = seq(0.1, 0.9, by = 0.1)))
rqn3.summ <- summary(rq(SND ~ LUI, data = random3, tau = seq(0.1, 0.9, by = 0.1)))
rqf3.summ <- summary(rq(SFD ~ LUI, data = random3, tau = seq(0.1, 0.9, by = 0.1)))


par(mfrow=c(2,2))

# 2 sp niche
mean <- as.vector(matrix(nrow=1, ncol=length(rqn2.summ), NA))
sd <- as.vector(matrix(nrow=1, ncol=length(rqn2.summ), NA))
for(i in 1:length(rqn2.summ)) {
  mean[i] <- rqn2.summ[[i]]$coefficients[2,1]
  sd[i] <- rqn2.summ[[i]]$coefficients[2,2]
}
plot(seq(0.1, 0.9, by = 0.1), mean, pch = 16, cex=1.5, ylab="Estimate", xlab ="Quantile",
     ylim= c(min(mean), max(mean)), cex.lab=1.3, main="Structural niche differences \n2 species combination")
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
     ylim= c(min(mean), max(mean)), cex.lab=1.3, main="Structural niche differences \n3 species combination")
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
     ylim= c(min(mean)-0.01, max(mean)+0.02), cex.lab=1.3, main="Structural fitness differences \n2 species combination")
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
     ylim= c(min(mean)-0.01, max(mean)+0.02), cex.lab=1.3, main="Structural fitness differences \n3 species combination")
axis(side=1, at=seq(0.1, 0.9, by = 0.1))
segments(seq(0.1, 0.9, by = 0.1),mean-sd, seq(0.1, 0.9, by = 0.1),mean+sd)
abline(h=0.20, col="red", lty=2)
text("n.s", x=0.15, y=0)


# Non-linear regression ----

coex2 <- read.csv(file = "results/results-LUI_coexistence-2spp.csv")
coex2$feasibility <- as.factor(coex2$feasibility)

coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)


#then perform quantile regression to estimate whether LUI affects changes in median
# as well as changes in the tails of SND and SFD distibutions. 
library(quantreg)

#coex2
eq <- SND ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line
predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))

png(file="figures/quantile.reg.niche.2.png",width=700, height=500)
par(bg = 'darkgrey')
plot(coex2$LUI, coex2$SND, pch=3, main= "Non-linear quantile regression", ylab="SND", xlab ="LUI", cex.lab=1.3, cex.main=1.3)

l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
lines(SND ~ LUI, data = l1, col = "#ffe6e6")
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
lines(SND ~ LUI, data = l2, col = "#ffe6e6")
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
lines(SND ~ LUI, data = l3, col = "#ffcccc")
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
lines(SND ~ LUI, data = l4, col = "#ff9999")
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
lines(SND ~ LUI, data = l5, col = "#ff6666")
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
lines(SND ~ LUI, data = l6, col = "#ff3333")
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
lines(SND ~ LUI, data = l7, col = "#ff0000")
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
lines(SND ~ LUI, data = l8, col = "#cc0000")
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))
lines(SND ~ LUI, data = l9, col = "#b30000")

dev.off()



eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line
predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))

png(file="figures/quantile.reg.fitness.2.png",width=700, height=500)
par(bg = 'darkgrey')
plot(coex2$LUI, coex2$SFD, pch=3, main= "Non-linear quantile regression", ylab="SFD", xlab ="LUI", ylim=c(0,85), cex.lab=1.3, cex.main=1.3)

l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
lines(SFD ~ LUI, data = l1, col = "#ffe6e6")
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
lines(SFD ~ LUI, data = l2, col = "#ffe6e6")
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
lines(SFD ~ LUI, data = l3, col = "#ffcccc")
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
lines(SFD ~ LUI, data = l4, col = "#ff9999")
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
lines(SFD ~ LUI, data = l5, col = "#ff6666")
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
lines(SFD ~ LUI, data = l6, col = "#ff3333")
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
lines(SFD ~ LUI, data = l7, col = "#ff0000")
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
lines(SFD ~ LUI, data = l8, col = "#cc0000")
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))
lines(SFD ~ LUI, data = l9, col = "#b30000")

dev.off()

#coex3
eq <- SND ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line
predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
par(bg = 'darkgrey')
plot(coex3$LUI, coex3$SND, pch=3, main= "Non-linear quantile regression", ylab="SND", xlab ="LUI", ylim=c(0,2.5),cex.lab=1.3, cex.main=1.3)

l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
lines(SND ~ LUI, data = l1, col = "#ffe6e6")
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
lines(SND ~ LUI, data = l2, col = "#ffe6e6")
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
lines(SND ~ LUI, data = l3, col = "#ffcccc")
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
lines(SND ~ LUI, data = l4, col = "#ff9999")
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
lines(SND ~ LUI, data = l5, col = "#ff6666")
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
lines(SND ~ LUI, data = l6, col = "#ff3333")
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
lines(SND ~ LUI, data = l7, col = "#ff0000")
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
lines(SND ~ LUI, data = l8, col = "#cc0000")
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))
lines(SND ~ LUI, data = l9, col = "#b30000")


eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line
predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
par(bg = 'darkgrey')
plot(coex3$LUI, coex3$SFD, pch=3, main= "Non-linear quantile regression", ylab="SFD", xlab ="LUI", ylim=c(0,85), cex.lab=1.3, cex.main=1.3)

l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
lines(SFD ~ LUI, data = l1, col = "#ffe6e6")
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
lines(SFD ~ LUI, data = l2, col = "#ffe6e6")
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
lines(SFD ~ LUI, data = l3, col = "#ffcccc")
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
lines(SFD ~ LUI, data = l4, col = "#ff9999")
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
lines(SFD ~ LUI, data = l5, col = "#ff6666")
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
lines(SFD ~ LUI, data = l6, col = "#ff3333")
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
lines(SFD ~ LUI, data = l7, col = "#ff0000")
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
lines(SFD ~ LUI, data = l8, col = "#cc0000")
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))
lines(SFD ~ LUI, data = l9, col = "#b30000")


