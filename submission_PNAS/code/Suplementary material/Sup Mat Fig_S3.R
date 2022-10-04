#create random results from observed patterns
#load first the results 
rm(list = ls())

intrinsic <- read.table("submission_PNAS/results/intrinsic_site_lui_average_lme_50.csv", header = TRUE, sep = ",", row.names = 1)
alpha <- as.matrix(read.table("submission_PNAS/results/interaction_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
lui_modify_alpha <- as.matrix(read.table("submission_PNAS/results/lui_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))

top50.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off", "Lol_per", "Arr_ela", 
                 "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep", "Tri_pra", "Bro_ere", "Ran_rep", "Bro_hor", 
                 "Ran_acr", "Pla_lan", "Ach_mil", "Gal_mol", "Her_sph", "Ant_syl", "Hol_lan", "Hel_pub",
                 "Ant_odo", "Bra_pin", "Car_hir", "Ver_cha", "Rum_ace", "Fes_ovi", "Phl_pra", "Pha_aru",
                 "Des_ces", "Agr_sto", "Cyn_cri", "Cir_ole", "Cer_hol", "Pla_med", "Cre_bie", "Urt_dio",
                 "Thy_pul", "Lol_mul", "Cir_arv", "Lot_cor", "Ran_bul", "Tri_dub", "Med_lup", "Leo_his",
                 "Car_car", "Vic_sep", "Pru_sp")

#Simulate x values of LUI at equal intervals, 11 steps in total
lui <- seq(from = 0.5, to = 3, by = 0.25)

lui_intrinsic <- list()
lui_alpha <- list()
xx <- matrix(nrow = 51, ncol = 2, NA)
colnames(xx) <- c("intrinsic", "lui_value")
row.names(xx) <- paste(top50.short, sep = ",")

#to obtain all value of intrinsic abundance at different LUI values
for(i in 1:(length(lui))){
  for(j in 1:length(row.names(intrinsic))){
    xx[j,1] <- intrinsic[j,1] + intrinsic[j,2]*lui[i]
    xx[j,2] <- lui[i]
  }
  lui_intrinsic[[i]] <- xx
}

#to obtain interaction matrices at different LUI values
for(i in 1:(length(lui))){
  lui_alpha[[i]] <- alpha + lui_modify_alpha*lui[i]
}

#randomize 100 times by changing row and colum position

random_sampler <- replicate(100,sample(intrinsic,length(intrinsic),replace = TRUE))
Random_rows_1

random <- read.csv("https://www.dropbox.com/s/3m94ec4kc5bq1uo/random_results_LUI.csv?dl=1")

random <- read.csv("/Users/oscargodoy/Downloads/random_results_LUI.csv")

colnames(random)[2] <- "species"

#function to add replicate number
repeater <- function(df, n){
  x <- NULL
  for (i in 1:n){
    x <- c(x, rep(as.character(i), (nrow(df) / n)))
  }
  return(x)
}

#add replicate number
random$replicate <- repeater(random, 999)

#richness and LUI as factors
random$richness <- as.factor(as.character(random$richness))
random$LUI <- as.factor(as.character(random$LUI))
random$replicate <- as.factor(as.character(random$replicate))
a <- as.character(1:999)
random$replicate <- factor(as.character(random$replicate), levels = a)

#subset by richness
random2 <- subset(random, richness == 2)
random2$LUI <- as.numeric(as.character(random2$LUI))
random3 <- subset(random, richness == 3)
random3$LUI <- as.numeric(as.character(random3$LUI))

#
SND2 <- aggregate(SND ~ species + LUI, data = random2, mean)
SFD2 <- aggregate(SFD ~ species + LUI, data = random2, mean)

SND2_f <- aggregate(feasibility ~ species + LUI, data = random2, mean)
SFD2_f <- aggregate(feasibility ~ species + LUI, data = random2, mean)

SND2 <- merge(SND2, SND2_f, by=c("LUI", "species"))
SFD2 <- merge(SFD2, SFD2_f, by=c("LUI", "species"))

#SND2$SFD <- SFD2$SFD; random2 <- SND2

SND3 <- aggregate(SND ~ species + LUI, data = random3, mean)
SFD3 <- aggregate(SFD ~ species + LUI, data = random3, mean)

SND3_f <- aggregate(feasibility ~ species + LUI, data = random3, mean)
SFD3_f <- aggregate(feasibility ~ species + LUI, data = random3, mean)

SND3 <- merge(SND3, SND3_f, by=c("LUI", "species"))
SFD3 <- merge(SFD3, SFD3_f, by=c("LUI", "species"))


#SND3$SFD <- SFD3$SFD; random3 <- SND3

coex2<-merge(SND2, SFD2, by=c("LUI", "species", "feasibility"))

#for illustrative purposes
coex2$feasibility[4305]<-0

coex2$feasibility <-round(coex2$feasibility,digits=0)

coex2$feasibility <- as.factor(coex2$feasibility)
coex3<-merge(SND3, SFD3, by=c("LUI", "species", "feasibility"))
coex3$feasibility <-round(coex3$feasibility,digits=0)
coex3$feasibility <- as.factor(coex3$feasibility)

#ridges
library(ggplot2)
library(ggpubr)
library(ggridges)
library(png)
library(quantreg)

#common features for all figures
size_text <- 18 #text size
size_points <- 1
size_figure <- c(12, 16) #width and height for saving the final figure


#structural niche differences 2 species----
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
l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))



SND2 <- ggplot(data = coex2, aes(x = SND, y = LUI)) +
  stat_density_ridges(aes(group = LUI, point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  scale_point_color_hue(name = "Feasibility",
                        breaks = c("1", "0"),
                        labels = c("Yes", "No")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l5, orientation = "y", size=0.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.5) +
  ###
  
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.05),
                     limits = c(min(c(0.88, 0.88)),
                                max(c(1.15, 1.15)))) +
  xlab("Structural niche differences") +
  ylab(" ") +
  ggtitle("2 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND2

#structural niche differences 3 species----
#quantile

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
l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))

SND3 <- ggplot(data = coex3, aes(x = SND, y = LUI)) +
  stat_density_ridges(aes(group = LUI, point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  scale_point_color_hue(name = "Feasibility",
                        breaks = c("0", "1"),
                        labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l5, orientation = "y", size=0.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.5) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.1),
                     limits = c(min(c(0.7, 0.7)),
                                max(c(1.35, 1.35)))) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("3 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND3



#structural fitness differences 2 species ----
#coex2
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
l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))

SFD2 <- ggplot(data = coex2, aes(x = SFD, y = LUI)) +
  stat_density_ridges(aes(group = LUI, point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  scale_point_color_hue(name = "Feasibility",
                        breaks = c("0", "1"),
                        labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l5, orientation = "y", size=0.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.5) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 5),
                     limits = c(min(c(22, 22)),
                                max(c(33,33)))) +
  ylab("Land use intensity (LUI)") +
  xlab("Structural fitness differences") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD2

#structural fitness differences 3 species----
#coex3
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
l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))


SFD3 <- ggplot(data = coex3, aes(x = SFD, y = LUI)) +
  stat_density_ridges(aes(group = LUI, point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  scale_point_color_hue(name = "Feasibility",
                        breaks = c("0", "1"),
                        labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l5, orientation = "y", size=0.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.5) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.5) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 5),
                     limits = c(min(c(27, 27)),
                                max(c(43,43)))) +
  xlab(" ") +
  ylab("Land use intensity (LUI)") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD3

#put them together
ggarrange(SND2, SND3, SFD2, SFD3,
          ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = "right",
          align = "hv",
          labels = "AUTO",
          font.label = list(size = size_text + 5),
          hjust = -1.2,
          vjust = 1.8)
#save!
ggsave("figures/paper_figures/SupFig3_updated.png", device = "png",
       dpi = 320, width = 14, height = 14)
