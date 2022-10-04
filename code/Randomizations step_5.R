###Step 5 Randomize obtained matrices across LUI and test how niche and fitness differences change.

rm(list = ls())


#1.Obtaining combinations to calculate niche and fitness diff.----
#loading average values
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

#remove everything except for matrix and intrinsic with LUI
rm(list=ls()[! ls() %in% c("lui_alpha", "lui_intrinsic", "top50.short", "lui")])

# 100 randomizations per LUI value, no replacement in order to not repeat species within each reshuffle.

random_alpha <- list()
random_lui <- list()
random_column <- as.data.frame(replicate(100,sample(top50.short,length(top50.short),replace = FALSE))) 
random_row <- as.data.frame(replicate(100,sample(top50.short,length(top50.short),replace = FALSE)))

for(i in 1:length(lui)){
  for(j in 1:length(random_column)){
    random_lui[[j]]<-lui_alpha[[i]][unlist(random_row[j]), unlist(random_column[j])] #changing column position
  }
  random_alpha[[i]] <- random_lui
}

#2.Computing calculations of niche and fitness diff for each randomization.----
#for combinations of 2 and 3 species. 

combos_lui2 <- list()
for(i in 1:length(lui_alpha)){
  combos_lui2[[i]] <- t(combn(row.names(lui_alpha[[i]]),2))
}

combos_lui3 <- list()
for(i in 1:length(lui_alpha)){
  combos_lui3[[i]] <- t(combn(row.names(lui_alpha[[i]]),3))
}

library(mvtnorm)

source('code/toolbox_coexistence.R')
source('code/toolbox_figure.R')

library(foreach)
library(doParallel)
library(parallel)
library(doSNOW)


cores = detectCores()
cl <- makeCluster(cores[1])
registerDoSNOW(cl) #defines parallel computation 
startime <- Sys.time() # to see how much it takes to run


#Combinations of two species. 
col_results  <- c("omega", "theta", "feasibility", "lui")
neg_intras <- data.frame()

results_coex <- foreach (i = 1:length(combos_lui2), .combine = rbind, .packages = c("mvtnorm", "data.table")) %dopar% {
  #
  results_combo <- matrix (nrow = nrow(combos_lui2[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos_lui2[[i]], 1, paste, collapse=".")
  colnames(results_combo) <- col_results
  results_combo <- lapply(seq_len(100), function(X) results_combo) #repeat the same structure for each randomization
  
for(h in 1:length(random_column)){
  #
  for (j in 1:(nrow(combos_lui2[[i]]))){
    ll <- as.matrix(combos_lui2[[i]][j,])
    mm <- random_alpha[[i]][[h]][which(rownames(random_alpha[[i]][[h]]) %in% ll), which(colnames(random_alpha[[i]][[h]]) %in% ll)]
    mm <- -1 * mm #this is because intras has to be positive in a LV model, the GLMM fitting use the inverse characterization
    
    #save info to locate the species in case of negative intras
    if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
      neg_intras <- rbind(neg_intras,
                          data.frame("lui_level" = i,
                                     "combo" = j,
                                     "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
    }
    ii <- subset(lui_intrinsic[[i]], rownames(lui_intrinsic[[i]]) %in% ll)
    results_combo[[h]][j, 1] <- 10^Omega(mm) # this is niche differences
    results_combo[[h]][j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
    results_combo[[h]][j, 3] <- test_feasibility(mm, ii[, 1]) #this is whether all species can coexist. 
    results_combo[[h]][j, 4] <- unique(lui_intrinsic[[i]][, 2])
    }
  }
  print(i)
  return(results_combo)
}
stopCluster(cl) #stop cluster
endtime <- Sys.time() #save time once finished
(totime <- endtime - startime)

xx <-list()
results_coex_final <-list()

for(i in 1:11){ #there are 11 LUI levels
  for(j in 1:100){ #there are 100 simulations
  xx[[j]] <-as.data.frame(results_coex[i,j])
  colnames(xx[[j]])<-c("omega", "theta", "feasibility", "lui")
  }
  results_coex_final[[i]]<-xx
}

saveRDS(results_coex_final, file = "submission_PNAS/results/results-LUI_coexistence-2spp_random.rds")



cores = detectCores()
cl <- makeCluster(cores[1])
registerDoSNOW(cl) #defines parallel computation 
startime <- Sys.time() # to see how much it takes to run


#Combinations of two species. 
col_results  <- c("omega", "theta", "feasibility", "lui")
neg_intras <- data.frame()
#Combinations of three species. 
col_results  <- c("omega", "theta", "feasibility", "lui")
neg_intras <- data.frame()

results_coex <- foreach (i = 1:length(combos_lui3), .combine = rbind, .packages = c("mvtnorm", "data.table")) %dopar% {
  #
  results_combo <- matrix (nrow = nrow(combos_lui3[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos_lui3[[i]], 1, paste, collapse=".")
  colnames(results_combo) <- col_results
  results_combo <- lapply(seq_len(100), function(X) results_combo) #repeat the same structure for each randomization
  
  for(h in 1:length(random_column)){
    #
    for (j in 1:(nrow(combos_lui3[[i]]))){
      ll <- as.matrix(combos_lui3[[i]][j,])
      mm <- random_alpha[[i]][[h]][which(rownames(random_alpha[[i]][[h]]) %in% ll), which(colnames(random_alpha[[i]][[h]]) %in% ll)]
      mm <- -1 * mm #this is because intras has to be positive in a LV model, the GLMM fitting use the inverse characterization
      
      #save info to locate the species in case of negative intras
      if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
        neg_intras <- rbind(neg_intras,
                            data.frame("lui_level" = i,
                                       "combo" = j,
                                       "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
      }
      ii <- subset(lui_intrinsic[[i]], rownames(lui_intrinsic[[i]]) %in% ll)
      results_combo[[h]][j, 1] <- 10^Omega(mm) # this is niche differences
      results_combo[[h]][j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
      results_combo[[h]][j, 3] <- test_feasibility(mm, ii[, 1]) #this is whether all species can coexist. 
      results_combo[[h]][j, 4] <- unique(lui_intrinsic[[i]][, 2])
    }
  }
  print(i)
  return(results_combo)
}
stopCluster(cl) #stop cluster
endtime <- Sys.time() #save time once finished
(totime <- endtime - startime)

xx <-list()
results_coex_final <-list()

for(i in 1:11){ #there are 11 LUI levels
  for(j in 1:100){ #there are 100 simulations
    xx[[j]] <-as.data.frame(results_coex[i,j])
    colnames(xx[[j]])<-c("omega", "theta", "feasibility", "lui")
  }
  results_coex_final[[i]]<-xx
}

saveRDS(results_coex_final, file = "submission_PNAS/results/results-LUI_coexistence-3spp_random.rds")

#3.Plotting average values for SND and SFD according to randomization.----
#for combinations of 2 and 3 species.

rm(list = ls())

#average by species combinations and then plot the output.

#mean trend ----
library(plyr)

#load coexistence 2 species results
random2 <- readRDS(file = "submission_PNAS/results/results-LUI_coexistence-2spp_random.rds")
random3 <- readRDS(file = "submission_PNAS/results/results-LUI_coexistence-3spp_random.rds")

#This is to calculate the average across 100 randomizations for each LUI value.
random2_ave <-list()
for (i in 1:length(random2)){
  random2_ave[[i]]<- aaply(laply(random2[[i]], as.matrix), c(2, 3), mean)
  
}
coex2 <- as.data.frame(do.call("rbind", random2_ave))
coex2$feasibility <- round(coex2$feasibility, digits=0)
coex2$feasibility <- as.factor(coex2$feasibility)
colnames(coex2) <- c("SND", "SFD", "feasibility", "LUI")

random3_ave <-list()
for (i in 1:length(random3)){
  random3_ave[[i]]<- aaply(laply(random3[[i]], as.matrix), c(2, 3), mean)
}
coex3 <- as.data.frame(do.call("rbind", random3_ave))
coex3$feasibility <- round(coex3$feasibility, digits=0)
coex3$feasibility <- as.factor(coex3$feasibility)
colnames(coex3) <- c("SND", "SFD", "feasibility", "LUI")

rm(random2, random3) # to free some space

write.csv(coex2, file = "submission_PNAS/results/results-LUI_coexistence-2spp_random_average.csv")
write.csv(coex3, file = "submission_PNAS/results/results-LUI_coexistence-3spp_random_average.csv")

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
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="black", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(0, 0)),
                                max(c(1.5, 1.5)))) +
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

nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)

nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)

nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)

nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)

nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)

nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)

nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)

nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)


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
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="black", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(coex2$SND, coex3$SND)),
                                max(c(2.5, 2.5)))) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("3 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5))



#structural fitness differences 2 species ----
#coex2
eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.15)
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
                        breaks = c("1", "0"),
                        labels = c("Yes", "No")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="black", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(200,200)))) +
  ylab("Land use intensity (LUI)") +
  xlab("Structural fitness differences") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD2

#structural fitness differences 3 species----
#coex3
eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)

nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)

nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)

nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)

nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)

nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)

nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)

nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)

nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)


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
                        breaks = c("1", "0"),
                        labels = c("Yes", "No")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="black", size=1.5, ) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(200,200)))) +
  xlab(" ") +
  ylab("Land use intensity (LUI)") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text))

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

ggsave("submission_PNAS/figures/SupFig.3.png", device = "png",
       dpi = 320, width = size_figure[2], height = size_figure[1])