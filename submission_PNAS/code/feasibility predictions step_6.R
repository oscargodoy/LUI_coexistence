###Step 6 Compare observed decline of species richness with LUI to predicted and randomized
rm(list = ls())

library(tibble)
library(stringr)
library(ggplot2)

##load observed data

###
### Modify the raw data.frame to discrete LUI values and Presence/absence plant data
###

#number of plants
n_plants <- 50

#load LUI data
lui <- read.csv("data/raw_data/LUI06_15.csv", header  = TRUE)

#stick with LUI at different years
lui.only <- lui[, grep("LUI", names(lui))]
lui.only2 <- lui.only[, -c(1:2)] ## remove 2006 and 2007
lui_tot <- cbind("plot" = lui$Plot, lui.only2)

#restructure LUI
lu <- c(lui_tot$LUI_08, lui_tot$LUI_09, lui_tot$LUI_10, lui_tot$LUI_11,
        lui_tot$LUI_12, lui_tot$LUI_13, lui_tot$LUI_14, lui_tot$LUI_15)
yy <- c(rep(2008, nrow(lui_tot)), rep(2009, nrow(lui_tot)), rep(2010, nrow(lui_tot)), rep(2011, nrow(lui_tot)),
        rep(2012, nrow(lui_tot)), rep(2013, nrow(lui_tot)), rep(2014, nrow(lui_tot)), rep(2015, nrow(lui_tot)))
pp <- rep(lui_tot$plot, length(unique(yy)))

lui_total <- data.frame("Plot" = pp, "Year" = yy, "LUI" = lu)
rm(lui, lui_tot, lui.only, lui.only2, pp, lu, yy)

#load plant data
plants <- read.csv("data/raw_data/BE.plants08.16.csv", header = TRUE)

#remove data from 2016 because there is no LUI data
plants <- plants[plants$Year != 2016, ]

#remove info columns
#format is the same order as LUI, so we'll later merge with LUI to have all info
plant_only <- plants[-c(1:4)]

### top --- select the most common plant species
top <- rev(sort(apply(plant_only, 2, mean, na.rm = TRUE)))[1:n_plants]

#short standard names for the selected plants --- WARNING: CURRENTLY, A MANUAL STEP
top.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_sp",
               "Lol_per", "Arr_ela", "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep",
               "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", "Bro_hor", "Pla_lan",
               "Ach_mil", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub",
               "Car_hir", "Bra_pin", "Pha_aru", "Ant_odo", "Ver_cha", "Fes_ovi",
               "Rum_ace", "Des_ces", "Phl_pra", "Agr_sto", "Cyn_cri", "Cir_ole",
               "Cre_bie", "Cer_hol", "Pla_med", "Thy_pul", "Urt_dio", "Lol_mul",
               "Cir_arv", "Ran_bul", "Tri_dub", "Lot_cor", "Car_car", "Leo_his",
               "Vic_sep", "Med_lup")

plants <- plants[, match(names(top), names(plants))] #dataset with the 51 most common plant species
names(plants) <- top.short #give them standard names

#change to presence/absence data (0 or 1)
plants_div <- plants
for (i in 1:nrow(plants)){
  for (j in 1:ncol(plants)){
    if(is.na(plants[i, j])){
      plants[i, j] <- 0
    } else {
      if (plants[i, j] > 0){
        plants_div[i, j] <- 1
      }
    }
  }
}

spp_LUI <- cbind(lui_total, plants_div) #final data frame for plant presence/absence
spp_LUI <- na.omit(spp_LUI) #remove NAs
rm(lui_total, plants, plants_div, i, j, top)#, top.short)

#Year as factor
spp_LUI$Year <- as.factor(spp_LUI$Year)


### Values of LUI and feasible interactions ####
lui_seq <- seq(0.5, 3.0, by = 0.25)
lui_seq2 <- c(lui_seq - 0.25/2, 3.0 + 0.25/2)
LUI_intervals <- data.frame()

#clustering by LUI value
for (i in 1:length(lui_seq)){
  for (j in 1:nrow(spp_LUI)){
    if (spp_LUI$LUI[j] >= lui_seq2[i] && spp_LUI$LUI[j] < lui_seq2[i+1]){
      LUI_s <- data.frame(as.factor(lui_seq[i]))
      names(LUI_s) <- "LUI_s"
      LUI_intervals <- rbind(LUI_intervals, cbind(LUI_s, spp_LUI[j,]))
    }
  }
}
LUI <- LUI_intervals

#keep only valuable objects
rm(list=ls()[! ls() %in% c("LUI", "lui_seq", "top.short")])



#sd and mean
per_plot <- data.frame(
  "lui" = LUI$LUI_s,
  "spp" = apply(LUI[, 5:54], MARGIN = 1, FUN = sum)
)
deviation <- aggregate(spp ~ lui, data = per_plot, FUN = sd)
deviation$spp <- round(deviation$spp)
spp_mean <- aggregate(spp ~ lui, data = per_plot, FUN = mean)
spp_mean$spp <- round(spp_mean$spp)

#calculate the maximum number of species per plot
c <- 0
any_spp <- NULL
max_spp <- NULL
lui_value <- NULL
for(i in 1:length(lui_seq)){
  c <- 0
  for (j in which(LUI$LUI_s == lui_seq[i])){
    if (sum(LUI[j, 5:54]) > c){
      c <- sum(LUI[j, 5:54])
    }
    lui_value <- c(lui_value, lui_seq[i])
    any_spp <- c(any_spp, sum(LUI[j, 5:54]))
  }
  max_spp <- c(max_spp, c)
}

df <- data.frame("lui" = lui_seq, "max_spp" = max_spp)
ddff <- data.frame("lui" = lui_value, "spp" = any_spp)

ggplot(data = df, aes(x = lui, y = max_spp)) + geom_line() + geom_point()

#modify the df to have a count of the presence of the plants per LUI
l <- as.vector(unique(LUI$LUI_s))
s <- top.short; rm(top.short)
observed <- NULL
for (i in 1:length(l)){
  ldata <- subset(LUI, LUI_s == l[i])
  for (j in 1:length(s)){
    observed <- rbind(observed, data.frame(
      "species" = s[j],
      "LUI" = l[i],
      "observed" = sum(ldata[, match(s[j], colnames(LUI))]),
      "presence" = 0
    ))
  }
}

#if a especies is present, put a value of 1
for (i in 1:nrow(observed)){
  if (observed$observed[i] > 0){
    observed$presence[i] <- 1
  }
}

#prepare observed
tot_observed <- aggregate(observed ~ species + LUI, observed, sum)
for (i in 1:nrow(tot_observed)){
  if (tot_observed$observed[i] > 0){
    tot_observed$observed[i] <- 1
  }
}
tot_observed <- aggregate(observed ~ LUI, tot_observed, sum)
tot_observed <- data.frame("Type" = "Observed",
                           "LUI" = tot_observed$LUI,
                           "species_number" = spp_mean$spp,
                           "species_sd" = deviation$spp)

tot_observed$species_number <-  df[,2]

## load average outcomes
predicted <- read.csv(file = "submission_PNAS/results/results-LUI_coexistence-2spp.csv", row.names = 1)
lui <- seq(from = 0.5, to = 3, by = 0.25)
predicted$species <-row.names(predicted)
predicted <- predicted[predicted$feasibility>0.99,] #only the coexisting pairs which is equal to 1.

feas_predicted<- as.data.frame(rbind(
  cbind(str_split(predicted$species, pattern = "\\.", n = 3, simplify = TRUE)[, 1],
        predicted$lui),
  cbind(str_split(predicted$species, pattern = "\\.", n = 3, simplify = TRUE)[, 2],
        predicted$lui)))
colnames(feas_predicted) <- c("combos", "LUI")

feas_predicted$temp <- paste(feas_predicted$combos, feas_predicted$LUI, sep="_")
feas_predicted <- feas_predicted[!duplicated(feas_predicted$temp),] 

# sum number of species by LUI value to plot predictions
tot_predicted <- feas_predicted %>% dplyr::count(LUI)
tot_predicted$n <- tot_predicted$n +1
zz <-rep("Predicted_pairs", times=nrow(tot_predicted))
zero <-rep(0, times=nrow(tot_predicted))
tot_predicted <-cbind(zz,tot_predicted,zero)
colnames(tot_predicted)<-colnames(tot_observed)
total <- rbind(tot_observed,tot_predicted)

size_text <- 10
total$Type <- as.factor(total$Type)
total$LUI <- as.numeric(total$LUI)

library(ggplot2)
colors_obs_pred <- c('#999999',"#4291d7")
(obs_pred <- ggplot(data = NULL, aes(x = LUI, y = species_number)) +
    geom_ribbon(data = subset(total, Type == "Observed"),
                aes(ymin = (species_number - species_sd),
                    ymax = (species_number + species_sd)),
                fill = "grey85") +
    geom_line(data = total, aes(linetype = Type, color = Type), size = 1.5) +
    geom_point(data = total, aes(color = Type), size = 3) +
    scale_linetype_manual(values=c("solid", "dotted", "dotted"),
                          labels = c("Observed, with SD", "Predicted (species pairs)", "Predicted (multispecies)")) +
    scale_color_manual(values = colors_obs_pred,
                       labels = c("Observed, with SD", "Predicted (species pairs)", "Predicted (multispecies)")) +
    scale_x_continuous(name = "Land use intensity (LUI)",
                       breaks = as.vector(unique(total$LUI))) +
    scale_y_continuous(name = "Number of species",
                       breaks = seq(0, 35, 1)) +
    theme(legend.position = "right",
          text = element_text(size = size_text))
)

#load random results
random <- readRDS(file = "submission_PNAS/results/results-LUI_coexistence-2spp_random.rds")
lui <- seq(from = 0.5, to = 3, by = 0.25)

random_ave <-list()
for (i in 1:length(random)){
  Y <- as.matrix(do.call(cbind, random[[i]]))
  Y <- array(Y, dim=c(dim(random[[i]][[1]]), length(random[[i]])))
  random_ave[[i]]<- apply(Y, c(1, 2), mean, na.rm = TRUE)
}

for (i in 1:length(random_ave)){ #this is a trick to avoid the problem of assigning different names with later do.call operation
  row.names(random_ave[[i]])<- paste(row.names(random[[i]][[1]]), random_ave[[i]][,4], sep=".")
  colnames(random_ave[[i]])<- colnames(random[[i]][[1]])
}

coex2 <- as.data.frame(do.call("rbind", random_ave))
coex2 <- tibble::rownames_to_column(coex2, "species")
coex2 <- coex2[coex2$feasibility==1,]


# deconstruct to understand persistence at the species level
feas_random<- as.data.frame(rbind(
  cbind(str_split(coex2$species, pattern = "\\.", n = 3, simplify = TRUE)[, 1],
        coex2$lui),
  cbind(str_split(coex2$species, pattern = "\\.", n = 3, simplify = TRUE)[, 2],
        coex2$lui)))
colnames(feas_random) <- c("combos", "LUI")

#remove values that are duplicated
feas_random$temp <- paste(feas_random$combos, feas_random$LUI, sep="_")
feas_random <- feas_random[!duplicated(feas_random$temp),] 

# sum number of species by LUI value to plot predictions
tot_random <- feas_random %>% dplyr::count(LUI)
zz <-rep("Predicted_random", times=nrow(tot_random))
zero <-rep(0, times=nrow(tot_random))
tot_random <-cbind(zz,tot_random,zero)
colnames(tot_random)<-colnames(tot_observed)

#join the three datasets
total <- rbind(tot_observed,tot_predicted,tot_random)

size_text <- 14
total$Type <- as.factor(total$Type)
total$LUI <- as.numeric(total$LUI)

library(ggplot2)
colors_obs_pred <- c('#999999',"#4291d7", "#db9112")
(obs_pred <- ggplot(data = NULL, aes(x = LUI, y = species_number)) +
    geom_ribbon(data = subset(total, Type == "Observed"),
                aes(ymin = (species_number - species_sd),
                    ymax = (species_number + species_sd)),
                fill = "grey85") +
    geom_line(data = total, aes(linetype = Type, color = Type), size = 1.5) +
    geom_point(data = total, aes(color = Type), size = 3) +
    scale_linetype_manual(values=c("solid", "dotted", "dotted"),
                          labels = c("Observed, with SD", "Predicted (species pairs)", "Predicted Random")) +
    scale_color_manual(values = colors_obs_pred,
                       labels = c("Observed, with SD", "Predicted (species pairs)", "Predicted Random")) +
    scale_x_continuous(name = "Land use intensity (LUI)",
                       breaks = as.vector(unique(total$LUI))) +
    scale_y_continuous(name = "Number of species",
                       breaks = seq(0, 50, 1)) +
    theme(legend.position = "right",
          text = element_text(size = size_text))
)





#plot species number vs LUI by observed or predicted
library(ggplot2)
colors_obs_pred <- c('#999999',"#4291d7", "#db9112")
(obs_pred <- ggplot(data = NULL, aes(x = LUI, y = species_number)) +
    geom_ribbon(data = subset(total, Type == "Observed"),
                aes(ymin = (species_number - species_sd),
                    ymax = (species_number + species_sd)),
                fill = "grey85") +
    geom_line(data = total, aes(linetype = Type, color = Type), size = 1.5) +
    geom_point(data = total, aes(color = Type), size = 3) +
    scale_linetype_manual(values=c("solid", "dotted", "dotted"),
                          labels = c("Observed, with SD", "Predicted (species pairs)", "Predicted Random (species pairs)")) +
    scale_color_manual(values = colors_obs_pred,
                       labels = c("Observed, with SD", "Predicted (species pairs)", "Predicted Random (species pairs)")) +
    scale_x_continuous(name = "Land use intensity (LUI)",
                       breaks = as.vector(unique(total$LUI))) +
    scale_y_continuous(name = "Number of species",
                       breaks = seq(0, 45, 1)) +
    theme(legend.position = "right",
          text = element_text(size = 14))
)

ggsave("submission_PNAS/figures/Fig4_main.png", device = "png",
       dpi = 320, width = 9, height = 6, limitsize = FALSE)
