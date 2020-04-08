#number of plants
n_plants <- 26

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

plants <- plants[plants$Year != 2016, ]

plants <- plants[-c(1:4)]

### top26 --- select the 51 most common plant species
top50 <- rev(sort(apply(plants[, -c(1:5)], 2, mean, na.rm = TRUE)))[1:n_plants]

top50.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off", "Lol_per", "Arr_ela", 
                 "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep", "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", 
                 "Bro_hor", "Pla_lan", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub", "Car_hir",
                 "Bra_pin", "Pha_aru", "Ant_odo", "Ver_cha", "Fes_ovi", "Rum_ace", "Des_ces", "Phl_pra", 
                 "Agr_sto", "Cyn_cri", "Cir_ole", "Cre_bie", "Cer_hol", "Pla_med", "Thy_pul", "Urt_dio",
                 "Lol_mul", "Cir_arv", "Ran_bul", "Tri_dub", "Lot_cor", "Car_car", "Leo_his", "Vic_sep",
                 "Med_lup", "Pru_spp")#, "Sym_off")

plants <- plants[, match(names(top50), names(plants))] #dataset with the 51 most common plant species
names(plants) <- top50.short #give them standard names

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
rm(lui_total, plants, plants_div, i, j, top50)#, top50.short)

#Year as factor
spp_LUI$Year <- as.factor(spp_LUI$Year)

#save
write.table(spp_LUI, file = "data/spp_LUI.txt", row.names = FALSE, sep = "\t")

### Values of LUI and feasible interactions ####
lui_seq <- seq(0.5, 3.0, by = 0.125)
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
rm(LUI_s, spp_LUI, i, j, lui_seq2, LUI_intervals)


#Loop to understand which species interactions are feasible at each LUI value
sublui_save <- list()
lui_feas <- list()
spp <- names(LUI[, 5:(ncol(LUI)-1)])
for (w in 1:length(lui_seq)){
  sublui <- subset(LUI, LUI_s == lui_seq[w])
  sublui <- sublui[, 5:(ncol(LUI)-1)]
  sublui_save[[w]] <- sublui
  lui_feas[[w]] <- matrix(nrow = length(spp), ncol = length(spp))
  colnames(lui_feas[[w]]) <- spp; rownames(lui_feas[[w]]) <- spp
  
  for (i in 1:nrow(lui_feas[[w]])){
    for (j in 1:ncol(lui_feas[[w]])){
      n <- 0
      for (k in 1:nrow(sublui)){
        if ((sublui[k,i] + sublui[k,j]) == 2){
          n <- n + 1
        }
        if (i == j){
          lui_feas[[w]][i, j] <- NA #when it is an intra-specific combination, set to NA
        } else {
          lui_feas[[w]][i, j] <- n
        }
      }
    }
  }
  print(w/length(lui_seq)*100)#cheater
}

rm(i, j, k, w, n, spp, sublui)


#function to detect the percentage of values equal to zero (can be used for any other value)
non.feasible <- function(X, value, perc){
  return((((length(which(X == value))) / 2) / choose(nrow(X), 2)) * perc)
}

perc <- NULL
for(i in 1:length(lui_feas)){
  perc <- c(perc, non.feasible(lui_feas[[i]], 0, 100))
}

feas_int <- data.frame(lui_seq, perc)
ggplot(data = feas_int, aes(lui_seq, perc)) + geom_point()


#most common combinations
common <- list()
value <- seq(0.55, 0.70, 0.05)
sel <- NULL
tot <- NULL
val <- NULL
for (w in 1:length(value)){
  for(i in 1:length(lui_feas)){
    common[[i]] <- lui_feas[[i]] / nrow(sublui_save[[i]])
    sel <- c(sel, (length(which(common[[i]] >= value[[w]]))) / 2)
    tot <- c(tot, choose(nrow(sublui_save[[i]]), 2))
    val <- c(val, value[[w]])
  }
}

comb_selected <- data.frame("LUI" = rep(feas_int$lui_seq, length(sel) /nrow(feas_int)),
                            "threshold" = as.factor(val * 100), "percentage" = sel/tot)

#plots
color.blind <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sel1 <- ggplot(data = comb_selected, aes(x = LUI, y = percentage, color = threshold)) + geom_line(size = 0.75) +
  geom_point(size = 1.5) + scale_x_continuous(breaks = seq(0.5, 3, 0.5)) +
  labs(x = "LUI value", y = "Fraction of all possible pairs (%)", color = "Threshold (%)") +
  theme(legend.position = "none") + scale_color_manual(values = color.blind)

sel2 <- ggplot(data = comb_selected, aes(x = LUI, y = percentage, color = threshold)) + geom_line(size = 0.75) +
  geom_point(size = 1.5) + scale_x_continuous(breaks = seq(0.5, 3, 0.5)) +
  labs(x = "LUI value", y = "Fraction of all possible pairs (%)", color = "Threshold (%)") +
  coord_cartesian(ylim = c(0, 0.125)) + theme(legend.position = "none") + scale_color_manual(values = color.blind)

sel3 <- ggplot(data = comb_selected, aes(x = LUI, y = percentage, color = threshold)) + geom_line(size = 0.75) +
  geom_point(size = 1.5) + scale_x_continuous(breaks = seq(0.5, 3, 0.5)) +
  labs(x = "LUI value", y = "Fraction of all possible pairs (%)", color = "Threshold (%)") +
  coord_cartesian(ylim = c(0, 0.0125)) + theme(legend.position = "none") + scale_color_manual(values = color.blind)

library(ggpubr)
ggarrange(sel1, ggarrange(sel2, sel3, ncol = 2),
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "top")
ggsave("figures/selected_pairs.png", width = 8, height = 7, dpi = 320)



threshold <- 0.65
lui_val <- NULL
spp1 <- NULL
spp2 <- NULL
for (w in 1:length(lui_seq)){
  for (i in 2:nrow(common[[w]])){
    for (j in 1:ncol(common[[w]])){
      if (j >= i){
        #do nothing = select only one side of the diagonal
      } else {
        if (common[[w]][[i, j]] >= threshold){
          lui_val <- c(lui_val, lui_seq[w])
          spp1 <- c(spp1, rownames(common[[w]])[i])
          spp2 <- c(spp2, colnames(common[[w]])[j])
        }
      }
    }
  }
}

View(data.frame(lui_val, spp1, spp2))





