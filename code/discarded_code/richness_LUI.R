#Species diversity by LUI value

##### ALL POSSIBLE PLANTS

rm(list = ls())

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

#####################

#load plant data
plants <- read.csv("data/raw_data/BE.plants08.16.csv", header = TRUE)

#remove data from 2016
plants <- plants[plants$Year != 2016, ]

#remove info columns - format is the same order as LUI, so we'll later merge with LUI to have all info
plants <- plants[-c(1:5)]

#change to presence/absence data (0 or 1)
for (i in 1:nrow(plants)){
  for (j in 5:ncol(plants)){
    if(is.na(plants[i, j])){
      plants[i, j] <- 0
    } else {
      if (plants[i, j] > 0){
        plants[i, j] <- 1
      }
    }
  }
}

#number of species per plot
rich <- apply(plants[, -c(1:5)], 1, sum, na.rm = TRUE)

#merge lui and richness
richness_full_comm <- cbind(lui_total, rich)

#year as factor for plotting
richness_full_comm$Year <- as.factor(richness_full_comm$Year)

#########################################

#PLOTTING
library(ggplot2) #load ggplot2

#color-blind friendly colour palette
color.blind <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Plot 
(box <- ggplot(data = lui_total, aes(x = Year, y = LUI)) + geom_boxplot(aes(fill = Year), alpha = 0.75) +
    theme(legend.position = "none") + geom_abline(intercept = 3, slope = 0, linetype = "dashed") +
    geom_abline(intercept = 0.5, slope = 0, linetype = "dashed") +
    scale_x_discrete(limits = rev(levels(lui_total$Year))) +
    scale_y_continuous(limits = c(0, 4.5), breaks = seq(0, 4.5, by = 0.5)) + coord_flip() +
    scale_fill_manual(values = color.blind))


#boxplot of LUI through the years
(box_lui <- ggplot(data = lui_total, aes(x = Year, y = LUI)) + geom_boxplot(aes(fill = Year), alpha = 0.75) +
    theme(legend.position = "none") + geom_abline(intercept = 3, slope = 0, linetype = "dashed") +
    geom_abline(intercept = 0.5, slope = 0, linetype = "dashed") +
    scale_x_discrete(limits = rev(levels(lui_total$Year))) +
    scale_y_continuous(limits = c(0, 4.5), breaks = seq(0, 4.5, by = 0.5)) + coord_flip() +
    scale_fill_manual(values = color.blind))




(lui_rich_full <- ggplot(data = lui_total, aes(x = LUI, y = Richness, colour = Year)) +
    geom_point(alpha = 0.15) + geom_smooth(alpha = 0, size = 1.1) +
    scale_x_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.5)) +
    scale_y_continuous(position = "left") + ylab("Richness (all-species community)") +
    theme(legend.position = "none") + scale_colour_manual(values = color.blind))

#Chart LUI--DIVERSITY
library(ggpubr)
ggarrange(box, lui_rich_full, align = "h", widths = c(2, 3))
ggsave("figures/LUI_richness_full.png", width = 8, height = 4, dpi = 320)


##### SELECT 26 MOST COMMON

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

plants <- plants[-c(1:5)]

### top50 --- select the 51 most common plant species
top50 <- rev(sort(apply(plants[, -c(1:5)], 2, mean, na.rm = TRUE)))[1:50]

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
rm(lui_total, plants, plants_div, i, j, top50, top50.short)

#add number of species per plot
spp_LUI$Richness <- apply(spp_LUI[, -c(1:3)], 1, sum, na.rm = TRUE)
spp_LUI$Year <- as.factor(spp_LUI$Year)

#plots
library(ggplot2)
color.blind <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

(lui_rich_51 <- ggplot(data = spp_LUI, aes(x = LUI, y = Richness, colour = Year)) +
    geom_point(alpha = 0.15) + geom_smooth(alpha = 0, size = 1.1) +
    scale_x_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.5)) +
    scale_y_continuous(position = "left") + ylab("Richness (51 species community)") +
    theme(legend.position = "none") + scale_colour_manual(values = color.blind))

#Chart LUI--DIVERSITY
library(ggpubr)
both_lui_rich <- ggarrange(lui_rich_full, lui_rich_51, nrow = 2, align = "hv")
ggarrange(box, both_lui_rich, widths = c(3, 4))
ggsave("figures/LUI_richness_both.png", width = 7, height = 7, dpi = 320)
