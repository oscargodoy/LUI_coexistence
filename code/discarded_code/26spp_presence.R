#number of plants
n_plants <- 26

#load LUI data
lui <- read.csv("data/LUI06_15.csv", header  = TRUE)

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
plants <- read.csv("data/BE.plants08.16.csv", header = TRUE)

plants <- plants[plants$Year != 2016, ]

plants <- plants[-c(1:5)]

### top --- select the 51 most common plant species
top <- rev(sort(apply(plants[, -c(1:5)], 2, mean, na.rm = TRUE)))[1:n_plants]

top.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off", "Lol_per", "Arr_ela", 
                 "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep", "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", 
                 "Bro_hor", "Pla_lan", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub", "Car_hir",
                 "Bra_pin", "Pha_aru")#, "Ant_odo", "Ver_cha", "Fes_ovi", "Rum_ace", "Des_ces", "Phl_pra", 
                 #"Agr_sto", "Cyn_cri", "Cir_ole", "Cre_bie", "Cer_hol", "Pla_med", "Thy_pul", "Urt_dio",
                 #"Lol_mul", "Cir_arv", "Ran_bul", "Tri_dub", "Lot_cor", "Car_car", "Leo_his", "Vic_sep",
                 #"Med_lup", "Pru_spp")#, "Sym_off")

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

#save
write.table(spp_LUI, file = "data/spp_LUI_26.txt", row.names = FALSE, sep = "\t")

#clean
rm(spp_LUI, top.short)


####
#try
source("code/function_selector.R") #function to select the most common combinations based on a presence threshold
df <- read.table("data/spp_LUI_26.txt", header = TRUE, sep = "\t")[, 4:(n_plants + 3)] #data only with the plants in columns
rm(n_plants)
t <- 0 #above the threshold, the combo is selected

#res2 <- selector(df, n = 2, t)
#res3 <- selector(df, n = 3, t)
#res5 <- selector(df, n = 5, t)
#write.table(res2, "results/res2.txt", row.names = FALSE, sep = "\t")
#write.table(res3, "results/res3.txt", row.names = FALSE, sep = "\t")
#write.table(res5, "results/res5.txt", row.names = FALSE, sep = "\t")
##cluster:
#res7 <- selector(df, n = 7, t)
#res11 <- selector(df, n = 11, t)
#res17 <- selector(df, n = 17, t)

#FOR THE 26 SPECIES COMMUNITY
max_comb <- NULL
rich <- NULL
for (i in 2:25){
  rich <- c(rich, i)
  max_comb <- c(max_comb, length(which(apply(df, 1, sum) > i)) / nrow(df) * 100)
}

saturation <- data.frame("richness" = rich, "ratio" = max_comb)
write.table(saturation, "data/saturation.txt", row.names = FALSE, sep = "\t")

library(ggplot2)
#install.packages("rlang")

(ratio_combos <- ggplot(data = saturation, aes(x = richness, y = ratio)) +
  scale_x_continuous(breaks = c(2, 3, 5, 7, 11, 17, 25)) + ggtitle("Community of 26 species") +
  xlab("Combinations' species richness") + ylab("Presence in the data (%)") +
  geom_line(size = 1.2, color = "grey") + geom_point(size = 2, color = "darkgreen"))
ggsave(plot = ratio_combos, filename = "figures/ratio_combos.png", width = 6, height = 4, dpi = 320)

#select only species which are present at all
df11 <- df[which(apply(df, 1, sum) >= 11), ]
df17 <- df[which(apply(df, 1, sum) >= 17), ]; df17 <- df17[, -which(apply(df17, 2, sum) == 0)]

write.table(df11, "data/df11.txt", row.names = FALSE, sep = "\t")
write.table(df17, "data/df17.txt", row.names = FALSE, sep = "\t")
res17 <- read.table("results/res17.txt", header = TRUE, sep = "\t")





