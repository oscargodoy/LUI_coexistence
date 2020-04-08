library(tidyverse)
library(ggpubr)

#load data

#2 species
coex2 <- read.csv(file = "results/results-LUI_coexistence-2spp.csv")
coex2$richness <- 2
feas2 <- as.data.frame(rbind(
  cbind(str_split(coex2$combos, pattern = "\\.", n = 2, simplify = TRUE)[, 1],
           coex2$feasibility, coex2$richness, coex2$LUI),
  cbind(str_split(coex2$combos, pattern = "\\.", n = 2, simplify = TRUE)[, 2],
        coex2$feasibility, coex2$richness, coex2$LUI)))
colnames(feas2) <- c("combos", "feasibility", "richness", "LUI")

#3 species
coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3$richness <- 3
feas3 <- as.data.frame(rbind(
  cbind(str_split(coex3$combos, pattern = "\\.", n = 3, simplify = TRUE)[, 1],
        coex3$feasibility, coex3$richness, coex3$LUI),
  cbind(str_split(coex3$combos, pattern = "\\.", n = 3, simplify = TRUE)[, 2],
        coex3$feasibility, coex3$richness, coex3$LUI),
  cbind(str_split(coex3$combos, pattern = "\\.", n = 3, simplify = TRUE)[, 3],
        coex3$feasibility, coex3$richness, coex3$LUI)))
colnames(feas3) <- c("combos", "feasibility", "richness", "LUI")

#5 species
coex5 <- read.csv(file = "results/results-LUI_coexistence-5spp.csv")
coex5$richness <- 5
feas5 <- as.data.frame(rbind(
  cbind(str_split(coex5$combos, pattern = "\\.", n = 5, simplify = TRUE)[, 1],
        coex5$feasibility, coex5$richness, coex5$LUI),
  cbind(str_split(coex5$combos, pattern = "\\.", n = 5, simplify = TRUE)[, 2],
        coex5$feasibility, coex5$richness, coex5$LUI),
  cbind(str_split(coex5$combos, pattern = "\\.", n = 5, simplify = TRUE)[, 3],
        coex5$feasibility, coex5$richness, coex5$LUI),
  cbind(str_split(coex5$combos, pattern = "\\.", n = 5, simplify = TRUE)[, 4],
        coex5$feasibility, coex5$richness, coex5$LUI),
  cbind(str_split(coex5$combos, pattern = "\\.", n = 5, simplify = TRUE)[, 5],
        coex5$feasibility, coex5$richness, coex5$LUI)))
colnames(feas5) <- c("combos", "feasibility", "richness", "LUI")

#7 species
coex7 <- read.csv(file = "results/results-LUI_coexistence-7spp.csv")
coex7$richness <- 7
feas7 <- as.data.frame(rbind(
  cbind(str_split(coex7$combos, pattern = "\\.", n = 7, simplify = TRUE)[, 1],
        coex7$feasibility, coex7$richness, coex7$LUI),
  cbind(str_split(coex7$combos, pattern = "\\.", n = 7, simplify = TRUE)[, 2],
        coex7$feasibility, coex7$richness, coex7$LUI),
  cbind(str_split(coex7$combos, pattern = "\\.", n = 7, simplify = TRUE)[, 3],
        coex7$feasibility, coex7$richness, coex7$LUI),
  cbind(str_split(coex7$combos, pattern = "\\.", n = 7, simplify = TRUE)[, 4],
        coex7$feasibility, coex7$richness, coex7$LUI),
  cbind(str_split(coex7$combos, pattern = "\\.", n = 7, simplify = TRUE)[, 5],
        coex7$feasibility, coex7$richness, coex7$LUI),
  cbind(str_split(coex7$combos, pattern = "\\.", n = 7, simplify = TRUE)[, 6],
        coex7$feasibility, coex7$richness, coex7$LUI),
  cbind(str_split(coex7$combos, pattern = "\\.", n = 7, simplify = TRUE)[, 7],
        coex7$feasibility, coex7$richness, coex7$LUI)))
colnames(feas7) <- c("combos", "feasibility", "richness", "LUI")

#11 species
coex11 <- read.csv(file = "results/results-LUI_coexistence-11spp.csv")
coex11$richness <- 11
feas11 <- as.data.frame(rbind(
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 1],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 2],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 3],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 4],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 5],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 6],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 7],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 8],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 9],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 10],
        coex11$feasibility, coex11$richness, coex11$LUI),
  cbind(str_split(coex11$combos, pattern = "\\.", n = 11, simplify = TRUE)[, 11],
        coex11$feasibility, coex11$richness, coex11$LUI)))
colnames(feas11) <- c("combos", "feasibility", "richness", "LUI")

#remove coex files
rm(coex2, coex3, coex5, coex7, coex11)

#create a unique data.frame
feas <- rbind(feas2, feas3, feas5, feas7, feas11)
feas$feasibility <- as.integer(feas$feasibility) - 1

#remove single feas files
rm(feas2, feas3, feas5, feas7, feas11)


#create a df only with presence 
c <- as.vector(unique(feas$combos))
r <- as.vector(unique(feas$richness))
l <- as.vector(unique(feas$LUI))
feasible <- NULL
for (i in 1:length(c)){
  cdata <- subset(feas, combos == c[i])
  for (j in 1:length(r)){
    rdata <- subset(cdata, richness == r[j])
    for (k in 1:length(l)){
      ldata <- subset(rdata, LUI == l[k])
      feasible <- rbind(feasible, data.frame(
        "species" = c[i],
        "richness" = r[j],
        "LUI" = l[k],
        "feasible" = sum(ldata$feasibility)
      ))
    }
  }
}

for (i in 1:nrow(feasible)){
  if (feasible$feasible[i] > 0){
    feasible$feasible[i] <- 1
  }
}

#clean everything
rm(list=ls()[! ls() %in% c("feas", "feasible")])

spp <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off",
         "Lol_per", "Arr_ela", "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep",
         "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", "Bro_hor", "Pla_lan",
         "Ach_mil", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub",
         "Car_hir", "Bra_pin")
feasible$species <- as.character(feasible$species)
feasible$species <- factor(feasible$species,
                           levels = spp)

#write feasible
write.csv(feasible, "results/feasible_SupFig4.csv", row.names = FALSE)


####################### PLOT

feasible <- read.csv("results/feasible_SupFig4.csv")

#plot our predictions of coexistence
predictions <- ggplot(data = feasible, aes(x = LUI, y = species)) +
  geom_tile(aes(fill = as.factor(feasible))) +
  facet_grid(. ~ richness, labeller = label_both) +
  scale_fill_discrete(name = "Feasible?", labels = c("No", "Yes")) +
  xlab("Land use intensity (LUI)") +
  ylab("Plant species")

ggsave(predictions, filename = "figures/predictions_feasibility_species.png", device = "png",
       width = 18, height = 10)


###
### Modify the raw data.frame to discrete LUI values and Presence/absence plant data
###

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

#remove data from 2016 because there is no LUI data
plants <- plants[plants$Year != 2016, ]

#remove info columns
#format is the same order as LUI, so we'll later merge with LUI to have all info
plant_only <- plants[-c(1:4)]

### top --- select the most common plant species
top <- rev(sort(apply(plant_only, 2, mean, na.rm = TRUE)))[1:n_plants]

#short standard names for the selected plants --- WARNING: CURRENTLY, A MANUAL STEP
top.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off",
               "Lol_per", "Arr_ela", "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep",
               "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", "Bro_hor", "Pla_lan",
               "Ach_mil", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub",
               "Car_hir", "Bra_pin")

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

#write observed
observed$LUI <- as.numeric(as.character(observed$LUI))
write.csv(observed, "results/observed_SupFig4.csv", row.names = FALSE)





###PLOTS

#common to all subplots
size_text <- 10
spp <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off",
         "Lol_per", "Arr_ela", "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep",
         "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", "Bro_hor", "Pla_lan",
         "Ach_mil", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub",
         "Car_hir", "Bra_pin")

#plot of observed species by LUI
observed <- read.csv("results/observed_SupFig4.csv")
observed$species <- factor(observed$species, levels = spp)
levels(observed$species) <- str_replace(levels(observed$species), "_", replacement = "-")

observation_quan <- ggplot(data = observed, aes(x = LUI, y = species)) +
  geom_tile(aes(fill = observed)) +
  scale_fill_continuous(name = "Presence\n (number\n of plots)") +
  scale_x_continuous(name = NULL, expand = expand_scale(0),
                     breaks = as.vector(unique(observed$LUI))) +
  ylab("Plant species\n") +
  ggtitle("Observed") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(size = size_text + 5),
        legend.text = element_text(size = size_text + 5),
        plot.title = element_text(size = size_text + 5),
        axis.text.x = element_text(size = size_text + 5),
        axis.title.y = element_text(size = size_text + 5))# +
  #geom_point(aes(color = as.factor(presence)))


#plot our predictions of coexistence
feasible <- read.csv("results/feasible_SupFig4.csv")
feasible$species <- factor(feasible$species, levels = spp)
levels(feasible$species) <- str_replace(levels(feasible$species), "_", replacement = "-")
colnames(feasible)[2] <- "Richness"

predictions2 <- ggplot(data = subset(feasible, Richness == 2),
                       aes(x = LUI, y = species)) +
  geom_tile(aes(fill = as.factor(feasible))) +
  scale_fill_discrete(name = "Feasible", labels = c("No", "Yes")) +
  scale_x_continuous(name = NULL, expand = expand_scale(0),
                     breaks = as.vector(unique(feasible$LUI))) +
  ylab("Plant species\n") +
  ggtitle("Predicted: combinations of 2 species") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(size = size_text + 5),
        legend.text = element_text(size = size_text + 5),
        plot.title = element_text(size = size_text + 5),
        axis.text.x = element_text(size = size_text + 5),
        axis.title.y = element_text(size = size_text + 5))

predictions3 <- ggplot(data = subset(feasible, Richness == 3),
                       aes(x = LUI, y = species)) +
  geom_tile(aes(fill = as.factor(feasible))) +
  scale_fill_discrete(guide = guide_legend(override.aes = list(fill = "white"))) +
  scale_x_continuous(name = NULL, expand = expand_scale(0),
                     breaks = as.vector(unique(feasible$LUI))) +
  ylab("Plant species\n") +
  ggtitle("Predicted: combinations of 3 species") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(color = "white",
                                    size = size_text),
        legend.text = element_text(color = "white",
                                   size = size_text),
        plot.title = element_text(size = size_text + 5),
        axis.text.x = element_text(size = size_text + 5),
        axis.title.y = element_text(size = size_text + 5))

predictions5 <- ggplot(data = subset(feasible, Richness == 5),
                       aes(x = LUI, y = species)) +
  geom_tile(aes(fill = as.factor(feasible))) +
  scale_fill_discrete(guide = guide_legend(override.aes = list(fill = "white"))) +
  scale_x_continuous(name = NULL, expand = expand_scale(0),
                     breaks = as.vector(unique(feasible$LUI))) +
  ylab("Plant species\n") +
  ggtitle("Predicted: combinations of 5 species") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(color = "white",
                                    size = size_text),
        legend.text = element_text(color = "white",
                                   size = size_text),
        plot.title = element_text(size = size_text + 5),
        axis.text.x = element_text(size = size_text + 5),
        axis.title.y = element_text(size = size_text + 5))

predictions7 <- ggplot(data = subset(feasible, Richness == 7),
                       aes(x = LUI, y = species)) +
  geom_tile(aes(fill = as.factor(feasible))) +
  scale_fill_discrete(guide = guide_legend(override.aes = list(fill = "white"))) +
  scale_x_continuous(name = NULL, expand = expand_scale(0),
                     breaks = as.vector(unique(feasible$LUI))) +
  ylab("Plant species\n") +
  ggtitle("Predicted: combinations of 7 species") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(color = "white",
                                    size = size_text),
        legend.text = element_text(color = "white",
                                   size = size_text),
        plot.title = element_text(size = size_text + 5),
        axis.text.x = element_text(size = size_text + 5),
        axis.title.y = element_text(size = size_text + 5))

predictions11 <- ggplot(data = subset(feasible, Richness == 11),
                        aes(x = LUI, y = species)) +
  geom_tile(aes(fill = as.factor(feasible))) +
  scale_fill_discrete(guide = guide_legend(override.aes = list(fill = "white"))) +
  scale_x_continuous(name = "Land use intensity (LUI)",
                     expand = expand_scale(0),
                     breaks = as.vector(unique(feasible$LUI))) +
  ylab("Plant species\n") +
  ggtitle("Predicted: combinations of 11 species") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(color = "white",
                                    size = size_text),
        legend.text = element_text(color = "white",
                                   size = size_text),
        plot.title = element_text(size = size_text + 5),
        axis.text.x = element_text(size = size_text + 5),
        axis.title.x = element_text(size = size_text + 5),
        axis.title.y = element_text(size = size_text + 5))

#arrange them all
ggarrange(observation_quan,
          predictions2,
          predictions3,
          predictions5,
          predictions7,
          predictions11,
          ncol = 1, nrow = 6,
          align = "hv")

ggsave(filename = "figures/quantitative_obs_pred.png", device = "png",
       width = 10, height = 20, limitsize = FALSE)


#########
######### SupFig4
#########
library(tidyverse)
library(ggpubr)

#common to all subplots
size_text <- 15
spp <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off",
         "Lol_per", "Arr_ela", "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep",
         "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", "Bro_hor", "Pla_lan",
         "Ach_mil", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub",
         "Car_hir", "Bra_pin")

#plot of observed species by LUI
observed <- read.csv("results/observed_SupFig4.csv")
observed$species <- factor(observed$species, levels = spp)
levels(observed$species) <- str_replace(levels(observed$species), "_", replacement = "-")

#plot our predictions of coexistence
feasible <- read.csv("results/feasible_SupFig4.csv")
feasible$species <- factor(feasible$species, levels = spp)
levels(feasible$species) <- str_replace(levels(feasible$species), "_", replacement = "-")
colnames(feasible)[2] <- "Richness"


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
                           "species_number" = tot_observed$observed)

#prepare predicted
tot_predicted <- aggregate(feasible ~ species + LUI, feasible, sum)
for (i in 1:nrow(tot_predicted)){
  if (tot_predicted$feasible[i] > 0){
    tot_predicted$feasible[i] <- 1
  }
}
tot_predicted <- aggregate(feasible ~ LUI, tot_predicted, sum)
tot_predicted <- data.frame("Type" = "Predicted",
                            "LUI" = tot_predicted$LUI,
                            "species_number" = tot_predicted$feasible)

#bind them all
total <- rbind(tot_observed, tot_predicted)
write.csv(total, "results/observed_and_predicted.csv", row.names = FALSE)


#plotA
total <- read.csv("results/observed_and_predicted.csv")


#plot species number vs LUI by observed or predicted
library(ggplot2)
(obs_pred <- ggplot(data = total, aes(x = LUI, y = species_number, group = Type)) +
    geom_line(aes(linetype = Type, color = Type), size = 1.5) +
    geom_point(aes(color = Type), size = 3) +
    scale_linetype_manual(values=c("solid", "dotted")) +
    scale_color_manual(values=c('#999999', "darkgreen")) +
    scale_x_continuous(name = "Land use intensity (LUI)",
                       breaks = as.vector(unique(total$LUI))) +
    scale_y_continuous(name = "Number of species",
                       breaks = seq(0, 26, 1)) +
    theme(legend.position = "right",
          text = element_text(size = size_text)) +
    
)
ggsave(obs_pred, filename = "figures/observed_predicted.png", device = "png",
       width = 6, height = 4, limitsize = FALSE)


#plot our predictions of coexistence
(predictions <- ggplot(data = feasible, aes(x = LUI, y = species)) +
    geom_tile(aes(fill = as.factor(feasible))) +
    facet_grid(. ~ Richness, labeller = label_both) + 
    scale_fill_discrete(name = "Feasible", labels = c("No", "Yes")) +
    scale_x_continuous(name = "Land use intensity (LUI)",
                       expand = expand_scale(0),
                       breaks = as.vector(unique(feasible$LUI))) +
    ylab("Plant species") +
    ggtitle(" ") +
    theme(text = element_text(size = size_text),
          axis.text.x = element_text(angle = 90))
)

blank <- ggplot(data = feasible, aes(x = LUI, y = species)) +
  theme_void()

#mix them
a <- ggarrange(blank, obs_pred, blank, nrow = 1, ncol = 3, widths = c(0.75, 3, 0.6))
ggarrange(a, predictions, ncol = 1, nrow = 2, heights = c(1.5, 2))
ggsave(filename = "figures/paper_figures/SupFig4.png", device = "png",
              width = 15, height = 10, limitsize = FALSE)





