#load random results (takes time)
random <- read.csv("https://www.dropbox.com/s/3m94ec4kc5bq1uo/random_results_LUI.csv?dl=1")
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
random3 <- subset(random, richness == 3)

#
SND2 <- aggregate(SND ~ species + LUI, data = random2, mean)
SFD2 <- aggregate(SFD ~ species + LUI, data = random2, mean)
SND2$SFD <- SFD2$SFD; random2 <- SND2

SND3 <- aggregate(SND ~ species + LUI, data = random3, mean)
SFD3 <- aggregate(SFD ~ species + LUI, data = random3, mean)
SND3$SFD <- SFD3$SFD; random3 <- SND3

#
rm(list=ls()[! ls() %in% c("random2", "random3")])

random2$Type <- "Random"
random3$Type <- "Random"

#load coexistence 2 species results
coex2 <- read.csv(file = "results/results-LUI_coexistence-2spp.csv")
coex2 <- data.frame("species" = coex2$combos,
                    "LUI" = coex2$LUI,
                    "SND" = coex2$SND,
                    "SFD" = coex2$SFD,
                    "Type" = "Predicted")

coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3 <- data.frame("species" = coex3$combos,
                    "LUI" = coex3$LUI,
                    "SND" = coex3$SND,
                    "SFD" = coex3$SFD,
                    "Type" = "Predicted")

ridges2 <- rbind(coex2, random2)
ridges3 <- rbind(coex3, random3)

rm(list=ls()[! ls() %in% c("ridges2", "ridges3")])

write.csv(ridges2, "results/ridges2.csv", row.names = FALSE)
write.csv(ridges3, "results/ridges3.csv", row.names = FALSE)


#ridges
ridges2 <- read.csv("results/ridges2.csv")
ridges3 <- read.csv("results/ridges3.csv")

ridges2$Type <- factor(ridges2$Type, levels(ridges2$Type)[c(2, 1)])
ridges3$Type <- factor(ridges3$Type, levels(ridges3$Type)[c(2, 1)])
ridges2$LUI <- as.factor(as.character(ridges2$LUI))
levels(ridges2$LUI) <- c("0.50", "0.75", "1.00", "1.25", "1.50", "1.75", "2.00", "2.25", "2.50", "2.75", "3.00")
ridges3$LUI <- as.factor(as.character(ridges3$LUI))
levels(ridges3$LUI) <- c("0.50", "0.75", "1.00", "1.25", "1.50", "1.75", "2.00", "2.25", "2.50", "2.75", "3.00")

library(ggplot2)
library(ggpubr)
library(ggridges)

#common features for all figures
size_text <- 18 #text size
size_points <- 1
size_figure <- c(12, 16) #width and height for saving the final figure
fig_color <- c("deepskyblue2", "gray50")

#structural niche differences 2 species
randomSND2 <- ggplot(data = ridges2, aes(y = LUI)) +
  geom_density_ridges(aes(x = SND,
                          point_color = Type,
                          point_fill = Type,
                          fill = Type),
                      scale = 0.7,
                      alpha = 0.25,
                      size = 0.25,
                      jittered_points = TRUE,
                      point_size = 0.85) +
    scale_fill_manual(values = fig_color) +
    scale_discrete_manual("point_color", values = fig_color) +
    scale_discrete_manual("point_fill", values = fig_color) +
    scale_x_continuous(expand = expand_scale(0.1),
                       breaks = seq(0,
                                    round(max(c(ridges2$SND, ridges3$SND)), 1),
                                    by = 0.5),
                       limits = c(min(c(ridges2$SND, ridges3$SND)),
                                  max(c(ridges2$SND, ridges3$SND)))) +
    xlab(" ") +
    ylab("Land use intensity (LUI)") +
    ggtitle("2 species combinations") +
    theme(text = element_text(size = size_text))


#structural niche differences 3 species
randomSND3 <- ggplot(data = ridges3, aes(y = LUI)) +
  geom_density_ridges(aes(x = SND,
                          point_color = Type,
                          point_fill = Type,
                          fill = Type),
                      scale = 0.7,
                      alpha = 0.25,
                      size = 0.25,
                      jittered_points = TRUE, 
                      point_size = 0.85) +
  scale_fill_manual(values = fig_color) +
  scale_discrete_manual("point_color", values = fig_color) +
  scale_discrete_manual("point_fill", values = fig_color) +
  scale_x_continuous(expand = expand_scale(0.1),
                     breaks = seq(0,
                                  round(max(c(ridges2$SND, ridges3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(ridges2$SND, ridges3$SND)),
                                max(c(ridges2$SND, ridges3$SND)))) +
  xlab("Structural niche differences (SND)") +
  ylab("Land use intensity (LUI)") +
  ggtitle("3 species combinations") +
  theme(text = element_text(size = size_text))


#structural fitness differences 2 species
randomSFD2 <- ggplot(data = ridges2, aes(y = LUI)) +
  geom_density_ridges(aes(x = SFD,
                          point_color = Type,
                          point_fill = Type,
                          fill = Type),
                      scale = 0.7,
                      alpha = 0.25,
                      size = 0.25,
                      jittered_points = TRUE, 
                      point_size = 0.85) +
  scale_fill_manual(values = fig_color) +
  scale_discrete_manual("point_color", values = fig_color) +
  scale_discrete_manual("point_fill", values = fig_color) +
  scale_x_continuous(expand = expand_scale(0.1),
                     breaks = seq(0,
                                  round((max(c(ridges2$SFD, ridges3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(ridges2$SFD, ridges3$SFD)),
                                max(c(ridges2$SFD, ridges3$SFD)))) +
  xlab(" ") +
  ylab("Land use intensity (LUI)") +
  ggtitle(" ") +
  theme(text = element_text(size = size_text))

#structural niche differences 3 species
randomSFD3 <- ggplot(data = ridges3, aes(y = LUI)) +
  geom_density_ridges(aes(x = SFD,
                          point_color = Type,
                          point_fill = Type,
                          fill = Type),
                      scale = 0.7,
                      alpha = 0.25,
                      size = 0.25,
                      jittered_points = TRUE, 
                      point_size = 0.85) +
  scale_fill_manual(values = fig_color) +
  scale_discrete_manual("point_color", values = fig_color) +
  scale_discrete_manual("point_fill", values = fig_color) +
  scale_x_continuous(expand = expand_scale(0.1),
                     breaks = seq(0,
                                  round((max(c(ridges2$SFD, ridges3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(ridges2$SFD, ridges3$SFD)),
                                max(c(ridges2$SFD, ridges3$SFD)))) +
  scale_y_discrete(expand = expand_scale(0.075)) +
  xlab("Structural fitness differences (SFD)") +
  ylab("Land use intensity (LUI)") +
  ggtitle(" ") +
  theme(text = element_text(size = size_text))


#put them together
ggarrange(randomSND2, randomSFD2,
          randomSND3, randomSFD3,
          ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = "right",
          align = "hv",
          labels = "AUTO",
          font.label = list(size = size_text + 5),
          hjust = -1,
          vjust = 2.25)

#save!
ggsave("figures/paper_figures/SupFig3.png", device = "png",
       dpi = 320, width = size_figure[1], height = size_figure[2])
