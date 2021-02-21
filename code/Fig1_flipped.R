#load coexistence 2 species results
coex2 <- read.csv(file = "results/results-LUI_coexistence-2spp.csv")
coex2$feasibility <- as.factor(coex2$feasibility)

coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)

#ridges
library(ggplot2)
library(ggpubr)
library(ggridges)
library(png)
library(cowplot)
library(magick)

#common features for all figures
size_text <- 18 #text size
size_points <- 1
size_figure <- c(12, 16) #width and height for saving the final figure

#structural niche differences 2 species
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
                        breaks = c("0", "1"),
                        labels = c("No", "Yes")) +
  stat_density_ridges(aes(group = LUI, point_color = feasibility),
                      scale = 0.9,
                      alpha = 0,
                      quantile_lines = TRUE,
                      jittered_points = FALSE,
                      position = "points_sina",
                      show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(coex2$SND, coex3$SND)),
                                max(c(3, 3)))) +
  xlab("Structural niche differences") +
  ylab(" ") +
  ggtitle("2 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND2

#load quantile regression image
img <- load.image("figures/quantile.reg.niche.2.png")
SND2 <- ggdraw(SND2) +
  draw_image(img, x=.18, y=.24, scale=0.43)
SND2


#structural niche differences 3 species
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
  stat_density_ridges(aes(group = LUI, point_color = feasibility),
                      scale = 0.9,
                      alpha = 0,
                      quantile_lines = TRUE,
                      jittered_points = FALSE,
                      position = "points_sina",
                      show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(coex2$SND, coex3$SND)),
                                max(c(3, 3)))) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("3 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND3

#load quantile regression image
img <- load.image("figures/quantile.reg.niche.3.png")
SND3 <- ggdraw(SND3) +
  draw_image(img, x=.18, y=.24, scale=0.35)
SND3


#structural fitness differences 2 species
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
  stat_density_ridges(aes(group = LUI, point_color = feasibility),
                      scale = 0.9,
                      alpha = 0,
                      quantile_lines = TRUE,
                      jittered_points = FALSE,
                      position = "points_sina",
                      show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(120,120)))) +
  ylab("Land use intensity (LUI)") +
  xlab("Structural fitness differences") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD2

#load quantile regression image
img <- load.image("figures/quantile.reg.fitness.2.png")
SFD2 <- ggdraw(SFD2) +
  draw_image(img, x=.18, y=.24, scale=0.35)
SFD2


#structural fitness differences 3 species
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
  stat_density_ridges(aes(group = LUI, point_color = feasibility),
                      scale = 0.9,
                      alpha = 0,
                      quantile_lines = TRUE,
                      jittered_points = FALSE,
                      position = "points_sina",
                      show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(120,120)))) +
  xlab(" ") +
  ylab("Land use intensity (LUI)") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD3

#load quantile regression image
img <- load.image("figures/quantile.reg.fitness.3.png")
SFD3 <- ggdraw(SFD3) +
  draw_image(img, x=.18, y=.24, scale=0.35)
SFD3

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

ggsave("figures/Fig1_flipped.png", device = "png",
       dpi = 320, width = size_figure[2], height = size_figure[1])

#VIOLIN with BOXPLOT
#ggplot(data = coex3, aes(x = LUI, y = SFD, group = LUI)) +
#  geom_violin(fill = "grey") +
#  geom_boxplot(aes(group = LUI), width = 0.05, outlier.colour = NA) +
#  stat_summary(fun = mean, geom = "point", size = 2, color = "blue") +
#  xlab("Land use intensity") +
#  ylab("Structural fitness differences") +
#  scale_x_continuous(breaks = seq(0.5, 3, 0.25)) +
#  scale_y_continuous(expand = expansion(0.1),
#                     breaks = seq(0,
#                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
#                                  by = 20),
#                     limits = c(min(c(coex2$SFD, coex3$SFD)),
#                                max(c(coex2$SFD, coex3$SFD))))
#ggsave("figures/violin_SFD3.png", device = "png",
#       dpi = 320, width = 8, height = 6)
