#load coexistence 2 species results
coex2 <- read.csv(file = "results/results-LUI_coexistence-2spp.csv")
coex2$feasibility <- as.factor(coex2$feasibility)

coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)

#ridges
library(ggplot)
library(ggpubr)
library(ggridges)

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
  scale_point_color_hue(name = "Feasibility",
                        breaks = c("0", "1"),
                        labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expand_scale(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(coex2$SND, coex3$SND)),
                                max(c(coex2$SND, coex3$SND)))) +
  xlab(" ") +
  ylab("Land use intensity (LUI)") +
  ggtitle("2 species combinations") +
  theme(text = element_text(size = size_text))


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
  scale_point_color_hue(name = "Feasibility",
                        breaks = c("0", "1"),
                        labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expand_scale(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(coex2$SND, coex3$SND)),
                                max(c(coex2$SND, coex3$SND)))) +
  xlab("Structural niche differences (SND)") +
  ylab("Land use intensity (LUI)") +
  ggtitle("3 species combinations") +
  theme(text = element_text(size = size_text))


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
  scale_point_color_hue(name = "Feasibility",
                        breaks = c("0", "1"),
                        labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expand_scale(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(coex2$SFD, coex3$SFD)))) +
  xlab(" ") +
  ylab(" ") +
  ggtitle(" ") +
  theme(text = element_text(size = size_text))


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
  scale_point_color_hue(name = "Feasibility",
                        breaks = c("0", "1"),
                        labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expand_scale(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(coex2$SFD, coex3$SFD)))) +
  xlab("Structural fitness differences (SFD)") +
  ylab(" ") +
  ggtitle(" ") +
  theme(text = element_text(size = size_text))

#put them together
ggarrange(SND2, SFD2, SND3, SFD3,
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right",
                  align = "hv")

ggsave("figures/paper_figures/Fig1.png", device = "png",
       dpi = 320, width = size_figure[1], height = size_figure[2])
ggsave("figures/paper_figures/Fig1.pdf", device = "pdf",
       dpi = 320, width = size_figure[1], height = size_figure[2])
