#common features for all figures
size_text <- 18 #text size
size_figure <- c(8, 12) #width and height for saving the final figure
color.blind <- c("#E69F00", "#56B4E9", "#73c095", "#f95c8a") #palette; additional: CC79A7 #009E73


#load data
coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)
coex3$richness <- 3

coex5 <- read.csv(file = "results/results-LUI_coexistence-5spp.csv")
coex5$feasibility <- as.factor(coex5$feasibility)
coex5$richness <- 5

coex7 <- read.csv(file = "results/results-LUI_coexistence-7spp.csv")
coex7$feasibility <- as.factor(coex7$feasibility)
coex7$richness <- 7

coex11 <- read.csv(file = "results/results-LUI_coexistence-11spp.csv")
coex11$feasibility <- as.factor(coex11$feasibility)
coex11$richness <- 11

#put them together
coex <- rbind(coex3, coex5, coex7, coex11)
coex$richness <- as.factor(coex$richness)


#libraries
library(ggplot2)
library(ggpubr)


#differential
diff <- ggplot(data = coex, aes(x = LUI, y = differential, colour = richness)) +
  geom_smooth(alpha = 0, size = 1.5) +
  coord_cartesian(ylim = c(-0.05, 0.2), expand = TRUE) +
  scale_y_continuous(breaks = seq(-0.05, 0.2, 0.05)) +
  scale_x_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_colour_manual(name = "Species combinations' richness:",
                      values = color.blind) +
  xlab(" ") +
  ylab("Community-pair differential") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(size = size_text),
        legend.text = element_text(size = size_text))

#overlap
over <- ggplot(data = coex, aes(x = LUI, y = overlap, colour = richness)) +
  geom_smooth(alpha = 0, size = 1.5) +
  coord_cartesian(ylim = c(0, 1), expand = TRUE) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_colour_manual(name = "Species combinations' richness:",
                      values = color.blind) +
  xlab("Land use intensity (LUI)") +
  ylab("Community-pair overlap") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(size = size_text),
        legend.text = element_text(size = size_text))

#arrange
ggarrange(diff, over, ncol = 1, nrow = 2, align = "hv",
          common.legend = TRUE, legend = "top",
          labels = "AUTO",
          font.label = list(size = size_text + 5),
          hjust = -1,
          vjust = 0)

#save
ggsave("figures/paper_figures/Fig2.png", device = "png",
       dpi = 320, width = size_figure[1], height = size_figure[2])
ggsave("figures/paper_figures/Fig2.pdf", device = "pdf",
       dpi = 320, width = size_figure[1], height = size_figure[2])

