#common features for all figures

size_text <- 18 #text size
size_figure <- c(8, 12) #width and height for saving the final figure
color.blind <- c("#E69F00", "#56B4E9", "#73c095", "#f95c8a") #palette; additional: CC79A7 #009E73

#load data
coex3 <- read.csv(file = "submission_PNAS/results/results-LUI_coexistence-3spp_overlap.csv")
coex3$feasibility <- as.factor(coex3$feasibility)
coex3$richness <- 3

coex5 <- read.csv(file = "submission_PNAS/results/results-LUI_coexistence-5spp_overlap.csv")
coex5$feasibility <- as.factor(coex5$feasibility)
coex5$richness <- 5

coex7 <- read.csv(file = "submission_PNAS/results/results-LUI_coexistence-7spp_overlap.csv")
coex7$feasibility <- as.factor(coex7$feasibility)
coex7$richness <- 7

coex11 <- read.csv(file = "submission_PNAS/results/results-LUI_coexistence-11spp_overlap.csv")
coex11$feasibility <- as.factor(coex11$feasibility)
coex11$richness <- 11

#put them together
coex <- rbind(coex3, coex5, coex7, coex11)
rm(coex3, coex5, coex7, coex11)
coex$richness <- as.factor(coex$richness)


#libraries
library(ggplot2)
library(ggpubr)
library(grid)
library(ggtext)


#differential
diff <- ggplot(data = coex, aes(x = LUI, y = differential, colour = richness)) +
  geom_smooth(alpha = 0, size = 1.5) +
  coord_cartesian(ylim = c(-0.07, 0.2), xlim = c(0.5, 3.18)) +
  scale_y_continuous(breaks = seq(-0.05, 0.2, 0.05)) +
  scale_x_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_colour_manual(name = "Number of species in combination",
                      values = color.blind) +
  xlab(" ") +
  ylab("Community-pair differential") +
  theme(text = element_text(size = size_text),
        legend.title = element_text(size = size_text),
        legend.position="top",
        legend.text = element_text(size = size_text))+
  geom_segment(aes(x=3.10, xend = 3.10 , y=0.003, yend = 0.20), size=0.9,
               color="black",arrow = arrow(length = unit(0.4,"cm")))+
  geom_segment(aes(x=3.10, xend = 3.10 , y=-0.003, yend = -0.07), size=0.9,
               color="black",arrow = arrow(length = unit(0.4,"cm")))+
  annotate("text", x=3.04, y=-0.035, angle = 90, size=4, label= "Decreases") + 
  annotate("text", x = 3.04, y=0.10, angle = 90, size=4, label = "Increases") +
  annotate("richtext", x = 3.22, y=0.065, angle = 90, size=4, fill="grey85" ,label ="Effects of indirect interactions on coexistence  region"); #diff 


#overlap
over <- ggplot(data = coex, aes(x = LUI, y = overlap, colour = richness)) +
  geom_smooth(alpha = 0, size = 1.5) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_colour_manual(name = "Number of species in combination",
                      values = color.blind) +
  xlab("Land use intensity (LUI)") +
  ylab("Community-pair overlap") +
  geom_segment(aes(x=3.05, xend = 3.05 , y=0, yend = 1), size=0.9,
               color="black",arrow = arrow(length = unit(0.4,"cm"))) +
  theme(text = element_text(size = size_text),
        legend.title = element_text(size = size_text),
        legend.position="top",
        legend.text = element_text(size = size_text)) +
  annotate("richtext", x = 3.18, y=0.50, angle = 90, size=4, fill="grey85" ,label = "Relative importance of indirect interactions for  coexistence") +
  coord_cartesian(ylim = c(0, 1), xlim= c(0.5,3.15), clip="off");#over

#arrange
ggarrange(diff, over, ncol = 1, nrow = 2, align = "hv",
          common.legend = TRUE, legend = "top",
          labels = "AUTO",
          font.label = list(size = size_text + 5),
          hjust = -1,
          vjust = 0)

#save
ggsave("submission_PNAS/figures/Fig3_main.png", device = "png",
       dpi = 320, width = size_figure[1], height = size_figure[2])
