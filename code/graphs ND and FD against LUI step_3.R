###Step 3 Plotting structural niche and fitness differences against LUI and performing quantile regressions 
rm(list = ls())

#mean trend ----

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
library(quantreg)


#common features for all figures
size_text <- 18 #text size
size_points <- 1
size_figure <- c(12, 16) #width and height for saving the final figure


#structural niche differences 2 species----
#coex2
eq <- SND ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))


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
              breaks = c("1", "0"),
              labels = c("Yes", "No")) +
  
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="black", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(0, 0)),
                                max(c(1.5, 1.5)))) +
  xlab("Structural niche differences") +
  ylab(" ") +
  ggtitle("2 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND2

#structural niche differences 3 species----
#quantile

#coex3
eq <- SND ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))

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
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="black", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(coex2$SND, coex3$SND)),
                                max(c(2.5, 2.5)))) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("3 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND3



#structural fitness differences 2 species ----
#coex2
eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.15)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))

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
        breaks = c("1", "0"),
        labels = c("Yes", "No")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="black", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(200,200)))) +
  ylab("Land use intensity (LUI)") +
  xlab("Structural fitness differences") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD2

#structural fitness differences 3 species----
#coex3
eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))


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
          breaks = c("1", "0"),
          labels = c("Yes", "No")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="black", size=1.5, ) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(200,200)))) +
  xlab(" ") +
  ylab("Land use intensity (LUI)") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD3

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

ggsave("figures/Fig2_main.png", device = "png",
       dpi = 320, width = size_figure[2], height = size_figure[1])


#lower bounds ----

#load coexistence 2 species results
coex2 <- read.csv(file = "results/results_LUI_lower_2spp.csv")
coex2$feasibility <- as.factor(coex2$feasibility)

coex3 <- read.csv(file = "results/results_LUI_lower_3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)

#ridges
library(ggplot2)
library(ggpubr)
library(ggridges)
library(png)
library(quantreg)


#common features for all figures
size_text <- 18 #text size
size_points <- 1
size_figure <- c(12, 16) #width and height for saving the final figure


#structural niche differences 2 species----
#coex2
eq <- SND ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))


SND2 <- ggplot(data = coex2, aes(x = SND, y = LUI)) +
  stat_density_ridges(aes(group = LUI), #point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  #scale_point_color_hue(name = "Feasibility",
                        #breaks = c("0", "1"),
                        #labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="red", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(0, 0)),
                                max(c(1.5, 1.5)))) +
  xlab("Structural niche differences") +
  ylab(" ") +
  ggtitle("2 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND2

#structural niche differences 3 species----
#quantile

#coex3
eq <- SND ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))

SND3 <- ggplot(data = coex3, aes(x = SND, y = LUI)) +
  stat_density_ridges(aes(group = LUI), #point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  #scale_point_color_hue(name = "Feasibility",
                        #breaks = c("0", "1"),
                        #labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="red", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(coex2$SND, coex3$SND)),
                                max(c(2.5, 2.5)))) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("3 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND3



#structural fitness differences 2 species ----
#coex2
eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))

SFD2 <- ggplot(data = coex2, aes(x = SFD, y = LUI)) +
  stat_density_ridges(aes(group = LUI), #point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  #scale_point_color_hue(name = "Feasibility",
                        #breaks = c("0", "1"),
                        #labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="red", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(200,200)))) +
  ylab("Land use intensity (LUI)") +
  xlab("Structural fitness differences") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD2

#structural fitness differences 3 species----
#coex3
eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))


SFD3 <- ggplot(data = coex3, aes(x = SFD, y = LUI)) +
  stat_density_ridges(aes(group = LUI), #point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  #scale_point_color_hue(name = "Feasibility",
                        #breaks = c("0", "1"),
                        #labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="red", size=1.5, ) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(200,200)))) +
  xlab(" ") +
  ylab("Land use intensity (LUI)") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD3

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

ggsave("figures/FigS4_lower.png", device = "png",
       dpi = 320, width = size_figure[2], height = size_figure[1])

#upper bounds ----

#load coexistence 2 species results
coex2 <- read.csv(file = "results/results_LUI_upper_2spp.csv")
coex2$feasibility <- as.factor(coex2$feasibility)

coex3 <- read.csv(file = "results/results_LUI_upper_3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)


#common features for all figures
size_text <- 18 #text size
size_points <- 1
size_figure <- c(12, 16) #width and height for saving the final figure


#structural niche differences 2 species----
#coex2
eq <- SND ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))


SND2 <- ggplot(data = coex2, aes(x = SND, y = LUI)) +
  stat_density_ridges(aes(group = LUI), #point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  #scale_point_color_hue(name = "Feasibility",
  #breaks = c("0", "1"),
  #labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="red", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(0, 0)),
                                max(c(2.3, 2.3)))) +
  xlab("Structural niche differences") +
  ylab(" ") +
  ggtitle("2 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND2

#structural niche differences 3 species----
#quantile

#coex3
eq <- SND ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SND <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SND <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SND <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SND <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SND <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SND <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SND <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SND <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SND <- predict(nlrq9,  newdata = predict_range))

SND3 <- ggplot(data = coex3, aes(x = SND, y = LUI)) +
  stat_density_ridges(aes(group = LUI), #point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  #scale_point_color_hue(name = "Feasibility",
  #breaks = c("0", "1"),
  #labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="red", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round(max(c(coex2$SND, coex3$SND)), 1),
                                  by = 0.5),
                     limits = c(min(c(coex2$SND, coex3$SND)),
                                max(c(4.2, 4.2)))) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("3 species combinations") +
  coord_flip() +
  theme(text = element_text(size = size_text),
        plot.title = element_text(hjust = 0.5)); SND3



#structural fitness differences 2 species ----
#coex2
eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex2, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex2, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))

SFD2 <- ggplot(data = coex2, aes(x = SFD, y = LUI)) +
  stat_density_ridges(aes(group = LUI), #point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  #scale_point_color_hue(name = "Feasibility",
  #breaks = c("0", "1"),
  #labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="red", size=1.5) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(200,200)))) +
  ylab("Land use intensity (LUI)") +
  xlab("Structural fitness differences") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD2

#structural fitness differences 3 species----
#coex3
eq <- SFD ~ a * LUI^2 + b * LUI  + c

nlrq1 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.1)
summary(nlrq1)
nlrq2 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.2)
summary(nlrq2)
nlrq3 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.3)
summary(nlrq3)
nlrq4 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.4)
summary(nlrq4)
nlrq5 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.5)
summary(nlrq5)
nlrq6 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.6)
summary(nlrq6)
nlrq7 <- nlrq(eq, data=coex3, start = list(a = 3, b = 5, c = 5), tau = 0.7)
summary(nlrq7)
nlrq8 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.8)
summary(nlrq8)
nlrq9 <- nlrq(eq, data=coex3, start = list(a = 2, b = 2, c = 5), tau = 0.9)
summary(nlrq9)

# the usual calculation of the corresponding line

predict_range <- data.frame(LUI = seq(0.5, 3.0, length = 100))
l1 <- within(predict_range,  SFD <- predict(nlrq1, newdata = predict_range))
l2 <- within(predict_range,SFD <- predict(nlrq2,  newdata = predict_range))
l3 <- within(predict_range,  SFD <- predict(nlrq3, newdata = predict_range))
l4 <- within(predict_range,SFD <- predict(nlrq4,  newdata = predict_range))
l5 <- within(predict_range,  SFD <- predict(nlrq5, newdata = predict_range))
l6 <- within(predict_range,SFD <- predict(nlrq6,  newdata = predict_range))
l7 <- within(predict_range,  SFD <- predict(nlrq7, newdata = predict_range))
l8 <- within(predict_range,SFD <- predict(nlrq8,  newdata = predict_range))
l9 <- within(predict_range,SFD <- predict(nlrq9,  newdata = predict_range))


SFD3 <- ggplot(data = coex3, aes(x = SFD, y = LUI)) +
  stat_density_ridges(aes(group = LUI), #point_color = feasibility),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = FALSE,
                      jittered_points = TRUE,
                      point_size = size_points,
                      position = "points_sina",
                      show.legend = TRUE) +
  #scale_point_color_hue(name = "Feasibility",
  #breaks = c("0", "1"),
  #labels = c("No", "Yes")) +
  ###
  #quantiles
  geom_line(data=l1,linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l2, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l3, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l4, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l5, orientation = "y", color="red", size=1.5, ) + # this is because it is the median.
  geom_line(data=l6, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l7, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l8, linetype = "longdash", orientation = "y", size=0.7) +
  geom_line(data=l9, linetype = "longdash", orientation = "y", size=0.7) +
  ###
  scale_y_continuous(breaks = seq(0.5, 3, 0.25)) +
  scale_x_continuous(expand = expansion(0.1),
                     breaks = seq(0,
                                  round((max(c(coex2$SFD, coex3$SFD))/10))*10,
                                  by = 20),
                     limits = c(min(c(coex2$SFD, coex3$SFD)),
                                max(c(200,200)))) +
  xlab(" ") +
  ylab("Land use intensity (LUI)") +
  ggtitle(" ") +
  coord_flip() +
  theme(text = element_text(size = size_text)); SFD3

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

ggsave("figures/FigS4_upper.png", device = "png",
       dpi = 320, width = size_figure[2], height = size_figure[1])


