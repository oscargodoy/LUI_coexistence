#Species diversity by LUI value

rm(list = ls())


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

#load plant data
plants <- read.csv("data/BE.plants08.16.csv", header = TRUE)

#change to presence/absence data (0 or 1)
plants_div <- plants
for (i in 1:nrow(plants)){
  for (j in 5:ncol(plants)){
    if(is.na(plants[i, j])){
      plants[i, j] <- 0
    } else {
      if (plants[i, j] > 0){
        plants_div[i, j] <- 1
      }
    }
  }
}

#number of species per plot
rich <- apply(plants_div[, -c(1:5)], 1, sum, na.rm = TRUE)
richness <- data.frame("Year" = plants_div$Year, "Plot" = plants_div$Useful_EP_PlotID, "div" = rich)

#add the column with plant richness
lui_total <- cbind(lui_total, "Richness" = richness$div[richness$Year != 2016])
lui_total$Year <- as.factor(lui_total$Year)

library(ggplot2)
ggplot(data = lui_total, aes(x = LUI, y = Richness)) +
  geom_smooth(alpha = 0.25, size = 1.1, color = "black") + geom_point(alpha = 0.1)

(lui_rich <- ggplot(data = lui_total, aes(x = LUI, y = Richness, colour = Year)) +
  geom_smooth(alpha = 0, size = 1.1) + geom_point(alpha = 0.2) +
  scale_x_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.5)) +
  ylab("Species richness"))

#LUI values to be selected?
(box <- ggplot(data = lui_total, aes(x = Year, y = LUI)) + geom_boxplot(aes(fill = Year), alpha = 0.75) +
  theme(legend.position = "none") + geom_abline(intercept = 3, slope = 0, linetype = "dashed") +
  geom_abline(intercept = 0.5, slope = 0, linetype = "dashed") +
  scale_x_discrete(limits = rev(levels(lui_total$year))) +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(0, 4.5, by = 0.5)) + coord_flip())

library(ggpubr)

#Chart LUI--DIVERSITY
ggarrange(box, lui_rich, align = "h", widths = c(1, 2))
ggsave("figures/LUI_richness.png", width = 10, height = 5, dpi = 320)

