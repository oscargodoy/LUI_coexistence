coex5 <- read.csv("results/results-LUI_coexistence-5spp.csv")
coex5$feasibility <- as.factor(coex5$feasibility)

#save the position of all NAs in the database
undone_combos_position <- unique(c(which(is.na(coex5$SND)), which(is.na(coex5$SFD)),
                          which(is.na(coex5$overlap)), which(is.na(coex5$differential)),
                          which(is.na(coex5$feasibility)), which(is.na(coex5$pair_to_all)),
                          which(is.na(coex5$LUI))))

#check whether the omitted combos match with the database
(nrow(coex5) - nrow(na.omit(coex5))) == length(undone_combos_position)

#save the combos and LUI value that could not be done
undone_combos <- data.frame("LUI" = coex5$LUI[undone_combos_position],
                            "combo" = coex5$combos[undone_combos_position])

if(length(undone_combos_position > 0)){
  write.csv(undone_combos, "results/undone_combos-5spp.csv", row.names = FALSE)
  #omit NAs from teh database
  coex5 <- na.omit(coex5)
  write.csv(coex5, "results/results-LUI_coexistence-5spp.csv", row.names = FALSE)
}

#load ggplot2
library(ggplot2)

#some plots

#x = SND, y = SFD, color = feasibility
SFD_SND5 <- ggplot(data = coex5, aes(x = SND, y = SFD, color = feasibility)) +
  geom_point(alpha = 0.4, size = 1.5) +
  ggtitle("Combinations of 5 species")

#x = LUI, y = SND
SND5 <- ggplot(data = coex5, aes(x = LUI, y = SND)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 5 species")

#x = LUI, y = SFD
SFD5 <- ggplot(data = coex5, aes(x = LUI, y = SFD)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 5 species")

#x = LUI, y = overlap
over5 <- ggplot(data = coex5, aes(x = LUI, y = overlap)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 5 species")

#x = LUI, y = differential
diff5 <- ggplot(data = coex5, aes(x = LUI, y = differential)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 5 species")

#x = LUI, y = feasibility
feas5 <- ggplot(data = coex5, aes(x = LUI, y = as.integer(feasibility)-1)) +
  geom_point() +
  geom_smooth(alpha = 0, size = 2) +
  ylab("Feasibility") +
  ggtitle("Combinations of 5 species")

ggsave(plot = SFD_SND5, filename = "figures/SFD_SND5.png")
ggsave(plot = SND5, filename = "figures/SND5.png")
ggsave(plot = SFD5, filename = "figures/SFD5.png")
ggsave(plot = over5, filename = "figures/over5.png")
ggsave(plot = diff5, filename = "figures/diff5.png")
ggsave(plot = feas5, filename = "figures/feas5.png")
