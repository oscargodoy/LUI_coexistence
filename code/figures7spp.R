coex7 <- read.csv("results/results-LUI_coexistence-7spp.csv")
coex7$feasibility <- as.factor(coex7$feasibility)

#save the position of all NAs in the database
undone_combos_position <- unique(c(which(is.na(coex7$SND)), which(is.na(coex7$SFD)),
                          which(is.na(coex7$overlap)), which(is.na(coex7$differential)),
                          which(is.na(coex7$feasibility)), which(is.na(coex7$pair_to_all)),
                          which(is.na(coex7$LUI))))

#check whether the omitted combos match with the database
(nrow(coex7) - nrow(na.omit(coex7))) == length(undone_combos_position)

#save the combos and LUI value that could not be done
undone_combos <- data.frame("LUI" = coex7$LUI[undone_combos_position],
                            "combo" = coex7$combos[undone_combos_position])

if(length(undone_combos_position > 0)){
  write.csv(undone_combos, "results/undone_combos-7spp.csv", row.names = FALSE)
  #omit NAs from teh database
  coex7 <- na.omit(coex7)
  write.csv(coex7, "results/results-LUI_coexistence-7spp.csv", row.names = FALSE)
}

#load ggplot2
library(ggplot2)

#some plots

#x = SND, y = SFD, color = feasibility
SFD_SND7 <- ggplot(data = coex7, aes(x = SND, y = SFD, color = feasibility)) +
  geom_point(alpha = 0.4, size = 1.5) +
  ggtitle("Combinations of 7 species")

#x = LUI, y = SND
SND7 <- ggplot(data = coex7, aes(x = LUI, y = SND)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 7 species")

#x = LUI, y = SFD
SFD7 <- ggplot(data = coex7, aes(x = LUI, y = SFD)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 7 species")

#x = LUI, y = overlap
over7 <- ggplot(data = coex7, aes(x = LUI, y = overlap)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 7 species")

#x = LUI, y = differential
diff7 <- ggplot(data = coex7, aes(x = LUI, y = differential)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 7 species")

#x = LUI, y = feasibility
feas7 <- ggplot(data = coex7, aes(x = LUI, y = as.integer(feasibility)-1)) +
  geom_point() +
  geom_smooth(alpha = 0, size = 2) +
  ylab("Feasibility") +
  ggtitle("Combinations of 7 species")

ggsave(plot = SFD_SND7, filename = "figures/SFD_SND7.png")
ggsave(plot = SND7, filename = "figures/SND7.png")
ggsave(plot = SFD7, filename = "figures/SFD7.png")
ggsave(plot = over7, filename = "figures/over7.png")
ggsave(plot = diff7, filename = "figures/diff7.png")
ggsave(plot = feas7, filename = "figures/feas7.png")
