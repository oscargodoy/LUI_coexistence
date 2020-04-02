coex11 <- read.csv("results/results-LUI_coexistence-11spp.csv")
coex11$feasibility <- as.factor(coex11$feasibility)

#save the position of all NAs in the database
undone_combos_position <- unique(c(which(is.na(coex11$SND)), which(is.na(coex11$SFD)),
                          which(is.na(coex11$overlap)), which(is.na(coex11$differential)),
                          which(is.na(coex11$feasibility)), which(is.na(coex11$pair_to_all)),
                          which(is.na(coex11$LUI))))

#check whether the omitted combos match with the database
(nrow(coex11) - nrow(na.omit(coex11))) == length(undone_combos_position)

#save the combos and LUI value that could not be done
undone_combos <- data.frame("LUI" = coex11$LUI[undone_combos_position],
                            "combo" = coex11$combos[undone_combos_position])

if(length(undone_combos_position > 0)){
  write.csv(undone_combos, "results/undone_combos-11spp.csv", row.names = FALSE)
  #omit NAs from teh database
  coex11 <- na.omit(coex11)
  write.csv(coex11, "results/results-LUI_coexistence-11spp.csv", row.names = FALSE)
}

#load ggplot2
library(ggplot2)

#some plots

#x = SND, y = SFD, color = feasibility
SFD_SND11 <- ggplot(data = coex11, aes(x = SND, y = SFD, color = feasibility)) +
  geom_point(alpha = 0.4, size = 1.5) +
  ggtitle("Combinations of 11 species")

#x = LUI, y = SND
SND11 <- ggplot(data = coex11, aes(x = LUI, y = SND)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 11 species")

#x = LUI, y = SFD
SFD11 <- ggplot(data = coex11, aes(x = LUI, y = SFD)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 11 species")

#x = LUI, y = overlap
over11 <- ggplot(data = coex11, aes(x = LUI, y = overlap)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 11 species")

#x = LUI, y = differential
diff11 <- ggplot(data = coex11, aes(x = LUI, y = differential)) +
  geom_point(alpha = 0.5) +
  geom_smooth(alpha = 0, size = 2) +
  ggtitle("Combinations of 11 species")

#x = LUI, y = feasibility
feas11 <- ggplot(data = coex11, aes(x = LUI, y = as.integer(feasibility)-1)) +
  geom_point() +
  geom_smooth(alpha = 0, size = 2) +
  ylab("Feasibility") +
  ggtitle("Combinations of 11 species")

ggsave(plot = SFD_SND11, filename = "figures/SFD_SND11.png")
ggsave(plot = SND11, filename = "figures/SND11.png")
ggsave(plot = SFD11, filename = "figures/SFD11.png")
ggsave(plot = over11, filename = "figures/over11.png")
ggsave(plot = diff11, filename = "figures/diff11.png")
ggsave(plot = feas11, filename = "figures/feas11.png")
