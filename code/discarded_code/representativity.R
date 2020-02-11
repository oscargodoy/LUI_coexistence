# representativity of the number of combinations selected

n_spp <- 50
div <- seq(from = 2, to = n_spp/2, by = 1)

combi <- NULL

for (i in 1:length(div)){
  combi <- c(combi, choose(n_spp, div[i]))
}

s_10_4 <- (10^4 / combi) * 100
n_subsample <- rep("10^4", length(s_10_4))
s_10_5 <- (10^5 / combi) * 100
n_subsample <- c(n_subsample, rep("10^5", length(s_10_4)))
s_10_6 <- (10^6 / combi) * 100
n_subsample <- c(n_subsample, rep("10^6", length(s_10_4)))
s_10_7 <- (10^7 / combi) * 100
n_subsample <- c(n_subsample, rep("10^7", length(s_10_4)))
s_10_8 <- (10^8 / combi) * 100
n_subsample <- c(n_subsample, rep("10^8", length(s_10_4)))
s_10_9 <- (10^9 / combi) * 100
n_subsample <- c(n_subsample, rep("10^9", length(s_10_4)))

per_s <- c(s_10_4, s_10_5, s_10_6, s_10_7, s_10_8, s_10_9)

repre <- data.frame("richness" = rep(div, 6), n_subsample, per_s)

for (j in 1:nrow(repre)){
  if (repre$per_s[j] > 100){
    repre$per_s[j] <- 100
  }
}

library(ggplot2)

full <- ggplot(data = repre, aes(x = richness, y = per_s, colour = n_subsample)) + geom_line() +
  scale_x_continuous(name = "Species richness", breaks = c(seq(2, 13, 1), seq(13, 25, 2))) +
  ggtitle("50 species community") +
  scale_y_continuous(name = "Representativity (%)") + labs(colour = "Subsample") + theme_grey()

limited1 <- ggplot(data = repre, aes(x = richness, y = per_s, colour = n_subsample)) + geom_line() +
  scale_x_continuous(name = "Species richness", breaks = seq(9, 25, 4)) +
  scale_y_continuous(name = "Representativity (%)") + labs(colour = "Subsample") + theme_grey() +
  coord_cartesian(xlim = c(8, 25), ylim = c(0, 0.0005)) + theme(legend.position = "none")

limited2 <- ggplot(data = repre, aes(x = richness, y = per_s, colour = n_subsample)) + geom_line() +
    scale_x_continuous(name = "Species richness", breaks = seq(13, 25, 4)) +
    scale_y_continuous(name = NULL) + labs(colour = "Subsample") + theme_grey() +
    coord_cartesian(xlim = c(12, 25), ylim = c(0, 0.000005)) + theme(legend.position = "none")

limited3 <- ggplot(data = repre, aes(x = richness, y = per_s, colour = n_subsample)) + geom_line() +
    scale_x_continuous(name = "Species richness", breaks = seq(17, 25, 4)) +
    scale_y_continuous(name = NULL) + labs(colour = "Subsample") + theme_grey() +
    coord_cartesian(xlim = c(16, 25), ylim = c(0, 0.0000001)) + theme(legend.position = "none")

library(ggpubr)

ggarrange(full, ggarrange(limited1, limited2, limited3, ncol = 3),
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
ggsave("figures/representativity.png", width = 6.5, height = 6.5, dpi = 320)



