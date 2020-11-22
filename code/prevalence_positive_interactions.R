#Takes some time to run

#load alpha and intrinsic
load("results/LUI_effects.RData")

lui <- seq(0.5, 3.0, 0.25)
count_pairs <- NULL
tot_pairs <- NULL
count_triplets <- NULL
tot_triplets <- NULL


for (i in 1:length(lui)){
  
  #pairs
  inter <- 2
  counter <- 0
  n <- 1:ncol(combn(colnames(lui_alpha[[i]]), inter))
  tot_pairs <- c(tot_pairs, length(n))
  for (j in 1:length(n)){
    spp <- t(combn(colnames(lui_alpha[[i]]), inter))[j, ]
    alpha <- lui_alpha[[i]][spp, spp]
    diag(alpha) <- diag(alpha) * 0
    if(length(which(alpha > 0)) > 0){
      counter <- counter + 1
    }
  }
  count_pairs <- c(count_pairs, counter)
  
  #triplets
  inter <- 3
  counter <- 0
  n <- 1:ncol(combn(colnames(lui_alpha[[i]]), inter))
  tot_triplets <- c(tot_triplets, length(n))
  for (j in 1:length(n)){
    spp <- t(combn(colnames(lui_alpha[[i]]), inter))[j, ]
    alpha <- lui_alpha[[i]][spp, spp]
    diag(alpha) <- diag(alpha) * 0
    if(length(which(alpha > 0)) > 0){
      counter <- counter + 1
    }
  }
  count_triplets <- c(count_triplets, counter)
  
}


facilitation <- data.frame("LUI" = as.factor(rep(lui, 2)),
                           "interaction" = c(rep("Pair", 11),
                                             rep("Triplet", 11)),
                           "prevalence" = c(count_pairs/tot_pairs,
                                            count_triplets/tot_triplets))


library(ggplot2)
p_facilitation <- ggplot(data = facilitation, aes(x = LUI, y = prevalence, fill = interaction)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab("Prevalence (%) of positive interactions") +
  labs(x="LUI", fill = "Combination") +
  theme(
    axis.text=element_text(size=14),
    plot.title = element_text(size=14),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  )

ggsave("figures/facilitation.png", device = "png", dpi = 320)


#arrange both intra- inter plots and facilitation

ggarrange(p_interactions, p_facilitation,
          ncol = 1, nrow = 2,
          heights = c(2, 2),
          labels = "AUTO",
          #font.label = list(size = size_text + 5),
          hjust = c(-2.5, -2.15),
          vjust = c(1.5, 0.5))
ggsave(filename = "figures/paper_figures/SupFig1_interactions and facilitation.png", device = "png",
       width = 15, height = 13, limitsize = FALSE)



