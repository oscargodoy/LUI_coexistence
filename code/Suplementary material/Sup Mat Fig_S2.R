# Relative strength intra versus interspecific competition---- PANEL A
rm(list = ls())

intrinsic <- read.table("results/intrinsic_site_lui_average_lme_50.csv", header = TRUE, sep = ",", row.names = 1)
alpha <- as.matrix(read.table("results/interaction_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
lui_modify_alpha <- as.matrix(read.table("results/lui_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))

top50.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off", "Lol_per", "Arr_ela", 
                 "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep", "Tri_pra", "Bro_ere", "Ran_rep", "Bro_hor", 
                 "Ran_acr", "Pla_lan", "Ach_mil", "Gal_mol", "Her_sph", "Ant_syl", "Hol_lan", "Hel_pub",
                 "Ant_odo", "Bra_pin", "Car_hir", "Ver_cha", "Rum_ace", "Fes_ovi", "Phl_pra", "Pha_aru",
                 "Des_ces", "Agr_sto", "Cyn_cri", "Cir_ole", "Cer_hol", "Pla_med", "Cre_bie", "Urt_dio",
                 "Thy_pul", "Lol_mul", "Cir_arv", "Lot_cor", "Ran_bul", "Tri_dub", "Med_lup", "Leo_his",
                 "Car_car", "Vic_sep", "Pru_sp")

#Simulate x values of LUI at equal intervals, 11 steps in total
lui <- seq(from = 0.5, to = 3, by = 0.25)

lui_intrinsic <- list()
lui_alpha <- list()
xx <- matrix(nrow = 51, ncol = 2, NA)
colnames(xx) <- c("intrinsic", "lui_value")
row.names(xx) <- paste(top50.short, sep = ",")

#to obtain all value of intrinsic abundance at different LUI values
for(i in 1:(length(lui))){
  for(j in 1:length(row.names(intrinsic))){
    xx[j,1] <- intrinsic[j,1] + intrinsic[j,2]*lui[i]
    xx[j,2] <- lui[i]
  }
  lui_intrinsic[[i]] <- xx
}

#to obtain interaction matrices at different LUI values
for(i in 1:(length(lui))){
  lui_alpha[[i]] <- alpha + lui_modify_alpha*lui[i]
}

#we create the matrices to build the violin plots


intra <-list()
inter<-list()
number_intra<-list()
number_inter <-list()
  
for(i in 1:length(lui_alpha)){
  intra[[i]] <- diag(matrix(unlist(lui_alpha[[i]]), ncol = ncol(lui_alpha[[i]]), byrow = TRUE))
  A <- matrix(unlist(lui_alpha[[i]]), ncol = ncol(lui_alpha[[i]]), byrow = TRUE)
  inter[[i]] <- unlist(A[row(A)!=col(A)])
  
  number_intra[[i]] <- length(intra[[i]])
  number_inter[[i]] <- length(inter[[i]])
  

}
#Because they are of different length
library(plyr)
#obtain a list of different length to code de data later on
lui_intra <- mapply(rep, lui, c(unlist(number_intra)))
lui_intra <- ldply(lui_intra, rbind)
lui_intra <- t(lui_intra)
lui_inter <- mapply(rep, lui, c(unlist(number_inter)))
lui_inter <- ldply(lui_inter, rbind)
lui_inter <- t(lui_inter)
  
# do the same with the values themselves
intra <- ldply(intra, rbind)
intra <- t(intra)
inter <- ldply(inter, rbind)
inter <- t(inter)

# convert everthing into a vector and name the variables
lui_intra <- c(lui_intra)
intra_code <- rep("intraspecific", times=length(lui_intra))
lui_inter <- c(lui_inter)
inter_code <- rep("interspecific", times=length(lui_inter))
intra <- c(intra)
inter <- c(inter)
p1 <- cbind(lui_intra, intra_code, intra)
p2 <- cbind(lui_inter, inter_code, inter)
data <-rbind(p1, p2)
data <- as.data.frame(data)
colnames(data)<- c("LUI", "intra_inter", "int_strength")
#remove NAs
data <- data[complete.cases(data), ]
data$LUI <- as.factor(data$LUI)
data$intra_inter <- as.factor(data$intra_inter)
data$int_strength <- as.numeric(data$int_strength)


#Now plot the violins plots 

require(ggplot2)


p<- ggplot(data, aes(x=LUI, y=int_strength, fill=intra_inter)) + coord_cartesian(ylim = c(-17, 1.2))+
geom_boxplot(position=position_dodge(1), outlier.size=-1, notch=TRUE) + facet_wrap(~intra_inter, scales="free") + labs(x = "LUI", y="Interaction strength")
p_interactions <- p + theme(
  axis.text=element_text(size=14),
  plot.title = element_text(size=14),
  axis.title.x = element_text(size=14),
  axis.title.y = element_text(size=14, face="bold")
)

p_interactions

# Prevalence positive interactions---- PANEL B

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

#arrange both intra-inter and facilitation prevalence plots
library(ggpubr)
ggarrange(p_interactions, p_facilitation,
          ncol = 1, nrow = 2,
          heights = c(2, 2),
          labels = "AUTO",
          #font.label = list(size = size_text + 5),
          hjust = c(-2.5, -2.15),
          vjust = c(1.5, 0.5))
ggsave(filename = "figures/SupFig2.png", device = "png",
       width = 15, height = 13, limitsize = FALSE)





