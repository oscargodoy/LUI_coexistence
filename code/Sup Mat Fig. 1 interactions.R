#Compute structural metrics for different species richness combos across the LUI gradient

rm(list = ls())

intrinsic <- read.table("results/OG_results/intrinsic_site_lui_average_lme_50.csv", header = TRUE, sep = ",", row.names = 1)
alpha <- as.matrix(read.table("results/OG_results/interaction_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
lui_modify_alpha <- as.matrix(read.table("results/OG_results/lui_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))

top50.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off", "Lol_per", "Arr_ela", 
                 "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep", "Tri_pra", "Bro_ere", "Ran_rep", "Bro_hor", 
                 "Ran_acr", "Pla_lan", "Ach_mil", "Gal_mol", "Her_sph", "Ant_syl", "Hol_lan", "Hel_pub",
                 "Ant_odo", "Bra_pin", "Car_hir", "Ver_cha", "Rum_ace", "Fes_ovi", "Phl_pra", "Pha_aru",
                 "Des_ces", "Agr_sto", "Cyn_cri", "Cir_ole", "Cer_hol", "Pla_med", "Cre_bie", "Urt_dio",
                 "Thy_pul", "Lol_mul", "Cir_arv", "Lot_cor", "Ran_bul", "Tri_dub", "Med_lup", "Leo_his",
                 "Car_car", "Vic_sep", "Pru_sp")

#Simulate x values of LUI at equal intervals
lui <- c(0.5, 0.75, 1, 1.15, 1.50, 1.75, 2, 2.25, 2.50, 2.75, 3) 

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

#to obtain species with positive intrinsic at each site
lui_intrinsic_positive <-  list()
zz <- list()
for(i in 1:(length(lui_intrinsic))){
  zz[[i]] <- row.names(subset(lui_intrinsic[[i]], lui_intrinsic[[i]][,1]>0))
  lui_intrinsic_positive[[i]] <- lui_intrinsic[[i]][which(rownames(lui_intrinsic[[i]]) %in% zz[[i]]),]
}

#to obtain interaction matrices at different LUI values
#subset the interaction matrices at different LUIs to those species showing positive growth rates.
hh <- list()

for(i in 1:(length(lui))){
  lui_alpha[[i]] <- alpha + lui_modify_alpha*lui[i]
  lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% row.names(lui_intrinsic_positive[[i]])),
                                   which(colnames(lui_alpha[[i]]) %in% row.names(lui_intrinsic_positive[[i]]))]
  hh[[i]] <- row.names(subset(lui_alpha[[i]], diag(lui_alpha[[i]]) < 0)) #we hold those with negative intra
  lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% hh[[i]]),
                                   which(colnames(lui_alpha[[i]]) %in% hh[[i]])]
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

p<- ggplot(data, aes(x=intra_inter, y=int_strength, fill=LUI)) + coord_cartesian(ylim = c(-1.3, 0.09))+
geom_boxplot(outlier.size=-1, notch=TRUE) + facet_wrap(~intra_inter, scales="free") + labs(x = " ", y="Interaction strength", tag = "A")
p + theme(
  axis.text=element_text(size=14),
  plot.title = element_text(size=14),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size=14, face="bold")
  
)

