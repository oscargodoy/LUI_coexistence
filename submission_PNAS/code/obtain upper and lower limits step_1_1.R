#extract the upper and lower limits to compute coexistence metrics

rm(list = ls())

#loading average values
intrinsic <- read.table("submission_PNAS/results/intrinsic_site_lui_average_lme_50.csv", header = TRUE, sep = ",", row.names = 1)
alpha <- as.matrix(read.table("submission_PNAS/results/interaction_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
lui_modify_alpha <- as.matrix(read.table("submission_PNAS/results/lui_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
#loading std. error. values

intrinsic_error <- read.table("submission_PNAS/results/intrinsic_site_lui_std_error_lme_50.csv", header = TRUE, sep = ",", row.names = 1)
alpha_error <- as.matrix(read.table("submission_PNAS/results/interaction_matrix_lme_std_error_50.csv", header = TRUE, sep = ",", row.names = 1))
lui_modify_alpha_error <- as.matrix(read.table("submission_PNAS/results/lui_matrix_lme_std_error_50.csv", header = TRUE, sep = ",", row.names = 1))


top50.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off", "Lol_per", "Arr_ela", 
                 "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep", "Tri_pra", "Bro_ere", "Ran_rep", "Bro_hor", 
                 "Ran_acr", "Pla_lan", "Ach_mil", "Gal_mol", "Her_sph", "Ant_syl", "Hol_lan", "Hel_pub",
                 "Ant_odo", "Bra_pin", "Car_hir", "Ver_cha", "Rum_ace", "Fes_ovi", "Phl_pra", "Pha_aru",
                 "Des_ces", "Agr_sto", "Cyn_cri", "Cir_ole", "Cer_hol", "Pla_med", "Cre_bie", "Urt_dio",
                 "Thy_pul", "Lol_mul", "Cir_arv", "Lot_cor", "Ran_bul", "Tri_dub", "Med_lup", "Leo_his",
                 "Car_car", "Vic_sep", "Pru_sp")

#Simulate x values of LUI at equal intervals
lui <- c(0.5, 0.75, 1, 1.15, 1.50, 1.75, 2, 2.25, 2.50, 2.75, 3) 

# we obtain the extremes of the distributions according to the standard errors. 
lower_intrinsic <- intrinsic - intrinsic_error*0.3
upper_intrinsic <- intrinsic + intrinsic_error

lower_alpha <- alpha - alpha_error*0.3
upper_alpha <- alpha + alpha_error

lower_lui_modify_alpha <- lui_modify_alpha - lui_modify_alpha_error*0.3
upper_lui_modify_alpha <- lui_modify_alpha + lui_modify_alpha_error

#create the datasets
#lower ----

lui_intrinsic <- list()
lui_alpha <- list()
xx <- matrix(nrow = 51, ncol = 2, NA)
colnames(xx) <- c("intrinsic", "lui_value")
row.names(xx) <- paste(top50.short, sep = ",")

#to obtain all value of intrinsic abundance at different LUI values
for(i in 1:(length(lui))){
  for(j in 1:length(row.names(lower_intrinsic))){
    xx[j,1] <- lower_intrinsic[j,1] + lower_intrinsic[j,2]*lui[i]
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
  lui_alpha[[i]] <- lower_alpha + lower_lui_modify_alpha*lui[i]
  lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% row.names(lui_intrinsic_positive[[i]])),
                                   which(colnames(lui_alpha[[i]]) %in% row.names(lui_intrinsic_positive[[i]]))]
  hh[[i]] <- row.names(subset(lui_alpha[[i]], diag(lui_alpha[[i]]) < 0)) #we hold those with negative intra
  lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% hh[[i]]),
                                   which(colnames(lui_alpha[[i]]) %in% hh[[i]])]
}

#get back to the intrinsic and reduce the species list to those with negative intras
for(i in 1:(length(lui_intrinsic_positive))){
  lui_intrinsic_positive[[i]] <- lui_intrinsic_positive[[i]][which(rownames(lui_intrinsic_positive[[i]]) %in% rownames(lui_alpha[[i]])),]
}

#remove everything except for matrix and intrinsic with LUI
rm(list=ls()[! ls() %in% c("lui_alpha", "lui_intrinsic_positive")])

save(lui_alpha, lui_intrinsic_positive, file = "submission_PNAS/results/lower_50sps.RData")

#create the datasets
#upper ----

lui_intrinsic <- list()
lui_alpha <- list()
xx <- matrix(nrow = 51, ncol = 2, NA)
colnames(xx) <- c("intrinsic", "lui_value")
row.names(xx) <- paste(top50.short, sep = ",")

#to obtain all value of intrinsic abundance at different LUI values
for(i in 1:(length(lui))){
  for(j in 1:length(row.names(upper_intrinsic))){
    xx[j,1] <- upper_intrinsic[j,1] + upper_intrinsic[j,2]*lui[i]
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
  lui_alpha[[i]] <- upper_alpha + upper_lui_modify_alpha*lui[i]
  lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% row.names(lui_intrinsic_positive[[i]])),
                                   which(colnames(lui_alpha[[i]]) %in% row.names(lui_intrinsic_positive[[i]]))]
  hh[[i]] <- row.names(subset(lui_alpha[[i]], diag(lui_alpha[[i]]) < 0)) #we hold those with negative intra
  lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% hh[[i]]),
                                   which(colnames(lui_alpha[[i]]) %in% hh[[i]])]
}

#get back to the intrinsic and reduce the species list to those with negative intras
for(i in 1:(length(lui_intrinsic_positive))){
  lui_intrinsic_positive[[i]] <- lui_intrinsic_positive[[i]][which(rownames(lui_intrinsic_positive[[i]]) %in% rownames(lui_alpha[[i]])),]
}

#remove everything except for matrix and intrinsic with LUI
rm(list=ls()[! ls() %in% c("lui_alpha", "lui_intrinsic_positive")])

save(lui_alpha, lui_intrinsic_positive, file = "submission_PNAS/results/upper_50sps.RData")

