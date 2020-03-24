rm(list = ls())

#load coefficients
alpha <- as.matrix(read.table("results/interaction_matrix_lme_average_26spp.txt", header = TRUE, sep = "\t"))
lui_modify_alpha <- as.matrix(read.table("results/lui_matrix_lme_average_26spp.txt", header = TRUE, sep = "\t"))
intrinsic <- as.matrix(read.table("results/intrinsic_site_lui_average_lme_26spp.txt", header = TRUE, sep = "\t"))

#species
top.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off",
               "Lol_per", "Arr_ela", "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep",
               "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", "Bro_hor", "Pla_lan",
               "Ach_mil", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub",
               "Car_hir", "Bra_pin")

#lui intervals
lui <- seq(from = 0.5, to = 3, by = 0.25)

lui_intrinsic <- list()
lui_alpha <- list()
xx <- matrix(nrow = length(top.short), ncol = 2, NA)
colnames(xx) <- c("intrinsic", "lui_value")
row.names(xx) <- paste(top.short, sep = ",")

#to obtain all value of intrinsic abundance at different LUI values
for(i in 1:(length(lui))){
  for(j in 1:length(row.names(intrinsic))){
    xx[j, 1] <- intrinsic[j, 1] + intrinsic[j, 2] * lui[i]
    xx[j, 2] <- lui[i]
  }
  lui_intrinsic[[i]] <- xx
}

#to obtain species with positive intrinsic at each site
lui_intrinsic_positive <-  list()
zz <- list()
for(i in 1:(length(lui_intrinsic))){
  zz[[i]] <- row.names(subset(lui_intrinsic[[i]], lui_intrinsic[[i]][, 1] > 0))
  lui_intrinsic_positive[[i]] <- lui_intrinsic[[i]][which(rownames(lui_intrinsic[[i]]) %in% zz[[i]]), ]
}

#to obtain interaction matrices at different LUI values
#subset the interaction matrices at different LUIs to those species showing positive growth rates.
hh <- list()

for(i in 1:(length(lui))){
  lui_alpha[[i]] <- alpha + lui_modify_alpha * lui[i]
  lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in%
                                           row.names(lui_intrinsic_positive[[i]])),
                                   which(colnames(lui_alpha[[i]]) %in%
                                           row.names(lui_intrinsic_positive[[i]]))]
  hh[[i]] <- row.names(subset(lui_alpha[[i]],
                              diag(as.matrix(lui_alpha[[i]])) < 0)) #we hold those with negative intra
  lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% hh[[i]]),
                                   which(colnames(lui_alpha[[i]]) %in% hh[[i]])]
}

#get back to the intrinsic and reduce the species list to those with negative intras
for(i in 1:(length(lui_intrinsic_positive))){
  lui_intrinsic_positive[[i]] <- lui_intrinsic_positive[[i]][which(rownames(lui_intrinsic_positive[[i]]) %in% rownames(lui_alpha[[i]])), ]
}

#remove everything except for matrix and intrinsic with LUI
rm(list=ls()[! ls() %in% c("lui_alpha", "lui_intrinsic_positive")])


#2. Compute structural metrics----
#Here this part of the code, compute metrics for each of the combos under different LUI values. 
#This the part where we need PARALLEL CLUSTER AGAIN.

library(mvtnorm)

source('code/toolbox_coexistence.R')
#source('code/toolbox_figure.R')


library(foreach)
library(doParallel)
library(parallel)
library(doSNOW)


cores = detectCores()
cl <- makeCluster(cores[1])
registerDoSNOW(cl) #defines parallel computation 
startime <- Sys.time() # to see how much it takes to run


#We load the combos first
combos_lui2 <- readRDS("results/selected_combos/combos2.rds")


# We are going to separate the database in several pieces to do not colapso with the computing

#First five LUI values  ----
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")
neg_intras <- data.frame()

results_coex <- foreach (i = 1:length(combos_lui2), .combine = rbind, .packages = "mvtnorm") %dopar% {
  
  combos_lui2[[i]] <- as.matrix(combos_lui2[[i]])
  
  results_combo <- matrix (nrow = nrow(combos_lui2[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos_lui2[[i]], 1, paste, collapse=".") 
  colnames(results_combo) <- col_results
  
  for (j in 1:(nrow(combos_lui2[[i]]))){
    ll <- combos_lui2[[i]][j,]
    mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
    mm <- -1 * mm #this is because intras has to be positive
    
    #save info to locate the species in case of negative intras
    if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
      neg_intras <- rbind(neg_intras,
                          data.frame("lui_level" = i,
                                     "combo" = j,
                                     "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
    }
    ii <- subset(lui_intrinsic_positive[[i]], rownames(lui_intrinsic_positive[[i]]) %in% ll)
    results_combo[j, 1] <- 10^Omega(mm) # this is niche differences
    results_combo[j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
    co <- compute_overlap(mm, 1000)
    results_combo[j, 3] <- co$overlap # this is community overlap
    results_combo[j, 4] <- co$Omega - co$Omega_all # this is community differential
    results_combo[j, 5] <- test_feasibility(mm, ii[, 1]) #this is whether all speceis can coexist. 
    results_combo[j, 6] <- sum(test_feasibility_pairs(mm, ii[, 1])$feasibility) / length(test_feasibility_pairs(mm, ii[, 1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
    results_combo[j, 7] <- unique(lui_intrinsic_positive[[i]][, 2])
  }
  print(i)
  
  return(results_combo)
}

stopCluster(cl) #stop cluster
endtime <- Sys.time() #save time once finished
(totime <- endtime - startime)

source("code/beep.R")

#specific changes to 2 species
results_coex <- results_coex[, c(1:3, 6, 8)]
colnames(results_coex) <- c("combo", "SND", "SFD", "feasibility", "LUI")
write.csv(results_coex, file = "results/results-LUI_coexistence-2spp.csv", row.names = FALSE)
