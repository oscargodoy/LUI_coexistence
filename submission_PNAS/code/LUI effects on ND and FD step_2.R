###Step 2 Computing structural metrics for different species richness combos across the LUI gradient
  
rm(list = ls())

  
#1.Obtaining combinations to calculate niche and fitness diff.----
#loading average values
intrinsic <- read.table("submission_PNAS/results/intrinsic_site_lui_average_lme_50.csv", header = TRUE, sep = ",", row.names = 1)
alpha <- as.matrix(read.table("submission_PNAS/results/interaction_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
lui_modify_alpha <- as.matrix(read.table("submission_PNAS/results/lui_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
  
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
  
#remove everything except for matrix and intrinsic with LUI
rm(list=ls()[! ls() %in% c("lui_alpha", "lui_intrinsic")])
  
#Obtain a list of combination of all species all combination of 2 and 3 and so on species until all species with positive abundance.
combos_lui2 <- t(combn(row.names(lui_alpha[[1]]), 2))
combos_lui3 <- t(combn(row.names(lui_alpha[[1]]), 3))

combos_lui2 <- list()
  for(i in 1:length(lui_alpha)){
    combos_lui2[[i]] <- t(combn(row.names(lui_alpha[[i]]),2))
  }

combos_lui3 <- list()
for(i in 1:length(lui_alpha)){
  combos_lui3[[i]] <- t(combn(row.names(lui_alpha[[i]]),3))
}
  
#2. Compute structural metrics----
  
library(mvtnorm)
  
source('code/toolbox_coexistence.R')
source('code/toolbox_figure.R')
  
library(foreach)
library(doParallel)
library(parallel)
library(doSNOW)
  
  
cores = detectCores()
cl <- makeCluster(cores[1])
registerDoSNOW(cl) #defines parallel computation 
startime <- Sys.time() # to see how much it takes to run
  
  
#Combinations of two species. 
combos_lui2 <- readRDS("submission_PNAS/data/combos2.rds")
  
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")
neg_intras <- data.frame()
  
results_coex <- foreach (i = 1:length(combos_lui2), .combine = rbind, .packages = "mvtnorm") %dopar% {
    
  results_combo <- matrix (nrow = nrow(combos_lui2[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos_lui2[[i]], 1, paste, collapse=".") 
  colnames(results_combo) <- col_results
    
    for (j in 1:(nrow(combos_lui2[[i]]))){
      ll <- as.matrix(combos_lui2[[i]][j,])
      mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
      mm <- -1 * mm #this is because intras has to be positive
      
      #save info to locate the species in case of negative intras
      if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
        neg_intras <- rbind(neg_intras,
                            data.frame("lui_level" = i,
                                       "combo" = j,
                                       "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
      }
      ii <- subset(lui_intrinsic[[i]], rownames(lui_intrinsic[[i]]) %in% ll)
      results_combo[j, 1] <- 10^Omega(mm) # this is niche differences
      results_combo[j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
      co <- compute_overlap(mm, 1000)
      results_combo[j, 3] <- co$overlap # this is community overlap
      results_combo[j, 4] <- co$Omega - co$Omega_all # this is community differential
      results_combo[j, 5] <- test_feasibility(mm, ii[, 1]) #this is whether all species can coexist. 
      results_combo[j, 6] <- sum(test_feasibility_pairs(mm, ii[, 1])$feasibility) / length(test_feasibility_pairs(mm, ii[, 1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
      results_combo[j, 7] <- unique(lui_intrinsic[[i]][, 2])
    }
    print(i)
    
    return(results_combo)
  }
  
  stopCluster(cl) #stop cluster
  endtime <- Sys.time() #save time once finished
  (totime <- endtime - startime)
  
  results_coex <- as.data.frame(results_coex)
  results_coex <- tibble::rownames_to_column(results_coex, "combo")
  write.csv(results_coex, file = "submission_PNAS/results/results-LUI_coexistence-2spp.csv", row.names = FALSE)
  
  #Combinations of three species. 

  cores = detectCores()
  cl <- makeCluster(cores[1])
  registerDoSNOW(cl) #defines parallel computation 
  startime <- Sys.time() # to see how much it takes to run
  
  
  #Combinations of three species. 
  combos_lui3 <- readRDS("submission_PNAS/data/combos3.rds")
  
  col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")
  neg_intras <- data.frame()
  
  results_coex <- foreach (i = 1:length(combos_lui3), .combine = rbind, .packages = "mvtnorm") %dopar% {
    
    results_combo <- matrix (nrow = nrow(combos_lui3[[i]]), ncol = length(col_results))
    row.names(results_combo) <- apply(combos_lui3[[i]], 1, paste, collapse=".") 
    colnames(results_combo) <- col_results
    
    for (j in 1:(nrow(combos_lui3[[i]]))){
      ll <- as.matrix(combos_lui3[[i]][j,])
      mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
      mm <- -1 * mm #this is because intras has to be positive
      
      #save info to locate the species in case of negative intras
      if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
        neg_intras <- rbind(neg_intras,
                            data.frame("lui_level" = i,
                                       "combo" = j,
                                       "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
      }
      ii <- subset(lui_intrinsic[[i]], rownames(lui_intrinsic[[i]]) %in% ll)
      results_combo[j, 1] <- 10^Omega(mm) # this is niche differences
      results_combo[j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
      co <- compute_overlap(mm, 1000)
      results_combo[j, 3] <- co$overlap # this is community overlap
      results_combo[j, 4] <- co$Omega - co$Omega_all # this is community differential
      results_combo[j, 5] <- test_feasibility(mm, ii[, 1]) #this is whether all species can coexist. 
      results_combo[j, 6] <- sum(test_feasibility_pairs(mm, ii[, 1])$feasibility) / length(test_feasibility_pairs(mm, ii[, 1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
      results_combo[j, 7] <- unique(lui_intrinsic[[i]][, 2])
    }
    print(i)
    
    return(results_combo)
  }
  
  stopCluster(cl) #stop cluster
  endtime <- Sys.time() #save time once finished
  (totime <- endtime - startime)
  
  #source("code/beep.R")
  
  #specific changes to 3 species
  results_coex <- results_coex[, c(1:5, 7)]
  colnames(results_coex) <- c("SND", "SFD", "overlap", "differential", "feasibility", "LUI")
  write.csv(results_coex, file = "submission_PNAS/results/results-LUI_coexistence-3spp.csv", row.names = FALSE)

#3. Compute structural metrics for 3, 5, 7 and 11 sps---- 
## with the 26 most common species in order to make it feasible due to computer power.
## This info will be used in step 4.
  
intrinsic <- read.table("submission_PNAS/results/intrinsic_site_lui_average_lme_50.csv", header = TRUE, sep = ",", row.names = 1)
alpha <- as.matrix(read.table("submission_PNAS/results/interaction_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
lui_modify_alpha <- as.matrix(read.table("submission_PNAS/results/lui_matrix_lme_average_50.csv", header = TRUE, sep = ",", row.names = 1))
  
    
spp_26 <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off",
           "Lol_per", "Arr_ela", "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep",
           "Tri_pra", "Ran_rep", "Bro_ere", "Ran_acr", "Bro_hor", "Pla_lan",
           "Ach_mil", "Ant_syl", "Her_sph", "Gal_mol", "Hol_lan", "Hel_pub",
           "Car_hir", "Bra_pin")

#Simulate x values of LUI at equal intervals, 11 steps in total
lui <- seq(from = 0.5, to = 3, by = 0.25)

lui_intrinsic <- list()
lui_alpha <- list()
xx <- matrix(nrow = 26, ncol = 2, NA)
colnames(xx) <- c("intrinsic", "lui_value")
row.names(xx) <- paste(spp_26, sep = ",")

#subset the vector of intrinsic and the matrix of species interactions to the 26 sps.

intrinsic <- intrinsic[spp_26,]
alpha <- alpha[spp_26, spp_26]
lui_modify_alpha <- lui_modify_alpha[spp_26, spp_26]

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

#remove everything except for matrix and intrinsic with LUI
rm(list=ls()[! ls() %in% c("lui_alpha", "lui_intrinsic")])

combos_lui3 <- t(combn(row.names(lui_alpha[[1]]), 3))
combos_lui5 <- t(combn(row.names(lui_alpha[[1]]), 5))
combos_lui7 <- t(combn(row.names(lui_alpha[[1]]), 7))
combos_lui11 <- t(combn(row.names(lui_alpha[[1]]), 11))

combos_lui3 <- list()
for(i in 1:length(lui_alpha)){
  combos_lui3[[i]] <- t(combn(row.names(lui_alpha[[i]]),3))
}

combos_lui5 <- list()
for(i in 1:length(lui_alpha)){
  combos_lui5[[i]] <- t(combn(row.names(lui_alpha[[i]]),5))
}

combos_lui7 <- list()
for(i in 1:length(lui_alpha)){
  combos_lui7[[i]] <- t(combn(row.names(lui_alpha[[i]]),7))
}

combos_lui11 <- list()
for(i in 1:length(lui_alpha)){
  combos_lui11[[i]] <- t(combn(row.names(lui_alpha[[i]]),11))
}

for(i in 1:length(combos_lui11)){
  combos_lui11[[i]] <- combos_lui11[[i]][sample(nrow(combos_lui11[[i]]), 65780), ]
}


#saveRDS(combos_lui3, file = "submission_PNAS/results/combos_lui3_overlap.rds")
#saveRDS(combos_lui5, file = "submission_PNAS/results/combos_lui5_overlap.rds")
#saveRDS(combos_lui7, file = "submission_PNAS/results/combos_lui7_overlap.rds")
#saveRDS(combos_lui11, file = "submission_PNAS/results/combos_lui11_overlap.rds")

library(mvtnorm)

source('code/toolbox_coexistence.R')
source('code/toolbox_figure.R')

library(foreach)
library(doParallel)
library(parallel)
library(doSNOW)

#Combinations of three species. 
combos_lui3 <-readRDS(file = "submission_PNAS/results/combos_lui3_overlap.rds")
  
cores = detectCores()
cl <- makeCluster(cores[1])
registerDoSNOW(cl) #defines parallel computation 
startime <- Sys.time() # to see how much it takes to run
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")
neg_intras <- data.frame()

results_coex <- foreach (i = 1:length(combos_lui3), .combine = rbind, .packages = "mvtnorm") %dopar% {
  
  results_combo <- matrix (nrow = nrow(combos_lui3[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos_lui3[[i]], 1, paste, collapse=".") 
  colnames(results_combo) <- col_results
  
  for (j in 1:(nrow(combos_lui3[[i]]))){
    ll <- as.matrix(combos_lui3[[i]][j,])
    mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
    mm <- -1 * mm #this is because intras has to be positive
    
    #save info to locate the species in case of negative intras
    if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
      neg_intras <- rbind(neg_intras,
                          data.frame("lui_level" = i,
                                     "combo" = j,
                                     "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
    }
    ii <- subset(lui_intrinsic[[i]], rownames(lui_intrinsic[[i]]) %in% ll)
    results_combo[j, 1] <- 10^Omega(mm) # this is niche differences
    results_combo[j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
    co <- compute_overlap(mm, 1000)
    results_combo[j, 3] <- co$overlap # this is community overlap
    results_combo[j, 4] <- co$Omega - co$Omega_all # this is community differential
    results_combo[j, 5] <- test_feasibility(mm, ii[, 1]) #this is whether all species can coexist. 
    results_combo[j, 6] <- sum(test_feasibility_pairs(mm, ii[, 1])$feasibility) / length(test_feasibility_pairs(mm, ii[, 1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
    results_combo[j, 7] <- unique(lui_intrinsic[[i]][, 2])
  }
  print(i)
  
  return(results_combo)
}

stopCluster(cl) #stop cluster
endtime <- Sys.time() #save time once finished
(totime <- endtime - startime)

#source("code/beep.R")

#specific changes to 3 species
results_coex <- results_coex[, c(1:5, 7)]
colnames(results_coex) <- c("SND", "SFD", "overlap", "differential", "feasibility", "LUI")
write.csv(results_coex, file = "submission_PNAS/results/results-LUI_coexistence-3spp_overlap.csv", row.names = FALSE)

#Combinations of five species. 
combos_lui5 <-readRDS(file = "submission_PNAS/results/combos_lui5_overlap.rds")

cores = detectCores()
cl <- makeCluster(cores[1])
registerDoSNOW(cl) #defines parallel computation 
startime <- Sys.time() # to see how much it takes to run
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")
neg_intras <- data.frame()

results_coex <- foreach (i = 1:length(combos_lui5), .combine = rbind, .packages = "mvtnorm") %dopar% {
  
  results_combo <- matrix (nrow = nrow(combos_lui5[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos_lui5[[i]], 1, paste, collapse=".") 
  colnames(results_combo) <- col_results
  
  for (j in 1:(nrow(combos_lui5[[i]]))){
    ll <- as.matrix(combos_lui5[[i]][j,])
    mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
    mm <- -1 * mm #this is because intras has to be positive
    
    #save info to locate the species in case of negative intras
    if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
      neg_intras <- rbind(neg_intras,
                          data.frame("lui_level" = i,
                                     "combo" = j,
                                     "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
    }
    ii <- subset(lui_intrinsic[[i]], rownames(lui_intrinsic[[i]]) %in% ll)
    results_combo[j, 1] <- 10^Omega(mm) # this is niche differences
    results_combo[j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
    co <- compute_overlap(mm, 1000)
    results_combo[j, 3] <- co$overlap # this is community overlap
    results_combo[j, 4] <- co$Omega - co$Omega_all # this is community differential
    results_combo[j, 5] <- test_feasibility(mm, ii[, 1]) #this is whether all species can coexist. 
    results_combo[j, 6] <- sum(test_feasibility_pairs(mm, ii[, 1])$feasibility) / length(test_feasibility_pairs(mm, ii[, 1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
    results_combo[j, 7] <- unique(lui_intrinsic[[i]][, 2])
  }
  print(i)
  
  return(results_combo)
}

stopCluster(cl) #stop cluster
endtime <- Sys.time() #save time once finished
(totime <- endtime - startime)

#source("code/beep.R")

results_coex <- results_coex[, c(1:5, 7)]
colnames(results_coex) <- c("SND", "SFD", "overlap", "differential", "feasibility", "LUI")
write.csv(results_coex, file = "submission_PNAS/results/results-LUI_coexistence-5spp_overlap.csv", row.names = FALSE)

#Combinations of seven species. 
combos_lui7 <-readRDS(file = "submission_PNAS/results/combos_lui7_overlap.rds")

cores = detectCores()
cl <- makeCluster(cores[1])
registerDoSNOW(cl) #defines parallel computation 
startime <- Sys.time() # to see how much it takes to run
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")
neg_intras <- data.frame()

results_coex <- foreach (i = 1:length(combos_lui7), .combine = rbind, .packages = "mvtnorm") %dopar% {
  
  results_combo <- matrix (nrow = nrow(combos_lui7[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos_lui7[[i]], 1, paste, collapse=".") 
  colnames(results_combo) <- col_results
  
  for (j in 1:(nrow(combos_lui7[[i]]))){
    ll <- as.matrix(combos_lui7[[i]][j,])
    mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
    mm <- -1 * mm #this is because intras has to be positive
    
    #save info to locate the species in case of negative intras
    if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
      neg_intras <- rbind(neg_intras,
                          data.frame("lui_level" = i,
                                     "combo" = j,
                                     "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
    }
    ii <- subset(lui_intrinsic[[i]], rownames(lui_intrinsic[[i]]) %in% ll)
    results_combo[j, 1] <- 10^Omega(mm) # this is niche differences
    results_combo[j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
    co <- compute_overlap(mm, 1000)
    results_combo[j, 3] <- co$overlap # this is community overlap
    results_combo[j, 4] <- co$Omega - co$Omega_all # this is community differential
    results_combo[j, 5] <- test_feasibility(mm, ii[, 1]) #this is whether all species can coexist. 
    results_combo[j, 6] <- sum(test_feasibility_pairs(mm, ii[, 1])$feasibility) / length(test_feasibility_pairs(mm, ii[, 1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
    results_combo[j, 7] <- unique(lui_intrinsic[[i]][, 2])
  }
  print(i)
  
  return(results_combo)
}

stopCluster(cl) #stop cluster
endtime <- Sys.time() #save time once finished
(totime <- endtime - startime)

#source("code/beep.R")

results_coex <- results_coex[, c(1:5, 7)]
colnames(results_coex) <- c("SND", "SFD", "overlap", "differential", "feasibility", "LUI")
results_coex <- as.data.frame(results_coex)
results_coex <- tibble::rownames_to_column(results_coex, "combo")
write.csv(results_coex, file = "submission_PNAS/results/results-LUI_coexistence-7spp_overlap.csv", row.names = FALSE)

#Combinations of eleven species. 
combos_lui11 <-readRDS(file = "submission_PNAS/results/combos_lui11_overlap.rds")

cores = detectCores()
cl <- makeCluster(cores[1])
registerDoSNOW(cl) #defines parallel computation 
startime <- Sys.time() # to see how much it takes to run
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")
neg_intras <- data.frame()

results_coex <- foreach (i = 1:length(combos_lui11), .combine = rbind, .packages = "mvtnorm") %dopar% {
  
  results_combo <- matrix (nrow = nrow(combos_lui11[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos_lui11[[i]], 1, paste, collapse=".") 
  colnames(results_combo) <- col_results
  
  for (j in 1:(nrow(combos_lui11[[i]]))){
    ll <- as.matrix(combos_lui11[[i]][j,])
    mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
    mm <- -1 * mm #this is because intras has to be positive
    
    #save info to locate the species in case of negative intras
    if(length(names(which((diag(mm) < 0) == TRUE)) != 0)){
      neg_intras <- rbind(neg_intras,
                          data.frame("lui_level" = i,
                                     "combo" = j,
                                     "species" = paste(names(which((diag(mm) > 0) == TRUE)), collapse = " & ")))
    }
    ii <- subset(lui_intrinsic[[i]], rownames(lui_intrinsic[[i]]) %in% ll)
    results_combo[j, 1] <- 10^Omega(mm) # this is niche differences
    results_combo[j, 2] <- theta(mm, ii[, 1]) # this is fitness differences
    co <- compute_overlap(mm, 1000)
    results_combo[j, 3] <- co$overlap # this is community overlap
    results_combo[j, 4] <- co$Omega - co$Omega_all # this is community differential
    results_combo[j, 5] <- test_feasibility(mm, ii[, 1]) #this is whether all species can coexist. 
    results_combo[j, 6] <- sum(test_feasibility_pairs(mm, ii[, 1])$feasibility) / length(test_feasibility_pairs(mm, ii[, 1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
    results_combo[j, 7] <- unique(lui_intrinsic[[i]][, 2])
  }
  print(i)
  
  return(results_combo)
}

stopCluster(cl) #stop cluster
endtime <- Sys.time() #save time once finished
(totime <- endtime - startime)

#source("code/beep.R")

results_coex <- results_coex[, c(1:5, 7)]
colnames(results_coex) <- c("SND", "SFD", "overlap", "differential", "feasibility", "LUI")
write.csv(results_coex, file = "submission_PNAS/results/results-LUI_coexistence-11spp_overlap.csv", row.names = FALSE)
