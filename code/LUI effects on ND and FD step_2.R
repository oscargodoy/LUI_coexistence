  #Compute structural metrics for different species richness combos across the LUI gradient
  
  rm(list = ls())

  
  #1.Obtaining combinations to calculate niche and fitness diff.----
  #loading average values
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
  
  #Simulate x values of LUI at equal intervals
  lui1 <- seq(from = 0, to = 2, by = 0.2)
  lui2 <- seq(from = 2.3, to = 4.50, by = 0.3)
  lui <- c(lui1,lui2) #This gives in total a 20 LUI values to model, more replication in the section from 0 to 2. 
  
  
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
      hh[[i]] <- row.names(subset(lui_alpha[[i]], diag(lui_alpha[[i]])<0)) #we hold those with negative intra
      lui_alpha[[i]] <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% hh[[i]]),
                                       which(colnames(lui_alpha[[i]]) %in% hh[[i]])]
  }
  
  #get back to the intrinsic and reduce the species list to those with negative intras
  for(i in 1:(length(lui_intrinsic_positive))){
    lui_intrinsic_positive[[i]] <- lui_intrinsic_positive[[i]][which(rownames(lui_intrinsic_positive[[i]]) %in% rownames(lui_alpha[[i]])),]
  }
  
  #Obtain a list of combination of all species all combination of 3, 4, 5 and so on species until all species with positive abundance.
  #This is super high demanding PARELLEL CLUSTER. Suggestion to parallel one node per each LUI value. Therefore a total of 20 nodes, because we have 20 LUI values (see line 24)
  #combos <- list()
  #combos_full <- list()
  
  
  #combos_lui_full <- list()
  
  #do some parallel work
  library(foreach)
  library(doParallel)
  library(parallel)
  library(doSNOW)
  library(RcppAlgos)
  library(MASS)
  
  #set implicit cluster
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoSNOW(cl) #defines parallel computation 
  
  ##compute task 
  start_time <- Sys.time() # to see how much it takes to run
  
  combos<- list ()
  combos_lui <- list()
  
  # 
  combos_lui <- foreach (i= 1:length(lui_alpha)) %dopar% {
     for(j in 2:length(row.names(lui_alpha[[i]]))){
       if(RcppAlgos::comboCount(row.names(lui_alpha[[i]]),j) < 10000){
         cc2 <- RcppAlgos::comboGeneral(row.names(lui_alpha[[i]]),j)
         
       } else
         cc2 <- RcppAlgos::comboSample(row.names(lui_alpha[[i]]), j, n = 10000)
       
       combos[[j]] <- cc2
     }
     return(combos)
     gc()
   }
                                
   saveRDS(combos_lui, 'results/combos_lui_sps_reduced_10000.rds') #This is to compute it once and save it to do not compute every time we run the code.
  
  
  #for (i in 1:length(lui_alpha)) {
   #combos <- foreach(j= 1:length(row.names(lui_alpha[[i]]))) %dopar% {
   #cc <- t(combn(row.names(lui_alpha[[i]]),j))
   #if(length(cc[,1]) > 10000){
   #cc2<- cc[sample(nrow(cc),size=10000,replace=FALSE),] # If the number of combos is higher than 10.000 then perform random subsampling
   #} else {
   #cc2<-cc
   #}
   #}
   #combos_lui[[i]] <- combos
   #}

#saveRDS(combos_lui, 'new results/combos_lui_sps_reduced_10000.rds') #This is to compute it once and save it to do not compute every time we run the code.
#saveRDS(combos_lui_full, 'new results/combos_lui_sps_full.rds') Not computed because it is very high demanding
stopCluster(cl) #stop cluster

end_time <- Sys.time() #save time once finished

end_time - start_time



#2. Compute structural metrics----
#Here this part of the code, compute metrics for each of the combos under different LUI values. 
#This the part where we need PARALLEL CLUSTER AGAIN.

library(mvtnorm)

source('code/toolbox_coexistence.R')
source('code/toolbox_figure.R')


library(foreach)
library(doParallel)
library(parallel)
library(doSNOW)


cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoSNOW(cl) #defines parallel computation 
start_time <- Sys.time() # to see how much it takes to run


#We load the data first
combos_lui<-readRDS('results/combos_lui_sps_reduced_10000.rds')


# We are going to separate the database in several pieces to do not colapso with the computing

#First five LUI values  ----
combos_lui2 <- combos_lui[1:5]
results <- list()
results_coex <- list()
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")


results_coex <- foreach (i = 1:length(combos_lui2), .packages = "mvtnorm") %dopar% { 
  
  for(j in 2:length(combos_lui2[[i]])){ 
    results_combo <- matrix (nrow = nrow(combos_lui2[[i]][[j]]), ncol= length(col_results))
    row.names(results_combo) <- apply(combos_lui2[[i]][[j]], 1, paste, collapse=".") 
    colnames(results_combo) <- col_results
    
    for (h in 1:(length(combos_lui2[[i]][[j]])/j)) {
      ll <- combos_lui2[[i]][[j]][h,]
      mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
      mm <- -1*mm #this is because intras has to be positive. 
      ii <- subset(lui_intrinsic_positive[[i]], rownames(lui_intrinsic_positive[[i]]) %in% ll)
      results_combo[h,1] <- 10^Omega(mm) # this is niche differences
      results_combo[h,2] <- theta(mm, ii[,1]) # this is fitness differences
      co <- compute_overlap(mm,1000)
      results_combo[h,3] <- co$overlap # this is community overlap
      results_combo[h,4] <- co$Omega - co$Omega_all # this is community differential
      results_combo[h,5] <- test_feasibility(mm,ii[,1]) #this is whether all speceis can coexist. 
      results_combo[h,6] <- sum(test_feasibility_pairs(mm,ii[,1])$feasibility)/length(test_feasibility_pairs(mm,ii[,1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
      results_combo[h,7] <- unique(lui_intrinsic_positive[[i]][,2])
    }
    results[[j]] <- results_combo
    print(paste(i,j))
  }
  return(results)
}

saveRDS(results_coex, 'results/results_coexistence_sps_10000_4_4.rds')



#Second five LUI values  ----
combos_lui2 <- combos_lui[6:10] 
results <- list()
results_coex <- list()
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")



results_coex <- foreach (i = 1:length(combos_lui2), .packages = "mvtnorm") %dopar% { 
  
  for(j in 2:length(combos_lui2[[i]])){ 
    results_combo <- matrix (nrow = nrow(combos_lui2[[i]][[j]]), ncol= length(col_results))
    row.names(results_combo) <- apply(combos_lui2[[i]][[j]], 1, paste, collapse=".") 
    colnames(results_combo) <- col_results
    
    for (h in 1:(length(combos_lui2[[i]][[j]])/j)) {
      ll <- combos_lui2[[i]][[j]][h,]
      mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
      mm <- -1*mm #this is because intras has to be positive. 
      ii <- subset(lui_intrinsic_positive[[i]], rownames(lui_intrinsic_positive[[i]]) %in% ll)
      results_combo[h,1] <- 10^Omega(mm) # this is niche differences
      results_combo[h,2] <- theta(mm, ii[,1]) # this is fitness differences
      co <- compute_overlap(mm,1000)
      results_combo[h,3] <- co$overlap # this is community overlap
      results_combo[h,4] <- co$Omega - co$Omega_all # this is community differential
      results_combo[h,5] <- test_feasibility(mm,ii[,1]) #this is whether all speceis can coexist. 
      results_combo[h,6] <- sum(test_feasibility_pairs(mm,ii[,1])$feasibility)/length(test_feasibility_pairs(mm,ii[,1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
      results_combo[h,7] <- unique(lui_intrinsic_positive[[i]][,2])
    }
    results[[j]] <- results_combo
    print(paste(i,j))
  }
  return(results)
}

saveRDS(results_coex, 'results/results_coexistence_sps_10000_2_4.rds')


#Third five LUI values  ----
combos_lui2 <- combos_lui[11:15] 
results <- list()
results_coex <- list()
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")



results_coex <- foreach (i = 1:length(combos_lui2), .packages = "mvtnorm") %dopar% { 
  
  for(j in 2:length(combos_lui2[[i]])){ 
    results_combo <- matrix (nrow = nrow(combos_lui2[[i]][[j]]), ncol= length(col_results))
    row.names(results_combo) <- apply(combos_lui2[[i]][[j]], 1, paste, collapse=".") 
    colnames(results_combo) <- col_results
    
    for (h in 1:(length(combos_lui2[[i]][[j]])/j)) {
      ll <- combos_lui2[[i]][[j]][h,]
      mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
      mm <- -1*mm #this is because intras has to be positive. 
      ii <- subset(lui_intrinsic_positive[[i]], rownames(lui_intrinsic_positive[[i]]) %in% ll)
      results_combo[h,1] <- 10^Omega(mm) # this is niche differences
      results_combo[h,2] <- theta(mm, ii[,1]) # this is fitness differences
      co <- compute_overlap(mm,1000)
      results_combo[h,3] <- co$overlap # this is community overlap
      results_combo[h,4] <- co$Omega - co$Omega_all # this is community differential
      results_combo[h,5] <- test_feasibility(mm,ii[,1]) #this is whether all speceis can coexist. 
      results_combo[h,6] <- sum(test_feasibility_pairs(mm,ii[,1])$feasibility)/length(test_feasibility_pairs(mm,ii[,1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
      results_combo[h,7] <- unique(lui_intrinsic_positive[[i]][,2])
    }
    results[[j]] <- results_combo
    print(paste(i,j))
  }
  return(results)
}

saveRDS(results_coex, 'results/results_coexistence_sps_10000_3_4.rds')


#Forth five LUI values  ----
combos_lui2 <- combos_lui[16:17]
lui_alpha <- lui_alpha[16:17]
lui_intrinsic_positive <- lui_intrinsic_positive[16:20]
results <- list()
results_coex <- list()
col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")



results_coex <- foreach (i = 1:length(combos_lui2), .packages = "mvtnorm") %dopar% { 
  
  for(j in 2:length(combos_lui2[[i]])){ 
    results_combo <- matrix (nrow = nrow(combos_lui2[[i]][[j]]), ncol= length(col_results))
    row.names(results_combo) <- apply(combos_lui2[[i]][[j]], 1, paste, collapse=".") 
    colnames(results_combo) <- col_results
    
    for (h in 1:(length(combos_lui2[[i]][[j]])/j)) {
      ll <- combos_lui2[[i]][[j]][h,]
      mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
      mm <- -1*mm #this is because intras has to be positive. 
      ii <- subset(lui_intrinsic_positive[[i]], rownames(lui_intrinsic_positive[[i]]) %in% ll)
      results_combo[h,1] <- 10^Omega(mm) # this is niche differences
      results_combo[h,2] <- theta(mm, ii[,1]) # this is fitness differences
      co <- compute_overlap(mm,1000)
      results_combo[h,3] <- co$overlap # this is community overlap
      results_combo[h,4] <- co$Omega - co$Omega_all # this is community differential
      results_combo[h,5] <- test_feasibility(mm,ii[,1]) #this is whether all speceis can coexist. 
      results_combo[h,6] <- sum(test_feasibility_pairs(mm,ii[,1])$feasibility)/length(test_feasibility_pairs(mm,ii[,1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
      results_combo[h,7] <- unique(lui_intrinsic_positive[[i]][,2])
    }
    results[[j]] <- results_combo
    print(paste(i,j))
  }
  return(results)
}

saveRDS(results_coex, 'results/results_coexistence_sps_10000_4_4.rds')



stopCluster(cl) #stop cluster
end_time <- Sys.time() #save time once finished. 
end_time - start_time


#results_coex <- for(i in 1:length(combos_lui)) { 
  
  #for(j in 2:length(combos_lui[[i]])){ 
    #results_combo <- matrix (nrow = nrow(combos_lui[[i]][[j]]), ncol= length(col_results))
    #row.names(results_combo) <- apply(combos_lui[[i]][[j]], 1, paste, collapse=".") 
    #colnames(results_combo) <- col_results
    
    #foreach (h=1:length(combos_lui[[i]][[j]])/j) %dopar% {
      #ll <- combos_lui[[i]][[j]][h,]
      #mm <- lui_alpha[[i]][which(rownames(lui_alpha[[i]]) %in% ll), which(colnames(lui_alpha[[i]]) %in% ll)]
      #mm <- -1*mm #this is because intras has to be positive. 
      #ii <- subset(lui_intrinsic_positive[[i]], rownames(lui_intrinsic_positive[[i]]) %in% ll)
      #results_combo[h,1] <- 10^Omega(mm) # this is niche differences
      #results_combo[h,2] <- theta(mm, ii[,1]) # this is fitness differences
      #co <- compute_overlap(mm,1000)
      #results_combo[h,3] <- co$overlap # this is community overlap
      #results_combo[h,4] <- co$Omega - co$Omega_all # this is community differential
      #results_combo[h,5] <- test_feasibility(mm,ii[,1]) #this is whether all speceis can coexist. 
      #results_combo[h,6] <- sum(test_feasibility_pairs(mm,ii[,1])$feasibility)/length(test_feasibility_pairs(mm,ii[,1])$feasibility) # this is whether pairs do not coexist but the multispecies assemblage does
      #results_combo[h,7] <- unique(lui_intrinsic_positive[[i]][,2])
      #}
    #results[[j]] <- results_combo
    #}
  #results_coex[[i]] <- results
  #}

#saveRDS(results_coex, 'new results/results_coexistence_sps.rds')





