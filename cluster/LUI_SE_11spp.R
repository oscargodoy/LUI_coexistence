#cluster run structural coexistence --- combos of 5 species

#load the alpha and intrinsic
load(file = "LUI_effects.RData")

#load the combos
combos_lui <- readRDS("combos11.rds")


### LOAD PACKAGES
library(mvtnorm)
library(foreach)
library(doParallel)
library(parallel)
library(MASS)
library(doSNOW)

#input parameters:
#alpha = competition strenght matrix 
#r = vector of intrinsic growth rates

#structural niche difference (output on a log scale)
Omega <- function(alpha){
  n <- nrow(alpha)
  Sigma <-solve(t(alpha) %*% alpha, tol = 1e-40)
  d <- pmvnorm(lower = rep(0,n), upper = rep(Inf,n), mean = rep(0,n), sigma = Sigma)
  out <- log10(d[1]) + n * log10(2)
  return(out) 
}

#vector defining the centroid of the feasibility domain
r_centroid <- function(alpha){
  n <- nrow(alpha)
  D <- diag(1/sqrt(diag(t(alpha)%*%alpha)))
  alpha_n <- alpha %*% D
  r_c <- rowSums(alpha_n) /n 
  r_c <- t(t(r_c))
  return(r_c)
}


#structural fitness difference (in degree)
theta <- function(alpha,r){
  r_c <- r_centroid(alpha)
  out <- acos(sum(r_c*r)/(sqrt(sum(r^2))*sqrt(sum(r_c^2))))*180/pi
  return(out)
}


#test if a system (alpha and r) is feasible (output 1 = feasible, 0 = not feasible)
test_feasibility <- function(alpha,r){
  out <- prod(solve(alpha,r)>0)
  return(out)
}


#test which pairs in a system (alpha and r) are feasible (output 1 = feasible, 0 = not feasible)
test_feasibility_pairs <- function(alpha,r){
  n <- length(r)
  c <- combn(n,2)
  nc <- dim(c)[2]
  f <- rep(NA,nc)
  for (i in 1:nc){
    f[i] <- prod(solve(alpha[c[,i],c[,i]],r[c[,i]])>0)
  }
  out <- list(pairs = c, feasibility = f)
  return(out)
}


#compute the feasiblity domain, the feasibility domain of all pairs, and their overlap (Nrand = number of randomization)
compute_overlap <- function(alpha,Nrand){
  
  n <- dim(alpha)[1]
  
  counter_f <- 0
  counter_overlap <- 0
  counter_all <- 0
  
  for (i in 1:Nrand){
    
    r_rand <- abs(rnorm(n))  
    r_rand <- r_rand/sqrt(sum(r_rand^2))
    
    f1 <- test_feasibility(alpha,r_rand)  
    f2 <- test_feasibility_pairs(alpha,r_rand)$feasibility  
    
    counter_f <- counter_f + f1
    counter_all <- counter_all + prod(f2)
    counter_overlap <- counter_overlap + f1*prod(f2)
    
  }
  
  Omega <- counter_f/Nrand
  Omega_all <- counter_all/Nrand
  overlap <- counter_overlap/Nrand
  
  out <- list(Omega = Omega, Omega_all = Omega_all, overlap = overlap)
  return(out)
  
}


#function to compute the structural coexistence given a certan set of combos
str_coex_combos <- function(alpha, intrinsic, combos){
  
  combos[[i]] <- as.matrix(combos[[i]])
  
  col_results  <- c("omega", "theta", "overlap", "differential", "feasibility", "feasibility_pair_to_all", "lui")
  
  results_combo <- matrix (nrow = nrow(combos[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos[[i]], 1, paste, collapse=".") 
  colnames(results_combo) <- col_results
  
  for (j in 1:(nrow(combos[[i]]))){
    ll <- combos[[i]][j,]
    mm <- alpha[[i]][which(rownames(alpha[[i]]) %in% ll), which(colnames(alpha[[i]]) %in% ll)]
    mm <- -1 * mm #this is because intras has to be positive
    
    ii <- subset(intrinsic[[i]], rownames(intrinsic[[i]]) %in% ll)
    #this is niche differences
    results_combo[j, 1] <- 10^Omega(mm)
    #this is fitness differences
    results_combo[j, 2] <- theta(mm, ii[, 1])
    co <- compute_overlap(mm, 1000)
    #this is community overlap
    results_combo[j, 3] <- co$overlap
    #this is community differential
    results_combo[j, 4] <- co$Omega - co$Omega_all
    #this is whether all speceis can coexist
    results_combo[j, 5] <- test_feasibility(mm, ii[, 1]) 
    #this is whether pairs do not coexist but the multispecies assemblage does
    results_combo[j, 6] <- sum(test_feasibility_pairs(mm, ii[, 1])$feasibility) /
      length(test_feasibility_pairs(mm, ii[, 1])$feasibility)
    results_combo[j, 7] <- unique(intrinsic[[i]][, 2])
  }
  return(results_combo)
}


#parallel
number_of_cores <- as.numeric(Sys.getenv("OMP_NUM_THREADS"))
cluster <- makeCluster(number_of_cores)
registerDoParallel(cluster)


#compute combinations of 3 species
results_coex <- list()
results_coex <- foreach (i = 1:length(combos_lui), .packages = "mvtnorm") %dopar% {
  str_coex_combos(alpha = lui_alpha, intrinsic = lui_intrinsic_positive, combos = combos_lui)
}

#re-structure
coex <- data.frame()
for(i in 1:length(results_coex)){
  a <- data.frame("combos" = rownames(results_coex[[i]]),
                  "SND" = results_coex[[i]][, 1],
                  "SFD" = results_coex[[i]][,2],
                  "overlap" = results_coex[[i]][, 3],
                  "differential" = results_coex[[i]][, 4],
                  "feasibility" = results_coex[[i]][, 5],
                  "pair_to_all" = results_coex[[i]][, 6],
                  "LUI" = results_coex[[i]][, 7])
  coex <- rbind(coex, a)
}

#WRITE
write.csv(coex, file = "results-LUI_coexistence-11spp.csv", row.names = FALSE)

#stop parallel computing (cluster)
stopCluster(cluster)
