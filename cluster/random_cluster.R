#run random results in the cluster for 2 and 3 species

#load all needed elements
load(file = "cluster/random_cluster.RData")

### LOAD PACKAGES
library(mvtnorm)
library(foreach)
library(doParallel)
library(parallel)
library(MASS)

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


#function to compute the structural coexistence given a certan set of combos
str_coex_combos <- function(alpha, intrinsic, combos){
  
  combos[[i]] <- as.matrix(combos[[i]])
  
  col_results  <- c("omega", "theta", "feasibility", "LUI")
  
  results_combo <- matrix (nrow = nrow(combos[[i]]), ncol = length(col_results))
  row.names(results_combo) <- apply(combos[[i]], 1, paste, collapse=".") 
  colnames(results_combo) <- col_results
  
  for (j in 1:(nrow(combos[[i]]))){
    ll <- combos[[i]][j,]
    mm <- alpha[[i]][which(rownames(alpha[[i]]) %in% ll), which(colnames(alpha[[i]]) %in% ll)]
    mm <- -1 * mm #this is because intras has to be positive
    
    ii <- subset(intrinsic[[i]], rownames(intrinsic[[i]]) %in% ll)
    #this is niche differences
    results_combo[j, 1] <- tryCatch({10^Omega(mm)}, error = function(e){NA})
    #this is fitness differences
    results_combo[j, 2] <- tryCatch({theta(mm, ii[, 1])}, error = function(e){NA})
    #this is whether all speceis can coexist
    results_combo[j, 3] <- tryCatch({test_feasibility(mm, ii[, 1])}, error = function(e){NA})
    #LUI value
    results_combo[j, 4] <- tryCatch({unique(intrinsic[[i]][, 2])}, error = function(e){NA})
  }
  return(results_combo)
}


#parallel
number_of_cores <- as.numeric(Sys.getenv("OMP_NUM_THREADS"))
cluster <- makeCluster(number_of_cores)
registerDoParallel(cluster)


#compute combinations of 2 species
results_coex <- list()
results_coex <- foreach (i = 1:length(combos2), .packages = "mvtnorm") %dopar% {
  str_coex_combos(alpha = alpha, intrinsic = intrinsic, combos = combos2)
}

#re-structure
richness <- 2
coex <- data.frame()
for(i in 1:length(results_coex)){
  a <- data.frame("richness" = richness, "combos" = rownames(results_coex[[i]]),
                  "SND" = results_coex[[i]][, 1],
                  "SFD" = results_coex[[i]][,2],
                  "feasibility" = results_coex[[i]][, 3],
                  "LUI" = results_coex[[i]][, 4])
  coex <- rbind(coex, a)
}


#compute combinations of 3 species
results_coex <- list()
results_coex <- foreach (i = 1:length(combos3), .packages = "mvtnorm") %dopar% {
  str_coex_combos(alpha = alpha, intrinsic = intrinsic, combos = combos3)
}

#re-structure
richness <- 3
for(i in 1:length(results_coex)){
  a <- data.frame("richness" = richness, "combos" = rownames(results_coex[[i]]),
                  "SND" = results_coex[[i]][, 1],
                  "SFD" = results_coex[[i]][,2],
                  "feasibility" = results_coex[[i]][, 3],
                  "LUI" = results_coex[[i]][, 4])
  coex <- rbind(coex, a)
}


#WRITE
write.csv(coex, file = "random_results_LUI.csv", row.names = FALSE)

#stop parallel computing (cluster)
stopCluster(cluster)
