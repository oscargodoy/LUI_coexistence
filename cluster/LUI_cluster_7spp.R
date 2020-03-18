### Script for coexistence depending on LUI --- 7 species
### Cluster version

### LOAD DATA
load("cluster_LUI.RData")

#change name depending of the combos' richness level
combos <- lui_combos7
richness <- 7
path <- "coex_LUI_7spp.txt"


### LOAD PACKAGES
library(mvtnorm)
library(foreach)
library(doParallel)
library(parallel)
library(MASS)


### DEFINE FUNCTIONS

#structural niche difference (output on a log scale)
Omega <- function(alpha){
  n <- nrow(alpha)
  Sigma <- solve(t(alpha) %*% alpha, tol = 1e-20) #tolerance modified due to singularities
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
  out <- prod(solve(alpha, r) > 0)
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

#all possible combinations! MODIFIED TO INCLUDE PRE-DEFINED COMBOS!
structural_coex <- function(alpha, intrinsic, n, combos, treatment){
  
  alpha <- as.matrix(alpha)
  intrinsic <- as.matrix(intrinsic)
  combos <- as.matrix(combos)
  rownames(combos) <- NULL; colnames(combos) <- NULL
  
  pair_names <- apply(combn(n, 2), 2, paste, collapse = "_") #name all the possible pairs
  
  #combos <- t(combn(rownames(alpha), n)) #matrix with all possible combinations of n species
  
  results_combos <- matrix(nrow = dim(combos)[1], ncol = (5 + ncol(combn(n, 2)) + 1))
  row.names(results_combos) <- apply(combos, 1, paste, collapse = "_") #change the 'collapse' feature if needed
  colnames(results_combos) <- c("Omega", "theta", "differential", "overlap", "feasibility", pair_names, "coex_rate", "treatment")
  
  for(i in 1:nrow(combos)){
    zeroes <- matrix(data = 0, nrow = n, ncol = n)
    intrinsic2 <- as.matrix(subset(intrinsic, rownames(intrinsic) %in% combos[i,]))
    alpha2 <- as.matrix(alpha[combos[i,], combos[i,]])
    
    if(isFALSE(FALSE %in% (alpha2 == zeroes))){
      results_combos[i, ] <- NA
    } else {
      #omega
      results_combos[i, 1] <- 10^Omega(alpha2)
      #theta
      results_combos[i, 2] <- theta(alpha2, intrinsic2)
      #differential and overlap
      y <- compute_overlap(alpha2, 10000)
      results_combos[i, 3] <- y$Omega - y$Omega_all
      results_combos[i, 4] <- y$overlap
      #feasibility (all)
      results_combos[i, 5] <- test_feasibility(alpha2, intrinsic2)
      x <- test_feasibility_pairs(alpha2, intrinsic2)
      #feasibility (pair by pair)
      results_combos[i, 6:(5 + ncol(combn(n, 2)))] <- x$feasibility
      #coexistence rate
      results_combos[i, (ncol(results_combos) - 1)] <- sum(x$feasibility) / ncol(combn(n, 2))
      #treatment
      results_combos[i, ncol(results_combos)] <- treatment
    }
    
  }
  results_combos <- as.data.frame(results_combos)
  
  results_combos$combos <- rownames(results_combos)
  
  return(results_combos)
}


#set parallel computing (cluster)
number_of_cores <- as.numeric(Sys.getenv("OMP_NUM_THREADS"))
cluster <- makeCluster(number_of_cores)
registerDoParallel(cluster)

#compute structural coexistence and shape results' dataframe
alpha <- lui_alpha
intrinsic <- list()
for (i in 1:length(lui_intrinsic_positive)){
  intrinsic[[i]] <- lui_intrinsic_positive[[i]][, 1]
}
lui <- seq(from = 0.5, to = 3, by = 0.25)

#compute
results <- foreach(i = 1:length(alpha), .combine = rbind, .packages = 'mvtnorm') %dopar% {
  structural_coex(alpha = alpha[[i]], intrinsic = intrinsic[[i]], n = richness, combos = combos[[i]], treatment = lui[i])
}

#stop parallel computing (cluster)
stopCluster(cluster)

#save, just in case
write.table(results, path, row.names = FALSE, sep = "\t")


#end of the script