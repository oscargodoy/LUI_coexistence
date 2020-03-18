#save again for the cluster
load("cluster_combos.RData")

#define new lists to save the combos
lui_combos2 <- list()
lui_combos3 <- list()
lui_combos5 <- list()
lui_combos7 <- list()
lui_combos11 <- list()
lui_combos17 <- list()

#loops to select all the combos based on species interactions under LUI effects

#combinations of 2 species
for (i in 1:length(lui_alpha)){
  dd <- data.frame()
  for (j in 1:nrow(combos2)){
    include <- TRUE
    for (k in 1:ncol(combos2)){
      if (combos2[j, k] %in% rownames(lui_alpha[[i]])){
        #keep include as TRUE
      } else {
        include <- FALSE
      }
    }
    if (isTRUE(include)){
      dd <- rbind(dd, combos2[j, ])
    }
  }
  lui_combos2[[i]] <- dd
}

#combinations of 3 species
for (i in 1:length(lui_alpha)){
  dd <- data.frame()
  for (j in 1:nrow(combos3)){
    include <- TRUE
    for (k in 1:ncol(combos3)){
      if (combos3[j, k] %in% rownames(lui_alpha[[i]])){
        #keep include as TRUE
      } else {
        include <- FALSE
      }
    }
    if (isTRUE(include)){
      dd <- rbind(dd, combos3[j, ])
    }
  }
  lui_combos3[[i]] <- dd
}

#combinations of 5 species
for (i in 1:length(lui_alpha)){
  dd <- data.frame()
  for (j in 1:nrow(combos5)){
    include <- TRUE
    for (k in 1:ncol(combos5)){
      if (combos5[j, k] %in% rownames(lui_alpha[[i]])){
        #keep include as TRUE
      } else {
        include <- FALSE
      }
    }
    if (isTRUE(include)){
      dd <- rbind(dd, combos5[j, ])
    }
  }
  lui_combos5[[i]] <- dd
}

#combinations of 7 species
for (i in 1:length(lui_alpha)){
  dd <- data.frame()
  for (j in 1:nrow(combos7)){
    include <- TRUE
    for (k in 1:ncol(combos7)){
      if (combos7[j, k] %in% rownames(lui_alpha[[i]])){
        #keep include as TRUE
      } else {
        include <- FALSE
      }
    }
    if (isTRUE(include)){
      dd <- rbind(dd, combos7[j, ])
    }
  }
  lui_combos7[[i]] <- dd
}

#combinations of 11 species
for (i in 1:length(lui_alpha)){
  dd <- data.frame()
  for (j in 1:nrow(combos11)){
    include <- TRUE
    for (k in 1:ncol(combos11)){
      if (combos11[j, k] %in% rownames(lui_alpha[[i]])){
        #keep include as TRUE
      } else {
        include <- FALSE
      }
    }
    if (isTRUE(include)){
      dd <- rbind(dd, combos11[j, ])
    }
  }
  lui_combos11[[i]] <- dd
}

#combinations of 17 species
for (i in 1:length(lui_alpha)){
  dd <- data.frame()
  for (j in 1:nrow(combos17)){
    include <- TRUE
    for (k in 1:ncol(combos17)){
      if (combos17[j, k] %in% rownames(lui_alpha[[i]])){
        #keep include as TRUE
      } else {
        include <- FALSE
      }
    }
    if (isTRUE(include)){
      dd <- rbind(dd, combos17[j, ])
    }
  }
  lui_combos17[[i]] <- dd
}

#write
#keep only what is needed
rm(list=ls()[! ls() %in% c("lui_alpha", "lui_combos2", "lui_combos3",
                           "lui_combos5", "lui_combos7", "lui_combos11",
                           "lui_combos17", "lui_intrinsic_positive")])

#create image
save.image(file = "LUI_combos.RData")