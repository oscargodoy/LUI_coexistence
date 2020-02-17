#Cluster 7

#function to select the combinations above a certain threshold of presence
selector <- function(df, n, threshold){
  
  #vector with the names of all the species
  vec <- colnames(df)
  
  #define a vector with the maximum values different elements can reach
  maxim <- vec[(length(vec) - (n - 1)):length(vec)]
  
  #data frame to save the selected combinations
  sel_comb <- data.frame()
  
  #vector to save the combination to look for at each moment
  combo <- vec[1:n]
  
  #counter of times the combo is present
  counter <- 0
  
  #starting with the first combo
  for (w in 1:nrow(df)){
    
    #if all the species of the combo are present at a given spot
    if(isFALSE(0 %in% (df[w, which((vec %in% combo) == TRUE)]))){
      counter <- counter + 1 #add one to te counter
    }
  }
  
  #if the presence overcomes a certain threshold, in %, the combos is saved
  if ((counter / w * 100) > threshold){
    sel_comb <- rbind(sel_comb, cbind(t(combo), (counter / w * 100)))
  }
  
  
  #loop i starting at row two for all the rows
  i = 2 #define the starting point
  while (i != choose(length(vec), n)){
    
    #loop j for the elements of the combo vector, starting at position 2
    for (j in 1:n){
      
      #conditions to match
      if (combo[j] != maxim[j]){ #while the value is not the maximum
        
        if (j != n){ #if the position is not the last one yet
          #do nothing, continue to the next element
          
        } else { #if the element is, in fact, the last one
          combo[j] <- vec[which(combo[j] == vec) + 1] #write the following corresp. value
        }
        
      } else { #if it is actually the maximum value for that element
        #write the following corresponding values from the previous position to the end
        combo[(j - 1):n] <- vec[(which(combo[j - 1] == vec) + 1):
                                  ((which(combo[j - 1] == vec) + 1) + ((n - j) + 1))]
        break #enough - changes have been made
      }
    } #end j
    
    #counter of times the combo is present
    counter <- 0
    
    #continuing with all the other combos
    for (w in 1:nrow(df)){
      
      #if all the species of the combo are present at a given spot
      if(isFALSE(0 %in%(df[w, which((vec %in% combo) == TRUE)]))){
        counter <- counter + 1 #add one to te counter
      }
    }
    
    #if the presence overcomes a certain threshold, in %, the combos is saved
    if ((counter / w * 100) > threshold){
      sel_comb <- rbind(sel_comb, cbind(t(combo), (counter / w * 100)))
    }
    
    #next step
    i <- i + 1
    
  } #end i while
  return(sel_comb) #return the final df with all saved combinations
} #end selector()

#load data
p_plant_only <- read.table("p_plant_only.txt", header = TRUE, sep = "\t")

#compute the combos
combos7 <- selector(df = p_plant_only, n = 7, threshold = 0)

#save results
write.table(combos7, "combos7.txt", sep = "\t", row.names = FALSE)

#end