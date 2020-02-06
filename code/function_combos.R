#attempt
n <- 3 #combination of n species
vec <- c("a", "b", "c", "d", "e") #set of species


#function to match all species combinations
combinator <- function(vec, n){
  
  #create an output matrix with nrow = number of combos & ncol = n
  M <- matrix(nrow = choose(length(vec), n), ncol = n)
  
  #set the first row of the matrix to the first combination
  M[1, 1:n] <- vec[1:n]
  
  #define a vector with the maximum values different columns can reach
  maxim <- vec[(length(vec) - (n - 1)):length(vec)]
  
  #loop i starting at row two for all the rows
  for (i in 2:nrow(M)){
    
    #copy the same values as in the previous row in order to check them
    M[i, 1:n] <- M[(i - 1), 1:n]
    
    #loop j for the columns, starting at column 2 (ncol(M) == n)
    for (j in 1:n){
      
      #conditions to match
      if (M[i, j] != maxim[j]){ #while the value is not the maximum
        if (j != n){ #if the column is not the last one yet
          #do nothing, continue to the next column
        } else { #if the column is, in fact, the last one
          M[i, j] <- vec[which(M[i, j] == vec) + 1] #write the following corresponding value
        }
      } else { #if it is actually the maximum value for that column
        M[i, (j - 1):n] <- vec[(which(M[i, (j - 1)] == vec) + 1):((which(M[i, (j - 1)] == vec) + 1) + ((n - j) + 1))] #write the following corresponding values from the previous columns to the end
        break #go to the following row
      }
    } #end j
  } #end i
  return(M) #return the matrix with all the combinations
} #end combinator()




#function to select the combinations above a certain threshold of presence
selector <- function(df, n, threshold){
  
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
    if(isFALSE(0 %in% (df[w, which((vec %in% combo) == TRUE)]))){
      counter <- counter + 1
    }
  }
  
  #if the presence overcomes a certain threshold, in %, the combos is saved
  if ((counter / w * 100) >= threshold){
    sel_comb <- rbind(sel_comb, cbind(t(combo), (counter / w * 100)))
  }
  
  
  #loop i starting at row two for all the rows
  i = 2 #define the starting point
  while (i != choose(length(vec), n)){
    
    #loop j for the elements of the combo vector, starting at position 2 (length(combo) == n)
    for (j in 1:n){
      
      #conditions to match
      if (combo[j] != maxim[j]){ #while the value is not the maximum
        if (j != n){ #if the position is not the last one yet
          #do nothing, continue to the next element
        } else { #if the element is, in fact, the last one
          combo[j] <- vec[which(combo[j] == vec) + 1] #write the following corresponding value
        }
      } else { #if it is actually the maximum value for that column
        combo[(j - 1):n] <- vec[(which(combo[j - 1] == vec) + 1):((which(combo[j - 1] == vec) + 1) + ((n - j) + 1))] #write the following corresponding values from the previous columns to the end
        break #enough - changes have been made
      }
    } #end j
    
    #counter of times the combo is present
    counter <- 0
    
    #starting with the first combo
    for (w in 1:nrow(df)){
      if(isFALSE(0 %in%(df[w, which((vec %in% combo) == TRUE)]))){
        counter <- counter + 1
      }
    }
    
    #if the presence overcomes a certain threshold, in %, the combos is saved
    if ((counter / w * 100) >= threshold){
      sel_comb <- rbind(sel_comb, cbind(t(combo), (counter / w * 100)))
    }
    
    #next step
    i <- i + 1
    
  } #end i while
  return(sel_comb) #return the final df with all saved combinations
} #end selector()


#attempt
n <- 2 #combination of n species
threshold <- 50
df <- spp_LUI[4:53]

pairs50 <- selector(df, n, threshold)












