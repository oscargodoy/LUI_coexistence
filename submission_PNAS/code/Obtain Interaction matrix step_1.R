###Step 1 Obtaining interaction coefficients and its variation across the LUI gradient
### Data is log transformed of yeat t+1/t in order to linearize the LV model on its discrete form.
rm(list = ls())
library(reshape2)
library(nlme)
library(car)
library(SciViews)

#load plant data
plants <- read.csv("submission_PNAS/data/raw_data/BE.plants08.16.csv", header = TRUE)

#load LUI data
lui <- read.csv("submission_PNAS/data/raw_data/LUI06_15.csv", header  = TRUE)

#stick with LUI at different years
lui.only <- lui[, grep("LUI", names(lui))]
lui.only2 <- lui.only[, -c(1:2)] ## remove 2006 and 2007

#leave only plant species' columns
plant.only <- plants[-c(1:4)]

### top50 --- select the 51 most common plant species
top50 <- rev(sort(apply(plants[,-c(1:5)], 2, mean,na.rm=T)))[1:51] 
top50.short <- c("Poa_tri", "Poa_pra", "Alo_pra", "Dac_glo", "Tri_rep", "Tar_off", "Lol_per", "Arr_ela", 
                 "Fes_rub", "Fes_pra", "Tri_fla", "Ely_rep", "Tri_pra", "Bro_ere", "Ran_rep", "Bro_hor", 
                 "Ran_acr", "Pla_lan","Ach_mil", "Gal_mol", "Her_sph", "Ant_syl", "Hol_lan", "Hel_pub",
                 "Ant_odo", "Bra_pin", "Car_hir", "Ver_cha", "Rum_ace", "Fes_ovi", "Phl_pra", "Pha_aru",
                 "Des_ces", "Agr_sto", "Cyn_cri", "Cir_ole", "Cer_hol", "Pla_med", "Cre_bie", "Urt_dio",
                 "Thy_pul", "Lol_mul", "Cir_arv", "Lot_cor", "Ran_bul", "Tri_dub", "Med_lup", "Leo_his",
                 "Car_car", "Vic_sep", "Pru_sp")

plants2 <- plants[,match(names(top50), names(plants))]

Rest <- apply(plant.only[, -match(names(top50), names(plant.only))], 1, sum,na.rm=T)

plants3 <- cbind(plants2, Rest)
plants3 <- plants3/apply(plants3,1,sum) # to rscale to no more than 100%
names(plants3)[1:51] <- top50.short


pyear <- split(plants3, plants$Year) #create a different dataset within a list for each year
pchange <- list()
pchange.logit <- list()
pchange.log <- list()
pchange.relative <- list()


# This is to prepare the database in order to conduct the analyses where t+1/t (y axis) will be compared to t (x axis)
for(i in 1:(length(pyear) - 1)){
  xx <- pyear[[i + 1]]
  names(xx) <- paste(names(xx), "_delta", sep = "")
  xx2 <- cbind("LUI" = lui.only2[, i], xx)
  pchange[[i]] <- cbind(xx2, pyear[[i]])
}

pchange.all <- do.call("rbind", pchange)
Year_change <- paste(2008:2015, 2009:2016, sep = "to")
pchange.all2 <- data.frame("Plot" = rep(unique(plants$Plot), 8), "Site" = strtrim(unique(plants$Plot), 1), "Year_change" = rep(Year_change, each = 150), "Yeart" = rep(1:8, each = 150), pchange.all)
# to calculate the log(Nt+1/Nt)
pchange.all2[,c(110:161)] <- log((pchange.all2[6:57]+0.01)/(pchange.all2[58:109]+0.01)) 
#The addition of 0.01 allows the model to converge without changing the distribution of point and barely changing the upper and lower bounds.
#adding more magnitude (0.1) reduces the limits of the y axis (-4 to 3), and adding less magnitude increases the limits (-6 to 6). Current limits without any addition (-4 to 4)

pchange.all2=do.call(data.frame, lapply
              (pchange.all2, function(value) replace
                (value, is.infinite(value),NA)))

hh <- names(pchange.all2)[grep("delta.1", names(pchange.all2))]
yy <- names(pchange.all2[,c(6:57)])
hh2 <-names(pchange.all2[,c(58:108)])


##Let's plot an example to see how the data looks like intraspecific versus interspecific
par(mfrow=c(2, 2))
plot(pchange.all2$Poa_pra, pchange.all2$Poa_pra_delta.1, xlab = "Cover Poa_pratensis_year_t",
     ylab = "Cover Poa_pratensis_yeart+1/year_t", main="Intraspecific")
plot(pchange.all2$Poa_tri, pchange.all2$Poa_pra_delta.1, xlab = "Cover Poa_trivialis_year_t",
     ylab = "Cover Poa_pratensis_yeart+1/year_t", main="Interspecific")

plot(pchange.all2$Dac_glo, pchange.all2$Dac_glo_delta.1, xlab = "Cover Dactilis_glomerata_year_t",
     ylab = "Cover Dactilis_glomerata_yeart+1/year_t", main="Intraspecific")
plot(pchange.all2$Dac_glo, pchange.all2$Alo_pra_delta.1, xlab = "Cover Dactilis_glomerata_year_t",
     ylab = "Cover Alo_pra_yeart+1/year_t", main="Interspecific")

#perform the modelling with lme and temporal autocorrelation
lCtr <- lmeControl(maxIter = 500, msMaxIter = 500, tolerance = 1e-6, niterEM = 250, msMaxEval = 200)

##running all the models for the 51 sps. Site alone included as random factor does not improve the model, nor temporal autocorrelation with longer lag times (t-2, t-3)
mlist <- list()
for(i in c(1:51)){
  mlist[[i]] <- lme(as.formula(paste(hh[i], "~ LUI*(", paste(hh2, collapse = "+"),")")), data = pchange.all2, 
                    random = ~1|Plot/Site, control = lCtr, correlation = corAR1(form = ~Yeart), method = 'REML', na.action = na.omit)
}

##AIC
for (i in 1:51){
  print(AIC(mlist[[i]]))
}

coef.list <- lapply(mlist, function(x)summary(x)$coef$fixed)

inter.mat <- matrix(nrow = length(yy) - 1, ncol = length(yy) - 1) #matrix of species interactions.
lui.mat <- matrix(nrow = length(yy) - 1, ncol = length(yy) - 1) #matrix of how LUI modify pairwise interactions.
intrinsic.site.lui <- matrix(nrow = length(yy) - 1, ncol = 2) #matrix of intrinsic ability to growth and how LUI modifies it. 

for(i in 1:length(coef.list)){
  cc <- coef.list[[i]] ## extract coefficients
  
  cc2 <- cc[-c(1:2)]
  cc3 <- cc2[c(1:51)]
  cc4 <- cc2[52:102]
  cc5 <- cc[1:2]
  
  inter.mat[i,] <- cc3
  lui.mat[i,] <- cc4
  intrinsic.site.lui[i,] <- cc5
}

row.names(inter.mat) <- hh2
colnames(inter.mat) <- hh2
row.names(lui.mat) <- hh2
colnames(lui.mat) <- hh2
row.names(intrinsic.site.lui) <- hh2
colnames(intrinsic.site.lui) <- c("Intrinsic", "LUI")


#Save all these matrices and then go to Step 2 to calculate structural stability metrics. 

write.csv(inter.mat, "submission_PNAS/results/interaction_matrix_lme_average_50.csv")
write.csv(lui.mat, "submission_PNAS/results/lui_matrix_lme_average_50.csv")
write.csv(intrinsic.site.lui, "submission_PNAS/results/intrinsic_site_lui_average_lme_50.csv")

#We also calculate errors----
coef.list.error <- lapply(mlist, function(x)summary(x)$tTable[, 2]) #column two correspond to std error

#the miscelaneous species "REST" is not saved
inter.mat.error <- matrix(nrow = length(yy) - 1, ncol = length(yy) - 1)
lui.mat.error <- matrix(nrow = length(yy) - 1, ncol = length(yy) - 1)
intrinsic.site.lui.error <- matrix(nrow = length(yy) - 1, ncol = 2)

for(i in 1:length(coef.list.error)){
  cc <- coef.list.error[[i]] ## extract coefficients
  
  cc2 <- cc[-c(1:2)]
  cc3 <- cc2[c(1:51)]
  cc4 <- cc2[52:102]
  cc5 <- cc[1:2]
  
  inter.mat.error[i,] <- cc3
  lui.mat.error[i,] <- cc4
  intrinsic.site.lui.error[i,] <- cc5
}

row.names(inter.mat.error) <- hh2
colnames(inter.mat.error) <- hh2
row.names(lui.mat.error) <- hh2
colnames(lui.mat.error) <- hh2
row.names(intrinsic.site.lui.error) <- hh2
colnames(intrinsic.site.lui.error) <- c("Intrinsic", "LUI")

write.csv(inter.mat.error, "submission_PNAS/results/interaction_matrix_lme_std_error_50.csv")
write.csv(lui.mat.error, "submission_PNAS/results/lui_matrix_lme_std_error_50.csv")
write.csv(intrinsic.site.lui.error, "submission_PNAS/results/intrinsic_site_lui_std_error_lme_50.csv")

