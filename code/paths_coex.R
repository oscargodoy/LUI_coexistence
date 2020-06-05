#this loads the data for 2 and 3 species combinations
coex2 <- read.csv(file = "results/results-LUI_coexistence-2spp.csv")
coex2$feasibility <- as.factor(coex2$feasibility)

coex3 <- read.csv(file = "results/results-LUI_coexistence-3spp.csv")
coex3$feasibility <- as.factor(coex3$feasibility)

#load random results (takes time)
random <- read.csv("https://www.dropbox.com/s/3m94ec4kc5bq1uo/random_results_LUI.csv?dl=1")
colnames(random)[2] <- "species"