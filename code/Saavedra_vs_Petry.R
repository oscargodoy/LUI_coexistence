n <- 1000 #times to run the loop
res_om1 <- NULL
res_om2 <- NULL
for (i in 1:n){
  alpha <- matrix(abs(rnorm(9)), 3, 3)
  res_om1 <- c(res_om1, Omega(alpha))
  res_om2 <- c(res_om2, compute_overlap(alpha, 1000)$Omega)
  cat((i/n)*100, "%")
}

res <- data.frame("Omega_simple" = 10^(res_om1),
                  "exp_Omega_simple" = exp(res_om1),
                  "Omega_overlap" = res_om2)

par(mfrow = c(1, 2))
plot(res$Omega_simple, res$Omega_overlap,
     xlab = "SND from:  10^(Omega(alpha))",
     ylab = "SND from:  compute_overlap(alpha)$Omega",
     main = "Serguei Saavedra's"); abline(lm(res$Omega_overlap ~ res$Omega_simple))

plot(res$exp_Omega_simple, res$Omega_overlap,
     xlab = "SND from:  exp(Omega(alpha))",
     ylab = " ",
     main = "Will Petry's"); abline(lm(res$Omega_overlap ~ res$exp_Omega_simple))
