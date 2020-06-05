#function to create the triangles
projection_3sp_with_pairwise <- function(alpha, r, species){
  library(stringr)
  species <- stringr::str_replace(string = species, pattern = "_", replacement = "-")
  make.bold <- function(x) as.expression(lapply(x, function(y) bquote(bold(.(y)))))
  par(mar = c(0, 0, 0, 0))
  D <- diag(1 / sqrt(diag(t(alpha) %*% alpha)))
  alpha_n <- alpha %*% D
  v1 <- alpha_n[, 1]
  v2 <- alpha_n[, 2]
  v3 <- alpha_n[, 3]
  vc <- (v1 + v2 + v3)
  vc <- vc / sqrt(sum(vc^2))
  v1 <- v1 / sum(v1)
  v2 <- v2 / sum(v2)
  v3 <- v3 / sum(v3)
  vc <- vc / sum(vc)
  Xf <- sqrt(2)
  Yf <- sqrt(6) / 2
  XX <- c(-Xf / 2, Xf / 2, 0, -Xf / 2)
  YY <- c(0, 0, Yf, 0)
  plot(-XX, YY, axes = FALSE, xlab = '', ylab = '',
       xlim = c(-Xf/2-0.05, Xf/2+0.05),
       ylim = c(0-0.05, Yf+0.05),
       col = "grey50", type = "l", lwd = 2, asp = 1)
  v1P <- v1
  v1P[3] <- 0
  v1P <- v1P / sum(v1P)
  v2P <- v2
  v2P[3] <- 0
  v2P <- v2P / sum(v2P)
  vcP <- v1P/sqrt(sum(v1P^2)) + v2P/sqrt(sum(v2P^2))
  vcP[3] <- 0
  vcP <- vcP / sum(vcP)
  v1C <- c((0.5-0.5*v1P[3]-v1P[1])*Xf, v1P[3]*Yf)
  v2C <- c((0.5-0.5*v2P[3]-v2P[1])*Xf, v2P[3]*Yf)
  vcC <- c((0.5-0.5*vcP[3]-vcP[1])*Xf, vcP[3]*Yf)
  lines(-c(v1C[1], v2C[1]), c(v1C[2], v2C[2]),
        col = "mediumseagreen", lwd = 1)
  lines(-c(v1C[1], XX[3]), c(v1C[2], YY[3]),
        col = "mediumseagreen", lty = 2, lwd = 1)
  lines(-c(v2C[1], XX[3]), c(v2C[2], YY[3]),
        col = "mediumseagreen", lty = 2, lwd = 1)
  color <- col2rgb("mediumseagreen")
  polygon(-c(v1C[1], v2C[1], XX[3], v1C[1]),
          c(v1C[2], v2C[2], YY[3], v1C[2]),
          col = rgb(color[1, 1], color[2, 1], color[3, 1],
                    30, maxColorValue = 255), border = FALSE)
  points(-c(v1C[1], v2C[1]), c(v1C[2], v2C[2]),
         col = "dodgerblue", pch = 16, cex = 2)
  v1P <- v1
  v1P[2] <- 0
  v1P <- v1P / sum(v1P)
  v3P <- v3
  v3P[2] <- 0
  v3P <- v3P / sum(v3P)
  vcP <- v1P/sqrt(sum(v1P^2)) + v3P/sqrt(sum(v3P^2))
  vcP[2] <- 0
  vcP <- vcP / sum(vcP)
  v1C <- c((0.5-0.5*v1P[3]-v1P[1])*Xf, v1P[3]*Yf)
  v3C <- c((0.5-0.5*v3P[3]-v3P[1])*Xf, v3P[3]*Yf)
  vcC <- c((0.5-0.5*vcP[3]-vcP[1])*Xf, vcP[3]*Yf)
  lines(-c(v1C[1], v3C[1]), c(v1C[2], v3C[2]),
        col = "mediumseagreen", lwd = 1)
  lines(-c(v1C[1], XX[2]), c(v1C[2], YY[2]),
        col = "mediumseagreen", lty = 2, lwd = 1)
  lines(-c(v3C[1], XX[2]), c(v3C[2], YY[2]),
        col = "mediumseagreen", lty = 2, lwd = 1)
  polygon(-c(v1C[1], v3C[1], XX[2], v1C[1]),
          c(v1C[2], v3C[2], YY[2], v1C[2]),
          col = rgb(color[1, 1], color[2, 1], color[3, 1],
                    30, maxColorValue = 255), border = FALSE)
  points(-c(v1C[1], v3C[1]), c(v1C[2], v3C[2]),
         col = "dodgerblue", pch = 16, cex = 2)
  # points(-vcC[1], vcC[2], col = "blue4", pch = 16, cex = 1.5)
  v2P <- v2
  v2P[1] <- 0
  v2P <- v2P / sum(v2P)
  v3P <- v3
  v3P[1] <- 0
  v3P <- v3P / sum(v3P)
  vcP <- v2P/sqrt(sum(v2P^2)) + v3P/sqrt(sum(v3P^2))
  vcP[1] <- 0
  vcP <- vcP / sum(vcP)
  v2C <- c((0.5-0.5*v2P[3]-v2P[1])*Xf, v2P[3]*Yf)
  v3C <- c((0.5-0.5*v3P[3]-v3P[1])*Xf, v3P[3]*Yf)
  vcC <- c((0.5-0.5*vcP[3]-vcP[1])*Xf, vcP[3]*Yf)
  lines(-c(v2C[1], v3C[1]), c(v2C[2], v3C[2]),
        col = "mediumseagreen", lwd = 1)
  lines(-c(v2C[1], XX[1]), c(v2C[2], YY[1]),
        col = "mediumseagreen", lty = 2, lwd = 1)
  lines(-c(v3C[1], XX[1]), c(v3C[2], YY[1]),
        col = "mediumseagreen", lty = 2, lwd = 1)
  polygon(-c(v2C[1], v3C[1], XX[1], v2C[1]),
          c(v2C[2], v3C[2], YY[1], v2C[2]),
          col = rgb(color[1, 1], color[2, 1], color[3, 1],
                    30, maxColorValue = 255), border = FALSE)
  points(-c(v2C[1], v3C[1]), c(v2C[2], v3C[2]),
         col = "dodgerblue", pch = 16, cex = 2)
  v1C <- c((0.5-0.5*v1[3]-v1[1])*Xf, v1[3]*Yf)
  v2C <- c((0.5-0.5*v2[3]-v2[1])*Xf, v2[3]*Yf)
  v3C <- c((0.5-0.5*v3[3]-v3[1])*Xf, v3[3]*Yf)
  vcC <- c((0.5-0.5*vc[3]-vc[1])*Xf, vc[3]*Yf)
  color <- col2rgb("green4")
  polygon(-c(v1C[1], v2C[1], v3C[1]), c(v1C[2], v2C[2], v3C[2]),
          col = rgb(color[1, 1], color[2, 1], color[3, 1],
                    90, maxColorValue = 255), border = FALSE)
  points(-c(v1C[1], v2C[1], v3C[1], v1C[1]),
         c(v1C[2], v2C[2], v3C[2], v1C[2]), col = "green4",
         type = 'l', cex = 1.5, lwd = 2)
  points(-c(v1C[1], v2C[1], v3C[1]), c(v1C[2], v2C[2], v3C[2]),
         col = "darkgreen", pch = 16, cex = 2)
  rX <- Xf*(0.5-0.5*((2*r[2]+r[3]) / (r[1]+r[2]+r[3])))
  rY <- Yf*(r[3] / (r[1]+r[2]+r[3]))
  points(rX, rY, col = "black", bg = "darkorange1", pch = 21, lwd = 2.5, cex = 3.5)
  points(-vcC[1], vcC[2], col = "black", pch = 4, cex = 3, lwd = 2.5) # plot centroid of D_F
  text(-XX, YY, labels = make.bold(species), cex = 2.5,
       pos = c(1, 1, 3))
}

#load alpha and intrinsic
load("results/LUI_effects.RData")

#LUI = 0.50
lui <- 1 #1, 6 u 11
n <- 1:ncol(combn(colnames(lui_alpha[[lui]]), 3))
png("figures/all-triangles_LUI-A.png", width = 88, height = 52, units = "in", res = 320)
par(mfrow = c(13, 22))
for (i in 1:length(n)){
  spp <- t(combn(colnames(lui_alpha[[lui]]), 3))[i, ]
  alpha <- lui_alpha[[lui]][spp, spp]
  diag(alpha) <- diag(alpha) * (-1)
  alpha <- alpha / max(alpha)
  r <- lui_intrinsic_positive[[lui]][spp, 1]
  projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
}
dev.off()

#selected triangle for LUI = 0.50
png("figures/triangle_LUI-A.png", width = 15, height = 15, units = "in", res = 320)
lui <- 1 #1, 6 u 11
n <- 161
spp <- t(combn(colnames(lui_alpha[[lui]]), 3))[n, ]
alpha <- lui_alpha[[lui]][spp, spp]
diag(alpha) <- diag(alpha) * (-1)
alpha <- alpha / max(alpha)
r <- lui_intrinsic_positive[[lui]][spp, 1]
projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
dev.off()


#LUI = 1.75
lui <- 6 #1, 6 u 11
n <- 1:ncol(combn(colnames(lui_alpha[[lui]]), 3))
png("figures/all-triangles_LUI-B.png", width = 88, height = 52, units = "in", res = 320)
par(mfrow = c(16, 17))
for (i in 1:(length(n)/3)){
  spp <- t(combn(colnames(lui_alpha[[lui]]), 3))[i, ]
  alpha <- lui_alpha[[lui]][spp, spp]
  diag(alpha) <- diag(alpha) * (-1)
  alpha <- alpha / max(alpha)
  r <- lui_intrinsic_positive[[lui]][spp, 1]
  projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
}
dev.off()

#selected triangle for LUI = 1.75
png("figures/triangle_LUI-B.png", width = 15, height = 15, units = "in", res = 320)
lui <- 6 #1, 6 u 11
n <- 167
spp <- t(combn(colnames(lui_alpha[[lui]]), 3))[n, ]
alpha <- lui_alpha[[lui]][spp, spp]
diag(alpha) <- diag(alpha) * (-1)
alpha <- alpha / max(alpha)
r <- lui_intrinsic_positive[[lui]][spp, 1]
projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
dev.off()


#LUI = 3.00
lui <- 11 #1, 6 u 11
n <- 1:ncol(combn(colnames(lui_alpha[[lui]]), 3))
png("figures/all-triangles_LUI-C.png", width = 88, height = 52, units = "in", res = 320)
par(mfrow = c(13, 22))
for (i in 1:length(n)){
  spp <- t(combn(colnames(lui_alpha[[lui]]), 3))[i, ]
  alpha <- lui_alpha[[lui]][spp, spp]
  diag(alpha) <- diag(alpha) * (-1)
  alpha <- alpha / max(alpha)
  r <- lui_intrinsic_positive[[lui]][spp, 1]
  projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
}
dev.off()

#selected triangle for LUI = 3.00
png("figures/triangle_LUI-C.png", width = 15, height = 15, units = "in", res = 320)
lui <- 11 #1, 6 u 11
n <- 24
spp <- t(combn(colnames(lui_alpha[[lui]]), 3))[n, ]
alpha <- lui_alpha[[lui]][spp, spp]
diag(alpha) <- diag(alpha) * (-1)
alpha <- alpha / max(alpha)
r <- lui_intrinsic_positive[[lui]][spp, 1]
projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
dev.off()

library(imager)
a <- load.image("figures/triangle_LUI-A.png")
b <- load.image("figures/triangle_LUI-B.png")
c <- load.image("figures/triangle_LUI-C.png")

png("figures/paper_figures/FigX.png", width = 60, height = 20, units = "in", res = 320)
par(mfrow = c(1, 3))
plot(a, axes = FALSE, xlab = '', ylab = '')
plot(b, axes = FALSE, xlab = '', ylab = '')
plot(c, axes = FALSE, xlab = '', ylab = '')
dev.off()


