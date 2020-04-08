#function to create the triangles
projection_3sp_with_pairwise <- function(alpha, r, species){
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


#ADD REAL DATA AND DO A LOOP TO PLOT EVERYTHING!
load(file = "results/LUI_effects.RData")

triangulate <- function(alpha, r, lui_value, combo){
  
  library(imager)
  
  #first, see which species are present through the whole list
  common <- colnames(lui_alpha[[1]])
  for (i in 2:length(lui_alpha)){
    common <- Reduce(intersect, list(common, colnames(lui_alpha[[i]])))
  }


  #first triangle
  triplet <- combn(common, 3)[, combo[1]]
  lui <- lui_value[1]
  projection_3sp_with_pairwise(alpha = lui_alpha[[lui]][triplet, triplet],
                             r = lui_intrinsic_positive[[lui]][triplet, 1],
                             species = triplet)

  #second triangle
  triplet <- combn(common, 3)[, combo[2]]
  lui <- lui_value[2]
  projection_3sp_with_pairwise(alpha = lui_alpha[[lui]][triplet, triplet],
                               r = lui_intrinsic_positive[[lui]][triplet, 1],
                               species = triplet)
  
  #third triangle
  triplet <- combn(common, 3)[, combo[3]]
  lui <- lui_value[3]
  projection_3sp_with_pairwise(alpha = lui_alpha[[lui]][triplet, triplet],
                               r = lui_intrinsic_positive[[lui]][triplet, 1],
                               species = triplet)
  
}

lui_value <- c(1, 6, 11) #3 different values from 1 to 11
combo <- 2 #scenarios from 1 to 20
triangulate(lui_alpha, lui_intrinsic_positive, lui_value, combo)

dev.off()


common <- colnames(lui_alpha[[1]])
for (i in 2:length(lui_alpha)){
  common <- Reduce(intersect, list(common, colnames(lui_alpha[[i]])))
}

#select the specific triplet
png("figures/attempt1.png", width = 18, height = 18, units = "in", res = 320)
lui_value <- c(1, 6, 11) #3 different values from 1 to 11
combo <- c(1, 2, 3) #3 scenarios from 1 to 20
triplet <- combn(common, 3)[, combo[1]]
lui <- lui_value[1]
par(mar=c(3, 3, 3, 3))
projection_3sp_with_pairwise(alpha = lui_alpha[[lui]][triplet, triplet],
                             r = lui_intrinsic_positive[[lui]][triplet, 1],
                             species = triplet)
dev.off()
try <- load.image("figures/attempt1.png")

par(mfrow = c(2, 2))
plot(try)

combo <- 2:4
png("figures/paper_figures/triangles.png", width = 66, height = 6, units = "in", res = 320)
par(mfrow = c(1, 11))
for (i in 1:length(lui_alpha)){
  alpha <- lui_alpha[[i]][combo, combo]
  diag(alpha) <- -1 * diag(alpha)
  r <- lui_intrinsic_positive[[i]][combo]
  projection_3sp_with_pairwise(alpha, r, species)
}
dev.off()

for(i in 1:length(combn(nrow(lui_alpha[[1]]), 3))){
  alpha <- lui_alpha[[1]][combn(nrow(lui_alpha[[1]]), 3)[, i],
                          combn(nrow(lui_alpha[[1]]), 3)[, i]]
  diag(alpha) <- diag(alpha) * (-1)
  alpha <- alpha / max(abs(alpha))
  r <- lui_intrinsic_positive[[1]][combn(nrow(lui_alpha[[1]]), 3)[, i]]
  projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
  print(alpha)
  Sys.sleep(2)
}


png("figures/paper_figures/triangles_LUI_1.png", width = 88, height = 52, units = "in", res = 320)
par(mfrow = c(13, 22))
for(i in 1:length(combn(nrow(lui_alpha[[1]]), 3))){
  alpha <- lui_alpha[[1]][combn(nrow(lui_alpha[[1]]), 3)[, i],
                          combn(nrow(lui_alpha[[1]]), 3)[, i]]
  diag(alpha) <- diag(alpha) * (-1)
  alpha <- alpha / max(abs(alpha))
  r <- lui_intrinsic_positive[[1]][combn(nrow(lui_alpha[[1]]), 3)[, i]]
  projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
}
dev.off()

png("figures/paper_figures/triangles_LUI_1_abs.png", width = 88, height = 52, units = "in", res = 320)
par(mfrow = c(13, 22))
for(i in 1:length(combn(nrow(lui_alpha[[1]]), 3))){
  alpha <- lui_alpha[[1]][combn(nrow(lui_alpha[[1]]), 3)[, i],
                          combn(nrow(lui_alpha[[1]]), 3)[, i]]
  alpha <- abs(alpha / max(abs(alpha)))
  r <- lui_intrinsic_positive[[1]][combn(nrow(lui_alpha[[1]]), 3)[, i]]
  projection_3sp_with_pairwise(alpha, r, species = colnames(alpha))
}
dev.off()



