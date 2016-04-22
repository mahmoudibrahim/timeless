rm(list = ls())
library(psych)

numClusters = 11

score <- function(x){
	x = scale(x, scale = FALSE)
	x  = ((x-min(x))/(max(x)-min(x))) * 100
	return(x)
}

l = read.table(paste0("classes-", numClusters, ".txt"))
k = read.table(paste0("allCountsNorm.txt"))

c1 = which(l[[1]] == 1)
c2 = which(l[[1]] == 2)
c3 = which(l[[1]] == 3)
c4 = which(l[[1]] == 4)
c5 = which(l[[1]] == 5)
c6 = which(l[[1]] == 6)
c7 = which(l[[1]] == 7)
c8 = which(l[[1]] == 8)
c9 = which(l[[1]] == 9)
c10 = which(l[[1]] == 10)
c11 = which(l[[1]] == 11)




k27ac = cbind(k[[5]], k[[6]], k[[7]], k[[8]])
k27me3 = cbind(k[[9]], k[[10]], k[[11]], k[[12]])
me3 = cbind(k[[13]], k[[14]], k[[15]], k[[16]])

me3 = matrix(score(as.vector(me3)), ncol = 4, byrow = FALSE)
k27ac = matrix(score(as.vector(k27ac)), ncol = 4, byrow = FALSE)
k27me3 = matrix(score(as.vector(k27me3)), ncol = 4, byrow = FALSE)


pdf(paste0("vals-", numClusters, ".pdf"), width = 20, height = 8)
library("psych")
cbbPalette <- c("gray", "#009E73", "#D50F25")

nn = ceiling(numClusters / 2)
par(mfrow = c(2,nn))

error.bars((cbind(me3[c1,1], me3[c1,2], me3[c1,3], me3[c1,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 1 - n=", length(c1)))
lines(colMeans((cbind(me3[c1,1], me3[c1,2], me3[c1,3], me3[c1,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c1,1], k27ac[c1,2], k27ac[c1,3], k27ac[c1,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c1,1], k27ac[c1,2], k27ac[c1,3], k27ac[c1,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c1,1], k27me3[c1,2], k27me3[c1,3], k27me3[c1,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c1,1], k27me3[c1,2], k27me3[c1,3], k27me3[c1,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)

error.bars((cbind(me3[c2,1], me3[c2,2], me3[c2,3], me3[c2,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 2 - n=", length(c2)))
lines(colMeans((cbind(me3[c2,1], me3[c2,2], me3[c2,3], me3[c2,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c2,1], k27ac[c2,2], k27ac[c2,3], k27ac[c2,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c2,1], k27ac[c2,2], k27ac[c2,3], k27ac[c2,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c2,1], k27me3[c2,2], k27me3[c2,3], k27me3[c2,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c2,1], k27me3[c2,2], k27me3[c2,3], k27me3[c2,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)


error.bars((cbind(me3[c3,1], me3[c3,2], me3[c3,3], me3[c3,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 3 - n=", length(c3)))
lines(colMeans((cbind(me3[c3,1], me3[c3,2], me3[c3,3], me3[c3,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c3,1], k27ac[c3,2], k27ac[c3,3], k27ac[c3,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c3,1], k27ac[c3,2], k27ac[c3,3], k27ac[c3,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c3,1], k27me3[c3,2], k27me3[c3,3], k27me3[c3,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c3,1], k27me3[c3,2], k27me3[c3,3], k27me3[c3,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)


error.bars((cbind(me3[c4,1], me3[c4,2], me3[c4,3], me3[c4,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 4 - n=", length(c4)))
lines(colMeans((cbind(me3[c4,1], me3[c4,2], me3[c4,3], me3[c4,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c4,1], k27ac[c4,2], k27ac[c4,3], k27ac[c4,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c4,1], k27ac[c4,2], k27ac[c4,3], k27ac[c4,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c4,1], k27me3[c4,2], k27me3[c4,3], k27me3[c4,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c4,1], k27me3[c4,2], k27me3[c4,3], k27me3[c4,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)


error.bars((cbind(me3[c5,1], me3[c5,2], me3[c5,3], me3[c5,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 5 - n=", length(c5)))
lines(colMeans((cbind(me3[c5,1], me3[c5,2], me3[c5,3], me3[c5,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c5,1], k27ac[c5,2], k27ac[c5,3], k27ac[c5,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c5,1], k27ac[c5,2], k27ac[c5,3], k27ac[c5,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c5,1], k27me3[c5,2], k27me3[c5,3], k27me3[c5,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c5,1], k27me3[c5,2], k27me3[c5,3], k27me3[c5,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)


error.bars((cbind(me3[c6,1], me3[c6,2], me3[c6,3], me3[c6,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 6 - n=", length(c6)))
lines(colMeans((cbind(me3[c6,1], me3[c6,2], me3[c6,3], me3[c6,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c6,1], k27ac[c6,2], k27ac[c6,3], k27ac[c6,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c6,1], k27ac[c6,2], k27ac[c6,3], k27ac[c6,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c6,1], k27me3[c6,2], k27me3[c6,3], k27me3[c6,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c6,1], k27me3[c6,2], k27me3[c6,3], k27me3[c6,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)



error.bars((cbind(me3[c7,1], me3[c7,2], me3[c7,3], me3[c7,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 7 - n=", length(c7)))
lines(colMeans((cbind(me3[c7,1], me3[c7,2], me3[c7,3], me3[c7,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c7,1], k27ac[c7,2], k27ac[c7,3], k27ac[c7,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c7,1], k27ac[c7,2], k27ac[c7,3], k27ac[c7,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c7,1], k27me3[c7,2], k27me3[c7,3], k27me3[c7,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c7,1], k27me3[c7,2], k27me3[c7,3], k27me3[c7,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)



error.bars((cbind(me3[c8,1], me3[c8,2], me3[c8,3], me3[c8,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 8 - n=", length(c8)))
lines(colMeans((cbind(me3[c8,1], me3[c8,2], me3[c8,3], me3[c8,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c8,1], k27ac[c8,2], k27ac[c8,3], k27ac[c8,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c8,1], k27ac[c8,2], k27ac[c8,3], k27ac[c8,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c8,1], k27me3[c8,2], k27me3[c8,3], k27me3[c8,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c8,1], k27me3[c8,2], k27me3[c8,3], k27me3[c8,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)



error.bars((cbind(me3[c9,1], me3[c9,2], me3[c9,3], me3[c9,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 9 - n=", length(c9)))
lines(colMeans((cbind(me3[c9,1], me3[c9,2], me3[c9,3], me3[c9,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c9,1], k27ac[c9,2], k27ac[c9,3], k27ac[c9,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c9,1], k27ac[c9,2], k27ac[c9,3], k27ac[c9,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c9,1], k27me3[c9,2], k27me3[c9,3], k27me3[c9,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c9,1], k27me3[c9,2], k27me3[c9,3], k27me3[c9,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)



error.bars((cbind(me3[c10,1], me3[c10,2], me3[c10,3], me3[c10,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 10 - n=", length(c10)))
lines(colMeans((cbind(me3[c10,1], me3[c10,2], me3[c10,3], me3[c10,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c10,1], k27ac[c10,2], k27ac[c10,3], k27ac[c10,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c10,1], k27ac[c10,2], k27ac[c10,3], k27ac[c10,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c10,1], k27me3[c10,2], k27me3[c10,3], k27me3[c10,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c10,1], k27me3[c10,2], k27me3[c10,3], k27me3[c10,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)


error.bars((cbind(me3[c11,1], me3[c11,2], me3[c11,3], me3[c11,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[1], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[1], main = paste0("Cluster 11 - n=", length(c11)))
lines(colMeans((cbind(me3[c11,1], me3[c11,2], me3[c11,3], me3[c11,4]))), col = cbbPalette[1], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27ac[c11,1], k27ac[c11,2], k27ac[c11,3], k27ac[c11,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[2], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[2], add = TRUE)
lines(colMeans((cbind(k27ac[c11,1], k27ac[c11,2], k27ac[c11,3], k27ac[c11,4]))), col = cbbPalette[2], ylim = c(0,30), lwd = 4)
error.bars((cbind(k27me3[c11,1], k27me3[c11,2], k27me3[c11,3], k27me3[c11,4])),labels = c("EB0h","EB12h", "EB24h", "EB48h"), eyes = FALSE, sd = FALSE, bars = FALSE, arrow.col = cbbPalette[3], ylim = c(0,30), xaxt = "n", ylab = "Normalized Average Signal", xlab = "", col = cbbPalette[3], add = TRUE)
lines(colMeans((cbind(k27me3[c11,1], k27me3[c11,2], k27me3[c11,3], k27me3[c11,4]))), col = cbbPalette[3], ylim = c(0,30), lwd = 4)


dev.off()
