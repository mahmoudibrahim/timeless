rm(list = ls())
library(limma)

countsFile="allCounts.txt"


l = read.table(countsFile)
k27ac = normalizeQuantiles(cbind(l[[5]], l[[6]], l[[7]], l[[8]])) #H3K27ac
k27me3 = normalizeQuantiles(cbind(l[[9]], l[[10]], l[[11]], l[[12]])) #H3K27me3
me3 = normalizeQuantiles(cbind(l[[13]], l[[14]], l[[15]], l[[16]])) #H3K4me3


me3MED = mean(me3)
getThis2 = which((apply(me3, 1, function(x) any(x >= me3MED))) == TRUE)
k27acMED = mean(k27ac)
getThis3 = which((apply(k27ac, 1, function(x) any(x >= k27acMED))) == TRUE)
k27me3MED = mean(k27me3)
getThis4 = which((apply(k27me3, 1, function(x) any(x >= k27me3MED))) == TRUE)
g = unique(sort(c(getThis2, getThis3, getThis4)))

me3 = me3[g,] + 1
k27ac = k27ac[g,] + 1
k27me3 = k27me3[g,] + 1

k27acL = cbind(log2(k27ac[,2] / k27ac[,1]), log2(k27ac[,3] / k27ac[,2]), log2(k27ac[,4] / k27ac[,3]))
k27me3L = cbind(log2(k27me3[,2] / k27me3[,1]), log2(k27me3[,3] / k27me3[,2]), log2(k27me3[,4] / k27me3[,3]))
me3L = cbind(log2(me3[,2] / me3[,1]), log2(me3[,3] / me3[,2]), log2(me3[,4] / me3[,3]))

write(paste(l[[1]][g], l[[2]][g], l[[3]][g], l[[4]][g], k27acL[,1]*100, k27acL[,2]*100, k27acL[,3]*100, k27me3L[,1]*100, k27me3L[,2]*100, k27me3L[,3]*100, me3L[,1]*100, me3L[,2]*100, me3L[,3]*100, sep = "\t"), file = paste0("allFold.txt"), ncolumns = 1)
write(paste(l[[1]][g], l[[2]][g], l[[3]][g], l[[4]][g], k27ac[,1]-1, k27ac[,2]-1, k27ac[,3]-1, k27ac[,4]-1, k27me3[,1]-1, k27me3[,2]-1, k27me3[,3]-1, k27me3[,4]-1, me3[,1]-1, me3[,2]-1, me3[,3]-1, me3[,4]-1, sep = "\t"), file = paste0("allCountsNorm.txt"), ncolumns = 1)
