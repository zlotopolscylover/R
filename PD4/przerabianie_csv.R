library(dendextend)
library(rstudioapi)


dataset <- "atom"
data <- as.matrix(read.table(paste0(dataset, ".data.gz")))
lista1[[i]] <- data
labels <- as.integer(read.table(paste0(dataset, ".labels0.gz"))[,1])

path <- getActiveDocumentContext()$path
setwd(dirname(path))
#pierwszy folder
lista <- list()
lista1 <- list()
wektor <- c("atom","engytime","chainlink","hepta","lsun","target","tetra","twodiamonds","wingnut")
o <- matrix(0,ncol=3,nrow=length(wektor))
for (i in 1:length(wektor)){
dataset[i] <- wektor[i]
data <- as.matrix(read.table(paste0(dataset[i], ".data.gz")))
lista1[[i]] <- data
labels <- as.integer(read.table(paste0(dataset[i], ".labels0.gz"))[,1])
k <- max(labels)
V <- spectral_clustering(data, k, 5)[[1]]
lista[[i]] <- V
d <- dendextend::FM_index(V, labels)
b <- mclust::adjustedRandIndex(V,labels)
o[i,1] <- wektor[i]
o[i,2] <- d[i]
o[i,3] <- b[i]
}

colnames(o) <- c("nazwa","indeks_FM","indeks_AR")
o <- data.frame(o)
saveRDS(lista,'wyniki_spectral.rds')
saveRDS(lista1,'data.rds')

write.csv(o,'wyniki.csv')


