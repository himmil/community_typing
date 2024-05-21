library(NMF)
library(mia)
library(reshape2)
library(dplyr)

tse <- readRDS(".../tse.RData")

# Relative abundance
tse <- transformAssay(tse, method = "relabundance")

# Filtering
altExp(tse, "GenusPrevalent") <- mergeFeaturesByPrevalence(tse, rank="Genus", assay.type="relabundance",
                                        detection = 1/100, prevalence=5/100)
# Filter out "Other" taxa
altExp(tse, "GenusPrevalent") <- altExp(tse, "GenusPrevalent")[!(rownames(altExp(tse, "GenusPrevalent")) 
					%in% c("Other")),]

# Fit nmf with 5 components
x <- t(assay(altExp(tse, "GenusPrevalent"), "counts"))
set.seed(3221)
nmf5 <- nmf(x, 5)

# Pick NMF components
H <- nmf5@fit@H
W <- nmf5@fit@W

# Sort: Bacteroides, Alistipes, Bifidobacterium, Escherichia, Prevotella
order <- c(2,3,4,1,5)
H <- H[order, ]
W <- W[,order]

rownames(H) <- paste("nmf", 1:ncol(W), sep = "")
colnames(W) <- rownames(H)

# Add enterosignature scores to colData
# First remove conflicting entries
colData(tse) <- DataFrame(as.data.frame(colData(tse)) %>% select(!starts_with("nmf")))
W <- as.data.frame(W)
colData(tse) <- cbind(colData(tse), W)

# Add NMF loadings in the metadata
metadata(tse)$NMF_loadings <- H

# Obtain the relative abundance of ES in each sample
Wrel <- t(apply(W, 1, function (x) {x/sum(x)}))

# Define as primary ES the ES of a sample with the highest relative abundance
colData(tse)$ES_primary <- apply(Wrel, 1, which.max)

saveRDS(tse, "tse_nmf.RData")
