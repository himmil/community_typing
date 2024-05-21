library(mia)
library(dplyr)
library(bluster)
library(SummarizedExperiment)
library(DirichletMultinomial)

tse <- readRDS(".../tse.RData")

# Relative abundance
tse <- transformAssay(tse, method = "relabundance")

# Alternative experiments with different data agglomerations
# Filter out the least prevalent taxa
altExp(tse, "GenusPrevalent") <- mergeFeaturesByPrevalence(tse, rank="Genus", assay.type="relabundance", 
					detection=1/100, prevalence=5/100)

# Filter out "Other" taxa
altExp(tse, "GenusPrevalent") <- altExp(tse, "GenusPrevalent")[!(rownames(altExp(tse, "GenusPrevalent")) 
					%in% c("Other")),]

## Run the models and save them in an alternative experiment
altExp(tse, "dmm") <- cluster(altExp(tse, "GenusPrevalent"), name = "DMM",
			assay_name = "counts", 
			DmmParam(k = 5, type = "laplace"), 
                   	MARGIN = "samples", full = TRUE, seed = 3221)

# Save clusters for each sample in colData of the original tse object
model <- metadata(tse)$DMM$dmm[1]
clusters <- as.data.frame(model[[1]]@group)
taxa <- model[[1]]@fit$Estimate

# Change the order to be similar to LDA and NMF order 
# Sort: Bacteroides, Alistipes, Agathobacter, Gemmiger/Blautia, Prevotella
order <- c(3,2,1,4,5)
clusters <- clusters[,order]
taxa <- taxa[,order]

colnames(clusters) <- paste("dmm", 1:ncol(clusters), sep = "")
colnames(taxa) <- colnames(clusters)

# Add clusters into colData
colData(tse) <- cbind(colData(tse), clusters)

# Add taxa characteristics into metadata
metadata(tse)$DMM_taxa <- taxa
saveRDS(tse, "tse_dmm.RData")
