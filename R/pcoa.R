library(scater)
library(mia)

tse <- readRDS("tse.RData")
tse <- transformAssay(tse, method = "relabundance")

# principal coordinate analysis
tse <- runMDS(tse, FUN = vegan::vegdist, method = "bray",
              	assay.type = "relabundance",
              	name = "MDS_bray")

df <- as.data.frame(reducedDim(tse, "MDS_bray"))

# The first two dimensions explain the most variability in the data
bray_pcoa_df <- data.frame(pcoa1 = df[, 1], pcoa2 = df[, 2])
saveRDS(bray_pcoa_df, file = "bray_pcoa_df.RData")
