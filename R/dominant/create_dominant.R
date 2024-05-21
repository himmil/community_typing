library(mia)
library(microbiome)
library(dplyr)

### Dominant taxa
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

# Dominant genera for each sample
tse <- addPerSampleDominantFeatures(altExp(tse, "GenusPrevalent"), rank = "Genus", name = "dominant")

# Clean taxa names
tse$dominant <- gsub("Genus:", "", tse$dominant)
tse$dominant <- gsub("_\\d+","", tse$dominant)

### save all dominant
saveRDS(tse, "tse_dominant.RData")

## If there are multiple dominant features,
# pick one of those per sample at random
tops <- unname(sapply(tse$dominant, function (x) {sample(unlist(x),1)}))

# Identify the top 8 features
top <- microbiome::top(tops, n=8)

# Group the rest into the "Other" category
tops[!tops %in% names(top)] <- "Other"

# Also store the tops in the colData
tse$dominant_f <- factor(tops)
saveRDS(tse, "tse_dominant_top8.RData")
