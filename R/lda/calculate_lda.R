library(mia)
library(dplyr)
library(bluster)
library(topicmodels)

### Read the data and filter out the taxa that does not have mortality information
tse <- readRDS("../tse.RData")

tse <- transformAssay(tse, method = "relabundance")
altExp(tse, "GenusPrevalent") <- mergeFeaturesByPrevalence(tse, rank="Genus", assay.type="relabundance",
                                                detection=1/100, prevalence=5/100)
# Filter out "Other" taxa
altExp(tse, "GenusPrevalent") <- altExp(tse, "GenusPrevalent")[!(rownames(altExp(tse,
                                                        "GenusPrevalent")) %in% c("Other")),]

lda_model <- LDA(t(assay(altExp(tse, "GenusPrevalent"), "counts")), k = 5, control = list(seed = 3221))

# Save posterior probabilities for both taxa and sample distributions
df <- as.data.frame(t(assay(altExp(tse, "GenusPrevalent"), "counts")))

posteriors <- topicmodels::posterior(lda_model, df, control = list(seed = 123))

terms <- as.data.frame(posteriors$terms)
topics <- as.data.frame(posteriors$topics)

# Sort: Bacteroides, Agathobacter, Bifidobacterium, Escherichia, Prevotella
order <- c(4,5,2,1,3)
terms <- terms[order,]
topics <- topics[,order]

# Save the taxa contributions in the metadata
metadata(tse)$lda_terms <- terms

# Save enterosignature scores in colData
colnames(topics) <- paste("lda", 1:ncol(topics), sep = "")
colData(tse) <- cbind(colData(tse), topics)

# Save the whole model in the metadata
metadata(tse)$LDA <- lda_model

saveRDS(tse, "tse_lda.RData")
