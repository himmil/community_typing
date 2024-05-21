library(mia)
library(caret)
library(dplyr)
library(bluster)
library(SummarizedExperiment)
library(topicmodels)

### Read the data and filter out the taxa that does not have mortality information
tse_orig <- readRDS(".../tse.RData")

tse_orig <- transformAssay(tse_orig, method = "relabundance")
altExp(tse_orig, "GenusPrevalent") <- mergeFeaturesByPrevalence(tse_orig, rank="Genus", assay.type="relabundance",
                                                detection=1/100, prevalence=5/100)
# Filter out "Other" taxa
altExp(tse_orig, "GenusPrevalent") <- altExp(tse_orig, "GenusPrevalent")[!(rownames(altExp(tse_orig,
                                                        "GenusPrevalent")) %in% c("Other")),]

### k-fold Cross Validation

df <- as.data.frame(assay(tse_orig, "counts"))
set.seed(5)
folds <- createFolds(df, k = 5)

# Set directory where to save the models
setwd("")
for (i in seq_len(5)) {
	test_ind <- folds[[i]]
	tse <- tse_orig[, -test_ind]

	# Save the test indexes on a list
	name_test <- paste0("test_index", i, ".RData", sep = "")
	saveRDS(test_ind, name_test)

	# LDA
	lda_model <- topicmodels::LDA(t(assay(altExp(tse, "GenusPrevalent"), "counts")), k = 5, 
			control = list(seed = 3221))

	# Save the model in the metadata
	metadata(tse)$LDA <- lda_model		

	# Save into the same tse object
	name <- paste0("tse_train_lda", i, ".RData", sep = "")
        saveRDS(tse, name)
}
