library(miaViz)
library(mia)
library(dplyr)
library(purrr)
library(TreeSummarizedExperiment)
library(survival)
library(survminer)
library(ggplot2)
library(caret)

tse <- readRDS(".../tse.RData")

tse <- transformAssay(tse, method = "relabundance")
altExp(tse, "GenusPrevalent") <- mergeFeaturesByPrevalence(tse, rank="Genus", assay.type="relabundance",
                                                detection=1/100, prevalence=5/100)

# Testing survival predictions with 5fold cross validation

# setwd where trained models and test indexes are saved
setwd("")
# Read all objects in:
for (i in seq_len(5)) {
        train <- readRDS(file = paste0("tse_train_lda", i, ".RData"))
        test_index <- readRDS(file = paste0("test_index", i, ".RData"))
        test_tse <- tse[, test_index]
        test <- as.data.frame(t(assay(altExp(test_tse, "GenusPrevalent"), "counts")))

        ### LDA
        lda_model <- metadata(train)$LDA
        test_topics <- topicmodels::posterior(lda_model, test, control = list(seed = 123))

        terms <- as.data.frame(test_topics$terms)
        topics <- as.data.frame(test_topics$topics)
	
	# All test sets have the same topic order
	order <- c(3,1,2,5,4)
	terms <- terms[order, ]
	topics <- topics[, order]
	colnames(topics) <- c("lda1", "lda2", "lda3", "lda4", "lda5")
	rownames(topics) <- colData(test_tse)$FID
				
        assign(paste0("test",i), topics)
	
	# save the taxa information of all test sets into separate datasets for plotting scripts
	saveRDS(terms, paste0("terms",i,".RData")) 
}

predicted <- bind_rows(test1, test2, test3, test4, test5)
predicted$topic <- apply(predicted, 1, which.max)
predicted$FID <- rownames(predicted)

colData(tse) <- merge(colData(tse), predicted, by = "FID")

saveRDS(tse, "tse_predicted.RData")
