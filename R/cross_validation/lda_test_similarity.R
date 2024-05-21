library(alto)
library(purrr)
library(mia)
library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)

pwd <- getwd()

# setwd into the directory that includes taxa-topic information for each test set
setwd("")

# read in the taxa-topic information
for (i in seq_len(5)) {
	terms <- readRDS(file =  paste0("terms", i, ".RData"))
		
	terms <- as.data.frame(t(terms))		
	colnames(terms) <- rownames(terms)[apply(terms, 2, which.max)]
	colnames(terms) <- paste0(i, ":", colnames(terms))
	assign(paste0("terms",i), terms)
}
setwd(pwd)

df <- cbind(terms1, terms2, terms3, terms4, terms5)

# Clean colnames
colnames(df) <- gsub("Genus:", "", colnames(df))
colnames(df) <- gsub("_\\d+", "", colnames(df))


## Euclidean distances
euc_dists <- as.matrix(dist(t(df)))
dists <- melt(euc_dists)

colnames(dists) <- c("dim1", "dim2", "Distance")

jpeg("heatmap_lda_test.jpeg", width = 1000, height = 700)
p <- ggplot(dists, aes(x = dim1, y = dim2, fill = Distance)) +
        geom_tile() +
        scale_fill_viridis() +
        labs(title = "Stability of predicted topics (Euclidean)", 
		x = "", y = "") +
        scale_x_discrete(guide=guide_axis(angle = 45)) # n.dodge = 5

p <- p + theme(plot.title=element_text(size=26), text = element_text(size = 26)) +
        geom_hline(yintercept = c(5.5, 10.5, 15.5, 20.5), 
		col = "white", lwd = 2) +
        geom_vline(xintercept = c(5.5, 10.5, 15.5, 20.5), 
		col = "white", lwd = 2)
p
dev.off()

saveRDS(p, "heatmap_lda_test_plot.RData")
