library(NMF)
library(mia)
library(dplyr)
library(ggplot2)


tse <- readRDS("tse_nmf.RData")
pcoa <- readRDS(".../bray_pcoa_df.RData")

## H is genus loadings for each NMF component
H <- metadata(tse)$NMF_loadings

## W is sample memberships across NMF components
W <- as.data.frame(colData(tse)) %>% select(starts_with("nmf"))

# Simplify genus names
colnames(H) <- gsub("Genus:", "", colnames(H))
colnames(H) <- gsub("_\\d+", "", colnames(H))

# Name each topic based on the most influential genus name
H <- as.data.frame(H)

# Name the components based on the maximum contribution
rownames(H) <- colnames(H)[apply(H, 1, which.max)]

# Find the most influential topic for each sample
colnames(W) <- c(1:5)
colnames(W) <- rownames(H)
W$clusters <- tse$ES_primary

# Visualization
pcoa_df <- cbind(pcoa, component = as.factor(W$clusters))
bray_plot <- ggplot(data = pcoa_df, aes(x = pcoa1, y = pcoa2,
                       	color = component)) + geom_point() +
                       	theme_light() + ggtitle("") +
                        theme(legend.title=element_blank(),
                        legend.text = element_text(size = 24),
                        axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22),
			axis.text.x = element_text(size = 22), axis.text.y = element_text(size = 22))

# Save plot as an object for combining drivers
saveRDS(bray_plot, "nmf_pcoa_plot.RData")
