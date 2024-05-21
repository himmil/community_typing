library(miaViz)
library(mia)
library(DirichletMultinomial)
library(bluster)
library(dplyr)
library(ggplot2)

tse <- readRDS("tse_dmm.RData")
pcoa <- readRDS("../bray_pcoa_df.RData")

clusters <- as.data.frame(colData(tse)) %>% select(starts_with("dmm"))
clusters <- clusters %>%
	mutate(dmm = max.col(clusters))

pcoa_df <- cbind(pcoa, cluster = as.factor(clusters$dmm))
bray_plot <- ggplot(data = pcoa_df, aes(x = pcoa1, y = pcoa2, 
			color = cluster)) + geom_point() + 
			theme_light() + ggtitle("") +
                        theme(legend.title=element_blank(),
                        legend.text = element_text(size = 24),
                        axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22),
                        axis.text.x = element_text(size = 22), axis.text.y = element_text(size = 22))

# Save plot as an object for combining drivers
saveRDS(bray_plot, "dmm_pcoa_plot.RData")
