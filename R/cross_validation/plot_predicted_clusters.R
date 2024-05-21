library(mia)
library(dplyr)
library(bluster)
library(topicmodels)
library(ggplot2)

tse <- readRDS("tse_predicted.RData")
pcoa <- readRDS(".../bray_pcoa_df.RData")

pcoa_df <- cbind(pcoa, topic = as.factor(colData(tse)$topic))

# Visualization
bray_plot <- ggplot(data = pcoa_df, aes(x = pcoa1, y = pcoa2,
                        color = topic)) + geom_point() +
                        theme_light() + ggtitle("") +
                        theme(legend.title=element_blank(),
                        legend.text = element_text(size = 24),
                        axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22),
                        axis.text.x = element_text(size = 22), axis.text.y = element_text(size = 22))

# Save plot as an object
saveRDS(bray_plot, "lda_pcoa_predicted.RData")
