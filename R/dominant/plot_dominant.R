library(mia)
library(ggplot2)

# Plot the dominant genera
pcoa <- readRDS(".../pcoa_df.RData")
tse <- readRDS("tse_dominant_top8.RData")

bray_pcoa_df <- cbind(pcoa, Dominant = tse$dominant_f)

# Create a plot
bray_plot <- ggplot(data = bray_pcoa_df, aes(x = pcoa1, y = pcoa2, color = Dominant)) +
                geom_point() + theme_light() + ggtitle("") +
                theme(plot.title = element_text(size = 26),
                        legend.text = element_text(size = 24), legend.title = element_text(size = 24),
                        axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
			axis.text.x = element_text(size = 22), axis.text.y = element_text(size = 22))

jpeg("dominant_taxa_top8.jpeg", width = 800, height = 600)
bray_plot
dev.off()
