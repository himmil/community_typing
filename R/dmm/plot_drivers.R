library(miaViz)
library(mia)
library(DirichletMultinomial)
library(bluster)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

tse <- readRDS("tse_dmm.RData")

clusters <- as.data.frame(colData(tse)) %>% select(starts_with("dmm"))
clusters <- clusters %>%
        mutate(dmm = max.col(clusters))

# Get the estimates on how much each genus contributes on each cluster
df <- metadata(tse)$DMM_taxa

# Percentage
drivers <- as.data.frame(apply(df, 2, function(x) (x / sum(x))))

# Clean names
drivers$genus <- gsub("Genus:", "", rownames(drivers))
drivers$genus <- gsub("_\\d+","", drivers$genus)

# ggplot2 factor colors
colors <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

#### non elegant way for doing this

drivers1 <- drivers[order(drivers[[1]], decreasing = TRUE),]
p1 <- ggplot(head(drivers1, 5), aes(x = reorder(head(genus, 5), + head(drivers1[[1]], 5)), 
				y = head(drivers1[[1]], 5))) +
                                geom_bar(stat = "identity", fill = colors[1], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Cluster", 1)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits=c(0,0.4))  +
                                theme(plot.title=element_text(size=24), text = element_text(size = 24))

drivers2 <- drivers[order(drivers[[2]], decreasing = TRUE),]
p2 <- ggplot(head(drivers2, 5), aes(x = reorder(head(genus, 5), + head(drivers2[[2]], 5)), 
				y = head(drivers2[[2]], 5))) +
                                geom_bar(stat = "identity", fill = colors[2], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Cluster", 2)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits=c(0,0.4)) +
                                theme(plot.title=element_text(size=24), text = element_text(size = 24))

drivers3 <- drivers[order(drivers[[3]], decreasing = TRUE),]
p3 <- ggplot(head(drivers3, 5), aes(x = reorder(head(genus, 5), + head(drivers3[[3]], 5)), 
				y = head(drivers3[[3]], 5))) +
                                geom_bar(stat = "identity", fill = colors[3], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Cluster", 3)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits=c(0,0.4)) +
                                theme(plot.title=element_text(size=24), text = element_text(size = 24))

drivers4 <- drivers[order(drivers[[4]], decreasing = TRUE),]
p4 <- ggplot(head(drivers4, 5), aes(x = reorder(head(genus, 5), + head(drivers4[[4]], 5)), 
				y = head(drivers4[[4]], 5))) +
                                geom_bar(stat = "identity", fill = colors[4], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Cluster", 4)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits=c(0,0.4)) +
                                theme(plot.title=element_text(size=24), text = element_text(size = 24))

drivers5 <- drivers[order(drivers[[5]], decreasing = TRUE),]
p5 <- ggplot(head(drivers5, 5), aes(x = reorder(head(genus, 5), + head(drivers5[[5]], 5)), 
				y = head(drivers5[[5]], 5))) +
                                geom_bar(stat = "identity", fill = colors[5], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Cluster", 5)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits=c(0,0.4)) +
                                theme(plot.title=element_text(size=24), text = element_text(size = 24))

# Plot arrangement using grid.arrange
p0 <- readRDS("dmm_pcoa_plot.RData")

jpeg("dmm_all.jpeg", width=1800, height=600)
grid.arrange(p0, plot_grid(p1, p2, p3, ncol = 1, align="v"), plot_grid(p4, p5, NULL, ncol = 1, align="v"), ncol = 3)
dev.off()
