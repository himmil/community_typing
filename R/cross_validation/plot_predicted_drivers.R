library(miaViz)
library(mia)
library(bluster)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(topicmodels)
library(cowplot)

pwd <- getwd()

# setwd into the directory that includes taxa-topic information
setwd("")

for (i in seq_len(5)) {
        terms <- readRDS(file =  paste0("terms", i, ".RData"))
        terms <- as.data.frame(t(terms))
        colnames(terms) <- rownames(terms)[apply(terms, 2, which.max)]
        colnames(terms) <- paste0(i, ":", colnames(terms))
        assign(paste0("terms",i), terms)
}
setwd(pwd)

beta <- cbind(terms1, terms2, terms3, terms4, terms5)

# Clean colnames
rownames(beta) <- gsub("Genus:", "", rownames(beta))
rownames(beta) <- gsub("_\\d+", "", rownames(beta))

# Percentage
drivers <- as.data.frame(apply(beta, 2, function(x) (x / sum(x)))) 
drivers$genus <- rownames(drivers)

# ggplot2 factor colors
colors <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

drivers1 <- drivers[order(drivers[[1]], decreasing = TRUE),]
p1 <- ggplot(head(drivers1, 5), aes(x = reorder(head(genus, 5), + head(drivers1[[1]], 5)), 
				y = head(drivers1[[1]], 5))) +
                                geom_bar(stat = "identity", fill = colors[1], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Topic", 1)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits = c(0, 0.7)) +
				theme(plot.title=element_text(size=24), text = element_text(size = 24))

drivers2 <- drivers[order(drivers[[2]], decreasing = TRUE),]
p2 <- ggplot(head(drivers2, 5), aes(x = reorder(head(genus, 5), + head(drivers2[[2]], 5)), 
				y = head(drivers2[[2]], 5))) +
                                geom_bar(stat = "identity", fill = colors[2], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Topic", 2)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits = c(0, 0.7)) +
				theme(plot.title=element_text(size=24), text = element_text(size = 24))

drivers3 <- drivers[order(drivers[[3]], decreasing = TRUE),]
p3 <- ggplot(head(drivers3, 5), aes(x = reorder(head(genus, 5), + head(drivers3[[3]], 5)), 
				y = head(drivers3[[3]], 5))) +
                                geom_bar(stat = "identity", fill = colors[3], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Topic", 3)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits=c(0,0.7)) +
				theme(plot.title=element_text(size=24), text = element_text(size = 24))

drivers4 <- drivers[order(drivers[[4]], decreasing = TRUE),]
p4 <- ggplot(head(drivers4, 5), aes(x = reorder(head(genus, 5), + head(drivers4[[4]], 5)), 
				y = head(drivers4[[4]], 5))) +
                                geom_bar(stat = "identity", fill = colors[4], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Topic", 4)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits=c(0,0.7)) +
				theme(plot.title=element_text(size=24), text = element_text(size = 24))

drivers5 <- drivers[order(drivers[[5]], decreasing = TRUE),]
p5 <- ggplot(head(drivers5, 5), aes(x = reorder(head(genus, 5), + head(drivers5[[5]], 5)), 
				y = head(drivers5[[5]], 5))) +
                                geom_bar(stat = "identity", fill = colors[5], alpha = 0.7) +
                                coord_flip() + labs(title = paste("Topic", 5)) +
                                theme_light() + labs(x="", y="") + scale_y_continuous(limits=c(0,0.7)) +
				theme(plot.title=element_text(size=24), text = element_text(size = 24))


# Plot arrangement using grid.arrange
p0 <- readRDS("lda_pcoa_predicted.RData")

p_d <- grid.arrange(p0, plot_grid(p1, p2, p3, ncol = 1, align="v"), 
			plot_grid(p4, p5, NULL, ncol = 1, align="v"), ncol = 3)

p_h <- readRDS("heatmap_lda_test_plot.RData")
p_s <- readRDS("survival_prediction_plot.RData")

p_survival <- grid.arrange(NULL, p_s$plot, p_s$table, ncol = 1, heights = c(1,6,3))
p_heatmap <- grid.arrange(NULL, p_h, NULL, ncol = 1, heights = c(1,6,2))

jpeg("predicted_all_plots.jpeg", width = 2000, height = 1800)
grid.arrange(plot_grid(p_d, labels = c("A"), label_size = 36),
		plot_grid(p_survival, p_heatmap, labels = c("B","C"), label_size = 36, ncol = 2), 
		ncol = 1, heights = c(2,3))
dev.off() 
