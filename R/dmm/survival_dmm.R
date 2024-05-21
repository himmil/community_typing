library(TreeSummarizedExperiment)
library(mia)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

### DMM
tse <- readRDS("tse_dmm.RData")

clusters <- as.data.frame(colData(tse)) %>% select(starts_with("dmm"))
clusters <- clusters %>%
        mutate(cluster = max.col(clusters))

colData(tse)$cluster <- as.factor(clusters$cluster)
colData(tse)$cluster <- relevel(colData(tse)$cluster, ref = 1)

fit_dmm <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ cluster + BL_AGE + 
		MEN + BMI + CURR_SMOKE, data = colData(tse))
print(summary(fit_dmm))
print(cox.zph(fit_dmm))


##### Kaplain Meier plots
# add strata to enable plotting
fit_dmm2 <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ strata(cluster) + BL_AGE +
                MEN + BMI + CURR_SMOKE, data = colData(tse))

jpeg("dmm_survival_km.jpeg", width = 600, height = 800)
p <- ggsurvplot(survfit(fit_dmm2), data = colData(tse),
                ylim = c(0.92, 1), xlim = c(0, 20),
                title = "DMM", conf.int=FALSE,
                ggtheme = theme_minimal(base_size=26), risk.table = TRUE, fontsize = 8,
                legend.title="", risk.table.height = 0.3)
p$table$theme$text$size <- 24
p
dev.off()

saveRDS(p, "dmm_survival_plot.RData")
