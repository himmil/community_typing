library(mia)
library(dplyr)
library(TreeSummarizedExperiment)
library(survival)
library(survminer)
library(ggplot2)
library(caret)

tse <- readRDS("tse_lda.RData")

topics <- as.data.frame(colData(tse)) %>% select(starts_with("lda"))
colnames(topics) <- c(1:5)

# select the covariates
df <- colData(tse)[,c("Barcode","DEATH","DEATH_AGEDIFF", "BL_AGE", "MEN", "BMI", "CURR_SMOKE")]
df$topic <- colnames(topics)[apply(topics, 1, which.max)]

df$topic <- as.factor(df$topic)
df$topic <- relevel(df$topic, ref = 3) # Bacteroides as a reference level

fit_lda <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ topic + BL_AGE +
                MEN + BMI + CURR_SMOKE, data = df)
print(summary(fit_lda))
print(cox.zph(fit_lda))


##### Kaplan Meier plot

# add strata to enable plotting
fit_lda2 <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ strata(topic) + BL_AGE +
                MEN + BMI + CURR_SMOKE, data = df)

jpeg("lda_survival_km.jpeg", width = 600, height = 800)
p <- ggsurvplot(survfit(fit_lda2), data = df,
                ylim = c(0.92, 1), xlim = c(0, 20),
                title = "LDA", conf.int=FALSE,
                ggtheme = theme_minimal(base_size=26), risk.table = TRUE, fontsize = 8,
                legend.title="", risk.table.height = 0.3)
p$table$theme$text$size <- 24
p
dev.off()

saveRDS(p, "lda_survival_km_plot.RData")
