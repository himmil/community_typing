library(miaViz)
library(mia)
library(dplyr)
library(purrr)
library(TreeSummarizedExperiment)
library(survival)
library(survminer)
library(ggplot2)
library(caret)

predicted <- readRDS("tse_predicted.RData")

predicted$topic <- as.factor(predicted$topic)
predicted$topic <- relevel(predicted$topic, ref = 1)

df <- colData(predicted)[,c("DEATH","DEATH_AGEDIFF", "BL_AGE", "MEN", "BMI", "CURR_SMOKE", "topic")]

fit_lda <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ topic + BL_AGE +
                         MEN + BMI + CURR_SMOKE, data = df)
print(summary(fit_lda))

## Add strata for plotting
fit_lda2 <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ strata(topic) + BL_AGE +
                         MEN + BMI + CURR_SMOKE, data = df)

jpeg("survival_prediction.jpeg", width = 900, height = 1000)
p <- ggsurvplot(survfit(fit_lda2), data = df,
                ylim = c(0.92, 1), xlim = c(0, 20),
                title = "LDA", conf.int=FALSE,
                ggtheme = theme_minimal(base_size=26), risk.table = TRUE, fontsize = 8,
                legend.title="", risk.table.height = 0.3)
p$table$theme$text$size <- 24
p
dev.off()

saveRDS(p, "survival_prediction_plot.RData")
