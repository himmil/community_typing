library(TreeSummarizedExperiment)
library(mia)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

### Cox regression adjusted with age, sex, bmi, and smoking
tse <- readRDS("tse_nmf.RData")

# select the components
W <- as.data.frame(colData(tse)) %>% select(starts_with("nmf"))
colnames(W) <- c(1:5)

# select the covariates
df_nmf <- colData(tse)[,c("Barcode","DEATH","DEATH_AGEDIFF", "BL_AGE", "MEN", "BMI", "CURR_SMOKE")]
df_nmf$component <- colnames(W)[apply(W, 1, which.max)]

df_nmf$component <- as.factor(df_nmf$component)
df_nmf$component <- relevel(df_nmf$component, ref = 1) # Bacteroides as a reference level

fit_nmf <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ component + BL_AGE +
                MEN + BMI + CURR_SMOKE, data = df_nmf)
print(summary(fit_nmf))
print(cox.zph(fit_nmf))


##### Kaplan Meier plot

# Add strata to enable plotting
fit_nmf2 <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ strata(component) + BL_AGE +
                MEN + BMI + CURR_SMOKE, data = df_nmf)

jpeg("nmf_survival_km.jpeg", width = 600, height = 800)
p <- ggsurvplot(survfit(fit_nmf2), data = df_nmf,
                ylim = c(0.91, 1), xlim = c(0, 20),
                title = "NMF", conf.int=TRUE, 
		ggtheme = theme_minimal(base_size=26), risk.table = TRUE, fontsize = 8,
                legend.title="", risk.table.height = 0.3)
p$table$theme$text$size <- 24
p
dev.off())

saveRDS(p, "nmf_survival_km_plot.RData")
