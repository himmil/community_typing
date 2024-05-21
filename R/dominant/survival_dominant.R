library(TreeSummarizedExperiment)
library(mia)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

### Cox regression adjusted with age, sex, bmi, and smoking
tse <- readRDS("tse_dominant_top8.RData")
dominant <- tse$dominant_f

# select the covariates
df <- colData(tse)[,c("Barcode","DEATH","DEATH_AGEDIFF", "BL_AGE", "MEN", "BMI", "CURR_SMOKE")]
df$dominant <- tse$dominant_f

# Clean the names
df$dominant <- as.factor(df$dominant)
df$dominant <- relevel(df$dominant, ref = "Bacteroides_H")

#### survival analysis
fit_dom <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ dominant + BL_AGE +
		 MEN + BMI + CURR_SMOKE, data = df)
print(summary(fit_dom))
# Check if there is evidence against cox regression model
print(cox.zph(fit_dom))


# Add strata for plotting
fit_dom2 <- coxph(Surv(DEATH_AGEDIFF, DEATH) ~ dominant + BL_AGE +
                 MEN + BMI + CURR_SMOKE, data = df)

jpeg("dominant_survival_km.jpeg", width = 800, height = 1000)
p <- ggsurvplot(survfit(fit_dom2), data = df,
                ylim = c(0.92, 1), xlim = c(0, 20),
                title = "Dominant taxa", conf.int=FALSE,
                ggtheme = theme_minimal(base_size=26), risk.table = TRUE, fontsize = 8,
                legend.title="", risk.table.height = 0.4) + guides(colour = guide_legend(nrow = 3))
p$table$theme$text$size <- 24
p
dev.off()

saveRDS(p, "dominant_survival_km_plot.RData")
