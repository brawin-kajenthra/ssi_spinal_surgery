low_preoperative_haematocrit_dat <- read.csv('data/low_preoperative_haematocrit.csv')

low_preoperative_haematocrit_dat$logOR <- log(low_preoperative_haematocrit_dat$OR)
low_preoperative_haematocrit_dat$logLCI <- log(low_preoperative_haematocrit_dat$Lower.CI)
low_preoperative_haematocrit_dat$logUCI <- log(low_preoperative_haematocrit_dat$Upper.CI)
low_preoperative_haematocrit_dat$se <-  (low_preoperative_haematocrit_dat$logUCI - low_preoperative_haematocrit_dat$logLCI) / (2 * 1.96)

write.csv(low_preoperative_haematocrit_dat, 'metareg/low_preoperative_haematocrit.csv')

# Weigh studies using inverse variance

low_preoperative_haematocrit_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = low_preoperative_haematocrit_dat)

# Create forest plot and add results from tests of heterogeneity

forest(low_preoperative_haematocrit_meta, slab = low_preoperative_haematocrit_dat$First.Author, 
       header = TRUE, atransf = exp, xlab = 'Odds Ratio', 
       xlim = c(-5, 5), main = 'Low Preoperative Haematocrit')

# Add Q statistic text
text(-2.5, -1, paste("Q =", round(low_preoperative_haematocrit_meta$QE, 2), 
                     "(p =", round(low_preoperative_haematocrit_meta$QEp, 4), ")"), cex = 1)

# Add I² statistic text
text(-2.5, -1.5, paste("I^2 =", round(low_preoperative_haematocrit_meta$I2, 2), "%"), cex = 1)


# Forest Plot with Fixed Effect Models
low_preoperative_haematocrit_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = low_preoperative_haematocrit_dat, method = 'FE', weighted = F)
forest(low_preoperative_haematocrit_metafe, slab = low_preoperative_haematocrit_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Low Preoperative Haematocrit')