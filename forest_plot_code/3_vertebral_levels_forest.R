# Load in relevant packages

library(metafor)

# Read in and tidy data

vertebral_dat <- read.csv('data/vertebral_levels_3_and_greater.csv')

vertebral_dat$logOR <- log(vertebral_dat$OR)
vertebral_dat$logLCI <- log(vertebral_dat$Lower.CI)
vertebral_dat$logUCI <- log(vertebral_dat$Upper.CI)
vertebral_dat$se <-  (vertebral_dat$logUCI - vertebral_dat$logLCI) / (2 * 1.96)

write.csv(vertebral_dat, 'metareg/vertebral_levels_3_and_greater.csv')

vertebral_dat <- vertebral_dat[, order(names(vertebral_dat))]

# Weigh studies using inverse variance

vertebral_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = vertebral_dat)

# Create forest plot and add results from tests of heterogeneity

forest(vertebral_meta, slab = vertebral_dat$First.Author, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Procedures involving ≥ 3 vertebral levels')
text(-2, -1, paste("Q =", round(vertebral_meta$QE, 2), "(p =", round(vertebral_meta$QEp, 4), ")"), cex = 1)
text(-2, -1.5, paste("I^2 =", round(vertebral_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
vertebral_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = vertebral_dat, method = 'FE', weighted = F)
forest(vertebral_metafe, slab = vertebral_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Procedures involving three or more vertebral levels (FE model)')
