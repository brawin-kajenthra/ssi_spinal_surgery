# Load in relevant packages

library(metafor)

# Read in and tidy data

thoracic_spine_dat <- read.csv('data/thoracic_spine.csv')

thoracic_spine_dat$logOR <- log(thoracic_spine_dat$OR)
thoracic_spine_dat$logLCI <- log(thoracic_spine_dat$Lower.CI)
thoracic_spine_dat$logUCI <- log(thoracic_spine_dat$Upper.CI)
thoracic_spine_dat$se <-  (thoracic_spine_dat$logUCI - thoracic_spine_dat$logLCI) / (2 * 1.96)

write.csv(thoracic_spine_dat, 'metareg/thoracic_spine.csv')

# Weigh studies using inverse variance

thoracic_spine_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = thoracic_spine_dat)

# Create forest plot and add results from tests of heterogeneity

forest(thoracic_spine_meta, slab = thoracic_spine_dat$First.Author, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Surgery involving the thoracic spine')
text(-5, -1, paste("Q =", round(thoracic_spine_meta$QE, 2), "(p =", round(thoracic_spine_meta$QEp, 4), ")"), cex = 1)
text(-5, -1.5, paste("I^2 =", round(thoracic_spine_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
thoracic_spine_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = thoracic_spine_dat, method = 'FE', weighted = F)
forest(thoracic_spine_metafe, slab = thoracic_spine_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Surgery involving the thoracic spine')
