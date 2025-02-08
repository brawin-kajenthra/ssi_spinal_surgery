# Load in relevant packages

library(metafor)

# Read in and tidy data

revision_surgery_dat <- read.csv('data/revision_surgery.csv')

revision_surgery_dat$logOR <- log(revision_surgery_dat$OR)
revision_surgery_dat$logLCI <- log(revision_surgery_dat$Lower.CI)
revision_surgery_dat$logUCI <- log(revision_surgery_dat$Upper.CI)
revision_surgery_dat$se <-  (revision_surgery_dat$logUCI - revision_surgery_dat$logLCI) / (2 * 1.96)

write.csv(revision_surgery_dat, 'metareg/revision_surgery.csv')

# Weigh studies using inverse variance

revision_surgery_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = revision_surgery_dat)

# Create forest plot and add results from tests of heterogeneity

forest(revision_surgery_meta, slab = revision_surgery_dat$First.Author, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Revision Surgery')
text(-2.5, -1, paste("Q =", round(revision_surgery_meta$QE, 2), "(p =", round(revision_surgery_meta$QEp, 4), ")"), cex = 1)
text(-2.5, -1.5, paste("I^2 =", round(revision_surgery_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
revision_surgery_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = revision_surgery_dat, method = 'FE', weighted = F)
forest(revision_surgery_metafe, slab = revision_surgery_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Revision Surgery')
