# Load in relevant packages

library(metafor)

# Read in and tidy data

asa_dat <- read.csv('data/asa_4_and_5.csv')

asa_dat$logOR <- log(asa_dat$OR)
asa_dat$logLCI <- log(asa_dat$Lower.CI)
asa_dat$logUCI <- log(asa_dat$Upper.CI)
asa_dat$se <-  (asa_dat$logUCI - asa_dat$logLCI) / (2 * 1.96)

write.csv(asa_dat, 'metareg/asa_4_and_5.csv')

# Weigh studies using inverse variance

asa_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = asa_dat)

# Create forest plot and add results from tests of heterogeneity

forest(asa_meta, slab = asa_dat$First.Author, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'ASA 4-5')
text(-0.8, -1, paste("Q =", round(asa_meta$QE, 2), "(p =", round(asa_meta$QEp, 4), ")"), cex = 1)
text(-0.8, -1.5, paste("I^2 =", round(asa_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
asa_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = asa_dat, method = 'FE', weighted = F)
forest(asa_metafe, slab = asa_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'ASA Grades 4 and 5 (FE model)')
