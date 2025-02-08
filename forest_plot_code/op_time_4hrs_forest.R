# Load in relevant packages

library(metafor)

# Read in and tidy data

op_time_dat <- read.csv('data/operating_time_4hrs_plus.csv')

op_time_dat$logOR <- log(op_time_dat$OR)
op_time_dat$logLCI <- log(op_time_dat$Lower.CI)
op_time_dat$logUCI <- log(op_time_dat$Upper.CI)
op_time_dat$se <-  (op_time_dat$logUCI - op_time_dat$logLCI) / (2 * 1.96)

write.csv(op_time_dat, 'metareg/operating_time_4hrs_plus.csv')
op_time_dat <- op_time_dat[order(op_time_dat$Study), ]

# Weigh studies using inverse variance

op_time_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = op_time_dat)

# Create forest plot and add results from tests of heterogeneity

forest(op_time_meta, slab = op_time_dat$First.Author, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Operating Time (> 4hrs)')
text(-2.5, -1, paste("Q =", round(op_time_meta$QE, 2), "(p =", round(op_time_meta$QEp, 4), ")"), cex = 1)
text(-2.5, -1.5, paste("I^2 =", round(op_time_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
op_time_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = op_time_dat, method = 'FE', weighted = F)
forest(op_time_metafe, slab = op_time_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Operating Time (> 4hrs) (FE model)')
