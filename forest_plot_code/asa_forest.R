# Load in relevant packages

library(metafor)

# Read in and tidy data

asa_dat <- read.csv('data/asa_3_plus.csv')

asa_dat <- asa_dat[-c(1,2),]
asa_dat <- asa_dat[, -c(5:8)]

asa_dat <- setNames(asa_dat, c('Study', 'Year', 'Country', 'Type', 'OR', 'Lower CI', 'Upper CI'))

asa_dat$OR <- as.numeric(asa_dat$OR)
asa_dat$`Lower CI` <- as.numeric(asa_dat$`Lower CI`)
asa_dat$`Upper CI` <- as.numeric(asa_dat$`Upper CI`)

asa_dat$logOR <- log(asa_dat$OR)
asa_dat$logLCI <- log(asa_dat$`Lower CI`)
asa_dat$logUCI <- log(asa_dat$`Upper CI`)
asa_dat$se <-  (asa_dat$logUCI - asa_dat$logLCI) / (2 * 1.96)


summary(asa_dat) # Check if log transformations have been done correctly

write.csv(asa_dat, 'metareg/asa.csv')

# Weigh studies using inverse variance

asa_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = asa_dat)

# Create forest plot and add results from tests of heterogeneity

forest(asa_meta, slab = asa_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'ASA greater than or equal to 3')
text(-1.2, -1, paste("Q =", round(asa_meta$QE, 2), "(p =", round(asa_meta$QEp, 4), ")"), cex = 1)
text(-1.2, -1.5, paste("I^2 =", round(asa_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
asa_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = asa_dat, method = 'FE', weighted = F)
forest(asa_metafe, slab = asa_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'asa')

