chronic_steroid_use_dat <- read.csv('data/chronic_steroid_use.csv')

chronic_steroid_use_dat <- chronic_steroid_use_dat[-c(1,2),-c(6:9)]

chronic_steroid_use_dat <- setNames(chronic_steroid_use_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

chronic_steroid_use_dat$OR <- as.numeric(chronic_steroid_use_dat$OR)
chronic_steroid_use_dat$`Lower CI` <- as.numeric(chronic_steroid_use_dat$`Lower CI`)
chronic_steroid_use_dat$`Upper CI` <- as.numeric(chronic_steroid_use_dat$`Upper CI`)

chronic_steroid_use_dat$logOR <- log(chronic_steroid_use_dat$OR)
chronic_steroid_use_dat$logLCI <- log(chronic_steroid_use_dat$`Lower CI`)
chronic_steroid_use_dat$logUCI <- log(chronic_steroid_use_dat$`Upper CI`)

chronic_steroid_use_dat$se <-  (chronic_steroid_use_dat$logUCI - chronic_steroid_use_dat$logLCI) / (2 * 1.96)

summary(chronic_steroid_use_dat)

write.csv(chronic_steroid_use_dat, 'metareg/chronic_steroid_use.csv')

chronic_steroid_use_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = chronic_steroid_use_dat)

forest(chronic_steroid_use_meta, slab = chronic_steroid_use_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Chronic Steroid Use')
text(-2.5, -1, paste("Q =", round(chronic_steroid_use_meta$QE, 2), "(p =", round(chronic_steroid_use_meta$QEp, 4), ")"), cex = 1)
text(-2.5, -1.5, paste("I^2 =", round(chronic_steroid_use_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
chronic_steroid_use_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = chronic_steroid_use_dat, method = 'FE', weighted = F)
forest(chronic_steroid_use_metafe, slab = chronic_steroid_use_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'chronic_steroid_use')