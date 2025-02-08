iddm_dat <- read.csv('data/iddm.csv')

iddm_dat <- iddm_dat[-c(1,2),-c(6:9)]

iddm_dat <- setNames(iddm_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

iddm_dat$OR <- as.numeric(iddm_dat$OR)
iddm_dat$`Lower CI` <- as.numeric(iddm_dat$`Lower CI`)
iddm_dat$`Upper CI` <- as.numeric(iddm_dat$`Upper CI`)

iddm_dat$logOR <- log(iddm_dat$OR)
iddm_dat$logLCI <- log(iddm_dat$`Lower CI`)
iddm_dat$logUCI <- log(iddm_dat$`Upper CI`)

iddm_dat$se <-  (iddm_dat$logUCI - iddm_dat$logLCI) / (2 * 1.96)

summary(iddm_dat)

write.csv(iddm_dat, 'metareg/iddm.csv')

iddm_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = iddm_dat)

forest(iddm_meta, slab = iddm_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Insulin Dependent Diabetes Mellitus')
text(-2.5, -1, paste("Q =", round(iddm_meta$QE, 2), "(p =", round(iddm_meta$QEp, 4), ")"), cex = 1)
text(-2.5, -1.5, paste("I^2 =", round(iddm_meta$I2, 2), "%"), cex = 1)


# Forest Plot with Fixed Effect Models
iddm_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = iddm_dat, method = 'FE', weighted = F)
forest(iddm_metafe, slab = iddm_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'iddm')