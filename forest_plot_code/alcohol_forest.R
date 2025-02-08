alcohol_dat <- read.csv('data/alcohol_consumption.csv')

alcohol_dat <- alcohol_dat[-c(1,2),-c(6:9)]

alcohol_dat <- setNames(alcohol_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

alcohol_dat$OR <- as.numeric(alcohol_dat$OR)
alcohol_dat$`Lower CI` <- as.numeric(alcohol_dat$`Lower CI`)
alcohol_dat$`Upper CI` <- as.numeric(alcohol_dat$`Upper CI`)

alcohol_dat$logOR <- log(alcohol_dat$OR)
alcohol_dat$logLCI <- log(alcohol_dat$`Lower CI`)
alcohol_dat$logUCI <- log(alcohol_dat$`Upper CI`)

alcohol_dat$se <-  (alcohol_dat$logUCI - alcohol_dat$logLCI) / (2 * 1.96)

summary(alcohol_dat)

write.csv(alcohol_dat, 'metareg/alcohol.csv')

alcohol_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = alcohol_dat)

forest(alcohol_meta, slab = alcohol_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Alcohol')
text(-4.5, -1, paste("Q =", round(alcohol_meta$QE, 2), "(p =", round(alcohol_meta$QEp, 4), ")"), cex = 1)
text(-4.5, -1.5, paste("I^2 =", round(alcohol_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
alcohol_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = alcohol_dat, method = 'FE', weighted = F)
forest(alcohol_metafe, slab = alcohol_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Alcohol')