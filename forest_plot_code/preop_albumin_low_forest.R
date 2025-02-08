preop_albumin_low_dat <- read.csv('data/preop_albumin_low.csv')

preop_albumin_low_dat <- preop_albumin_low_dat[-c(1,2),-c(6:9)]

preop_albumin_low_dat <- setNames(preop_albumin_low_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

preop_albumin_low_dat$OR <- as.numeric(preop_albumin_low_dat$OR)
preop_albumin_low_dat$`Lower CI` <- as.numeric(preop_albumin_low_dat$`Lower CI`)
preop_albumin_low_dat$`Upper CI` <- as.numeric(preop_albumin_low_dat$`Upper CI`)

preop_albumin_low_dat$logOR <- log(preop_albumin_low_dat$OR)
preop_albumin_low_dat$logLCI <- log(preop_albumin_low_dat$`Lower CI`)
preop_albumin_low_dat$logUCI <- log(preop_albumin_low_dat$`Upper CI`)

preop_albumin_low_dat$se <-  (preop_albumin_low_dat$logUCI - preop_albumin_low_dat$logLCI) / (2 * 1.96)

summary(preop_albumin_low_dat)

write.csv(preop_albumin_low_dat, 'metareg/preop_albumin_low.csv')

preop_albumin_low_dat <- preop_albumin_low_dat[order(preop_albumin_low_dat$Study), ]

preop_albumin_low_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = preop_albumin_low_dat)

forest(preop_albumin_low_meta, slab = preop_albumin_low_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Preoperative Albumin less than 3.5g/dL')
text(-4, -1, paste("Q =", round(preop_albumin_low_meta$QE, 2), "(p =", round(preop_albumin_low_meta$QEp, 4), ")"), cex = 1)
text(-4, -1.5, paste("I^2 =", round(preop_albumin_low_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
preop_albumin_low_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = preop_albumin_low_dat, method = 'FE', weighted = F)
forest(preop_albumin_low_metafe, slab = preop_albumin_low_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'preop_albumin_low')