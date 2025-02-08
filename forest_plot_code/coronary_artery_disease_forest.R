coronary_artery_disease_dat <- read.csv('data/coronary_artery_disease.csv')

coronary_artery_disease_dat <- coronary_artery_disease_dat[-c(1,2),-c(6:9)]

coronary_artery_disease_dat <- setNames(coronary_artery_disease_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

coronary_artery_disease_dat$OR <- as.numeric(coronary_artery_disease_dat$OR)
coronary_artery_disease_dat$`Lower CI` <- as.numeric(coronary_artery_disease_dat$`Lower CI`)
coronary_artery_disease_dat$`Upper CI` <- as.numeric(coronary_artery_disease_dat$`Upper CI`)

coronary_artery_disease_dat$logOR <- log(coronary_artery_disease_dat$OR)
coronary_artery_disease_dat$logLCI <- log(coronary_artery_disease_dat$`Lower CI`)
coronary_artery_disease_dat$logUCI <- log(coronary_artery_disease_dat$`Upper CI`)

coronary_artery_disease_dat$se <-  (coronary_artery_disease_dat$logUCI - coronary_artery_disease_dat$logLCI) / (2 * 1.96)

summary(coronary_artery_disease_dat)

write.csv(coronary_artery_disease_dat, 'metareg/coronary_artery_disease.csv')

coronary_artery_disease_dat <- coronary_artery_disease_dat[order(coronary_artery_disease_dat$Study), ]

coronary_artery_disease_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = coronary_artery_disease_dat)

forest(coronary_artery_disease_meta, slab = coronary_artery_disease_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Coronary Artery Disease')
text(-2, -1, paste("Q =", round(coronary_artery_disease_meta$QE, 2), "(p =", round(coronary_artery_disease_meta$QEp, 4), ")"), cex = 1)
text(-2, -1.5, paste("I^2 =", round(coronary_artery_disease_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
coronary_artery_disease_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = coronary_artery_disease_dat, method = 'FE', weighted = F)
forest(coronary_artery_disease_metafe, slab = coronary_artery_disease_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'coronary_artery_disease')