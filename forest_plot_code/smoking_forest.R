smoking_dat <- read.csv('data/smoking.csv')

smoking_dat <- smoking_dat[-c(1,2),-c(6:9)]

smoking_dat <- setNames(smoking_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

smoking_dat$OR <- as.numeric(smoking_dat$OR)
smoking_dat$`Lower CI` <- as.numeric(smoking_dat$`Lower CI`)
smoking_dat$`Upper CI` <- as.numeric(smoking_dat$`Upper CI`)

smoking_dat$logOR <- log(smoking_dat$OR)
smoking_dat$logLCI <- log(smoking_dat$`Lower CI`)
smoking_dat$logUCI <- log(smoking_dat$`Upper CI`)

smoking_dat$se <-  (smoking_dat$logUCI - smoking_dat$logLCI) / (2 * 1.96)

summary(smoking_dat)

write.csv(smoking_dat, 'metareg/smoking.csv')
smoking_dat <- smoking_dat[order(smoking_dat$Study), ]

smoking_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = smoking_dat)

forest(smoking_meta, slab = smoking_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Smoking')
text(-5.5, -1, paste("Q =", round(smoking_meta$QE, 2), "(p =", round(smoking_meta$QEp, 4), ")"), cex = 1)
text(-5.5, -1.5, paste("I^2 =", round(smoking_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
smoking_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = smoking_dat, method = 'FE', weighted = F)
forest(smoking_metafe, slab = smoking_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'smoking')