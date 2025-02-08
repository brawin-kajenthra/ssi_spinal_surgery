sex_dat <- read.csv('data/female_sex.csv')

sex_dat <- sex_dat[-c(1,2),-c(6:9)]

sex_dat <- setNames(sex_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

sex_dat$OR <- as.numeric(sex_dat$OR)
sex_dat$`Lower CI` <- as.numeric(sex_dat$`Lower CI`)
sex_dat$`Upper CI` <- as.numeric(sex_dat$`Upper CI`)

sex_dat$logOR <- log(sex_dat$OR)
sex_dat$logLCI <- log(sex_dat$`Lower CI`)
sex_dat$logUCI <- log(sex_dat$`Upper CI`)

sex_dat$se <-  (sex_dat$logUCI - sex_dat$logLCI) / (2 * 1.96)

summary(sex_dat)

write.csv(sex_dat, 'metareg/sex.csv')
sex_dat <- sex_dat[order(sex_dat$Study), ]

sex_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = sex_dat)

forest(sex_meta, slab = sex_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Sex (Reference: Female)')
text(-2.5, -1, paste("Q =", round(sex_meta$QE, 2), "(p =", round(sex_meta$QEp, 4), ")"), cex = 1)
text(-2.5, -1.5, paste("I^2 =", round(sex_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
sex_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = sex_dat, method = 'FE', weighted = F)
forest(sex_metafe, slab = sex_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'sex')