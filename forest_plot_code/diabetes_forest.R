diabetes_dat <- read.csv('data/diabetes.csv')

diabetes_dat <- diabetes_dat[-c(1,2),-c(6:9)]

diabetes_dat <- setNames(diabetes_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

diabetes_dat$OR <- as.numeric(diabetes_dat$OR)
diabetes_dat$`Lower CI` <- as.numeric(diabetes_dat$`Lower CI`)
diabetes_dat$`Upper CI` <- as.numeric(diabetes_dat$`Upper CI`)

diabetes_dat$logOR <- log(diabetes_dat$OR)
diabetes_dat$logLCI <- log(diabetes_dat$`Lower CI`)
diabetes_dat$logUCI <- log(diabetes_dat$`Upper CI`)

diabetes_dat$se <-  (diabetes_dat$logUCI - diabetes_dat$logLCI) / (2 * 1.96)

summary(diabetes_dat)

write.csv(diabetes_dat, 'metareg/diabetes.csv')

diabetes_dat <- diabetes_dat[order(diabetes_dat$Study), ]

diabetes_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = diabetes_dat)

forest(diabetes_meta, slab = diabetes_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Diabetes')
text(-3.7, -1, paste("Q =", round(diabetes_meta$QE, 2), "(p =", round(diabetes_meta$QEp, 4), ")"), cex = 1)
text(-3.7, -1.5, paste("I^2 =", round(diabetes_meta$I2, 2), "%"), cex = 1)

# Funnel Plot and Eggers Regression Test
funnel(diabetes_meta, main = 'Funnel Plot: Diabetes')
regtest(diabetes_meta)


# Forest Plot with Fixed Effect Models
diabetes_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = diabetes_dat, method = 'FE', weighted = F)
forest(diabetes_metafe, slab = diabetes_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Diabetes')
