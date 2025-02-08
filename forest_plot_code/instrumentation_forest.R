instrumentation_dat <- read.csv('data/instrumentation.csv')

instrumentation_dat <- instrumentation_dat[-c(1,2),-c(6:9)]

instrumentation_dat <- setNames(instrumentation_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

instrumentation_dat$OR <- as.numeric(instrumentation_dat$OR)
instrumentation_dat$`Lower CI` <- as.numeric(instrumentation_dat$`Lower CI`)
instrumentation_dat$`Upper CI` <- as.numeric(instrumentation_dat$`Upper CI`)

instrumentation_dat$logOR <- log(instrumentation_dat$OR)
instrumentation_dat$logLCI <- log(instrumentation_dat$`Lower CI`)
instrumentation_dat$logUCI <- log(instrumentation_dat$`Upper CI`)

instrumentation_dat$se <-  (instrumentation_dat$logUCI - instrumentation_dat$logLCI) / (2 * 1.96)

summary(instrumentation_dat)

write.csv(instrumentation_dat, 'metareg/instrumentation.csv')

instrumentation_dat <- instrumentation_dat[order(instrumentation_dat$Study), ]

instrumentation_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = instrumentation_dat)

forest(instrumentation_meta, slab = instrumentation_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Instrumentation')
text(-3, -1, paste("Q =", round(instrumentation_meta$QE, 2), "(p =", round(instrumentation_meta$QEp, 4), ")"), cex = 1)
text(-3, -1.5, paste("I^2 =", round(instrumentation_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
instrumentation_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = instrumentation_dat, method = 'FE', weighted = F)
forest(instrumentation_metafe, slab = instrumentation_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'instrumentation')