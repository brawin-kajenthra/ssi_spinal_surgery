vancomycin_dat <- read.csv('data/vancomycin_powder.csv')

vancomycin_dat <- vancomycin_dat[-c(1,2),-c(6:9)]
vancomycin_dat <- vancomycin_dat[,-c(9)]

vancomycin_dat <- setNames(vancomycin_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

vancomycin_dat$OR <- as.numeric(vancomycin_dat$OR)
vancomycin_dat$`Lower CI` <- as.numeric(vancomycin_dat$`Lower CI`)
vancomycin_dat$`Upper CI` <- as.numeric(vancomycin_dat$`Upper CI`)

vancomycin_dat$logOR <- log(vancomycin_dat$OR)
vancomycin_dat$logLCI <- log(vancomycin_dat$`Lower CI`)
vancomycin_dat$logUCI <- log(vancomycin_dat$`Upper CI`)

vancomycin_dat$se <-  (vancomycin_dat$logUCI - vancomycin_dat$logLCI) / (2 * 1.96)

summary(vancomycin_dat)

write.csv(vancomycin_dat, 'metareg/vancomycin.csv')

vancomycin_dat <- vancomycin_dat[order(vancomycin_dat$Study), ]

vancomycin_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = vancomycin_dat)

forest(vancomycin_meta, slab = vancomycin_dat$Study, header = T, atransf = exp, 
       xlab = 'Odds Ratio', main = 'Intraoperative Use of Vancomycin Powder')
text(-6.25, -1, paste("Q =", round(vancomycin_meta$QE, 2), "(p =", round(vancomycin_meta$QEp, 4), ")"), cex = 1)
text(-6.25, -1.5, paste("I^2 =", round(vancomycin_meta$I2, 2), "%"), cex = 1)

# Funnel Plot and Eggers Regression Test
funnel(vancomycin_meta, main = 'Funnel Plot: Intraoperative Use of Vancomycin Powder')
regtest(vancomycin_meta)

# Forest Plot with Fixed Effect Models
vancomycin_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = vancomycin_dat, method = 'FE', weighted = F)
forest(vancomycin_metafe, slab = vancomycin_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'vancomycin')