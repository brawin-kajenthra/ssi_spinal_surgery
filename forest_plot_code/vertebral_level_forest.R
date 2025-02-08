vertebral_level_dat <- read.csv('data/vertebral_level.csv')

vertebral_level_dat <- vertebral_level_dat[-c(1,2),-c(6:9)]

vertebral_level_dat <- setNames(vertebral_level_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

vertebral_level_dat$OR <- as.numeric(vertebral_level_dat$OR)
vertebral_level_dat$`Lower CI` <- as.numeric(vertebral_level_dat$`Lower CI`)
vertebral_level_dat$`Upper CI` <- as.numeric(vertebral_level_dat$`Upper CI`)

vertebral_level_dat$logOR <- log(vertebral_level_dat$OR)
vertebral_level_dat$logLCI <- log(vertebral_level_dat$`Lower CI`)
vertebral_level_dat$logUCI <- log(vertebral_level_dat$`Upper CI`)

vertebral_level_dat$se <-  (vertebral_level_dat$logUCI - vertebral_level_dat$logLCI) / (2 * 1.96)

summary(vertebral_level_dat)

write.csv(vertebral_level_dat, 'metareg/vertebral_level.csv')

vertebral_level_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = vertebral_level_dat)

forest(vertebral_level_meta, slab = vertebral_level_dat$Study, header = T, atransf = exp, 
       xlab = 'Odds Ratio', main = 'Surgery involving multiple vertebral levels')
text(-1.5, -1, paste("Q =", round(vertebral_level_meta$QE, 2), "(p =", round(vertebral_level_meta$QEp, 4), ")"), cex = 1)
text(-1.5, -1.5, paste("I^2 =", round(vertebral_level_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
vertebral_level_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = vertebral_level_dat, method = 'FE', weighted = F)
forest(vertebral_level_metafe, slab = vertebral_level_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'vertebral_level')
