subcut_fat_thickness_dat <- read.csv('data/subcut_fat_thickness.csv')

subcut_fat_thickness_dat <- subcut_fat_thickness_dat[-c(1,2),-c(6:9)]

subcut_fat_thickness_dat <- setNames(subcut_fat_thickness_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

subcut_fat_thickness_dat$OR <- as.numeric(subcut_fat_thickness_dat$OR)
subcut_fat_thickness_dat$`Lower CI` <- as.numeric(subcut_fat_thickness_dat$`Lower CI`)
subcut_fat_thickness_dat$`Upper CI` <- as.numeric(subcut_fat_thickness_dat$`Upper CI`)

subcut_fat_thickness_dat$logOR <- log(subcut_fat_thickness_dat$OR)
subcut_fat_thickness_dat$logLCI <- log(subcut_fat_thickness_dat$`Lower CI`)
subcut_fat_thickness_dat$logUCI <- log(subcut_fat_thickness_dat$`Upper CI`)

subcut_fat_thickness_dat$se <-  (subcut_fat_thickness_dat$logUCI - subcut_fat_thickness_dat$logLCI) / (2 * 1.96)

summary(subcut_fat_thickness_dat)

write.csv(subcut_fat_thickness_dat, 'metareg/subcut_fat_thickness.csv')

subcut_fat_thickness_dat <- subcut_fat_thickness_dat[order(subcut_fat_thickness_dat$Study), ]

subcut_fat_thickness_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = subcut_fat_thickness_dat)

forest(subcut_fat_thickness_meta, slab = subcut_fat_thickness_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Subcutaneous Fat Thickness >30mm')
text(-1.5, -1, paste("Q =", round(subcut_fat_thickness_meta$QE, 2), "(p =", round(subcut_fat_thickness_meta$QEp, 4), ")"))
text(-1.5, -1.4, paste("I^2 =", round(subcut_fat_thickness_meta$I2, 2), "%"))

# Forest Plot with Fixed Effect Models
subcut_fat_thickness_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = subcut_fat_thickness_dat, method = 'FE', weighted = F)
forest(subcut_fat_thickness_metafe, slab = subcut_fat_thickness_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'subcut_fat_thickness')