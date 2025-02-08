bmi_dat <- read.csv('data/increased_bmi.csv')

bmi_dat <- bmi_dat[-c(1,2),-c(6:9)]

bmi_dat <- setNames(bmi_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

bmi_dat$OR <- as.numeric(bmi_dat$OR)
bmi_dat$`Lower CI` <- as.numeric(bmi_dat$`Lower CI`)
bmi_dat$`Upper CI` <- as.numeric(bmi_dat$`Upper CI`)

bmi_dat$logOR <- log(bmi_dat$OR)
bmi_dat$logLCI <- log(bmi_dat$`Lower CI`)
bmi_dat$logUCI <- log(bmi_dat$`Upper CI`)

bmi_dat$se <-  (bmi_dat$logUCI - bmi_dat$logLCI) / (2 * 1.96)

summary(bmi_dat)

bmi_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = bmi_dat)
bmi_dat <- bmi_dat[order(bmi_dat$Study), ]

write.csv(bmi_dat, 'metareg/bmi.csv')

forest(bmi_meta, slab = bmi_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'BMI')
text(-2.25, -1, paste("Q =", round(bmi_meta$QE, 2), "(p =", round(bmi_meta$QEp, 4), ")"), cex = 1)
text(-2.25, -1.5, paste("I^2 =", round(bmi_meta$I2, 2), "%"), cex = 1)



# Funnel Plot and Eggers Regression Test
funnel(bmi_meta, main = 'Funnel Plot: BMI')
regtest(bmi_meta)

# Forest Plot with Fixed Effect Models
bmi_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = bmi_dat, method = 'FE', weighted = F)
forest(bmi_metafe, slab = bmi_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'bmi')