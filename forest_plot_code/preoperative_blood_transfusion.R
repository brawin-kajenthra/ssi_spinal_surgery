preop_blood_transfusion_dat <- read.csv('data/preop_blood_transfusion.csv')

preop_blood_transfusion_dat <- preop_blood_transfusion_dat[-c(1,2),-c(6:9)]

preop_blood_transfusion_dat <- setNames(preop_blood_transfusion_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

preop_blood_transfusion_dat$OR <- as.numeric(preop_blood_transfusion_dat$OR)
preop_blood_transfusion_dat$`Lower CI` <- as.numeric(preop_blood_transfusion_dat$`Lower CI`)
preop_blood_transfusion_dat$`Upper CI` <- as.numeric(preop_blood_transfusion_dat$`Upper CI`)

preop_blood_transfusion_dat$logOR <- log(preop_blood_transfusion_dat$OR)
preop_blood_transfusion_dat$logLCI <- log(preop_blood_transfusion_dat$`Lower CI`)
preop_blood_transfusion_dat$logUCI <- log(preop_blood_transfusion_dat$`Upper CI`)

preop_blood_transfusion_dat$se <-  (preop_blood_transfusion_dat$logUCI - preop_blood_transfusion_dat$logLCI) / (2 * 1.96)

summary(preop_blood_transfusion_dat)

write.csv(preop_blood_transfusion_dat, 'metareg/preop_blood_transfusion.csv')

preop_blood_transfusion_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = preop_blood_transfusion_dat)

forest(preop_blood_transfusion_meta, slab = preop_blood_transfusion_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Preoperative Blood Transfusion')
text(-2, -1, paste("Q =", round(preop_blood_transfusion_meta$QE, 2), "(p =", round(preop_blood_transfusion_meta$QEp, 4), ")"), cex = 1)
text(-2, -1.5, paste("I^2 =", round(preop_blood_transfusion_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
preop_blood_transfusion_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = preop_blood_transfusion_dat, method = 'FE', weighted = F)
forest(preop_blood_transfusion_metafe, slab = preop_blood_transfusion_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'preop_blood_transfusion')