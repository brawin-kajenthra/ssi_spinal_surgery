postop_albumin_low_dat <- read.csv('data/postop_albumin_low.csv')

postop_albumin_low_dat <- postop_albumin_low_dat[-c(1,2),-c(6:9)]

postop_albumin_low_dat <- setNames(postop_albumin_low_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

postop_albumin_low_dat$OR <- as.numeric(postop_albumin_low_dat$OR)
postop_albumin_low_dat$`Lower CI` <- as.numeric(postop_albumin_low_dat$`Lower CI`)
postop_albumin_low_dat$`Upper CI` <- as.numeric(postop_albumin_low_dat$`Upper CI`)

postop_albumin_low_dat$logOR <- log(postop_albumin_low_dat$OR)
postop_albumin_low_dat$logLCI <- log(postop_albumin_low_dat$`Lower CI`)
postop_albumin_low_dat$logUCI <- log(postop_albumin_low_dat$`Upper CI`)

postop_albumin_low_dat$se <-  (postop_albumin_low_dat$logUCI - postop_albumin_low_dat$logLCI) / (2 * 1.96)

summary(postop_albumin_low_dat)

write.csv(postop_albumin_low_dat, 'metareg/postop_albumin_low.csv')

postop_albumin_low_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = postop_albumin_low_dat)

forest(postop_albumin_low_meta, slab = postop_albumin_low_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Postoperative Albumin less than 3.5g/dL')
text(-2, -1, paste("Q =", round(postop_albumin_low_meta$QE, 2), "(p =", round(postop_albumin_low_meta$QEp, 4), ")"))
text(-2, -1.2, paste("I^2 =", round(postop_albumin_low_meta$I2, 2), "%"))

# Forest Plot with Fixed Effect Models
postop_albumin_low_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = postop_albumin_low_dat, method = 'FE', weighted = F)
forest(postop_albumin_low_metafe, slab = postop_albumin_low_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'postop_albumin_low')