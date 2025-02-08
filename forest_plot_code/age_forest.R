# Set up useful packages

library(metafor)

age_dat <- read.csv('data/age.csv')

age_dat <- age_dat[-c(1,2),-c(6:9)]

age_dat <- setNames(age_dat, c('Study', 'Year', 'Country', 'Type', 'Number of Patients', 'OR', 'Lower CI', 'Upper CI'))

age_dat$OR <- as.numeric(age_dat$OR)
age_dat$`Lower CI` <- as.numeric(age_dat$`Lower CI`)
age_dat$`Upper CI` <- as.numeric(age_dat$`Upper CI`)

age_dat$logOR <- log(age_dat$OR)
age_dat$logLCI <- log(age_dat$`Lower CI`)
age_dat$logUCI <- log(age_dat$`Upper CI`)

age_dat$se <-  (age_dat$logUCI - age_dat$logLCI) / (2 * 1.96)

summary(age_dat)

write.csv(age_dat, 'metareg/age.csv')

age_dat <- age_dat[order(age_dat$Study), ]

age_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = age_dat)

forest(age_meta, slab = age_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Age')
text(-1.5, -1, paste("Q =", round(age_meta$QE, 2), "(p =", round(age_meta$QEp, 4), ")"), cex = 1)
text(-1.5, -1.5, paste("I^2 =", round(age_meta$I2, 2), "%"), cex = 1)

# Funnel Plot and Eggers Regression Test
age_eggers <- regtest(age_meta, data = T)
funnel(age_meta, main = 'Funnel Plot: Age')
lines(coef(age_eggers$fit)[1] + coef(age_eggers$fit)[2]*age_eggers$fit$se, age_eggers$fit$se, lwd=2, lty = 2)

# Forest Plot with Fixed Effect Models
age_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = age_dat, method = 'FE', weighted = F)
forest(age_metafe, slab = age_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Age')


# Create a data frame for plotting
agemetadf <- data.frame(
  se = age_meta$se,
  y = age_meta$yi
)

# Add the regression line
reg_line <- data.frame(
  se = seq(min(agemetadf$se), max(agemetadf$se), length.out = 100),
  y = coef(age_eggers$fit[1])+ coef(age_eggers$fit[2]) * seq(min(agemetadf$se), max(agemetadf$se), length.out = 100)
)




