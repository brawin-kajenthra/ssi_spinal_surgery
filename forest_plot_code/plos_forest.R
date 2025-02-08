# Load in relevant packages

library(metafor)

# Read in and tidy data

perioperative_length_of_stay_dat <- read.csv('data/perioperative_length_of_stay.csv')

perioperative_length_of_stay_dat$logOR <- log(perioperative_length_of_stay_dat$OR)
perioperative_length_of_stay_dat$logLCI <- log(perioperative_length_of_stay_dat$Lower.CI)
perioperative_length_of_stay_dat$logUCI <- log(perioperative_length_of_stay_dat$Upper.CI)
perioperative_length_of_stay_dat$se <-  (perioperative_length_of_stay_dat$logUCI - perioperative_length_of_stay_dat$logLCI) / (2 * 1.96)

write.csv(perioperative_length_of_stay_dat, 'metareg/perioperative_length_of_stay.csv')

# Weigh studies using inverse variance

perioperative_length_of_stay_dat <- perioperative_length_of_stay_dat %>% 
  filter(First.Author != 'Lieber, B.')

perioperative_length_of_stay_meta <- rma(measure = 'OR', yi = logOR, sei = se, dat = perioperative_length_of_stay_dat)

# Create forest plot and add results from tests of heterogeneity

forest(perioperative_length_of_stay_meta, slab = perioperative_length_of_stay_dat$First.Author, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Perioperative Length of Stay')
text(-2.5, -1, paste("Q =", round(perioperative_length_of_stay_meta$QE, 2), "(p =", round(perioperative_length_of_stay_meta$QEp, 4), ")"), cex = 1)
text(-2.5, -1.5, paste("I^2 =", round(perioperative_length_of_stay_meta$I2, 2), "%"), cex = 1)

# Forest Plot with Fixed Effect Models
perioperative_length_of_stay_metafe <- rma(measure = 'OR', yi = logOR, sei = se, dat = perioperative_length_of_stay_dat, method = 'FE', weighted = F)
forest(perioperative_length_of_stay_metafe, slab = perioperative_length_of_stay_dat$Study, header = T, atransf = exp, xlab = 'Odds Ratio', main = 'Perioperative Length of Stay')
