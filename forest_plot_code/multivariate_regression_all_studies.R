# Load combined dataset

combined <- read.csv('metareg/combined_dat.csv')

# Fit multivariate regression model without intercept (as no clinical significance)
# Using maximum likelihood as coefficient estimation method

res <- rma(yi = logOR, sei = se, mods = ~ Factor - 1, data = combined, method = 'ML')

# Coefficients and their confidence intervals
coefficients <- coef(res)

# Extract standard errors of the coefficients
se <- res$se

# Compute 95% CIs for log odds ratios (logORs)
lower_CIs_logOR <- coefficients - 1.96 * se
upper_CIs_logOR <- coefficients + 1.96 * se

# Exponentiate the logORs and CIs to convert them back to odds ratios (ORs)
ORs <- exp(coefficients)
lower_CIs <- exp(lower_CIs_logOR)
upper_CIs <- exp(upper_CIs_logOR)

# Create a data frame for the plot
forest_data <- data.frame(
  Factor = names(coefficients),
  OR = ORs,
  LowerCI = lower_CIs,
  UpperCI = upper_CIs
)

# Replace the word 'Factor' with an empty string across all columns
forest_data <- data.frame(lapply(forest_data, function(x) gsub("\\bFactor\\b", "", x)))
forest_data$Variables <- gsub("Factor", "", forest_data$Factor)

forest_data <- subset(forest_data, select = -c(Factor))

write.csv(forest_data, 'metareg/multivariate_OR_table.csv')

# Influence analysis plot - Cook's Distance

combined <- combined[order(combined$Factor), ]

ggplot(combined) + 
  geom_point(aes(x = Study, y = cooks, color = influence == "TRUE")) + 
  scale_color_manual(values = c("black", "red"), guide = "none") +
  facet_wrap(~Factor, scales = 'free', ncol = 4) +
  scale_x_discrete(labels = scales::wrap_format(10)) + 
  theme_minimal() + 
  labs(y = "Cook's Distance") + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Smaller x-axis labels
    plot.caption = element_text(size = 8)                         # Reduce caption text size
  )

library(dplyr)


