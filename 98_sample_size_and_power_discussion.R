

# Packages
library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(pwr)
library(ggplot2)

# Load data
d <- read_csv('data/data_study_sample_sizes.csv') %>%
  clean_names() %>%
  filter(crossover_study == 'TRUE')

# Summary
d_samp <-
  d %>%
  summarise(median_n = round(median(total_n),0),
            q5 = round(quantile(total_n, probs = 0.05),0),
            q95 = round(quantile(total_n, probs = 0.95),0))
d_samp

# Pull power
effect_sizes <- seq(0,1, by = 0.001)
sample_sizes <- c(d_samp$median_n[1],
                  d_samp$q5[1],
                  d_samp$q95[1])

results_df <- data.frame(
  effect_size = numeric(),
  n = numeric(),
  power = numeric(),
  sig_level = numeric()
)

for (effect_size in effect_sizes) {
  for (n in sample_sizes) {
    result <- pwr.t.test(d = effect_size,
                         n = n,
                         sig.level = 0.05,
                         type = "paired")
    results_df <- rbind(results_df, data.frame(
      effect_size = effect_size,
      n = n,
      power = result$power,
      sig_level = result$sig.level
    ))
  }}


# Plot
results_df %>%
  mutate(y = recode_factor(as.factor(n),
         '5' = 'Lower 90% limit',
         '9' = 'Median sample size',
         '17' = 'Upper 90% limit'),
         y = factor(y, levels = c('Upper 90% limit',
                                  'Median sample size',
                                  'Lower 90% limit'))) %>%
  ggplot(
    aes(x = effect_size,
        y = y,
        fill = power*100))+ 
  geom_tile()+
  scale_x_continuous(n.breaks = 9)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(colour = 'black',
                                  size = 0.3),
        axis.ticks.length = unit(.2, "cm"))+
  #scale_fill_viridis_c(option = 'B')+
  scale_fill_gradient(low = "white", high = "navy")+
  labs(fill = 'Power (%)',
       y = '',
       x = '\nStandardised mean difference')


# ggsave(filename = 'figures/power.png',
#        dpi = 600,
#        height = 4,
#        width = 7,
#        bg = 'white')


# Small effect of 0.25
results_df %>% filter(effect_size == 0.25)

# Large effect of 0.75
results_df %>% filter(effect_size == 0.75) # Not used in discussion paragraph


#### End


