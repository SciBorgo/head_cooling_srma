

# Combine summary from all models and export as word doc table
# Borg DN
# September 2024

# Packages
library(tidyverse)
library(gt)

# Load data
df <-
  rbind(read.csv('results/table_time_trial_perf.csv') %>% mutate(var = 'TT', effect_size = 'SMD'),
        read.csv('results/table_time_to_exhaustion.csv') %>% mutate(var = 'TTE', effect_size = 'SMD'),
        read.csv('results/table_heart_rate.csv') %>% mutate(var = 'HR', effect_size = 'MD'),
        read.csv('results/table_core_temp.csv') %>% mutate(var = 'Tcore', effect_size = 'MD'),
        read.csv('results/table_mean_skin_temp.csv') %>% mutate(var = 'Mean skin', effect_size = 'MD'),
        read.csv('results/table_skin_temp_at_site.csv') %>% mutate(var = 'Skin at site', effect_size = 'MD'),
        read.csv('results/table_rpe.csv') %>% mutate(var = 'RPE', effect_size = 'SMD'),
        read.csv('results/table_thermal_sensation.csv') %>% mutate(var = 'Sensation', effect_size = 'SMD'),
        read.csv('results/table_thermal_comfort.csv') %>% mutate(var = 'Comfort', effect_size = 'SMD')) %>%
  as.data.frame()


# Make table
df %>%
  relocate(var,
           effect_size) %>%
  mutate(across(c(effect,
                  se,
                  ci_lower,
                  ci_upper,
                  f_value_subgroup,
                  i_squared_between_studies,
                  i_squared_within_studies,
                  i_squared_total,
                  eggers_b0,
                  eggers_lower,
                  eggers_upper),
                ~ round(.x, 2)),
         across(c(tau_squared_between,
                  tau_squared_within,
                  p_value,
                  subgroup_p_value),
                ~ round(.x, 3))) %>%
  select(-se) %>%
  gt() %>%
  cols_label(effect_size = "Effect size",
             effect = 'Effect',
             ci_upper = "Upper",
             ci_lower = "Lower",
             ci_upper = "Upper",
             p_value = "P-value",
             tau_squared_within = "tau^2 within",
             tau_squared_between = "tau^2 between",
             i_squared_within_studies = "I^2 within (%)",
             i_squared_between_studies = "I^2 between (%)",
             i_squared_total = "I^2 total (%)",
             f_value_subgroup = "F-value",
             f_value_df1 = "df1",
             f_value_df2 = "df2",
             subgroup_p_value = "subgroup P-value",
             eggers_b0 = "Egger's b0",
             eggers_lower = "Egger's lower",
             eggers_upper = "Egger's upper") %>%
  cols_align(align = "left") -> gtab; gtab

gtsave(gtab, "results/summary_table.docx")



#### End





