
# DN Borg
# September 2024

# Packages
library(readxl)
library(dplyr)
library(janitor)
library(naniar)
library(meta)
library(metafor)
library(dmetar)
library(ggplot2)
library(esc)
library(metaviz)
library(gt)

# Set working directory
here::here()

# Load data
d <- read_xlsx("data/sensation.xlsx") %>%
  clean_names() %>%
  mutate(subgroup = as.factor(subgroup),
         study_label = study,
         study_label = gsub(",", "", study_label))

d

# Check missing data
vis_miss(d)

# Calculate mean difference
d <- escalc(measure = "SMD",
                       m2i = con_me,
                       sd2i = con_sd,
                       n2i = con_n,
                       m1i = int_me,
                       sd1i = int_sd,
                       n1i = int_n,
                       data = d) %>%
  arrange(yi)

# Fit meta-analysis model
fit <- rma.mv(yi = yi,
              V = vi,
              slab = study,
              data = d,
              random = ~ 1 | study/es_id,
              #mods = ~ strategy_group,
              test = "t",
              method = "REML",
              dfs = "contain")

summary(fit)

# Fit with moderator (subgroup)
fit_mods <- rma.mv(yi = yi,
              V = vi,
              slab = study,
              dat = d,
              random = ~ 1 | study/es_id,
              mods = ~ subgroup,
              test = "t",
              method = "REML",
              dfs = "contain")

# Plot
png(file = 'figures/thermal_sensation.png', width = 6.5, height = 5.5, res = 600, units = "in") 
viz_forest(study_table = data.frame('Study' = d$study_label,
                                    'Subgroup' = d$subgroup),
           x = fit_mods,
           study_labels = paste(d$study),
           group = d$subgroup,
           #x_limit = c(-1.65,2.15),
           x_breaks = seq(-6,6, by = 1),
           annotate_CI = T,
           summary_col = 'Greens',
           col = 'Greys',
           text_size = 2,
           variant = 'classic',
           xlab = 'SMD')
dev.off()



# Pull pooled data for summary
# I-squared
i2 <- var.comp(fit)
summary(i2)

# Egger's test
eggers <-
  d %>% 
  mutate(y = yi/sqrt(vi), x = 1/sqrt(vi)) %>% 
  lme4::lmer(y ~ x + (1|study), data = .) %>% 
  broom.mixed::tidy(conf.int = T) %>%
  clean_names()

overall_results <- predict(fit) %>%
  clean_names() %>%
  as.data.frame() %>%
  rename(effect = pred,
         ci_lower = ci_lb,
         ci_upper = ci_ub) %>%
  dplyr::select(effect,
                se,
                ci_lower,
                ci_upper) %>%
  mutate(p_value = fit$pval,
         tau_squared_within = fit$sigma2[2],
         tau_squared_between = fit$sigma2[1],
         i_squared_within_studies = i2$results$I2[2],
         i_squared_between_studies = i2$results$I2[3],
         i_squared_total = i2$totalI2,
         f_value_subgroup = fit_mods$QM,
         f_value_df1 = fit_mods$QMdf[1],
         f_value_df2 = fit_mods$QMdf[2],
         subgroup_p_value = fit_mods$QMp,
         eggers_b0 = eggers$estimate[1],
         eggers_lower = eggers$conf_low[1],
         eggers_upper = eggers$conf_high[1])

overall_results

overall_results %>%
  write.csv(file = 'results/table_thermal_sensation.csv',
            row.names = F)


# Sensitivity analysis
results_list <- list()
num_effects <- nrow(d)

# Keep all effect sizes in
fit <- rma.mv(yi = yi,
              V = vi,
              slab = study,
              data = d,
              random = ~ 1 | study/es_id,
              test = "t",
              method = "REML",
              dfs = "contain")

i2 <- var.comp(fit)

result <- predict(fit) %>%
  clean_names() %>%
  as.data.frame() %>%
  rename(effect = pred,
         ci_lower = ci_lb,
         ci_upper = ci_ub) %>%
  dplyr::select(effect, se, ci_lower, ci_upper) %>%
  mutate(p_value = fit$pval,
         tau_squared_within = fit$sigma2[2],
         tau_squared_between = fit$sigma2[1],
         i_squared_within_studies = i2$results$I2[2],
         i_squared_between_studies = i2$results$I2[3],
         left_out_study = 'None',        # No study left out
         left_out_es_id = NA)        # No effect size left out

results_list[[1]] <- result

# Loop for LOO analysis
for (i in 1:num_effects) {
  d_loo <- d[-i, ]
  
  fits <- rma.mv(yi = yi,
                 V = vi,
                 slab = study,
                 data = d_loo,
                 random = ~ 1 | study/es_id,
                 test = "t",
                 method = "REML",
                 dfs = "contain")
  
  i2 <- var.comp(fits)
  
  result <- predict(fits) %>%
    clean_names() %>%
    as.data.frame() %>%
    rename(effect = pred,
           ci_lower = ci_lb,
           ci_upper = ci_ub) %>%
    dplyr::select(effect, se, ci_lower, ci_upper) %>%
    mutate(p_value = fits$pval,
           tau_squared_within = fits$sigma2[2],
           tau_squared_between = fits$sigma2[1],
           i_squared_within_studies = i2$results$I2[2],
           i_squared_between_studies = i2$results$I2[3],
           left_out_study = d$study[i],      # Left out study
           left_out_es_id = d$es_id[i])      # Left out effect size
  
  results_list[[i + 1]] <- result 
}

# Combine
final_results <- bind_rows(results_list)

final_results %>%
  as_tibble() %>%
  arrange(left_out_es_id) %>%
  select(-se) %>%
  relocate(left_out_study,
           left_out_es_id,
           effect) %>%
  rename(left_out_study_label = left_out_study,
         smd = effect) %>%
  mutate(i_squared_within_studies = as.numeric(i_squared_within_studies),
         i_squared_between_studies = as.numeric(i_squared_between_studies),
         across(c(smd,
                  ci_lower,
                  ci_upper,
                  tau_squared_within,
                  tau_squared_between,
                  i_squared_within_studies,
                  i_squared_between_studies),
                ~ round(.x, 2)),
         across(c(tau_squared_within,
                  tau_squared_within,
                  p_value),
                ~ round(.x, 3))) %>%
  gt() %>%
  cols_label(smd = "SMD",
             ci_upper = "Upper",
             ci_lower = "Lower",
             ci_upper = "Upper",
             p_value = "P-value",
             tau_squared_within = "tau^2 within",
             tau_squared_between = "tau^2 between",
             i_squared_within_studies = "I^2 within (%)",
             i_squared_between_studies = "I^2 between (%)") %>%
  cols_align(align = "left") -> sens_table_sensation; sens_table_sensation

gtsave(sens_table_sensation, "sensitivity results/sensitivity_sensation.docx")

#### End

