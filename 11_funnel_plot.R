
# Funnel plots
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

# Functions
load_data <- function(x, effect_type){
  d <- read_xlsx(paste0({{x}})) %>%
  clean_names() %>%
    mutate(subgroup = as.factor(subgroup),
           study_label = study,
           study_label = gsub(",", "", study_label))
  
  escalc(measure = paste0({{effect_type}}),
              m2i = con_me,
              sd2i = con_sd,
              n2i = con_n,
              m1i = int_me,
              sd1i = int_sd,
              n1i = int_n,
              data = d)
  }

meta_model <- function(data){rma.mv(yi = yi,
                     V = vi,
                     slab = study,
                     data = {{data}},
                     random = ~ 1 | study/es_id,
                     test = "t",
                     method = "REML",
                     dfs = "contain")
  }


# Data file names
data_tt <- "data/TT.xlsx"
data_tte <- "data/TTE.xlsx"
data_hr <- "data/HR.xlsx"
data_tcore <- "data/Tcore.xlsx"
data_tskin <- "data/T mean skin.xlsx"
data_skin_site <- "data/T site skin.xlsx"
data_rpe <- "data/RPE.xlsx"
data_sensation <- "data/sensation.xlsx"
data_comfort <- "data/comfort.xlsx"


# Make plot
png("figures/funnel_plots_panel.png", width = 10.5, height = 9, res = 600, units = "in")

# Set up a 3x3 panel for 9 plots
par(mfrow = c(3, 3))  # 3 rows and 3 columns

# Generate funnel plots
# TT
meta::funnel(x = meta_model(load_data(x = data_tt, effect_type = "SMD")),
             #xlim =
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
legend(x = 1, y = 0.01, 
       legend = c("p < .1", "p < .05", "p < .01"),
       fill = c("white", "gray55", "gray75"))
title(main = "a) Time trial performance", adj = 0, line = 1)

# TTE
meta::funnel(x = meta_model(load_data(x = data_tte, effect_type = "SMD")),
             #xlim =
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
# legend(x = 1.4, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("white", "gray55", "gray75"))
title(main = "b) Tests to exhaustion", adj = 0, line = 1)

# HR
meta::funnel(x = meta_model(load_data(x = data_hr, effect_type = "MD")),
             #xlim =
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
# legend(x = 20, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("white", "gray55", "gray75"))
title(main = "c) Heart rate", adj = 0, line = 1)

# Tcore
meta::funnel(x = meta_model(load_data(x = data_tcore, effect_type = "MD")),
             #xlim =
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
# legend(x = 0.35, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("white", "gray55", "gray75"))
title(main = "d) Core temperature", adj = 0, line = 1)

# Mean skin
meta::funnel(x = meta_model(load_data(x = data_tskin, effect_type = "MD")),
             #xlim =
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
# legend(x = 1.2, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("white", "gray55", "gray75"))
title(main = "e) Mean skin temperature", adj = 0, line = 1)

# Skin temperature at target site
meta::funnel(x = meta_model(load_data(x = data_skin_site, effect_type = "MD")),
             #xlim = c(-16,10),
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
# legend(x = -16, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("white", "gray55", "gray75"))
title(main = "f) Target site skin temperature", adj = 0, line = 1)

# RPE
meta::funnel(x = meta_model(load_data(x = data_rpe, effect_type = "SMD")),
             #xlim = c(-16,10),
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
# legend(x = 0.65, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("white", "gray55", "gray75"))
title(main = "g) RPE", adj = 0, line = 1)

# Sensation
meta::funnel(x = meta_model(load_data(x = data_sensation, effect_type = "SMD")),
             #xlim = c(-16,10),
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
# legend(x = 0.25, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("white", "gray55", "gray75"))
title(main = "h) Thermal sensation", adj = 0, line = 1)

# Comfort
meta::funnel(x = meta_model(load_data(x = data_comfort, effect_type = "SMD")),
             #xlim = c(-16,10),
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1)
# legend(x = 0.25, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("white", "gray55", "gray75"))
title(main = "i) Thermal comfort", adj = 0, line = 1)


dev.off()




# Investigate assymetry
# HR
meta::funnel(x = meta_model(load_data(x = data_hr, effect_type = "MD")),
             #xlim =
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1,
             label = T)
# Find studies
dsub <-
  load_data(data_hr, 'MD') %>%
  #filter(yi<0) %>%
  mutate(vi_z = sqrt(vi)) %>%
  filter(vi_z<6.18)
  
meta::funnel(x = meta_model(dsub),
             #xlim =
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1,
             label = T,
             cex = .5)

# Egger's test
dsub %>% 
  mutate(y = yi/sqrt(vi), x = 1/sqrt(vi)) %>% 
  lme4::lmer(y ~ x + (1|study), data = .) %>% 
  broom.mixed::tidy(conf.int = T) %>%
  clean_names()

# Skin temperature at target site
meta::funnel(x = meta_model(load_data(x = data_skin_site, effect_type = "MD")),
             #xlim = c(-16,10),
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1,
             label = T,
             cex = .5)

# RPE
meta::funnel(x = meta_model(load_data(x = data_rpe, effect_type = "SMD")),
             #xlim = c(-16,10),
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1,
             label = T,
             cex = .5)

# Remove Walters et al.
dsub <-
  load_data(data_rpe, 'SMD') %>%
  filter(study != 'Walters et al., 2017')

meta::funnel(x = meta_model(dsub),
             #xlim = c(-16,10),
             level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             legend = F,
             pch = 1,
             label = T,
             cex = .5)

# Egger's test
dsub %>% 
  mutate(y = yi/sqrt(vi), x = 1/sqrt(vi)) %>% 
  lme4::lmer(y ~ x + (1|study), data = .) %>% 
  broom.mixed::tidy(conf.int = T) %>%
  clean_names()


#### End


