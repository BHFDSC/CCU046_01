#
# NSTEMI: cardiac rehabilitation: Key models 
# - (complete cases on demographics, timing and co-morbidities only)
#
# K. Fleetwood
# 7 Feb 2025
#

#
# 1. Set-up -------------------------------------------------------------------
#

## required packages
library(tidyverse)
library(dplyr)
library(janitor)
library(knitr)
library(arsenal)
library(lubridate)
library(lme4)
library(lattice)

library(performance) # includes function for checking co-linearity

ccu046_folder <- "~/collab/CCU046"

ccu046_01_folder <- file.path(ccu046_folder, "CCU046_01")
outcome_folder <- file.path(ccu046_01_folder, "processed_data_nstemi_rehab")

cohort_folder <- file.path(ccu046_folder, "CCU046_02", "processed_data")

r_folder <- file.path(ccu046_folder, "r")

# Additional functions to help with summarising models
source(file.path(r_folder, "model_summary_functions.R"))  

#
# 2. Load data ----------------------------------------------------------------
#

cohort <- readRDS(file = file.path(cohort_folder, "01_cohort.rds"))

# Subset cohort
# Factor covariates for modelling
# Select complete cases
cohort <- 
  cohort %>% 
  filter(
    mi_type %in% "NSTEMI",
    discharge_destination %in% "Home",
    cardiac_rehab %in% c("Yes", "No", "Patient declined")
  ) %>%
  mutate(
    smi = 
      factor(
        smi, 
        levels = c("No SMI", "Schizophrenia", "Bipolar disorder", "Depression")
      ),
    sex = factor(sex, levels = c("Male", "Female")),
    imd_2019 = 
      factor(
        imd_2019, 
        levels = 
          c("10 (least deprived)", "1 (most deprived)", "2", "3", "4", "5", "6",
            "7", "8", "9")
      ),
    cardiac_rehab = factor(cardiac_rehab, levels = c("Yes", "No", "Patient declined")),
    outcome = cardiac_rehab %in% "Yes",
    cc = !is.na(imd_2019) & !is.na(ethnic_5) & !is.na(procode3)
  ) %>%
  filter(cc)

age_mi_mean <- mean(cohort$age_mi)
age_mi_sd <- sd(cohort$age_mi)

cohort <- 
  cohort %>%
  mutate(
    age_mi_scl     = (age_mi - age_mi_mean)/age_mi_sd
  )

# 4. Key models ---------------------------------------------------------------

# 4.1 Model 1 -----------------------------------------------------------------

# Mixed effects logistic regression model on complete cases
# Adjust for age and sex

mod_1_start <- Sys.time()
mod_1 <-
  glmer(
    outcome ~ smi + age_mi_scl + I(age_mi_scl^2) + sex  + (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa")
  )
mod_1_finish <- Sys.time()
mod_1_time <- mod_1_finish - mod_1_start

mod_1_out <- glmer_out(mod_1, prefix = "mod1_")

# 4.2 Model 2 -----------------------------------------------------------------
# Adjust for age, sex, IMD, ethnicity

mod_2_start <- Sys.time()
mod_2 <-
  glmer(
    outcome ~ smi + age_mi_scl + I(age_mi_scl^2) + sex + 
                            period_4 + mi_wd + mi_on  +
                            (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa")
  )
mod_2_finish <- Sys.time()
mod_2_time <- mod_2_finish - mod_2_start

mod_2_out <- glmer_out(mod_2, prefix = "mod2_")

# 4.3 Model 3 -----------------------------------------------------------------

mod_3_start <- Sys.time()
mod_3 <-
  glmer(
    outcome ~ smi + age_mi_scl + I(age_mi_scl^2) + sex + imd_2019 + ethnic_5 +
      period_4 + mi_wd + mi_on + (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
  )
mod_3_finish <- Sys.time()
mod_3_time <- mod_3_finish - mod_3_start

mod_3_out <- glmer_out(mod_3, prefix = "mod3_")

# 4.4 Model 4 -----------------------------------------------------------------

# Sociodemographics, timing and comorbidities
mod_4_start <- Sys.time()
mod_4 <-
  glmer(
    outcome ~ smi + age_mi_scl + I(age_mi_scl^2) + sex + imd_2019 + ethnic_5 +
      period_4 + mi_wd + mi_on + comorb_angina + comorb_mi +
      hx_pci +
      comorb_hf +  comorb_diab + 
      comorb_crf + comorb_cevd + (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
  )
mod_4_finish <- Sys.time()
mod_4_time <- mod_4_finish - mod_4_start

# Summarise results
mod_4_out <- glmer_out(mod_4, prefix = "mod4_")

# Repeat model 4 with binary SMI variable

# Create binary SMI variable
cohort <-
  cohort %>%
  mutate(
    smi_bin =
      case_when(
        smi %in% c("Schizophrenia", "Bipolar disorder", "Depression") ~ "Mental illness",
        smi %in% "No SMI" ~ "No mental illness"
      ),
    smi_bin = factor(smi_bin, levels = c("No mental illness", "Mental illness"))
  )

mod_4_bin_start <- Sys.time()
mod_4_bin <-
  glmer(
    outcome ~ smi_bin + age_mi_scl + I(age_mi_scl^2) + sex + imd_2019 + ethnic_5 +
      period_4 + mi_wd + mi_on + comorb_angina + comorb_mi +
      hx_pci +
      comorb_hf +  comorb_diab +
      comorb_crf + comorb_cevd + (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
  )
mod_4_bin_finish <- Sys.time()
mod_4_bin_time <- mod_4_bin_finish - mod_4_bin_start

# Summarise results
mod_4_bin_out <- glmer_out(mod_4_bin, prefix = "mod4_bin_")

# 4.5 Model 4 + interaction ---------------------------------------

# Binary SMI

mod_4_int_start <- Sys.time()
mod_4_int <-
  glmer(
    outcome ~ smi_bin + age_mi_scl + I(age_mi_scl^2) + sex + imd_2019 + ethnic_5 +
      period_4 + mi_wd + mi_on + comorb_angina + comorb_mi +
      hx_pci +
      comorb_hf +  comorb_diab +
      comorb_crf + comorb_cevd + smi_bin:period_4 + (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 40000))
  )
mod_4_int_finish <- Sys.time()
mod_4_int_time <- mod_4_int_finish - mod_4_int_start

mod_4_int_sum <- summary(mod_4_int)

# Summarise results
mod_4_int_out <- glmer_out(mod_4_int, prefix = "mod_4_int_")

# 5. Combine results ----------------------------------------------------------

mod_out <- 
  mod_4_out %>%
  left_join(mod_3_out) %>%
  left_join(mod_2_out) %>%
  left_join(mod_1_out) %>%
  select(
    covariate, 
    mod1_or_fmt, mod2_or_fmt, mod3_or_fmt, mod4_or_fmt,
    mod1_or, mod1_or_low, mod1_or_upp,
    mod2_or, mod2_or_low, mod2_or_upp,
    mod3_or, mod3_or_low, mod3_or_upp,
    mod4_or, mod4_or_low, mod4_or_upp
  )

anova_1to4 <- anova(mod_1, mod_2, mod_3, mod_4)
anova_1to4 <- as.data.frame(anova_1to4)

write.csv(
  mod_out, 
  file = file.path(outcome_folder, "14b_care_nstemi_rehab_mod1to4.csv")
)
write.csv(
  anova_1to4, 
  file.path(outcome_folder, "14b_care_nstemi_rehab_anova1to4.csv")
)

# Combine models 4 (binary) and 4 (interaction)

mod_out_int <- 
  mod_4_int_out %>%
  left_join(mod_4_bin_out) %>%
  select(
    covariate, 
    mod4_bin_or_fmt, mod_4_int_or_fmt,
    mod4_bin_or, mod4_bin_or_low, mod4_bin_or_upp,
    mod_4_int_or, mod_4_int_or_low, mod_4_int_or_upp,
  )

write.csv(
  mod_out_int, 
  file = file.path(outcome_folder, "14b_care_nstemi_rehab_mod_int.csv")
)

anova_int <- anova(mod_4_bin, mod_4_int)

write.csv(
  anova_int,
  file = file.path(outcome_folder, "14b_care_nstemi_rehab_anova_int.csv")
) 
