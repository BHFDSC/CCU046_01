#
# NSTEMI: angiogram receipt: Key models 
#
# K. Fleetwood
# 8 Feb 2025
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
outcome_folder <- file.path(ccu046_01_folder, "processed_data_nstemi_angrec")

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
st_deviation_no <- 
  c("No acute changes", "Left bundle branch block", "T wave changes only",
    "Other acute abnormality")
cohort <- 
  cohort %>% 
  filter(
    mi_type %in% "NSTEMI",
    angiogram_eligibility %in% "Yes"
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
    st_deviation = 
      case_when(
        ecg %in% st_deviation_no  ~ "No",
        ecg %in% c("ST segment elevation", "ST segment depression") ~ "Yes",
        is.na(ecg) ~ as.character(NA)
      ),
    angiogram_receipt = factor(angiogram_receipt, levels = c("Yes", "No", "Patient refused")),
    outcome = angiogram_receipt %in% "Yes",
    cc = !is.na(imd_2019)   & !is.na(ethnic_5)       & !is.na(cardiogenic_shock) &
      !is.na(ecg)        & !is.na(cardiac_arrest) & 
      !is.na(creatinine) & !is.na(sbp)            & !is.na(heart_rate) &  
      !is.na(procode3)
  ) %>%
  filter(cc)

age_mi_mean <- mean(cohort$age_mi)
age_mi_sd <- sd(cohort$age_mi)

crt_log_mean <- mean(log(cohort$creatinine))
crt_log_sd <- sd(log(cohort$creatinine))

sbp_mean <- mean(cohort$sbp)
sbp_sd <- sd(cohort$sbp)

hr_mean <- mean(cohort$heart_rate)
hr_sd <- sd(cohort$heart_rate)

cohort <- 
  cohort %>%
  mutate(
    age_mi_scl     = (age_mi - age_mi_mean)/age_mi_sd,
    crt_log_scl = (log(creatinine) - crt_log_mean)/crt_log_sd,
    sbp_scl        = (sbp - sbp_mean)/sbp_sd,
    heart_rate_scl = (heart_rate - hr_mean)/hr_sd
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
mod_1_time

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

# 4.5 Model 5 -----------------------------------------------------------------

mod_5_start <- Sys.time()
mod_5 <-
  glmer(
    outcome ~ smi + age_mi_scl + I(age_mi_scl^2) + sex + imd_2019 + ethnic_5 +
      period_4 + mi_wd + mi_on + comorb_angina + comorb_mi +
      hx_pci +
      comorb_hf +  comorb_diab + 
      comorb_crf + comorb_cevd + 
      cardiac_arrest + covid +  st_deviation + cardiogenic_shock + 
      crt_log_scl + I(crt_log_scl^2) +
      sbp_scl + I(sbp_scl^2) +
      heart_rate_scl + I(heart_rate_scl^2) +
      (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 45000))
  )
mod_5_finish <- Sys.time()
mod_5_time <- mod_5_finish - mod_5_start

# Summarise results
mod_5_out <- glmer_out(mod_5, prefix = "mod5_")

# 4.6 Model 5 (binary SMI) ----------------------------------------------------

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

mod_5_bin_start <- Sys.time()
mod_5_bin <-
  glmer(
    outcome ~ smi_bin + age_mi_scl + I(age_mi_scl^2) + sex + imd_2019 + ethnic_5 +
      period_4 + mi_wd + mi_on + comorb_angina + comorb_mi +
      hx_pci +
      comorb_hf +  comorb_diab +
      comorb_crf + comorb_cevd + 
      cardiac_arrest + covid +  st_deviation + cardiogenic_shock + 
      crt_log_scl + I(crt_log_scl^2) +
      sbp_scl + I(sbp_scl^2) +
      heart_rate_scl + I(heart_rate_scl^2) +
      (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
  )
mod_5_bin_finish <- Sys.time()
mod_5_bin_time <- mod_5_bin_finish - mod_5_bin_start

mod_5_bin_out <- glmer_out(mod_5_bin, prefix = "mod5_bin_")

# 4.7 Model 5 (binary SMI) + interaction --------------------------------------

mod_5_int_start <- Sys.time()
mod_5_int <-
  glmer(
    outcome ~ smi_bin + age_mi_scl + I(age_mi_scl^2) + sex + imd_2019 + ethnic_5 +
      period_4 + mi_wd + mi_on + comorb_angina + comorb_mi +
      hx_pci +
      comorb_hf +  comorb_diab +
      comorb_crf + comorb_cevd + 
      cardiac_arrest + covid +  st_deviation + cardiogenic_shock + 
      crt_log_scl + I(crt_log_scl^2) +
      sbp_scl + I(sbp_scl^2) +
      heart_rate_scl + I(heart_rate_scl^2) +
      smi_bin:period_4 + (1|procode3),
    family = binomial(link = "logit"),
    data = cohort,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 60000))
  )
mod_5_int_finish <- Sys.time()
mod_5_int_time <- mod_5_int_finish - mod_5_int_start

# Summarise results
mod_5_int_out <- glmer_out(mod_5_int, prefix = "mod5_int_")

# 5. Combine results ----------------------------------------------------------

dim(mod_5_out)

mod_out <- 
  mod_5_out %>%
  left_join(mod_4_out) %>%
  left_join(mod_3_out) %>%
  left_join(mod_2_out) %>%
  left_join(mod_1_out) %>%
  select(
    covariate, 
    mod1_or_fmt, mod2_or_fmt, mod3_or_fmt, mod4_or_fmt, mod5_or_fmt, 
    mod1_or, mod1_or_low, mod1_or_upp,
    mod2_or, mod2_or_low, mod2_or_upp,
    mod3_or, mod3_or_low, mod3_or_upp,
    mod4_or, mod4_or_low, mod4_or_upp,
    mod5_or, mod5_or_low, mod5_or_upp
  )

dim(mod_out)

anova_1to5 <- anova(mod_1, mod_2, mod_3, mod_4, mod_5)
anova_1to5 <- as.data.frame(anova_1to5)

write.csv(
  mod_out, 
  file = file.path(outcome_folder, "11a_care_nstemi_angrec_mod1to5.csv")
)
write.csv(
  anova_1to5, 
  file.path(outcome_folder, "11a_care_nstemi_angrec_anova1to5.csv")
)

# Combine models 5 (binary) and 6

dim(mod_5_int_out)  

mod_out_int <- 
  mod_5_int_out %>%
  left_join(mod_5_bin_out) %>%
  select(
    covariate, 
    mod5_bin_or_fmt, mod5_int_or_fmt,
    mod5_bin_or, mod5_bin_or_low, mod5_bin_or_upp,
    mod5_int_or, mod5_int_or_low, mod5_int_or_upp
  )

dim(mod_out_int)

write.csv(
  mod_out_int, 
  file = file.path(outcome_folder, "11a_care_nstemi_angrec_mod_int.csv")
)

anova_int <- anova(mod_5_bin, mod_5_int)

write.csv(
  anova_int,
  file = file.path(outcome_folder, "11a_care_nstemi_angrec_anova_int.csv")
) 
