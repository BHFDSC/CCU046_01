#
# Collate care results
#
# K.Fleetwood
# 8 Apr 2025
# 

#
# 1. Set-up -------------------------------------------------------------------
#

library(tidyverse)

smi_mi_folder <- "U:\\Datastore\\CMVM\\smgphs\\groups\\v1cjack1-CSO-SMI-MI"

res_folder <- file.path(smi_mi_folder, "Results", "Receipt of care")
nstemi_folder <- file.path(res_folder, "NSTEMI")
stemi_folder <- file.path(res_folder, "STEMI")

#
# 2. Load and wrangle data ----------------------------------------------------
#

wrangle_res <- function(df, outcome_lab, cohort_lab) {
  
  out <- 
    df %>%
    filter(
      substring(covariate, 1,3) %in% "smi"
    ) %>%
    mutate(
      smi = substring(covariate, 4, 100)
    ) %>%
    select(-X, -covariate, -ends_with("fmt")) %>%
    pivot_longer(
      !smi, 
      names_to = "model_est",
      values_to = "result"
    ) %>%
    mutate(
      model = substring(model_est, 1, 4),
      est = substring(model_est, 6, 100)
    ) %>%
    select(
      -model_est
    ) %>% 
    pivot_wider(
      names_from = est,
      values_from = result
    ) %>%
    mutate(
      or_fmt = 
        paste0(
          format(round(or, digits = 2), nsmall = 2), " (", 
          format(round(or_low, digits = 2), nsmall = 2), ", ",
          format(round(or_upp, digits = 2), nsmall = 2), ")"
        ),
      outcome = outcome_lab,
      cohort = cohort_lab
    ) %>%
    select(outcome, cohort, smi, model, or, or_low, or_upp, or_fmt)
  
  out
  
}

# 2.1 Load NSTEMI results -----------------------------------------------------

# Angiogram eligibility

nstemi_angel <- read.csv(file.path(nstemi_folder, "10a_care_nstemi_angel_mod1to5.csv"))
nstemi_angel <- wrangle_res(nstemi_angel, "Angiogram eligibility", "CC")

nstemi_angel_sa <- read.csv(file.path(nstemi_folder, "10b_care_nstemi_angel_mod1to4.csv"))
nstemi_angel_sa <- wrangle_res(nstemi_angel_sa, "Angiogram eligibility", "CC (SA)")

# Angiogram receipt

nstemi_angrec <- read.csv(file.path(nstemi_folder, "11a_care_nstemi_angrec_mod1to5.csv"))
nstemi_angrec <- wrangle_res(nstemi_angrec, "Angiogram receipt", "CC")

nstemi_angrec_sa <- read.csv(file.path(nstemi_folder, "11b_care_nstemi_angrec_mod1to4.csv"))
nstemi_angrec_sa <- wrangle_res(nstemi_angrec_sa, "Angiogram receipt", "CC (SA)")

# Angiogram within 72 hours

nstemi_ang72 <- read.csv(file.path(nstemi_folder, "12a_care_nstemi_ang72_mod1to5.csv"))
nstemi_ang72 <- wrangle_res(nstemi_ang72, "Angiogram within 72 hrs", "CC")

nstemi_ang72_sa <- read.csv(file.path(nstemi_folder, "12b_care_nstemi_ang72_mod1to4.csv"))
nstemi_ang72_sa <- wrangle_res(nstemi_ang72_sa, "Angiogram within 72 hrs", "CC (SA)")

# Admission to cardiac ward

nstemi_ward <- read.csv(file.path(nstemi_folder, "13a_care_nstemi_ward_mod1to5.csv"))
nstemi_ward <- wrangle_res(nstemi_ward, "Admission to cardiac ward", "CC")

nstemi_ward_sa <- read.csv(file.path(nstemi_folder, "13b_care_nstemi_ward_mod1to4.csv"))
nstemi_ward_sa <- wrangle_res(nstemi_ward_sa, "Admission to cardiac ward", "CC (SA)")

# Referral for cardiac rehabilitation

nstemi_rehab <- read.csv(file.path(nstemi_folder, "14a_care_nstemi_rehab_mod1to5.csv"))
nstemi_rehab <- wrangle_res(nstemi_rehab, "Referral for cardiac rehabilitation", "CC")

nstemi_rehab_sa <- read.csv(file.path(nstemi_folder, "14b_care_nstemi_rehab_mod1to4.csv"))
nstemi_rehab_sa <- wrangle_res(nstemi_rehab_sa, "Referral for cardiac rehabilitation", "CC (SA)")

# Receipt of all secondary prevention medication

nstemi_prev <- read.csv(file.path(nstemi_folder, "15a_care_nstemi_prev_mod1to5.csv"))
nstemi_prev <- wrangle_res(nstemi_prev, "Receipt of all secondary prevention medication", "CC")

nstemi_prev_sa <- read.csv(file.path(nstemi_folder, "15b_care_nstemi_prev_mod1to4.csv"))
nstemi_prev_sa <- wrangle_res(nstemi_prev_sa, "Receipt of all secondary prevention medication", "CC (SA)")

# 2.2 Load STEMI results ------------------------------------------------------

# Call to balloon within 120 minutes

stemi_ctb120 <- read.csv(file.path(stemi_folder, "20a_care_stemi_ctb120_mod1to5.csv"))
stemi_ctb120 <- wrangle_res(stemi_ctb120, "Call to balloon within 120 mins", "CC")

stemi_ctb120_sa <- read.csv(file.path(stemi_folder, "20b_care_stemi_ctb120_mod1to4.csv"))
stemi_ctb120_sa <- wrangle_res(stemi_ctb120_sa, "Call to balloon within 120 mins", "CC (SA)")

# Call to balloon within 150 minutes

stemi_ctb150 <- read.csv(file.path(stemi_folder, "21a_care_stemi_ctb150_mod1to5.csv"))
stemi_ctb150 <- wrangle_res(stemi_ctb150, "Call to balloon within 150 mins", "CC")

stemi_ctb150_sa <- read.csv(file.path(stemi_folder, "21b_care_stemi_ctb150_mod1to4.csv"))
stemi_ctb150_sa <- wrangle_res(stemi_ctb150_sa, "Call to balloon within 150 mins", "CC (SA)")

# Door to balloon within 60 minutes

stemi_dtb60 <- read.csv(file.path(stemi_folder, "22a_care_stemi_dtb60_mod1to5.csv"))
stemi_dtb60 <- wrangle_res(stemi_dtb60, "Door to balloon within 60 mins", "CC")

stemi_dtb60_sa <- read.csv(file.path(stemi_folder, "22b_care_stemi_dtb60_mod1to4.csv"))
stemi_dtb60_sa <- wrangle_res(stemi_dtb60_sa, "Door to balloon within 60 mins", "CC (SA)")

# Door to balloon within 90 minutes

stemi_dtb90 <- read.csv(file.path(stemi_folder, "23a_care_stemi_dtb90_mod1to5.csv"))
stemi_dtb90 <- wrangle_res(stemi_dtb90, "Door to balloon within 90 mins", "CC")

stemi_dtb90_sa <- read.csv(file.path(stemi_folder, "23b_care_stemi_dtb90_mod1to4.csv"))
stemi_dtb90_sa <- wrangle_res(stemi_dtb90_sa, "Door to balloon within 90 mins", "CC (SA)")

# Referral for cardiac rehabilitation

stemi_rehab <- read.csv(file.path(stemi_folder, "24a_care_stemi_rehab_mod1to5.csv"))
stemi_rehab <- wrangle_res(stemi_rehab, "Referral for cardiac rehabilitation", "CC")

stemi_rehab_sa <- read.csv(file.path(stemi_folder, "24b_care_stemi_rehab_mod1to4.csv"))
stemi_rehab_sa <- wrangle_res(stemi_rehab_sa, "Referral for cardiac rehabilitation", "CC (SA)")

# Receipt of all secondary prevention medication

stemi_prev <- read.csv(file.path(stemi_folder, "25a_care_stemi_prev_mod1to5.csv"))
stemi_prev <- wrangle_res(stemi_prev, "Receipt of all secondary prevention medication", "CC")

stemi_prev_sa <- read.csv(file.path(stemi_folder, "25b_care_stemi_prev_mod1to4.csv"))
stemi_prev_sa <- wrangle_res(stemi_prev_sa, "Receipt of all secondary prevention medication", "CC (SA)")

#
# 3. Create collated results data frame ---------------------------------------
#

# 3.1 NSTEMI ------------------------------------------------------------------

# Long version for plotting
nstemi_long <- 
  bind_rows(
    nstemi_angel,
    nstemi_angel_sa,
    nstemi_angrec, 
    nstemi_angrec_sa, 
    nstemi_ang72, 
    nstemi_ang72_sa, 
    nstemi_ward,
    nstemi_ward_sa,
    nstemi_rehab, 
    nstemi_rehab_sa,
    nstemi_prev, 
    nstemi_prev_sa
  )

write.csv(
  nstemi_long, 
  file.path(res_folder, "nstemi_collated_results_long.csv")
)


# 3.2 STEMI -------------------------------------------------------------------

stemi_long <- 
  bind_rows(
    stemi_ctb120,
    stemi_ctb120_sa,
    stemi_ctb150,
    stemi_ctb150_sa, 
    stemi_dtb60,
    stemi_dtb60_sa,
    stemi_dtb90,
    stemi_dtb90_sa,
    stemi_rehab,
    stemi_rehab_sa,
    stemi_prev,
    stemi_prev_sa
  )

write.csv(
  stemi_long, 
  file.path(res_folder, "stemi_collated_results_long.csv")
)

# 3.3 Combined ----------------------------------------------------------------

nstemi_wide <- 
  nstemi_long %>%
  select(-or, -or_low, -or_upp) %>%
  pivot_wider(
    names_from = smi,
    values_from = or_fmt
  ) %>%
  rename(
    nstemi_sch = Schizophrenia,
    nstemi_bd = `Bipolar disorder`,
    nstemi_dep = Depression
  )

stemi_wide <- 
  stemi_long %>%
  select(-or, -or_low, -or_upp) %>%
  pivot_wider(
    names_from = smi,
    values_from = or_fmt
  )  %>%
  rename(
    stemi_sch = Schizophrenia,
    stemi_bd = `Bipolar disorder`,
    stemi_dep = Depression
  ) 

res_wide <- 
  full_join(
    nstemi_wide,
    stemi_wide
  )

write.csv(
  res_wide, 
  file.path(res_folder, "collated_results_wide.csv")
)

