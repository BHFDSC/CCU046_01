#
# Mental illness, receipt of acute cardiac care following myocardial infarction
# and the impact of the COVID-19 pandemic: a cohort study
#
# Publication figures
# 
# K. Fleetwood
# 7 May 2025
# 

#
# 1. Set-up -------------------------------------------------------------------
#

library(tidyverse)
library(janitor)
library(ggh4x)

proj_folder <- "U:\\Datastore\\CMVM\\smgphs\\groups\\v1cjack1-CSO-SMI-MI\\"

res_folder <- file.path(proj_folder, "Results", "Receipt of care")
nstemi_folder <- file.path(res_folder, "NSTEMI")
stemi_folder <- file.path(res_folder, "STEMI")
  
pub_folder <- file.path(proj_folder, "Publications", "Care")

#
# 2. Outcomes over time -------------------------------------------------------
#

# 2.1 Load data ---------------------------------------------------------------

# NSTEMI: monthly -------------------------------------------------------------
nstemi_month <- 
  read.csv(file.path(nstemi_folder, "02_nstemi_month.csv"))

# Define outcome for combining with STEMI
nstemi_month <- 
  nstemi_month %>%
  mutate(
    out_comb = 
      case_when(
        outcome %in% "MI admissions (N)" ~ "NSTEMI admissions (N)",
        TRUE ~ paste("NSTEMI:", outcome)
      )
  )
nstemi_month %>% tabyl(out_comb)

# NSTEMI: 4-monthly periods by SMI --------------------------------------------

nstemi_per_smi <- 
  read.csv(file.path(nstemi_folder, "02_nstemi_period_smi_bin.csv"))

# Define outcome for combining with STEMI
nstemi_per_smi <- 
  nstemi_per_smi %>%
  mutate(
    out_comb = 
      case_when(
        outcome %in% "MI admissions (N)" ~ "NSTEMI admissions (N)",
        TRUE ~ paste("NSTEMI:", outcome)
      )
  )

# STEMI: monthly --------------------------------------------------------------
stemi_month <- 
  read.csv(file.path(stemi_folder, "03_stemi_month.csv"))

stemi_month <- 
  stemi_month %>%
  mutate(
    out_comb = 
      case_when(
        outcome %in% "MI admissions (N)" ~ "STEMI admissions (N)",
        TRUE ~ paste("STEMI:", outcome)
      )
  )
stemi_month %>% tabyl(out_comb)

# STEMI: 4-monthly periods by SMI ---------------------------------------------

stemi_per_smi <- 
  read.csv(
    file.path(stemi_folder, "03_stemi_period_bin_smi.csv")
  )

stemi_per_smi <- 
  stemi_per_smi %>%
  mutate(
    out_comb = 
      case_when(
        outcome %in% "MI admissions (N)" ~ "STEMI admissions (N)",
        TRUE ~ paste("STEMI:", outcome)
      )
  )

# 2.2 Combined monthly plot -----------------------------------------------------

out_comb_lvls <- 
  c("NSTEMI admissions (N)", "NSTEMI: Angiography eligibility (%)",
    "NSTEMI: Angiography receipt (%)", "NSTEMI: Angiography within 72 hrs (%)",
    "NSTEMI: Admission to cardiac ward (%)", 
    "NSTEMI: Referral for rehabilitation (%)", "NSTEMI: Secondary prevention (%)",
    "STEMI admissions (N)", "STEMI: Call-to-balloon within 120 minutes (%)", 
    "STEMI: Call-to-balloon within 150 minutes (%)", 
    "STEMI: Door-to-balloon within 60 minutes (%)", "STEMI: Door-to-balloon within 90 minutes (%)",
    "STEMI: Referral for rehabilitation (%)", "STEMI: Secondary prevention (%)")

mi_month <- bind_rows(nstemi_month, stemi_month)
mi_month <- 
  mi_month %>%
  mutate(
    mi_event_month = as.Date(mi_event_month),
    out_comb = factor(out_comb, levels = out_comb_lvls)
  )

month_plot <- 
  ggplot(mi_month, aes(mi_event_month, y = n_per)) + 
  geom_line() + 
  facet_wrap(
    vars(out_comb),
    ncol = 2,
    scales = "free_y",
    strip.position = "top",
    dir = "v"
  ) + 
  scale_x_date(
    date_labels = "%h %Y",
    expand = c(0,0)
  ) + 
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.spacing = unit(0.4, 'lines'),
    plot.margin = unit(c(1,1,1,1), "cm") # top, right, bottom, left
  ) +
  labs(
    x = "Month",
    y = ""
  )

# Save: A4 size
ggsave(
  month_plot,
  file = file.path(pub_folder, "outcomes_over_time.pdf"),
  width = 21, height = 29.7, units = "cm"
)


# 2.3 Combined 4-monthly by SMI plot -----------------------------------------------

mi_per_smi <- bind_rows(nstemi_per_smi, stemi_per_smi)
mi_per_smi <- 
  mi_per_smi %>%
  mutate(
    out_comb = factor(out_comb, levels = out_comb_lvls),
    period_4_date = as.Date(period_4_date)
  )

per_smi_plot <- 
  ggplot(mi_per_smi, aes(x = period_4_date, y = n_per, color = smi_bin)) + 
  geom_line() + 
  facet_wrap(
    vars(out_comb),
    ncol = 2,
    scales = "free_y",
    strip.position = "top",
    dir = "v"
  ) + 
  scale_x_date(
    date_labels = "%h %Y",
    expand = c(0,0)
  ) + 
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.spacing = unit(0.4, 'lines'),
    plot.margin = unit(c(1,1,1,1), "cm"), # top, right, bottom, left
    legend.position="bottom",
    legend.title=element_blank()
  ) +
  labs(
    x = "Month",
    y = ""
  )

# Save: A4 size
ggsave(
  per_smi_plot,
  file = file.path(pub_folder, "outcomes_over_time_by_smi.pdf"),
  width = 21, height = 29.7, units = "cm"
)

#
# 3. Forest plot of model estimates -------------------------------------------
#

# 3.1 Load data ---------------------------------------------------------------

nstemi_long <- 
  read.csv(file.path(res_folder, "nstemi_collated_results_long.csv"))

stemi_long <- 
  read.csv(file.path(res_folder, "stemi_collated_results_long.csv"))

jitter <- 0.1

# 3.2 Forest plot for NSTEMI --------------------------------------------------

nstemi_long <- 
  nstemi_long %>%
  mutate(
    pos_out = 
      case_when(
        outcome %in% "Angiogram eligibility"                          ~ 43,
        outcome %in% "Angiogram receipt"                              ~ 35,
        outcome %in% "Angiogram within 72 hrs"                        ~ 27,
        outcome %in% "Admission to cardiac ward"                      ~ 19,
        outcome %in% "Referral for cardiac rehabilitation"            ~ 11, 
        outcome %in% "Receipt of all secondary prevention medication" ~  3
      ),
    pos_model =
      case_when(
        model %in% "mod1" ~ 2,
        model %in% "mod2" ~ 1,
        model %in% "mod3" ~  0,
        model %in% "mod4" ~ -1,
        model %in% "mod5" ~ -2
      ),
    pos = pos_out + pos_model,
    smi = factor(smi, levels = c("Schizophrenia", "Bipolar disorder", "Depression")),
    model = substring(model, 4, 4)
  )


nstemi_forest <- 
  ggplot(
    filter(nstemi_long, cohort == "CC"), 
    aes(or, pos, col = model, label = or_fmt)
  ) +
  facet_wrap(vars(smi), ncol = 3) +
  geom_vline(xintercept = 1, col="lightgrey") +
  geom_point() +
  geom_text(vjust=-0.5, show.legend = FALSE, size = 3) +
  geom_errorbarh(aes(xmin = or_low, xmax = or_upp, height = 0)) +
  scale_x_log10(
    name = "Odds ratio (95% CI)",
    limits = c(0.11, 1.8)
  ) +
  scale_y_continuous(
    minor_breaks = c(7, 15, 23, 31, 39),
    breaks = c(43, 35, 27, 19, 11, 3),
    labels = c("Angiography\neligibility", "Angiography\nreceipt", "Angiography\nwithin\n72 hours", 
               "Cardiac ward\nadmission", "Cardiac\nrehabilitation\nreferral", "Receipt of\nindicated\nsecondary\nprevention\nmedication")
  ) +
  theme_bw() +
  theme(
    #axis.line.x.bottom = element_line(size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_line(colour = "grey92"),
    #axis.text.y = element_blank(),
    axis.title.y = element_blank(),#,
    plot.background = element_rect(fill = "white"),
    strip.background =element_rect(fill="white")
    #panel.grid = element_blank(),
    #panel.grid.major.y = 
    #plot.margin = unit(c(0.25, 0.25, 0.25, 2), "cm")
  )
nstemi_forest

ggsave(
  file.path(pub_folder, "nstemi_forest.pdf"), 
  width = 21, height = 29.7, units = "cm"
)

# 3.3 Forest plot for STEMI ---------------------------------------------------

stemi_long <- 
  stemi_long %>%
  mutate(
    pos_out = 
      case_when(
        outcome %in% "Call to balloon within 120 mins"                ~ 43,
        outcome %in% "Call to balloon within 150 mins"                ~ 35,
        outcome %in% "Door to balloon within 60 mins"                 ~ 27,
        outcome %in% "Door to balloon within 90 mins"                 ~ 19,
        outcome %in% "Referral for cardiac rehabilitation"            ~ 11, 
        outcome %in% "Receipt of all secondary prevention medication" ~  3
      ),
    pos_model =
      case_when(
        model %in% "mod1" ~ 2,
        model %in% "mod2" ~ 1,
        model %in% "mod3" ~  0,
        model %in% "mod4" ~ -1,
        model %in% "mod5" ~ -2
      ),
    pos = pos_out + pos_model,
    smi = factor(smi, levels = c("Schizophrenia", "Bipolar disorder", "Depression")),
    model = substring(model, 4, 4)
  )


stemi_forest <- 
  ggplot(
    filter(stemi_long, cohort == "CC"), 
    aes(or, pos, col = model, label = or_fmt)
  ) +
  facet_wrap(vars(smi), ncol = 3) +
  geom_vline(xintercept = 1, col="lightgrey") +
  geom_point() +
  geom_text(vjust=-0.5, show.legend = FALSE, size = 3) +
  geom_errorbarh(aes(xmin = or_low, xmax = or_upp, height = 0)) +
  scale_x_log10(
    name = "Odds ratio (95% CI)",
    limits = c(NA, 2.1)
  ) +
  scale_y_continuous(
    minor_breaks = c(7, 15, 23, 31, 39),
    breaks = c(43, 35, 27, 19, 11, 3),
    labels = c("Call-to-\nballoon\nwithin\n120 mins", "Call-to-\nballoon\nwithin\n150 mins", "Door-to-\nballoon\nwithin\n60 mins", 
               "Door-to-\nballoon\nwithin\n90 mins", "Cardiac\nrehabilitation\nreferral", "Receipt of\nindicated\nsecondary\nprevention\nmedication")
  ) +
  theme_bw() +
  theme(
    #axis.line.x.bottom = element_line(size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_line(colour = "grey92"),
    #axis.text.y = element_blank(),
    axis.title.y = element_blank(),#,
    plot.background = element_rect(fill = "white"),
    strip.background =element_rect(fill="white")
    #panel.grid = element_blank(),
    #panel.grid.major.y = 
    #plot.margin = unit(c(0.25, 0.25, 0.25, 2), "cm")
  )
stemi_forest

ggsave(
  file.path(pub_folder, "stemi_forest.pdf"), 
  width = 21, height = 29.7, units = "cm"
)

#
# 4. Simplified plot of model estimates ---------------------------------------
#

# 4.1 Load data ---------------------------------------------------------------

nstemi_long <- 
  read.csv(file.path(res_folder, "nstemi_collated_results_long.csv"))

stemi_long <- 
  read.csv(file.path(res_folder, "stemi_collated_results_long.csv"))

jitter <- 0.1

# 4.2 Forest plot for NSTEMI --------------------------------------------------

nstemi_long <- 
  nstemi_long %>%
  filter(model %in% "mod5") %>%
  mutate(
    pos = 
      case_when(
        outcome %in% "Angiogram eligibility"                          ~ 43,
        outcome %in% "Angiogram receipt"                              ~ 35,
        outcome %in% "Angiogram within 72 hrs"                        ~ 27,
        outcome %in% "Admission to cardiac ward"                      ~ 19,
        outcome %in% "Referral for cardiac rehabilitation"            ~ 11, 
        outcome %in% "Receipt of all secondary prevention medication" ~  3
      ),
    smi = factor(smi, levels = c("Schizophrenia", "Bipolar disorder", "Depression")),
    model = substring(model, 4, 4)
  )


nstemi_forest <- 
  ggplot(
    filter(nstemi_long, cohort == "CC"), 
    aes(or, pos, label = or_fmt)
  ) +
  facet_wrap(vars(smi), ncol = 3) +
  geom_vline(xintercept = 1, col="lightgrey") +
  geom_point() +
  geom_text(vjust=-0.5, show.legend = FALSE, size = 3) +
  geom_errorbarh(aes(xmin = or_low, xmax = or_upp, height = 0)) +
  scale_x_log10(
    name = "Odds ratio (95% CI)",
    # limits = c(0.05, 4.7) # optimized for portrait
    limits = c(0.05, 3.5) # optimized for landscape
  ) +
  scale_y_continuous(
    minor_breaks = c(7, 15, 23, 31, 39),
    breaks = c(43, 35, 27, 19, 11, 3),
    labels = c("Angiography\neligibility", "Angiography\nreceipt", "Angiography\nwithin\n72 hours", 
               "Cardiac ward\nadmission", "Cardiac\nrehabilitation\nreferral", "Receipt of\nindicated\nsecondary\nprevention\nmedication")
  ) +
  theme_bw() +
  theme(
    #axis.line.x.bottom = element_line(size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_line(colour = "grey92"),
    #axis.text.y = element_blank(),
    axis.title.y = element_blank(),#,
    plot.background = element_rect(fill = "white"),
    strip.background =element_rect(fill="white")
    #panel.grid = element_blank(),
    #panel.grid.major.y = 
    #plot.margin = unit(c(0.25, 0.25, 0.25, 2), "cm")
  )
nstemi_forest

# ggsave(
#   file.path(pub_folder, "nstemi_forest_simple.pdf"), 
#   width = 2.5*5, height = 2.5*6.75, units = "cm"
# )

ggsave(
  file.path(pub_folder, "nstemi_forest_simple_wide.pdf"), 
  width = 1.75*8.5, height = 1.75*7, units = "cm"
)

# Repeat without labels

nstemi_forest <- 
  ggplot(
    filter(nstemi_long, cohort == "CC"), 
    aes(or, pos)
  ) +
  facet_wrap(vars(smi), ncol = 3) +
  geom_vline(xintercept = 1, col="lightgrey") +
  geom_point() +
  geom_errorbarh(aes(xmin = or_low, xmax = or_upp, height = 0)) +
  scale_x_log10(
    name = "Odds ratio (95% CI)"#,
    #limits = c(0.1, 2.5) # optimized for landscape
  ) +
  scale_y_continuous(
    minor_breaks = c(7, 15, 23, 31, 39),
    breaks = c(43, 35, 27, 19, 11, 3),
    labels = c("Angiogram\neligibility", "Angiogram\nreceipt", "Angiogram\nwithin\n72 hours", 
               "Admission\nto cardiac\nward", "Referral\nfor cardiac\nrehab", "Receipt of all\nsecondary\nprevention\nmedication")
  ) +
  theme_bw() +
  theme(
    #axis.line.x.bottom = element_line(size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_line(colour = "grey92"),
    #axis.text.y = element_blank(),
    axis.title.y = element_blank(),#,
    plot.background = element_rect(fill = "white"),
    strip.background =element_rect(fill="white")
    #panel.grid = element_blank(),
    #panel.grid.major.y = 
    #plot.margin = unit(c(0.25, 0.25, 0.25, 2), "cm")
  )
nstemi_forest

# ggsave(
#   file.path(pub_folder, "nstemi_forest_simple.pdf"), 
#   width = 2.5*5, height = 2.5*6.75, units = "cm"
# )

ggsave(
  file.path(pub_folder, "nstemi_forest_simple_wide_nolabs.pdf"), 
  width = 2*6.75, height = 2*5, units = "cm"
)


# 4.3 Forest plot for STEMI ---------------------------------------------------

stemi_long <- 
  stemi_long %>%
  filter(model %in% "mod5") %>%
  mutate(
    pos = 
      case_when(
        outcome %in% "Call to balloon within 120 mins"                ~ 43,
        outcome %in% "Call to balloon within 150 mins"                ~ 35,
        outcome %in% "Door to balloon within 60 mins"                 ~ 27,
        outcome %in% "Door to balloon within 90 mins"                 ~ 19,
        outcome %in% "Referral for cardiac rehabilitation"            ~ 11, 
        outcome %in% "Receipt of all secondary prevention medication" ~  3
      ),
    smi = factor(smi, levels = c("Schizophrenia", "Bipolar disorder", "Depression")),
  )


stemi_forest <- 
  ggplot(
    filter(stemi_long, cohort == "CC"), 
    aes(or, pos, label = or_fmt)
  ) +
  facet_wrap(vars(smi), ncol = 3) +
  geom_vline(xintercept = 1, col="lightgrey") +
  geom_point() +
  geom_text(vjust=-0.5, show.legend = FALSE, size = 3) +
  geom_errorbarh(aes(xmin = or_low, xmax = or_upp, height = 0)) +
  scale_x_log10(
    name = "Odds ratio (95% CI)",
    limits = c(0.17, 2.9)
  ) +
  scale_y_continuous(
    minor_breaks = c(7, 15, 23, 31, 39),
    breaks = c(43, 35, 27, 19, 11, 3),
    labels = c("Call-to-\nballoon\nwithin\n120 mins", "Call-to-\nballoon\nwithin\n150 mins", "Door-to-\nballoon\nwithin\n60 mins", 
               "Door-to-\nballoon\nwithin\n90 mins", "Cardiac\nrehabilitation\nreferral", "Receipt of\nindicated\nsecondary\nprevention\nmedication")
  ) +
  theme_bw() +
  theme(
    #axis.line.x.bottom = element_line(size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_line(colour = "grey92"),
    #axis.text.y = element_blank(),
    axis.title.y = element_blank(),#,
    plot.background = element_rect(fill = "white"),
    strip.background =element_rect(fill="white")
    #panel.grid = element_blank(),
    #panel.grid.major.y = 
    #plot.margin = unit(c(0.25, 0.25, 0.25, 2), "cm")
  )
stemi_forest

ggsave(
  file.path(pub_folder, "stemi_forest_simple_wide.pdf"), 
  width = 1.75*8.5, height = 1.75*7, units = "cm"
)

#
# 5. Forest plot of model estimates (sensitivity analysis)---------------------
#

# 3.1 Load data ---------------------------------------------------------------

nstemi_long <- 
  read.csv(file.path(res_folder, "nstemi_collated_results_long.csv"))

stemi_long <- 
  read.csv(file.path(res_folder, "stemi_collated_results_long.csv"))

jitter <- 0.1

# 3.2 Forest plot for NSTEMI --------------------------------------------------

nstemi_long %>% tabyl(cohort)
nstemi_long %>% filter(cohort %in% "CC (SA)") %>% tabyl(model)

nstemi_long <- 
  nstemi_long %>%
  filter(cohort %in% "CC (SA)") %>%
  mutate(
    pos_out = 
      case_when(
        outcome %in% "Angiogram eligibility"                          ~ 38,
        outcome %in% "Angiogram receipt"                              ~ 31,
        outcome %in% "Angiogram within 72 hrs"                        ~ 24,
        outcome %in% "Admission to cardiac ward"                      ~ 17,
        outcome %in% "Referral for cardiac rehabilitation"            ~ 10, 
        outcome %in% "Receipt of all secondary prevention medication" ~  3
      ),
    pos_model =
      case_when(
        model %in% "mod1" ~ 1.5,
        model %in% "mod2" ~ 0.5,
        model %in% "mod3" ~  -0.5,
        model %in% "mod4" ~ -1.5
      ),
    pos = pos_out + pos_model,
    smi = factor(smi, levels = c("Schizophrenia", "Bipolar disorder", "Depression")),
    model = substring(model, 4, 4)
  )


nstemi_forest_sa <- 
  ggplot(
    filter(nstemi_long), 
    aes(or, pos, col = model, label = or_fmt)
  ) +
  facet_wrap(vars(smi), ncol = 3) +
  geom_vline(xintercept = 1, col="lightgrey") +
  geom_point() +
  geom_text(vjust=-0.5, show.legend = FALSE, size = 3) +
  geom_errorbarh(aes(xmin = or_low, xmax = or_upp, height = 0)) +
  scale_x_log10(
    name = "Odds ratio (95% CI)",
    limits = c(0.10, 1.7)
  ) +
  scale_y_continuous(
    minor_breaks = c(6.5, 13.5, 20.5, 27.5, 34.5),
    breaks = c(38, 31, 24, 17, 10, 3),
    labels = c("Angiography\neligibility", "Angiography\nreceipt", "Angiography\nwithin\n72 hours", 
               "Cardiac ward\nadmission", "Cardiac\nrehabilitation\nreferral", "Receipt of\nindicated\nsecondary\nprevention\nmedication")
  ) +
  theme_bw() +
  theme(
    #axis.line.x.bottom = element_line(size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_line(colour = "grey92"),
    #axis.text.y = element_blank(),
    axis.title.y = element_blank(),#,
    plot.background = element_rect(fill = "white"),
    strip.background =element_rect(fill="white")
    #panel.grid = element_blank(),
    #panel.grid.major.y = 
    #plot.margin = unit(c(0.25, 0.25, 0.25, 2), "cm")
  )
nstemi_forest_sa

ggsave(
  file.path(pub_folder, "nstemi_forest_sa.pdf"), 
  width = 21, height = 29.7, units = "cm"
)

# 3.3 Forest plot for STEMI ---------------------------------------------------

stemi_long <- 
  stemi_long %>%
  filter(cohort %in% "CC (SA)") %>%
  mutate(
    pos_out = 
      case_when(
        outcome %in% "Call to balloon within 120 mins"                ~ 38,
        outcome %in% "Call to balloon within 150 mins"                ~ 31,
        outcome %in% "Door to balloon within 60 mins"                 ~ 24,
        outcome %in% "Door to balloon within 90 mins"                 ~ 17,
        outcome %in% "Referral for cardiac rehabilitation"            ~ 10, 
        outcome %in% "Receipt of all secondary prevention medication" ~  3
      ),
    pos_model =
      case_when(
        model %in% "mod1" ~ 1.5,
        model %in% "mod2" ~ 0.5,
        model %in% "mod3" ~  -0.5,
        model %in% "mod4" ~ -1.5
      ),
    pos = pos_out + pos_model,
    smi = factor(smi, levels = c("Schizophrenia", "Bipolar disorder", "Depression")),
    model = substring(model, 4, 4)
  )


stemi_forest_sa <- 
  ggplot(
    filter(stemi_long), 
    aes(or, pos, col = model, label = or_fmt)
  ) +
  facet_wrap(vars(smi), ncol = 3) +
  geom_vline(xintercept = 1, col="lightgrey") +
  geom_point() +
  geom_text(vjust=-0.5, show.legend = FALSE, size = 3) +
  geom_errorbarh(aes(xmin = or_low, xmax = or_upp, height = 0)) +
  scale_x_log10(
    name = "Odds ratio (95% CI)",
    limits = c(0.18, 2)
  ) +
  scale_y_continuous(
    minor_breaks = c(6.5, 13.5, 20.5, 27.5, 34.5),
    breaks = c(38, 31, 24, 17, 10, 3),
    labels = c("Call-to-\nballoon\nwithin\n120 mins", "Call-to-\nballoon\nwithin\n150 mins", "Door-to-\nballoon\nwithin\n60 mins", 
               "Door-to-\nballoon\nwithin\n90 mins", "Cardiac\nrehabilitation\nreferral", "Receipt of\nindicated\nsecondary\nprevention\nmedication")
  ) +
  theme_bw() +
  theme(
    #axis.line.x.bottom = element_line(size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_line(colour = "grey92"),
    #axis.text.y = element_blank(),
    axis.title.y = element_blank(),#,
    plot.background = element_rect(fill = "white"),
    strip.background =element_rect(fill="white")
    #panel.grid = element_blank(),
    #panel.grid.major.y = 
    #plot.margin = unit(c(0.25, 0.25, 0.25, 2), "cm")
  )
stemi_forest_sa

ggsave(
  file.path(pub_folder, "stemi_forest_sa.pdf"), 
  width = 21, height = 29.7, units = "cm"
)

