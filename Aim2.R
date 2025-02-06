rstart_2 <- rstart |>
  mutate(
    pat1 = n_patients_served_18_40 + n_patients_served_41_65 + n_patients_served_66_89,
    pat2 = n_race_black + n_race_white + n_race_asian + n_race_hawaii + n_race_pi + n_race_aiai + n_race_unreported + n_race_other,
    pat3 = n_ethnicity_hispanic + n_ethnicity_non_hispanic + n_ethnicity_unreported,
    pat4 = n_public_insurance + n_private_insurance + n_uninsured,
    bmi_controlled = n_bmi - n_bmi_over30,
    bp_controlled = n_bp - n_bp_over140_90,
    rn = hours_budgeted_rn / n_total_visits_served_over18,
    ma = hours_budgeted_ma / n_total_visits_served_over18,
    lpn = hours_budgeted_lpn / n_total_visits_served_over18,
    other = hours_budgeted_other / n_total_visits_served_over18,
    rn2 = case_match(
      org,
      c("chn", "roc", "um") ~ as.numeric(hours_worked_rn) / n_total_visits_served_over18,
      "ws" ~ rn),
    ma2 = case_match(
      org,
      c("chn", "roc", "um") ~ as.numeric(hours_worked_ma) / n_total_visits_served_over18,
      "ws" ~ ma),
    lpn2 = case_match(
      org,
      c("chn", "roc", "um") ~ as.numeric(hours_worked_lpn) / n_total_visits_served_over18,
      "ws" ~ lpn),
    other2 = case_match(
      org,
      c("chn", "roc", "um") ~ as.numeric(hours_worked_other) / n_total_visits_served_over18,
      "ws" ~ other),
    n_a1c_over7 = n_a1c_7to79 + n_a1c_8to89 + n_a1c_over9
  )


## BMI

### Assessed

mod_bmi_assessed_simple <- glmer(
  cbind(n_bmi, n_total_visits_served_over18) ~ rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_bmi_assessed_simple)
confint(mod_bmi_assessed_simple, c("rn", "ma", "lpn", "other"))

mod_bmi_assessed_full <- glmer(
  cbind(n_bmi, n_total_visits_served_over18) ~ clinic_region_type + p_female + p_race_white + p_ethnicity_hispanic + p_patients_served_41_65 + p_patients_served_65plus + p_uninsured + p_public_insurance + rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_bmi_assessed_full)
confint(mod_bmi_assessed_full, c("rn", "ma", "lpn", "other"))


### Uncontrolled

mod_bmi_controlled_simple <- glmer(
  cbind(n_bmi_over30, n_bmi) ~ rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
  )

summary(mod_bmi_controlled_simple)
confint(mod_bmi_controlled_simple, c("rn", "ma", "lpn", "other"))


mod_bmi_controlled_full <- glmer(
  cbind(n_bmi_over30, n_bmi) ~ clinic_region_type + p_female + p_race_white + p_patients_served_41_65 + p_patients_served_65plus + p_ethnicity_hispanic + p_uninsured + p_public_insurance + rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_bmi_controlled_full)
confint(mod_bmi_controlled_full, c("rn", "ma", "lpn", "other"))


## BP

### Assessed

mod_bp_assessed_simple <- glmer(
  cbind(n_bp, n_total_visits_served_over18) ~ rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_bp_assessed_simple)
confint(mod_bp_assessed_simple, c("rn", "ma", "lpn", "other"))


mod_bp_assessed_full <- glmer(
  cbind(n_bp, n_total_visits_served_over18) ~ clinic_region_type + p_female + p_race_white + p_ethnicity_hispanic + p_patients_served_41_65 + p_patients_served_65plus + p_uninsured + p_public_insurance + rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_bp_assessed_full)
confint(mod_bp_assessed_full, c("rn", "ma", "lpn", "other"))


### Uncontrolled

mod_bp_controlled_simple <- glmer(
  cbind(n_bp_over140_90, n_bp) ~ rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_bp_controlled_simple)
confint(mod_bp_controlled_simple, c("rn", "ma", "lpn", "other"))

mod_bp_controlled_full <- glmer(
  cbind(n_bp_over140_90, n_bp) ~ clinic_region_type + p_female + p_race_white + p_patients_served_41_65 + p_patients_served_65plus + p_ethnicity_hispanic + p_uninsured + p_public_insurance + rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial(),
  control=glmerControl(optimizer="bobyqa")
)

summary(mod_bp_controlled_full)
confint(mod_bp_controlled_full, c("rn", "ma", "lpn", "other"))


## A1C

### Assessed

mod_a1c_assessed_simple <- glmer(
  cbind(n_a1c, n_total_visits_served_over18) ~ rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_a1c_assessed_simple)
confint(mod_a1c_assessed_simple, c("rn", "ma", "lpn", "other"))

mod_a1c_assessed_full <- glmer(
  cbind(n_a1c, n_total_visits_served_over18) ~ clinic_region_type + p_female + p_race_white + p_ethnicity_hispanic + p_patients_served_41_65 + p_patients_served_65plus + p_uninsured + p_public_insurance + rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_a1c_assessed_full)
confint(mod_a1c_assessed_full, c("rn", "ma", "lpn", "other"))

### Uncontrolled

mod_a1c_controlled_simple <- glmer(
  cbind(n_a1c_over7, n_a1c) ~ rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial()
)

summary(mod_a1c_controlled_simple)
confint(mod_a1c_controlled_simple, c("rn", "ma", "lpn", "other"))

mod_a1c_controlled_full <- glmer(
  cbind(n_a1c_over7, n_a1c) ~ clinic_region_type + p_female + p_race_white + p_patients_served_41_65 + p_patients_served_65plus + p_ethnicity_hispanic + p_uninsured + p_public_insurance + rn + ma + lpn + other + (1|clinic_name),
  data = rstart_2,
  family = binomial(),
  control=glmerControl(optimizer="bobyqa")
)

summary(mod_a1c_controlled_full)
confint(mod_a1c_controlled_full, c("rn", "ma", "lpn", "other"))

table(rstart$n_rn_hours_worked_cent)


rstart_3 <- rstart |>
  mutate(
    cent_tele = case_match(
      org,
      c("chn","um") ~ TRUE,
      c("roc", "ws") ~ FALSE),
    cent_gen = case_match(
      org,
      c("roc","um") ~ TRUE,
      c("chn", "ws") ~ FALSE),
    bmi_controlled = n_bmi - n_bmi_over30,
    bp_controlled = n_bp - n_bp_over140_90,
    rn = hours_budgeted_rn / n_total_visits_served_over18,
    rn2 = case_match(
      org,
      c("chn", "roc", "um") ~ as.numeric(hours_worked_rn) / n_total_visits_served_over18,
      "ws" ~ rn),
    rn_c = case_match(
      org,
      "chn" ~ 68.92 / n_total_visits_served_over18,
      "roc" ~ 75 / n_total_visits_served_over18,
      "um" ~ 32.16 / n_total_visits_served_over18,
      "ws" ~ 0 / n_total_visits_served_over18),
    rn_m = case_match(
      org,
      "chn" ~ 86.15 / n_total_visits_served_over18,
      "roc" ~ 5.8 / n_total_visits_served_over18,
      "um" ~ 168.42 / n_total_visits_served_over18,
      "ws" ~ 104.18 / n_total_visits_served_over18),
    rn_tot = rn + rn_c + rn_m,
    rn2_tot = rn2 + rn_c + rn_m,
    ma = hours_budgeted_ma / n_total_visits_served_over18,
    lpn = hours_budgeted_lpn / n_total_visits_served_over18,
    other = hours_budgeted_other / n_total_visits_served_over18,
    n_a1c_over7 = n_a1c_7to79 + n_a1c_8to89 + n_a1c_over9,
    p_bmi_controlled = bmi_controlled / n_bmi,
    p_bp_controlled = bp_controlled / n_bp,
    p_a1c_controlled = n_a1c_under7 / n_a1c
    )

rstart_3 |>
  group_by(org) |>
  summarize(
    bp = mean(p_bp_controlled),
    bmi = mean(p_bmi_controlled),
    a1c = mean(p_a1c_controlled, na.rm=T)
  )

test <- glmer(
  cbind(bmi_controlled, n_bmi) ~ clinic_region_type + p_female + p_race_white + p_patients_served_41_65 + p_patients_served_65plus + p_ethnicity_hispanic + p_uninsured + p_public_insurance + (1|clinic_name),
  data = rstart_3,
  family = binomial()
)

summary(test)


# Additional RN Hours Tests

### BMI

bmi_rn <- glmer(
  cbind(n_bmi_over30, n_bmi) ~ rn + rn_c + rn_m + (1|clinic_name),
  data = rstart_3,
  family = binomial()
)

summary(bmi_rn)
confint(bmi_rn, c("rn", "rn_c", "rn_m"))

bmi_rn_full <- glmer(
  cbind(n_bmi_over30, n_bmi) ~ clinic_region_type + p_female + p_race_white + p_patients_served_41_65 + p_patients_served_65plus + p_ethnicity_hispanic + p_uninsured + p_public_insurance + rn + rn_c + rn_m + (1|clinic_name),
  data = rstart_3,
  family = binomial()
)

summary(bmi_rn_full)
confint(bmi_rn_full, c("rn", "rn_c", "rn_m"))

### BP

bp_rn <- glmer(
  cbind(n_bp_over140_90, n_bp) ~ rn + rn_c + rn_m + (1|clinic_name),
  data = rstart_3,
  family = binomial()
)

summary(bp_rn)
confint(bp_rn, c("rn", "rn_c", "rn_m"))

bp_rn_full <- glmer(
  cbind(n_bp_over140_90, n_bp) ~ clinic_region_type + p_female + p_race_white + p_patients_served_41_65 + p_patients_served_65plus + p_ethnicity_hispanic + p_uninsured + p_public_insurance + rn + rn_c + rn_m + (1|clinic_name),
  data = rstart_3,
  family = binomial()
)

summary(bp_rn_full)
confint(bp_rn_full, c("rn", "rn_c", "rn_m"))

### A1C

a1c_rn <- glmer(
  cbind(n_a1c_over7, n_a1c) ~ rn + rn_c + rn_m + (1|clinic_name),
  data = rstart_3,
  family = binomial()
)

summary(a1c_rn)
confint(a1c_rn, c("rn", "rn_c", "rn_m"))

a1c_rn_full <- glmer(
  cbind(n_a1c_over7, n_a1c) ~ clinic_region_type + p_female + p_race_white + p_patients_served_41_65 + p_patients_served_65plus + p_ethnicity_hispanic + p_uninsured + p_public_insurance + rn + rn_c + rn_m + (1|clinic_name),
  data = rstart_3,
  family = binomial(),
  control=glmerControl(optimizer="bobyqa")
)

summary(a1c_rn_full)
confint(a1c_rn_full, c("rn", "rn_c", "rn_m"))
