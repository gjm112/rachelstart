names(rstart)

rstart |>
  filter(is.na(hours_worked_rn)) |> view()


rstart <- rstart |>
  filter(!(org_name == "3019083950" & month == "2022-09-01"),
         !(org_name == "3019091900" & month == "2022-07-01"),
         !(org_name == "3049025340"),
         !(org_name == "3049000061"),
         !(org_name == "UMMC FM @ ATTICA"),
         !(org_name == "Primary Clinic #18"))



## First Page of Tables

table(rstart$org_region_type, rstart$clinic)

table(rstart$org_type, rstart$clinic)

rstart |>
  group_by(clinic) |>
  summarize(
    clinics = n_distinct(org_name)
  )

table(rstart$n_clinics_total, rstart$clinic)

table(rstart$n_clinics_ped_adult, rstart$clinic)

table(rstart$n_clinics_adult_only, rstart$clinic)

sum(rstart$n_patients_served_over18, na.rm=T)

rstart |>
  group_by(clinic) |>
  summarize(
    visits = sum(n_patients_served_over18, na.rm=T),
    months = n_distinct(org_name, month),
    v_per_m = round(visits / months)
  )

768499 / (78 + 330 + 114 + 262)


table(rstart$I_magnet, rstart$clinic)
table(rstart$I_magnet_w_amb_struture, rstart$clinic)
table(rstart$I_stand_alone, rstart$clinic)
table(rstart$I_chief_nursing_officer_present, rstart$clinic)



table(rstart$I_coll_barg, rstart$clinic)

table(rstart$I_cent_rn_staffinng, rstart$clinic)
table(rstart$I_tele_rn_staffing, rstart$clinic)

table(rstart$n_rn_hours_worked_cent)

rstart |>
  filter(clinic %in% c("um")) |>
  mutate(cent = as.numeric(n_rn_hours_worked_cent),
         tele = as.numeric(n_rn_hours_worked_tele)) |>
  summarise(
    months = n_distinct(org_name, month),
    cent = sum(cent) / months,
    tele = sum(tele) / months
  )



## Third Page

rstart |>
  mutate(
    n1 = n_patients_served_18_40 + n_patients_served_41_65 + n_patients_served_66_89 + n_patients_served_over89,
    p_1840 = n_patients_served_18_40 / n1,
    p_4165 = n_patients_served_41_65 / n1,
    p_o65 = (n_patients_served_66_89 + n_patients_served_over89) / n1
  ) |>
  summarize(
    mean1840 = mean(n_patients_served_18_40),
    mean4164 = mean(n_patients_served_41_65),
    mean65 = mean(n_patients_served_66_89 + n_patients_served_over89),
    mp1 = mean(p_1840),
    mp2 = mean(p_4165),
    mp3 = mean(p_o65)
  )

rstart |>
  mutate(
    n1 = n_patients_served_18_40 + n_patients_served_41_65 + n_patients_served_66_89 + n_patients_served_over89,
    p_1840 = n_patients_served_18_40 / n1,
    p_4165 = n_patients_served_41_65 / n1,
    p_o65 = (n_patients_served_66_89 + n_patients_served_over89) / n1
  ) |>
  group_by(clinic) |>
  summarize(
    mean1840 = mean(n_patients_served_18_40),
    mean4164 = mean(n_patients_served_41_65),
    mean65 = mean(n_patients_served_66_89 + n_patients_served_over89),
    mp1 = mean(p_1840),
    mp2 = mean(p_4165),
    mp3 = mean(p_o65)
  )
  
rstart |>
  mutate(
    n_race_pi = ifelse(is.na(as.numeric(n_race_pi)), 0, as.numeric(n_race_pi)),
    other = n_race_black + n_race_asian + n_race_hawaii + n_race_pi + n_race_aiai,
    n2 = n_race_white + other,
    pw = n_race_white / n2,
    pnw = other / n2
  ) |>
  summarize(
    mean_w = mean(n_race_white),
    mean_pw = mean(pw, na.rm=T),
    mean_o = mean(other, na.rm=T),
    mean_pnw = mean(pnw, na.rm=T)
  )

rstart |>
  mutate(
    n_race_pi = ifelse(is.na(as.numeric(n_race_pi)), 0, as.numeric(n_race_pi)),
    other = n_race_black + n_race_asian + n_race_hawaii + n_race_pi + n_race_aiai,
    n2 = n_race_white + other,
    pw = n_race_white / n2,
    pnw = other / n2
  ) |>
  group_by(clinic) |>
  summarize(
    mean_w = mean(n_race_white),
    mean_pw = mean(pw, na.rm=T),
    mean_o = mean(other, na.rm=T),
    mean_pnw = mean(pnw, na.rm=T)
  )

rstart |>
  mutate(
    n3 = n_ethnicity_hispanic + n_ethnicity_non_hispanic,
    p_hs = n_ethnicity_hispanic / n3,
    p_nhs = n_ethnicity_non_hispanic / n3
  ) |>
  summarize(
    mean_hs = mean(n_ethnicity_hispanic),
    mean_phs = mean(p_hs),
    mean_nhs = mean(n_ethnicity_non_hispanic),
    mean_pnhs = mean(p_nhs)
  )

rstart |>
  mutate(
    n3 = n_ethnicity_hispanic + n_ethnicity_non_hispanic,
    p_hs = n_ethnicity_hispanic / n3,
    p_nhs = n_ethnicity_non_hispanic / n3
  ) |>
  group_by(clinic) |>
  summarize(
    mean_hs = mean(n_ethnicity_hispanic),
    mean_phs = mean(p_hs),
    mean_nhs = mean(n_ethnicity_non_hispanic),
    mean_pnhs = mean(p_nhs)
  )

rstart |>
  mutate(
    n_gender_x = ifelse(clinic == "um", 0, as.numeric(n_gender_x)),
    n_gender_x = ifelse(is.na(as.numeric(n_gender_x)), 0, n_gender_x),
    n_gender_unk = ifelse(is.na(as.numeric(n_gender_unk)), 0, n_gender_unk),
    n4 = n_gender_male + n_gender_female + n_gender_unk + n_gender_x,
    p_m = n_gender_male / n4,
    p_f = n_gender_female / n4,
    p_u = n_gender_unk / n4,
    p_x = n_gender_x / n4
  ) |>
  summarize(
    mean_m = mean(n_gender_male),
    mean_pm = mean(p_m),
    mean_f = mean(n_gender_female),
    mean_pf = mean(p_f),
    mean_u = mean(n_gender_unk),
    mean_pu = mean(p_u),
    mean_x = mean(n_gender_x),
    mean_px = mean(p_x)
  )

rstart |>
  mutate(
    n_gender_x = ifelse(clinic == "um", 0, as.numeric(n_gender_x)),
    n_gender_x = ifelse(is.na(as.numeric(n_gender_x)), 0, n_gender_x),
    n_gender_unk = ifelse(is.na(as.numeric(n_gender_unk)), 0, n_gender_unk),
    n4 = n_gender_male + n_gender_female + n_gender_unk + n_gender_x,
    p_m = n_gender_male / n4,
    p_f = n_gender_female / n4,
    p_u = n_gender_unk / n4,
    p_x = n_gender_x / n4
  ) |>
  group_by(clinic) |>
  summarize(
    mean_m = mean(n_gender_male),
    mean_pm = mean(p_m),
    mean_f = mean(n_gender_female),
    mean_pf = mean(p_f),
    mean_u = mean(n_gender_unk),
    mean_pu = mean(p_u),
    mean_x = mean(n_gender_x),
    mean_px = mean(p_x)
  )

rstart |>
  mutate(
    n5 = n_uninsured + n_private_insurance + n_public_insurance,
    p_priv = n_private_insurance / n5,
    p_pub = n_public_insurance / n5
  ) |>
  summarize(
    mean_priv = mean(n_private_insurance),
    mean_ppriv = mean(p_priv, na.rm=T),
    mean_pub = mean(n_public_insurance),
    mean_ppub = mean(p_pub, na.rm=T)
  )

rstart |>
  mutate(
    n5 = n_uninsured + n_private_insurance + n_public_insurance,
    p_priv = n_private_insurance / n5,
    p_pub = n_public_insurance / n5
  ) |>
  group_by(clinic) |>
  summarize(
    mean_priv = mean(n_private_insurance),
    mean_ppriv = mean(p_priv, na.rm=T),
    mean_pub = mean(n_public_insurance),
    mean_ppub = mean(p_pub, na.rm=T)
  )



## Fourth Page

rstart |>
  mutate(
    p_bad_bp = n_bp_over140_90 / n_patients_served_over18,
    p_bad_bmi = n_bmi_over30 / n_patients_served_over18,
    p_bp = ifelse(p_bp > 1, 1, p_bp),
    p_bmi = ifelse(p_bmi > 1, 1, p_bmi)
  ) |>
  summarize(
    m_bp = mean(n_bp),
    sd_bp = sd(n_bp),
    mean_pbp = mean(p_bp),
    m_bad_bp = mean(n_bp_over140_90),
    sd_bad_bp = sd(n_bp_over140_90),
    mean_pbbp = mean(p_bad_bp),
    m_bmi = mean(n_bmi),
    sd_bmi = sd(n_bmi),
    mean_pbmi = mean(p_bmi),
    m_bad_bmi = mean(n_bmi_over30),
    sd_bad_bmi = sd(n_bmi_over30),
    mean_pbbmi = mean(p_bad_bmi)
  )

rstart |>
  mutate(
    p_bad_bp = n_bp_over140_90 / n_patients_served_over18,
    p_bad_bmi = n_bmi_over30 / n_patients_served_over18,
    p_bp = ifelse(p_bp > 1, 1, p_bp),
    p_bmi = ifelse(p_bmi > 1, 1, p_bmi)
  ) |>
  group_by(clinic) |>
  summarize(
    m_bp = mean(n_bp),
    sd_bp = sd(n_bp),
    mean_pbp = mean(p_bp),
    m_bad_bp = mean(n_bp_over140_90),
    sd_bad_bp = sd(n_bp_over140_90),
    mean_pbbp = mean(p_bad_bp),
    m_bmi = mean(n_bmi),
    sd_bmi = sd(n_bmi),
    mean_pbmi = mean(p_bmi),
    m_bad_bmi = mean(n_bmi_over30),
    sd_bad_bmi = sd(n_bmi_over30),
    mean_pbbmi = mean(p_bad_bmi)
  ) |> view()

rstart |>
  mutate(
    p_o9 = n_a1c_over9 / n_a1c,
    p_8 = n_a1c_8to89 / n_a1c,
    p_7 = n_a1c_7to79 / n_a1c,
    p_u7 = n_a1c_under7 / n_a1c,
    n_a1c_under9 = n_a1c_7to79 + n_a1c_8to89 + n_a1c_under7,
    p_u9 = n_a1c_under9 / n_a1c
  ) |>
  summarize(
    m_a = mean(n_a1c),
    sd_a = sd(n_a1c),
    mean_pa = mean(p_a1c),
    m_o9 = mean(n_a1c_over9, na.rm=T),
    sd_o9 = sd(n_a1c_over9, na.rm=T),
    mean_p09 = mean(p_o9, na.rm=T),
    m_8 = mean(n_a1c_8to89, na.rm=T),
    sd_8 = sd(n_a1c_8to89, na.rm=T),
    mean_p8 = mean(p_8, na.rm=T),
    m_7 = mean(n_a1c_7to79, na.rm=T),
    sd_7 = sd(n_a1c_7to79, na.rm=T),
    mean_p7 = mean(p_7, na.rm=T),
    m_u7 = mean(n_a1c_under7),
    sd_u7 = sd(n_a1c_under7),
    mean_pu7 = mean(p_u7, na.rm=T),
    m_u9 = mean(n_a1c_under9, na.rm=T),
    sd_u9 = sd(n_a1c_under9, na.rm=T),
    mean_pu9 = mean(p_u9, na.rm=T)
  )

rstart |>
  mutate(
    p_o9 = n_a1c_over9 / n_a1c,
    p_8 = n_a1c_8to89 / n_a1c,
    p_7 = n_a1c_7to79 / n_a1c,
    p_u7 = n_a1c_under7 / n_a1c,
    n_a1c_under9 = n_a1c_7to79 + n_a1c_8to89 + n_a1c_under7,
    p_u9 = n_a1c_under9 / n_a1c
  ) |>
  group_by(clinic) |>
  summarize(
    m_a = mean(n_a1c),
    sd_a = sd(n_a1c),
    mean_pa = mean(p_a1c),
    m_o9 = mean(n_a1c_over9, na.rm=T),
    sd_o9 = sd(n_a1c_over9, na.rm=T),
    mean_p09 = mean(p_o9, na.rm=T),
    m_8 = mean(n_a1c_8to89, na.rm=T),
    sd_8 = sd(n_a1c_8to89, na.rm=T),
    mean_p8 = mean(p_8, na.rm=T),
    m_7 = mean(n_a1c_7to79, na.rm=T),
    sd_7 = sd(n_a1c_7to79, na.rm=T),
    mean_p7 = mean(p_7, na.rm=T),
    m_u7 = mean(n_a1c_under7),
    sd_u7 = sd(n_a1c_under7),
    mean_pu7 = mean(p_u7, na.rm=T),
    m_u9 = mean(n_a1c_under9, na.rm=T),
    sd_u9 = sd(n_a1c_under9, na.rm=T),
    mean_pu9 = mean(p_u9, na.rm=T)
  ) |> view()



## Second Page

rstart |>
  mutate(
    hours_budgeted_pa = as.numeric(hours_budgeted_pa),
    hours_budgeted_md = as.numeric(hours_budgeted_md),
    hours_budgeted_contract = as.numeric(hours_budgeted_contract),
    hours_budgeted_apn = as.numeric(hours_budgeted_apn)
  ) |>
  summarize(
    m_rn = mean(hours_budgeted_rn, na.rm=T),
    sd_rn = sd(hours_budgeted_rn, na.rm=T),
    m_apn = mean(hours_budgeted_apn, na.rm=T),
    sd_apn = sd(hours_budgeted_apn, na.rm=T),
    m_pa = mean(hours_budgeted_pa, na.rm=T),
    sd_pa = sd(hours_budgeted_pa, na.rm=T),
    m_md = mean(hours_budgeted_md, na.rm=T),
    sd_md = sd(hours_budgeted_md, na.rm=T),
    m_ma = mean(hours_budgeted_ma, na.rm=T),
    sd_ma = sd(hours_budgeted_ma, na.rm=T),
    m_lpn = mean(hours_budgeted_lpn, na.rm=T),
    sd_lpn = sd(hours_budgeted_lpn, na.rm=T),
    m_o = mean(hours_budgeted_other, na.rm=T),
    sd_o = sd(hours_budgeted_other, na.rm=T),
    m_c = mean(hours_budgeted_contract, na.rm=T),
    sd_c = sd(hours_budgeted_contract, na.rm=T)
  )

rstart |>
  mutate(
    hours_budgeted_pa = as.numeric(hours_budgeted_pa),
    hours_budgeted_md = as.numeric(hours_budgeted_md),
    hours_budgeted_contract = as.numeric(hours_budgeted_contract),
    hours_budgeted_apn = as.numeric(hours_budgeted_apn)
  ) |>
  group_by(clinic) |>
  summarize(
    m_rn = mean(hours_budgeted_rn, na.rm=T),
    sd_rn = sd(hours_budgeted_rn, na.rm=T),
    m_apn = mean(hours_budgeted_apn, na.rm=T),
    sd_apn = sd(hours_budgeted_apn, na.rm=T),
    m_pa = mean(hours_budgeted_pa, na.rm=T),
    sd_pa = sd(hours_budgeted_pa, na.rm=T),
    m_md = mean(hours_budgeted_md, na.rm=T),
    sd_md = sd(hours_budgeted_md, na.rm=T),
    m_ma = mean(hours_budgeted_ma, na.rm=T),
    sd_ma = sd(hours_budgeted_ma, na.rm=T),
    m_lpn = mean(hours_budgeted_lpn, na.rm=T),
    sd_lpn = sd(hours_budgeted_lpn, na.rm=T),
    m_o = mean(hours_budgeted_other, na.rm=T),
    sd_o = sd(hours_budgeted_other, na.rm=T),
    m_c = mean(hours_budgeted_contract, na.rm=T),
    sd_c = sd(hours_budgeted_contract, na.rm=T)
  ) |> view()
