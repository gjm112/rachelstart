library(tidyverse)
library(xlsx)

dat <- rstart |>
  mutate(
    n_race_not_white = n_race_black + n_race_asian + n_race_hawaii + n_race_pi + n_race_aiai + n_race_multi,
    n_a1c_under9 = n_a1c_under7 + n_a1c_7to79 + n_a1c_8to89) |>
  group_by(clinic_name) |>
  reframe(
    Org = case_when(
      org == "chn" ~ "1",
      org == "roc" ~ "2",
      org == "um" ~ "3",
      org == "ws" ~ "4",
    ),
    Region = case_when(
      clinic_region_type == "community" ~ "Community",
      clinic_region_type == "rural" ~ "Rural",
      clinic_region_type == "urban" ~ "Urban",
    ),
    VpM = paste0(mean(n_total_visits_served_over18, na.rm=T) |> round(), " (", sd(n_total_visits_served_over18, na.rm=T) |> round(1), ")" ),
    RN = paste0(mean(hours_budgeted_rn/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(hours_budgeted_rn/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    MA = paste0(mean(hours_budgeted_ma/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(hours_budgeted_ma/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    LPN = paste0(mean(hours_budgeted_lpn/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(hours_budgeted_lpn/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    O = paste0(mean(hours_budgeted_other/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(hours_budgeted_other/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    APN = paste0(mean(hours_budgeted_apn/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(hours_budgeted_apn/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    PA = paste0(mean(hours_budgeted_pa/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(hours_budgeted_pa/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    MD = paste0(mean(hours_budgeted_md/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(hours_budgeted_md/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    P1840 = paste0(mean(n_patients_served_18_40) |> round(), " (", sd(n_patients_served_18_40) |> round(1), ")" ),
    P4165 = paste0(mean(n_patients_served_41_65) |> round(), " (", sd(n_patients_served_41_65) |> round(1), ")" ),
    P65 = paste0(mean(n_patients_served_65plus) |> round(), " (", sd(n_patients_served_41_65) |> round(1), ")" ),
    W = paste0(mean(n_race_white) |> round(), " (", sd(n_race_white) |> round(1), ")" ),
    NW = paste0(mean(n_race_not_white) |> round(), " (", sd(n_race_not_white, na.rm=T) |> round(1), ")" ),
    RU = paste0(mean(n_race_unreported) |> round(), " (", sd(n_race_unreported, na.rm=T) |> round(1), ")" ),
    NH = paste0(mean(n_ethnicity_non_hispanic) |> round(), " (", sd(n_ethnicity_non_hispanic, na.rm=T) |> round(1), ")" ),
    H = paste0(mean(n_ethnicity_hispanic) |> round(), " (", sd(n_ethnicity_hispanic, na.rm=T) |> round(1), ")" ),
    EU = paste0(mean(n_ethnicity_unreported) |> round(), " (", sd(n_ethnicity_unreported, na.rm=T) |> round(1), ")" ),
    GM = paste0(mean(n_gender_male) |> round(), " (", sd(n_gender_male, na.rm=T) |> round(1), ")" ),
    GF = paste0(mean(n_gender_female) |> round(), " (", sd(n_gender_female, na.rm=T) |> round(1), ")" ),
    PrI = paste0(mean(n_private_insurance) |> round(), " (", sd(n_private_insurance, na.rm=T) |> round(1), ")" ),
    PuI = paste0(mean(n_public_insurance) |> round(), " (", sd(n_public_insurance, na.rm=T) |> round(1), ")" ),
    UI = paste0(mean(n_uninsured) |> round(), " (", sd(n_uninsured, na.rm=T) |> round(1), ")" ),
    BPA = paste0(mean(n_bp) |> round(), " (", sd(n_bp, na.rm=T) |> round(1), ")" ),
    BPO = paste0(mean(n_bp_over140_90) |> round(), " (", sd(n_bp_over140_90, na.rm=T) |> round(1), ")" ),
    BMIA = paste0(mean(n_bmi) |> round(), " (", sd(n_bmi, na.rm=T) |> round(1), ")" ),
    BMIO = paste0(mean(n_bmi_over30) |> round(), " (", sd(n_bmi_over30, na.rm=T) |> round(1), ")" ),
    A1C = paste0(mean(n_a1c) |> round(), " (", sd(n_a1c, na.rm=T) |> round(1), ")" ),
    A1C_u7 = paste0(mean(n_a1c_under7) |> round(), " (", sd(n_a1c_under7, na.rm=T) |> round(1), ")" ),
    A1C_7_79 = paste0(mean(n_a1c_7to79) |> round(), " (", sd(n_a1c_7to79, na.rm=T) |> round(1), ")" ),
    A1C_8_89 = paste0(mean(n_a1c_8to89) |> round(), " (", sd(n_a1c_8to89, na.rm=T) |> round(1), ")" ),
    A1C_o9 = paste0(mean(n_a1c_over9, na.rm=T) |> round(), " (", sd(n_a1c_over9, na.rm=T) |> round(1), ")" ),
    A1C_u9 = paste0(mean(n_a1c_under9, na.rm=T) |> round(), " (", sd(n_a1c_under9, na.rm=T) |> round(1), ")" )
  ) |>
  distinct() |>
  arrange(Org)

write_excel_csv(dat,
                file = "ClinicTables.csv")

dat2 <- rstart |>
  mutate(
    n_race_not_white = n_race_black + n_race_asian + n_race_hawaii + n_race_pi + n_race_aiai + n_race_multi,
    n_a1c_under9 = n_a1c_under7 + n_a1c_7to79 + n_a1c_8to89) |>
  group_by(clinic_name) |>
  reframe(
    Org = case_when(
      org == "chn" ~ "1",
      org == "roc" ~ "2",
      org == "um" ~ "3",
      org == "ws" ~ "4",
    ),
    Region = case_when(
      clinic_region_type == "community" ~ "Community",
      clinic_region_type == "rural" ~ "Rural",
      clinic_region_type == "urban" ~ "Urban",
    ),
    VpM = mean(n_total_visits_served_over18, na.rm=T),
    RN = mean(hours_budgeted_rn/n_total_visits_served_over18, na.rm=T),
    MA = mean(hours_budgeted_ma/n_total_visits_served_over18, na.rm=T) ,
    LPN = mean(hours_budgeted_lpn/n_total_visits_served_over18, na.rm=T),
    O = mean(hours_budgeted_other/n_total_visits_served_over18, na.rm=T) ,
    APN = mean(hours_budgeted_apn/n_total_visits_served_over18, na.rm=T),
    PA = mean(hours_budgeted_pa/n_total_visits_served_over18, na.rm=T),
    MD = mean(hours_budgeted_md/n_total_visits_served_over18, na.rm=T),
    P1840 = mean(n_patients_served_18_40),
    P4165 = mean(n_patients_served_41_65),
    P65 = mean(n_patients_served_65plus),
    W = mean(n_race_white),
    NW = mean(n_race_not_white),
    RU = mean(n_race_unreported),
    NH = mean(n_ethnicity_non_hispanic),
    H = mean(n_ethnicity_hispanic),
    EU = mean(n_ethnicity_unreported),
    GM = mean(n_gender_male),
    GF = mean(n_gender_female),
    PrI = mean(n_private_insurance),
    PuI = mean(n_public_insurance),
    UI = mean(n_uninsured),
    BPA = mean(n_bp),
    BPO = mean(n_bp_over140_90),
    BMIA = mean(n_bmi),
    BMIO = mean(n_bmi_over30),
    A1C = mean(n_a1c),
    A1C_u7 = mean(n_a1c_under7),
    A1C_7_79 = mean(n_a1c_7to79),
    A1C_8_89 = mean(n_a1c_8to89),
    A1C_o9 = mean(n_a1c_over9, na.rm=T),
    A1C_u9 = mean(n_a1c_under9, na.rm=T)
  ) |>
  distinct()

table(dat2$Org)
table(dat2$Region)


summary(dat2$VpM)
summary(dat2$RN)
summary(dat2$MA)
summary(dat2$LPN)
summary(dat2$O)

dat3 <- dat2 |>
  filter(Org %in% c(1,4))

summary(dat3$APN)
summary(dat3$PA)
summary(dat3$MD)


summary(dat2$P1840)
summary(dat2$P4165)
summary(dat2$P65)

summary(dat2$W)
summary(dat2$NW)
summary(dat2$RU)

summary(dat2$H)
summary(dat2$NH)
summary(dat2$EU)

summary(dat2$GM)
summary(dat2$GF)

summary(dat2$PrI)
summary(dat2$PuI)
summary(dat2$UI)


summary(dat2$BPA)
summary(dat2$BPO)

summary(dat2$BMIA)
summary(dat2$BMIO)

summary(dat2$A1C)
summary(dat2$A1C_u7)
summary(dat2$A1C_7_79)
summary(dat2$A1C_8_89)
summary(dat2$A1C_o9)
summary(dat2$A1C_u9)


dat3 <- rstart |>
  group_by(clinic_name, org) |>
  reframe(
    Org = case_when(
      org == "chn" ~ "1",
      org == "roc" ~ "2",
      org == "um" ~ "3",
      org == "ws" ~ "4",
    ),
    cent_rn = case_match(
      org,
      "chn" ~ 68.92,
      "roc" ~ 75,
      "um" ~ 32.16,
      "ws" ~ 0),
    rn_manager = case_match(
      org,
      "chn" ~ 86.15,
      "roc" ~ 5.8,
      "um" ~ 168.42,
      "ws" ~ 104.18),
    CENT = paste0(mean(cent_rn/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(cent_rn/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    RN_C = paste0(mean((cent_rn+hours_budgeted_rn) /n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd((cent_rn+hours_budgeted_rn)/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    MAN = paste0(mean(rn_manager/n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd(rn_manager/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    RN_M = paste0(mean((rn_manager+hours_budgeted_rn) /n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd((rn_manager+hours_budgeted_rn)/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    CENT_MAN = paste0(mean((rn_manager+cent_rn) /n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd((rn_manager+cent_rn)/n_total_visits_served_over18, na.rm=T) |> round(2), ")" ),
    TOT = paste0(mean((cent_rn+rn_manager+hours_budgeted_rn) /n_total_visits_served_over18, na.rm=T) |> round(2), " (", sd((cent_rn+rn_manager+hours_budgeted_rn)/n_total_visits_served_over18, na.rm=T) |> round(2), ")" )
  ) |>
  distinct()|>
  arrange(Org)

write_excel_csv(dat3,
                file = "CH5.csv")

dat4 <- rstart |>
  group_by(clinic_name) |>
  reframe(
    cent_rn = case_match(
      org,
      "chn" ~ 68.92,
      "roc" ~ 75,
      "um" ~ 32.16,
      "ws" ~ 0),
    rn_manager = case_match(
      org,
      "chn" ~ 86.15,
      "roc" ~ 5.8,
      "um" ~ 168.42,
      "ws" ~ 104.18),
    RN = mean(hours_budgeted_rn / n_total_visits_served_over18, na.rm=T),
    C = mean(cent_rn / n_total_visits_served_over18, na.rm=T),
    RNC = mean((cent_rn + hours_budgeted_rn) / n_total_visits_served_over18, na.rm=T),
    M = mean(rn_manager / n_total_visits_served_over18, na.rm=T),
    RNM = mean((rn_manager + hours_budgeted_rn) / n_total_visits_served_over18, na.rm=T),
    CM = mean((cent_rn + rn_manager) / n_total_visits_served_over18, na.rm=T),
    TOT = mean((cent_rn + rn_manager + hours_budgeted_rn) / n_total_visits_served_over18, na.rm=T),
  ) |>
  distinct()

summary(dat4$RN)
summary(dat4$C)
summary(dat4$RNC)
summary(dat4$M)
summary(dat4$RNM)
summary(dat4$CM)
