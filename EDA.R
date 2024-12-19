library(tidyverse)
#This is the one with the actual data: Ethnicity..Numberof.Hispanic.Patient.Service.in.Unit.Month.Collected
chn <- read.csv("./cleaneddata20241108/CHN.csv")
chn <- chn %>% select(-Ethnicity..Number.of.Hispanic.Patients.Served.in.Unit.Month.Collected)
dim(chn)
chn$clinic <- "chn"

roc <- read.csv("./cleaneddata20241108/ROC.csv")
roc <- roc %>% select(-Ethnicity..Number.of.Hispanic.Patients.Served.in.Unit.Month.Collected)
dim(roc)
roc$clinic <- "roc"

um <- read.csv("./cleaneddata20241108/UM.csv")
um <- um %>% select(-Number.of.Patients.with.HGB.A1C.equal.9.0.in.unit.month.collected, 
                    -Ethnicity..Number.of.Hispanic.Patients.Served.in.Unit.Month.Collected) %>% 
  rename(Unit.of.Measurement.Total.Number.of.Patients.over.18.years.of.age.seen.month..Total.number.of.patients.seen.month..visits....telehealth.encounters...RN.Visits.Month.collected.for.the.6.months.collected...April.thru.September.2022...Sum.of.Column.U..V.and.W. = Unit.of.Measurement.Total.Number.of.Patients.over.18.years.of.age.seen.month..Total.number.of.patients.seen.month..visits....telehealth.encounters...RN.Visits.Month.collected.for.the.6.months.collected...April.thru.September.2022...Sum.of.Column.AB..AC..AD.)
dim(um)
um$clinic <- "um"

#Drop this column "Number.of.Patients.with.HGB.A1C.equal.9.0.in.unit.month.collected"  
ws <- read.csv("./cleaneddata20241108/WS.csv")
dim(ws)
ws$clinic <- "ws"

rstart <- rbind(chn, roc, ws, um)

#Who participated
rstart <- janitor::clean_names(rstart)

rstart %>% View()
rstart$number_of_ambulatory_clinics_total_clinics_primary_peds_primary_adult_primary_family_specialty_hospital_based_stand_alone_other
table(rstart$clinic,rstart$number_of_ambulatory_clinics_total_clinics_primary_peds_primary_adult_primary_family_specialty_hospital_based_stand_alone_other)
table(rstart$clinic,rstart$number_of_primary_care_clinics_serving_adults_over_18_years_of_age_with_rn_manager)
table(rstart$clinic,rstart$total_number_of_ambulatory_primary_care_clinics_that_serve_adults_over_18_years_of_age)

table(rstart$clinic,rstart$ratio_of_rn_manager_to_primary_care_clinic_serving_over_18_year_old_adults_choose_the_following_1_1_1_ratio_2_3_1_ratio_3_6_1_ratio_4_1_10_or_over_clinics_ratio)
table(rstart$clinic,rstart$organization_magnet_status_yes_1_no_2)

table(rstart$clinic,rstart$if_centralized_telehealth_center_in_place_for_ambulatory_primary_care_clinics_submitted_what_are_the_actual_hours_worked_month_by_that_combined_staff)


table(rstart$clinic,rstart$gender_at_birth_number_of_patients_who_identify_as_x_gender_at_time_of_birth_in_unit_month)
table(rstart$clinic,rstart$gender_at_birth_number_of_patients_who_identify_as_unknown_gender_at_time_of_birth_in_unit_month_collected)


#Notes: 
#Variable: number_of_ambulatory_clinics_total_clinics_primary_peds_primary_adult_primary_family_specialty_hospital_based_stand_alone_other
#um is correct.  All rows should have the same number.  chn should be 78.  roc and ws we can add up and replace the total number. 
#Columns 7,8,9, and 14

#Drop the column: ratio_of_rn_manager_to_primary_care_clinic_serving_over_18_year_old_adults_choose_the_following_1_1_1_ratio_2_3_1_ratio_3_6_1_ratio_4_1_10_or_over_clinics_ratio
#We can compute this using: 
#number_of_primary_care_clinics_serving_adults_over_18_years_of_age_with_rn_manager divided by number_of_primary_care_clinics_serving_adults_over_18_years_of_age_with_rn_manager

#if_centralized_telehealth_center_in_place_for_ambulatory_primary_care_clinics_submitted_what_are_the_actual_hours_worked_month_by_that_combined_staff
#Each FTE is worth 80 hours
#5.6*80 = 448 hours

#Race: Black White Asian Other. 

#August 2022 wellstar gender x 50?

#Descriptive stats: 


#Regression: 
#1. Org by org: 
#2. All 4 orgs together: 


#Two models
glm(cbind(controlled, evaluated) ~ x, family = "binomial")
#Remove clinics who have no one evaluated.

glm(cbind(evaluated, total) ~ x, family = "binomial")

#random intercept for clinic across the months.  

#To start:
#logistics regression: number_of_patients_with_hgb_a1c_over_9_0_in_unit_month_collected out of number_of_patients_with_hgb_a1c_assessed_in_unit_month_collected
#covariates: to start: hours_budgeted_rn (not hours worked) 
#covariates: patient covariates: demographic covariates: race (White, Black, Asian, Other? or White and Not White?), age (18-40, 41-65, 66+), sex (M, F), insurer (public, private, uninsured)
#proportion of people serving the patients: rn and ma are the most important.  

  
#Repeat for bmi, and bp
  
#Number measured isn't the same as number in clinic total?   
  
table(rstart$clinic, rstart$n_uninsured)

  
#same for hyper tension and BMI


rstart  <- rstart %>% rename(month = "month_we_are_collecting_april_may_june_july_august_september_of_2022_for_each_clinic",
                             org_name = "unit_of_measure_name_primary_clinic_1_primary_clinic_2_etc_if_you_have_150_clinics_you_should_have_150_lines_of_data_submitted_here",
                             org_region_type = "organization_region_type_rural_community_urban_rural_1_community_2_urban_3",
                             org_type = "organization_type_stand_alone_medical_group_academic_health_system_integrated_health_system_federally_qualitifed_health_system_rural_health_system_stand_alone_1_academic_2_integrated_3_federally_qualified_4_rural_5",
                             n_clinics_total = "number_of_ambulatory_clinics_total_clinics_primary_peds_primary_adult_primary_family_specialty_hospital_based_stand_alone_other",
                             n_clinics_ped_adult = "total_number_of_ambulatory_primary_care_clinics_that_care_for_all_ages_pediatric_through_adult",
                             n_clinics_adult_only = "total_number_of_ambulatory_primary_care_clinics_that_serve_adults_over_18_years_of_age",
                             n_patients_served_18_40 = "number_of_patients_served_between_18_40_years_of_age_clinic_month",
                             n_patients_served_41_65 = "number_of_patients_served_between_41_65_years_of_age_clinic_month",
                             n_patients_served_66_89 = "number_of_patients_served_between_66_89_years_of_age_clinic_month",
                             n_patients_served_over89 = "number_of_patients_served_over_89_years_of_age_clinic_month", 
                             I_magnet = "organization_magnet_status_yes_1_no_2",
                             I_magnet_w_amb_struture = "organization_magnet_status_inclusive_of_ambulatory_structure_yes_1_no_2",
                             I_stand_alone = "organization_stand_alone_for_ambulatory_structure_achievement_of_magnet_yes_1_no_2",
                             I_chief_nursing_officer_present = "chief_nursing_officer_present_for_ambulatory_structure_yes_1_no_2",
                             n_pcc_w_rn_manager = "number_of_primary_care_clinics_serving_adults_over_18_years_of_age_with_rn_manager", 
                             ratio_rn_manager_to_pcc = "ratio_of_rn_manager_to_primary_care_clinic_serving_over_18_year_old_adults_choose_the_following_1_1_1_ratio_2_3_1_ratio_3_6_1_ratio_4_1_10_or_over_clinics_ratio", 
                             I_coll_barg = "collective_bargaining_unit_in_place_ambulatory_structure_yes_1_no_2",
                             I_cent_rn_staffinng = "centralized_rn_staffing_in_place_for_ambulatory_primary_care_clinics_submitted_yes_1_or_no_2", 
                             n_rn_hours_worked_cent = "if_centralized_rn_staffing_in_place_for_ambulatory_primary_clinics_submitted_what_are_actual_rn_hours_worked_month_by_that_combined_staff",
                             n_a1c = "number_of_patients_with_hgb_a1c_assessed_in_unit_month_collected",
                             n_a1c_under7 = "number_of_patients_with_hgb_a1c_under_7_0",
                             n_a1c_7to79 = "number_of_patients_with_hgb_a1c_under_7_0_7_9in_unit_month_collected",
                             n_a1c_8to89 = "number_of_patients_with_hgb_a1c_8_0_8_9_in_unit_month_collected",
                             n_a1c_over9 = "number_of_patients_with_hgb_a1c_over_9_0_in_unit_month_collected",
                             n_bmi_over30 = "number_of_patients_with_body_mass_index_over_30_in_unit_month_collected",
                             n_bmi = "number_of_patients_with_body_mass_index_assessed_in_unit_month_collected",
                             n_bp_over140_90 = "number_of_patients_with_bp_over_140_90_unit_month_collected",
                             n_bp = "number_of_patients_blood_pressure_assessed_in_unit_month_collected",
                             n_uninsured = "number_of_patients_that_are_uninsured_in_unit_month_collected",
                             n_public_insurance = "number_of_patients_with_public_governmental_payor_in_unit_month_collected",
                             n_private_insurance = "number_of_patients_with_private_payor_in_unit_month_collected",
                             n_gender_x = "gender_at_birth_number_of_patients_who_identify_as_x_gender_at_time_of_birth_in_unit_month",
                             n_gender_unk = "gender_at_birth_number_of_patients_who_identify_as_unknown_gender_at_time_of_birth_in_unit_month_collected",
                             n_gender_male = "gender_at_birth_number_of_male_patients_served_in_unit_month_collected",
                             n_gender_female = "gender_at_birth_number_of_female_patients_served_in_unit_month_collected",
                             n_ethnicity_unreported = "ethnicity_number_of_unreported_ethnicity_patients_served_in_unit_month_collected",
                             n_ethnicity_non_hispanic = "ethnicity_number_of_non_hispanic_patients_served_in_unit_month_collected",
                             n_ethnicity_hispanic = "ethnicity_numberof_hispanic_patient_service_in_unit_month_collected",
                             n_race_unreported = "race_number_of_unreported_race_patients_served_in_unit_month_collected",
                             n_race_multi = "race_number_of_multiracial_patients_more_than_1_race_identified_served_in_unit_month_collected",
                             n_race_aiai = "race_number_of_american_indian_alaskan_indian_patients_served_in_unit_month_collected",
                             n_race_pi = "race_number_of_other_pacific_islander_patients_served_in_unit_month_collected",
                             n_race_hawaii =  "race_number_of_native_hawaiaan_patients_served_in_unit_month_collected",
                             n_race_asian = "race_number_of_asian_patients_served_in_unit_month_collected",
                             n_race_white = "race_number_of_caucasian_white_patients_served_in_unit_month_collected",
                             n_race_black = "race_number_of_black_african_american_patients_served_in_unit_month_collected",
                             I_tele_rn_staffing = "centralized_telehealth_center_in_place_for_ambulatory_primary_care_clinics_for_the_purposes_of_access_population_health_or_care_coordination_yes_1_or_no_2",
                             n_rn_hours_worked_tele = "if_centralized_telehealth_center_in_place_for_ambulatory_primary_care_clinics_submitted_what_are_the_actual_hours_worked_month_by_that_combined_staff",
                             n_patients_served_over18 = "unit_of_measurement_total_number_of_patients_over_18_years_of_age_seen_month_total_number_of_patients_seen_month_visits_telehealth_encounters_rn_visits_month_collected_for_the_6_months_collected_april_thru_september_2022_sum_of_column_u_v_and_w",
                             n_visits_over18 =  "unit_of_measurement_total_number_of_visits_month_6_months_collected_for_patients_over_18_years_of_age",
                             n_rn_visits_over18 = "unit_of_measurement_total_number_of_rn_visits_month_6_months_collected_for_patients_over_18_years_of_age",
                             n_rn_tele_vistis = "unit_of_measurement_total_number_of_rn_tele_or_virtual_visits_month_6_months_collected",
                             hours_worked_rn = "actual_hours_worked_rn_clinic_month_collected",
                             hours_worked_apn = "actual_hours_worked_apn_clinic_month_collected",
                             hours_worked_pa = "actual_hours_worked_pa_clinic_month_collected",
                             hours_worked_ma = "actual_hours_worked_ma_clinic_month_collected",
                             hours_worked_md = "actual_hours_worked_md_clinic_month_collected",
                             hours_worked_lpn = "actual_hours_worked_lpn_clinic_month_collected",
                             hours_worked_other = "actual_hours_worked_other_ancillary_staff",
                             hours_worked_contract = "actual_hours_worked_contract_staff_clinic_month_collected",
                             hours_budgeted_rn = "budgeted_hours_worked_rn_clinic_month_collected",
                             hours_budgeted_apn = "budgeted_hours_worked_apn_clinic_month_collected",
                             hours_budgeted_pa = "budgeted_hours_worked_pa_clinic_month_collected",
                             hours_budgeted_ma = "budgeted_hours_worked_ma_clinic_month_collected",
                             hours_budgeted_md = "budgeted_hours_worked_md_clinic_month_collected",
                             hours_budgeted_lpn = "budgeted_hours_worked_lpn_clinic_month_collected",
                             hours_budgeted_other = "budgeted_hours_worked_other_ancillary_staff_clinic_month_collected",
                             hours_budgeted_contract = "budgeted_hours_worked_contract_staff_clinic_month_collected"
                             )

#rstart %>% filter(is.na(num_patients_served_18_40)) %>% select(1:5)
#UMMC FM @ ATTICA
#Missing pretty much everything: Primary Clinic #18
rstart <- rstart %>% mutate(month = case_when(substring(month,1,5) == "April" ~ "2022-04-01",
                                    substring(month,1,3) == "May" ~ "2022-05-01",
                                    substring(month,1,4) == "June" ~ "2022-06-01",
                                    substring(month,1,4) == "July" ~ "2022-07-01",
                                    substring(month,1,3) == "Aug" ~ "2022-08-01",
                                    substring(month,1,3) == "Sep" ~ "2022-09-01",
                                    .default = month),
                             
                            org_region_type = case_when(org_region_type == 1 ~ "rural",
                                                        org_region_type == 2 ~ "community",
                                                        org_region_type == 3 ~ "urban"),
                            org_type = case_when(org_type == 1 ~ "stand_alone",
                                                 org_type == 2 ~ "academic",
                                                 org_type == 3 ~ "integrated",
                                                 org_type == 4 ~ "federally_qualified",
                                                 org_type == 5 ~ "rural",
                                                 org_type == "Medical Group" ~ "stand_alone",
                                                 org_type == "2-Academic HealthSystem" ~ "academic"),
                            I_magnet = case_when(I_magnet == 1 ~ "Yes",
                                                 I_magnet == 2 ~ "No",
                                                 .default = I_magnet),
                            I_magnet_w_amb_struture = case_when(I_magnet_w_amb_struture == 1 ~ "Yes",
                                                                I_magnet_w_amb_struture == 2 ~ "No",
                                                                .default = I_magnet_w_amb_struture),
                            I_stand_alone = case_when(I_stand_alone == 1 ~ "Yes",
                                                      I_stand_alone == 2 ~ "No",
                                                      .default = I_stand_alone),
                            I_chief_nursing_officer_present = case_when(I_chief_nursing_officer_present == 1 ~ "Yes",
                                                                        I_chief_nursing_officer_present == 2 ~ "No",
                                                                        .default = I_chief_nursing_officer_present),
                            I_coll_barg = case_when(I_coll_barg == 1 ~ "Yes",
                                                    I_coll_barg == 2 ~ "No",
                                                    .default = I_coll_barg),
                            I_cent_rn_staffinng = case_when(I_cent_rn_staffinng == 1 ~ "Yes",
                                                            I_cent_rn_staffinng == 2 ~ "No",
                                                            .default = I_cent_rn_staffinng),
                            I_tele_rn_staffing = case_when(I_tele_rn_staffing == 1 ~ "Yes",
                                                           I_tele_rn_staffing == 2 ~ "No",
                                                           .default = I_tele_rn_staffing),
                            )



#Ratio column needs to get cleaned.  
#What are the dates in the first bunch of rows? 
table(rstart$org_region_type)
names(rstart)
table(rstart[,9])
names(rstart)[10]
View(rstart)


#Look at demographic differences across the four major clinics
rstart %>%  select(clinic, num_patients_served_18_40:`num_patients_served_90+`) %>% pivot_longer(cols =  num_patients_served_18_40:`num_patients_served_90+`, names_to = "age_group",values_to = "n") %>% group_by(clinic, age_group) %>% summarize(n = sum(n, na.rm = TRUE)) %>% ggplot(aes(x = clinic, y = n, fill = age_group)) + geom_bar(position="stack", stat="identity")
rstart %>%  select(clinic, num_patients_served_18_40:`num_patients_served_90+`) %>% pivot_longer(cols =  num_patients_served_18_40:`num_patients_served_90+`, names_to = "age_group",values_to = "n") %>% group_by(clinic, age_group) %>% summarize(n = sum(n, na.rm = TRUE)) %>% ggplot(aes(x = clinic, y = n, fill = age_group)) + geom_bar(position="fill", stat="identity")




#Staffing and outcomes
rstart <- rstart %>% mutate(n = Number.of.Patients.with.HGB.A1C.under.7.0 + )

ggplot(aes(y = Number.of.Patients.with.HGB.A1C.under.7.0, x = factor(clinic)), data = rstart) + geom_boxplot()

#Models
HGB ~ demographics + staffing


