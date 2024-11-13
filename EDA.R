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
names(rstart)[2] <- "month"
names(rstart)[3] <- "org_name"
names(rstart)[5] <- "org_region_type"
names(rstart)[6] <- "org_type"
names(rstart)[41:44] <- c("num_patients_served_18_40","num_patients_served_41_65","num_patients_served_66_89","num_patients_served_90+")

rstart %>% filter(is.na(num_patients_served_18_40)) %>% select(1:5)
#UMMC FM @ ATTICA
#Missing pretty much everytying: Primary Clinic #18

rstart <- rstart %>% mutate(month = case_when(substring(month,1,5) == "April" ~ "2022-04-01",
                                    substring(month,1,3) == "May" ~ "2022-05-01",
                                    substring(month,1,4) == "June" ~ "2022-06-01",
                                    substring(month,1,4) == "July" ~ "2022-07-01",
                                    .default = month)
                            ,
                              ,
                            org_region_type = case_when(org_region_type == 1 ~ "rural",
                                                        org_region_type == 2 ~ "community",
                                                        org_region_type == 3 ~ "urban"),
                            org_type = case_when(org_type == 1 ~ "stand_alone",
                                                 org_type == 2 ~ "academic",
                                                 org_type == 3 ~ "integrated",
                                                 org_type == 4 ~ "federally_qualified",
                                                 org_type == 5 ~ "rural")
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


