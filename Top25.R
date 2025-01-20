ranks <- rstart |>
  group_by(clinic_name, org) |>
  summarize(
    months = n_distinct(month),
    n = sum(n_total_visits_served_over18),
    n_bp = sum(n_bp),
    n_bmi = sum(n_bmi),
    n_a1c = sum(n_a1c),
    bp_bad = sum(n_bp_over140_90),
    bmi_bad = sum(n_bmi_over30),
    a1c_good = sum(n_a1c_under7),
    p_bp = 1 - (bp_bad / n_bp),
    p_bmi = 1 - (bmi_bad / n_bmi),
    p_a1c = a1c_good / n_a1c
  ) |>
  ungroup() |>
  filter(n_a1c >= 50, n_bp >= 50, n_bmi >= 50) |>
  mutate(
    p_a1c = ifelse(is.na(p_a1c), 0, p_a1c),
    bp_rank = rank(-p_bp, ties.method = "min"),
    bmi_rank = rank(-p_bmi, ties.method = "min"),
    a1c_rank = rank(-p_a1c, ties.method = "min"),
    rank = (bp_rank + bmi_rank + a1c_rank) / 3
  )

t25 <- ranks |> arrange(rank) |> select(clinic_name, org, rank) |> head(32)
t25

table(t25$org)

rstart |>
  group_by(org) |>
  summarize(
    n = n_distinct(clinic_name)
  )

t25_data <- t25 |>
  select(clinic_name) |>
  left_join(rstart, by = "clinic_name")

n25 <- setdiff(rstart, t25_data)

## RN, MA, LPN, and other

t25_data |>
  summarize(
    mean_rn = mean(hours_budgeted_rn),
    sd_rn = sd(hours_budgeted_rn),
    mean_ma = mean(hours_budgeted_ma),
    sd_ma = sd(hours_budgeted_ma),
    mean_lpn = mean(hours_budgeted_lpn),
    sd_lpn = sd(hours_budgeted_lpn),
    mean_other = mean(hours_budgeted_other),
    sd_other = sd(hours_budgeted_other),
  )
 
n25 |>
  summarize(
    mean_rn = mean(hours_budgeted_rn, na.rm=T),
    sd_rn = sd(hours_budgeted_rn, na.rm=T),
    mean_ma = mean(hours_budgeted_ma, na.rm=T),
    sd_ma = sd(hours_budgeted_ma, na.rm=T),
    mean_lpn = mean(hours_budgeted_lpn, na.rm=T),
    sd_lpn = sd(hours_budgeted_lpn, na.rm=T),
    mean_other = mean(hours_budgeted_other, na.rm=T),
    sd_other = sd(hours_budgeted_other, na.rm=T),
  )

t25_data |>
  group_by(org) |>
  summarize(
    mean_rn = mean(hours_budgeted_rn),
    sd_rn = sd(hours_budgeted_rn),
    mean_ma = mean(hours_budgeted_ma),
    sd_ma = sd(hours_budgeted_ma),
    mean_lpn = mean(hours_budgeted_lpn),
    sd_lpn = sd(hours_budgeted_lpn),
    mean_other = mean(hours_budgeted_other),
    sd_other = sd(hours_budgeted_other),
  )

n25 |>
  group_by(org) |>
  summarize(
    mean_rn = mean(hours_budgeted_rn, na.rm=T),
    sd_rn = sd(hours_budgeted_rn, na.rm=T),
    mean_ma = mean(hours_budgeted_ma, na.rm=T),
    sd_ma = sd(hours_budgeted_ma, na.rm=T),
    mean_lpn = mean(hours_budgeted_lpn, na.rm=T),
    sd_lpn = sd(hours_budgeted_lpn, na.rm=T),
    mean_other = mean(hours_budgeted_other, na.rm=T),
    sd_other = sd(hours_budgeted_other, na.rm=T),
  )

# APN, PA, and MD

t25_data |>
  filter(org %in% c("chn", "ws")) |>
  summarize(
    mean_apn = mean(hours_budgeted_apn, na.rm=T),
    sd_apn = sd(hours_budgeted_apn, na.rm=T),
    mean_pa = mean(hours_budgeted_pa, na.rm=T),
    sd_pa = sd(hours_budgeted_pa, na.rm=T),
    mean_md = mean(hours_budgeted_md, na.rm=T),
    sd_md = sd(hours_budgeted_md, na.rm=T),
  )

n25 |>
  filter(org %in% c("chn", "ws")) |>
  summarize(
    mean_apn = mean(hours_budgeted_apn, na.rm=T),
    sd_apn = sd(hours_budgeted_apn, na.rm=T),
    mean_pa = mean(hours_budgeted_pa, na.rm=T),
    sd_pa = sd(hours_budgeted_pa, na.rm=T),
    mean_md = mean(hours_budgeted_md, na.rm=T),
    sd_md = sd(hours_budgeted_md, na.rm=T),
  )

t25_data |>
  filter(org %in% c("chn", "ws")) |>
  group_by(org) |>
  summarize(
    mean_apn = mean(hours_budgeted_apn, na.rm=T),
    sd_apn = sd(hours_budgeted_apn, na.rm=T),
    mean_pa = mean(hours_budgeted_pa, na.rm=T),
    sd_pa = sd(hours_budgeted_pa, na.rm=T),
    mean_md = mean(hours_budgeted_md, na.rm=T),
    sd_md = sd(hours_budgeted_md, na.rm=T),
  )

n25 |>
  filter(org %in% c("chn", "ws")) |>
  group_by(org) |>
  summarize(
    mean_apn = mean(hours_budgeted_apn, na.rm=T),
    sd_apn = sd(hours_budgeted_apn, na.rm=T),
    mean_pa = mean(hours_budgeted_pa, na.rm=T),
    sd_pa = sd(hours_budgeted_pa, na.rm=T),
    mean_md = mean(hours_budgeted_md, na.rm=T),
    sd_md = sd(hours_budgeted_md, na.rm=T),
  )

ttest <- rstart |>
  mutate(T25 = clinic_name %in% t25$clinic_name)

