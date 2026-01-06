# Filter safety population and keep required visits

adlb<-adlb
advs_filtered <- advs %>%
  filter(SAFFL == "Y" & VISIT != "AMBUL ECG REMOVAL")
ADD<-advs
# Calculate summary statistics: N, Mean, Median, SD, Min, Max
stats_table <- advs_filtered %>%
  group_by(PARAM, VISIT, TRT01A) %>%
  summarise(
    n = as.character(n()),
    mean_sd = paste0(
      format(round(mean(AVAL, na.rm = TRUE), 2), nsmall = 2),
      " (",
      format(round(sd(AVAL, na.rm = TRUE), 3), nsmall = 3),
      ")"
    ),
    median = format(round(median(AVAL, na.rm = TRUE), 2), nsmall = 2),
    min = format(round(min(AVAL, na.rm = TRUE), 1)),
    max = format(round(max(AVAL, na.rm = TRUE), 1))
  ) %>%
  pivot_longer(cols = -c(PARAM, VISIT, TRT01A),
               names_to = "Statistic", values_to = "Observed_Value")

# Change-from-baseline summary
chg_table <- advs_filtered %>%
  filter(VISIT != "Baseline") %>%
  group_by(PARAM, VISIT, TRT01A) %>%
  summarise(
    n = as.character(n()),
    mean_sd = paste0(
      format(round(mean(CHG, na.rm = TRUE), 2), nsmall = 2),
      " (",
      format(round(sd(CHG, na.rm = TRUE), 3), nsmall = 3),
      ")"
    ),
    median = format(round(median(CHG, na.rm = TRUE), 2), nsmall = 2),
    min = format(round(min(CHG, na.rm = TRUE), 0)),
    max = format(round(max(CHG, na.rm = TRUE), 0))
  ) %>%
  mutate(n = NA) %>%
  select(PARAM, VISIT, TRT01A, n, mean_sd, median,  min, max) %>%
  pivot_longer(cols = -c(PARAM, VISIT, TRT01A),
               names_to = "Statistic", values_to = "Change_From_Baseline")

# Combine observed and change-from-baseline
combined_table <- left_join(stats_table, chg_table,
                            by = c("PARAM", "VISIT", "TRT01A", "Statistic"))

# Prepare final format
final_table <- combined_table %>%
  pivot_wider(
    names_from = TRT01A,
    values_from = c(Observed_Value, Change_From_Baseline),
    names_sep = "_"
  ) %>%
  arrange(PARAM, VISIT)


