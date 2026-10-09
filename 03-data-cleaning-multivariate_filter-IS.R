# install.packages("isotree")
library(dplyr)
library(isotree)

gc()
#=============================================================================
# Detect multivariate outliers using the Isolation Forest algorithm.
# Observations identified as outliers are excluded from the analytical dataset.
#=============================================================================

iso_model <- isolation.forest(
  data = my_data_step2 %>% dplyr::select(age_in_months, height, weight), 
  ntrees = 500, 
  ndim = 1,
  missing_action = "impute")

# add flags

my_data_multivariate <- my_data_step2 %>%
  
  mutate(
    anomaly_score = predict(iso_model, my_data_step2 %>% dplyr::select(age_in_months, height, weight)),
      iso_outlier = case_when(
      is.na(age_in_months) | is.na(height) | is.na(weight) ~ NA_real_,
      anomaly_score > 0.65 ~ 1,
      TRUE ~ 0))

total_rows <- nrow(my_data_multivariate)

outliers_count <- sum(my_data_multivariate$iso_outlier == 1, na.rm = TRUE)

cat("In", total_rows, "(total sample) —", outliers_count, "were flagged as outliers.\n")


my_data_outliers <- my_data_multivariate %>%
  
  mutate(
    main_outlier_reason = dplyr::case_when(
      iso_outlier == 1 & abs(zhfa_WHO) > abs(zbfa_WHO) ~ "Due to height",
      iso_outlier == 1 & abs(zbfa_WHO) > abs(zhfa_WHO) ~ "Due to BMI",
      iso_outlier == 1 ~ "Both", 
      
      TRUE ~ "OKAY"))

write.csv(my_data_outliers,"Supplementary/outliers.csv")

# table without outliers (ISO forest)
my_data_step3 <- my_data_outliers %>%
  
  dplyr::filter(iso_outlier == 0 | is.na(iso_outlier))

#==============================================================================
# Now the dataset is ready for the next step
#==============================================================================
rm(list = setdiff(ls(), c("my_data_step3", "regional_reference")))

#Output files: outliers.csv in Supplementary