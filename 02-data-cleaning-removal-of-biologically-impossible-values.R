#==============================================================================
# The next step includes BMI calculation, calculation of WHO and regional reference z-scores,
# removal of biologically implausible observations, and exclusion of observations outside
# the predefined range according to the regional reference protocol, removal of children aged less that 5 years and 12.4 years.
#==============================================================================


# Installation of packages
if (!requireNamespace("anthroplus", quietly = TRUE)) install.packages("anthroplus")
library(anthroplus)
library(dplyr)


# calculation of BMI
my_data_step1 <- my_data_step1 %>%
  mutate(
    BMI = weight / ((height / 100)^2)
  )

#remove children aged less that 5 years and older than 12.4
range(my_data_step1$age_year)
small_age_groups_years <- my_data_step1 %>%
  dplyr::group_by(sex) %>%
  dplyr::count(floor(age_year+0.5), name = "count") %>%
  dplyr::filter(count < 500)


cat("Age groups with less than 500:\n")
print(small_age_groups_years)
my_data_step1 %>% 
  dplyr::filter(age_year < 5.0) %>% 
  nrow()
sum(small_age_groups_years$count)

my_data_step1 <-  my_data_step1 %>%
  dplyr::filter(age_in_months >= 60 & age_in_months <= 149)

#==============================================================================
# 1590 observations was deleted (1170 less than 5.0 years and 279 older than 2.4 years)
# Total samle size is 18 316 now
#==============================================================================

#------------------------------------------------------------------------------
# Z-score calculation based on regional reference data
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Regional reference: Rudnev SG, Soboleva NP, Sterlikov SA, Nikolaev DV,
# Starunova OA, Chernykh SP, Eryukova TA, Kolesnikov VA,
# Melnichenko OA, Ponomareva EG. (2014).
# Bioimpedance study of body composition in the Russian population.
# Moscow: RIO TSNIIOIZ; 2014. 493 p. ISBN 5-94116-018-6.
#------------------------------------------------------------------------------

regional_reference <- tibble(
  age_year = rep(5:12, times = 4),
  sex      = rep(c(1, 1, 2, 2), each = 8),
  metric   = rep(c("BMI", "Height", "BMI", "Height"), each = 8),
  
  M = c(
    15.5, 15.58, 15.91, 16.48, 17.02, 17.56, 18.10, 18.61,       # Boys BMI
    110.2, 114.2, 120.9, 127.0, 132.5, 137.9, 143.2, 148.7,       # Boys Height
    15.1, 15.3, 15.8, 16.1, 16.6, 17.1, 17.7, 18.2,               # Girls BMI
    107.8, 111.4, 119.7, 126.6, 133.5, 139.0, 145.0, 149.2        # Girls Height
  ),
  
 
  S = c(
    0.111, 0.117, 0.127, 0.138, 0.149, 0.160, 0.169, 0.173,
    0.043, 0.042, 0.041, 0.041, 0.042, 0.043, 0.044, 0.046,
    0.134, 0.137, 0.144, 0.149, 0.154, 0.157, 0.158, 0.158,
    0.047, 0.047, 0.046, 0.046, 0.045, 0.045, 0.044, 0.043
  ),
  
 
  L = c(
    -2.39, -2.32, -2.21, -2.11, -2.03, -1.94, -1.87, -1.80,
    -0.031, 0.045, 0.165, 0.272, 0.368, 0.457, 0.539, 0.615,
    -2.61, -2.56, -2.42, -2.32, -2.21, -2.13, -2.04, -1.98,
    -0.067, -0.060, -0.043, -0.029, -0.015, -0.005, 0.007, 0.015
  ),
  
  
  TAU = c(
    149.8, 141.0, 128.1, 117.7, 109.0, 101.6, 95.2, 89.6,
    4.18, 4.43, 4.85, 5.25, 5.65, 6.04, 6.42, 6.79,
    90.9, 89.5, 86.2, 83.6, 81.2, 79.4, 77.4, 76.0,
    5.66, 5.76, 6.00, 6.21, 6.41, 6.58, 6.76, 6.90
  ),
  
 
  N = c(
    1023, 4624, 9077, 10075, 9474, 10132, 10358, 10376,
    1023, 4624, 9077, 10075, 9474, 10132, 10358, 10376,
    876, 4692, 7806, 9621, 7677, 9587, 8160, 9432,
    876, 4692, 7806, 9621, 7677, 9587, 8160, 9432
  )
) %>%
  dplyr::arrange(metric, sex, age_year)

ref_wide <- regional_reference %>%
  dplyr::select(age_year, sex, metric, M, S, L) %>%
  tidyr::pivot_wider(
    names_from = metric, 
    values_from = c(M, S, L),
    names_glue = "{.value}_{tolower(metric)}" 
  )

# Calculation of z-scores based on regional reference data
my_data_step1 <- my_data_step1 %>%
  
  dplyr::mutate( age_join = floor(age_year + 0.5)) %>% 
 
  dplyr::left_join(ref_wide, by = c("age_join" = "age_year", "sex")) %>%
  
  dplyr::mutate(
    zhfa_RR = (((height / M_height)^L_height) - 1) / (L_height * S_height),
    zbfa_RR    = (((BMI / M_bmi)^L_bmi) - 1) / (L_bmi * S_bmi)) %>%
  
  dplyr::select(-age_join, -starts_with("M_"), -starts_with("S_"), -starts_with("L_"))

#===============================================================================
# Next step is BMI and height values outside the ranges specified in the regional reference protocol
#===============================================================================

removed_count <- my_data_step1 %>%
  
  dplyr::filter(height <= 60 | BMI <= 12)  %>%
  
  nrow()

cat("Removed:", removed_count, "\n")

my_data_step1 <- my_data_step1 %>%
  
  dplyr::filter(
    height > 60,
    BMI > 12)

cat("After:", nrow(my_data_step1), "\n")

#===============================================================================
# 159 was removed, left 18 134
#===============================================================================


#===============================================================================
# Next step is removal of biologically ompossible values based on WHO protocol
#===============================================================================

# Quality Control by WHO values (biv - biologically impossible values). Sex should be write as 1 == Males, 2 == Females
# calculation of z-scores based on WHO
my_data_biv <- my_data_step1 %>%
  
  bind_cols(
    anthroplus_zscores(
      sex = my_data_step1$sex,
      age_in_months = my_data_step1$age_in_months,
      height_in_cm = my_data_step1$height,        
      weight_in_kg = my_data_step1$weight)%>% 
      
      dplyr::select(zhfa_WHO = zhfa, zbfa_WHO = zbfa))

#-----------------------------------------------------------------------------
# Check data variability by age and sex
#-----------------------------------------------------------------------------
my_data_biv %>%
  
  mutate(age_years = floor(age_year+0.5)) %>% 
  
  group_by(sex, age_years) %>% # Age in years
  summarise(
    # Статистика для Роста к возрасту
    min_haz_WHO = min(zhfa_WHO, na.rm = TRUE),
    max_haz_WHO = max(zhfa_WHO, na.rm = TRUE),
    min_haz_RR = min(zhfa_RR, na.rm = TRUE),
    max_haz_RR = max(zhfa_RR, na.rm = TRUE),
    
    # Статистика для ИМТ к возрасту
    min_baz_WHO = min(zbfa_WHO, na.rm = TRUE),
    max_baz_WHO = max(zbfa_WHO, na.rm = TRUE),
    min_baz_RR = min(zbfa_RR, na.rm = TRUE),
    max_baz_RR = max(zbfa_RR, na.rm = TRUE),
    
    .groups = 'drop'
  )


extreme_values_count <- my_data_biv %>%
  
  dplyr::filter(zhfa_WHO < -5.0 | zhfa_WHO > 5.0 | zbfa_WHO < -5.0 | zbfa_WHO > 5.0 ) %>%
  
  nrow()

cat("Number of observations:", extreme_values_count, "\n")


my_data_step1 <- my_data_biv %>%
  
  dplyr::filter(
    zhfa_WHO > -5.0 & zhfa_WHO < 5.0,
    zbfa_WHO > -5.0 & zbfa_WHO < 5.0
  )

cat("Sample size after removal:", nrow(my_data_step1), "\n")

#===============================================================================
# 71 was removed, left 18 059
#===============================================================================

data_variability <- my_data_biv %>%
  
  mutate(age_years = floor(age_year+0.5)) %>% 
  
  group_by(sex, age_years) %>% # Age in years
  
  summarise(
    min_haz_WHO = min(zhfa_WHO, na.rm = TRUE),
    max_haz_WHO = max(zhfa_WHO, na.rm = TRUE),
    min_haz_RR = min(zhfa_RR, na.rm = TRUE),
    max_haz_RR = max(zhfa_RR, na.rm = TRUE),
    
    min_baz_WHO = min(zbfa_WHO, na.rm = TRUE),
    max_baz_WHO = max(zbfa_WHO, na.rm = TRUE),
    min_baz_RR = min(zbfa_RR, na.rm = TRUE),
    max_baz_RR = max(zbfa_RR, na.rm = TRUE),
    
    .groups = 'drop'
  )

write.csv(data_variability,"Supplementary/z-score-variablity.csv")

#==============================================================================
# Now the dataset is ready for the next step
#==============================================================================
my_data_step2 <- my_data_step1

rm(list = setdiff(ls(), c("my_data_step2", "regional_reference")))

# output files: z-score-variablity.csv in Supplementary
