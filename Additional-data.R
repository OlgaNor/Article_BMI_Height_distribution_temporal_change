#===============================================================================
#------------------------reference data-----------------------------------
#===============================================================================

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

#===============================================================================
#------------------------reference parameters-----------------------------------
#===============================================================================
# Centiles calculation
#=======================================================

#--------------centiles 2012 ---------------------------
centiles_2012 <- regional_reference %>%
  dplyr::mutate(
    p15  = gamlss.dist::qBCT(0.15, mu = M, sigma = S, nu = L, tau = TAU),
    p50 = gamlss.dist::qBCT(0.50, mu = M, sigma = S, nu = L, tau = TAU),
    p85 = gamlss.dist::qBCT(0.85, mu = M, sigma = S, nu = L, tau = TAU),
    p90 = gamlss.dist::qBCT(0.90, mu = M, sigma = S, nu = L, tau = TAU),
    p95 = gamlss.dist::qBCT(0.95, mu = M, sigma = S, nu = L, tau = TAU),
    p97 = gamlss.dist::qBCT(0.97, mu = M, sigma = S, nu = L, tau = TAU),
    p99 = gamlss.dist::qBCT(0.99, mu = M, sigma = S, nu = L, tau = TAU),
    Source = "Data, 2012"
  ) %>%
  dplyr::select(metric, sex, age_year, p15, p50, p85, p90, p95, p97, p99, Source) %>%
  dplyr::arrange(metric, sex, age_year)

empirical_2025 <- my_data_step3 %>%
  group_by(age_year, sex) %>%
  summarise(
    BMI_p15 = quantile(BMI, 0.15, na.rm = TRUE),
    BMI_p50 = quantile(BMI, 0.50, na.rm = TRUE),
    BMI_p85 = quantile(BMI, 0.85, na.rm = TRUE),
    BMI_p90 = quantile(BMI, 0.90, na.rm = TRUE),
    BMI_p95 = quantile(BMI, 0.95, na.rm = TRUE),
    BMI_p97 = quantile(BMI, 0.97, na.rm = TRUE),
    BMI_p99 = quantile(BMI, 0.99, na.rm = TRUE),
    
    Height_p15   = quantile(height, 0.15, na.rm = TRUE),
    Height_p50   = quantile(height, 0.50, na.rm = TRUE),
    Height_p85   = quantile(height, 0.85, na.rm = TRUE),
    Height_p90   = quantile(height, 0.90, na.rm = TRUE),
    Height_p95   = quantile(height, 0.95, na.rm = TRUE),
    Height_p97   = quantile(height, 0.97, na.rm = TRUE),
    Height_p99   = quantile(height, 0.99, na.rm = TRUE))