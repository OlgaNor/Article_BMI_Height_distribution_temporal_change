#==============================================================================
# Fit BCT models using the 2012 model parameterization
#==============================================================================

#age as years,month,days, i.e. 7.11. Calculated as (age in months / 12)
#agepower == age^0.25

# Formula from the regional reference protocol
# Variable ~ cs(agepower, df = 6),
# sigma.fo = ~ cs(agepower, df = 4),
# nu.fo = ~ agepower,
# tau.fo = ~ agepower,
# family = BCT

#==============================================================================
# What we get after this code will done:
#==============================================================================


#list_of_models <- list(
#bct_boys_bmi_based_on_2012   = bct_boys_bmi_based_on_2012,
#bct_girls_bmi_based_on_2012   = bct_girls_bmi_based_on_2012, 
#bct_boys_height_based_on_2012 = bct_boys_height_based_on_2012, 
#bct_girls_height_based_on_2012 = bct_girls_height_based_on_2012)


library(gamlss)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(gamlss.dist)

#===================================================================
#BOYS
#===================================================================

#--------------------BMI------------------
# First, reproduce the 2012 model on 2025 data. Obtain all the necessary data.
# Model parameters: BCT	Model 2012, μ	cs(age^0.25, df = 6), σ	cs(age^0.25, df = 4), ν =	age^0.25, τ =	age^0.25, distribution	BCT
my_data_boys <- my_data_step3 %>%
  dplyr::filter(sex==1,!is.na(BMI) & !is.na(age_in_months))

data_GAM_boys <- my_data_boys %>% dplyr::select(c("BMI","age_year"))

bct_boys_bmi_based_on_2012 <- gamlss::gamlss(
  BMI ~ cs(I(age_year^0.25), df = 6),
  sigma.fo = ~ cs(I(age_year^0.25), df = 4),
  nu.fo = ~ I(age_year^0.25),
  tau.fo = ~ I(age_year^0.25),
  family = BCPE,   
  data = data_GAM_boys,
  keep = FALSE,
  save = FALSE,
  control = gamlss.control(n.cyc = 50)
)

#--------------------Height------------------
data_GAM_boys <- my_data_boys %>% dplyr::select(c("height","age_year"))

bct_boys_height_based_on_2012 <- gamlss::gamlss(
  height ~ cs(I(age_year^0.25), df = 6),
  sigma.fo = ~ cs(I(age_year^0.25), df = 4),
  nu.fo = ~ I(age_year^0.25),
  tau.fo = ~ I(age_year^0.25),
  family = BCT,   data = data_GAM_boys,
  keep = FALSE,
  save = FALSE,
  control = gamlss.control(n.cyc = 20))

#=========================================================================
# GIRLS
#=========================================================================

#--------------------BMI------------------
# First, we reproduce the 2012 model. We obtain all the necessary data.
# Model parameters: BCT	Model 2012, μ	cs(age^0.25, df = 6), σ	cs(age^0.25, df = 4), ν =	age^0.25, τ =	age^0.25, distribution	BCT

my_data_girls <- my_data_step3 %>%
  dplyr::filter(sex==2,!is.na(BMI) & !is.na(age_in_months))

data_GAM_girls <- my_data_girls %>% dplyr::select(c("BMI","age_year"))


bct_girls_bmi_based_on_2012 <- gamlss::gamlss(
  BMI ~ cs(I(age_year^0.25), df = 6),
  sigma.fo = ~ cs(I(age_year^0.25), df = 4),
  nu.fo = ~ I(age_year^0.25),
  tau.fo = ~ I(age_year^0.25),
  family = BCT,   
  data = data_GAM_girls,
  keep = FALSE,
  save = FALSE,
  control = gamlss.control(n.cyc = 20)
)

#--------------------Height------------------
data_GAM_girls <- my_data_girls %>% dplyr::select(c("height","age_year"))

bct_girls_height_based_on_2012 <- gamlss::gamlss(
  height ~ cs(I(age_year^0.25), df = 6),
  sigma.fo = ~ cs(I(age_year^0.25), df = 4),
  nu.fo = ~ I(age_year^0.25),
  tau.fo = ~ I(age_year^0.25),
  family = BCT,   data = data_GAM_girls,
  keep = FALSE,
  save = FALSE,
  control = gamlss.control(n.cyc = 20))

#==============================================================================
#==============================================================================
rm(list = setdiff(ls(), c("my_data_step3", "regional_reference","bct_boys_bmi_based_on_2012","bct_girls_bmi_based_on_2012",
                          "bct_boys_height_based_on_2012","bct_girls_height_based_on_2012","data_GAM_girls","data_GAM_boys")))

list_of_models <- list(
  data_boys_bmi     = final_mod_boys_bmi,
  data_girls_bmi    = final_mod_girls_bmi,
  data_boys_height  = final_mod_boys_height,
  data_girls_height = final_mod_girls_height,
  bct_boys_bmi_based_on_2012   = bct_boys_bmi_based_on_2012,
  bct_girls_bmi_based_on_2012   = bct_girls_bmi_based_on_2012, 
  bct_boys_height_based_on_2012 = bct_boys_height_based_on_2012, 
  bct_girls_height_based_on_2012 = bct_girls_height_based_on_2012)


# =============================================================================
# Quality control
# =============================================================================
quality_summary <- names(list_of_models) %>%
  purrr::map_df(function(model_name) {
    mod <- list_of_models[[model_name]]
    
    res <- residuals(mod)
    
     ks_test <- ks.test(res, "pnorm", mean = 0, sd = 1)
    
   
     tibble::tibble(
      Model = model_name,
      Family = mod$family[1],
      Converged = mod$converged,                    # Should be TRUE 
      Iterations = mod$no.cyc,                      # Number of cycles
      GD = deviance(mod),                           # Global Deviance
      AIC = AIC(mod),                               # AIC
      SBC_BIC = AIC(mod, k = log(mod$N)),           # BIC
      Residuals_Mean = mean(res, na.rm = TRUE),     # Close to 0
      Residuals_SD = sd(res, na.rm = TRUE),         # Close to 1
      KS_p_value = ks_test$p.value                  # p > 0.05 
    )
  })

cat("\n=== GAMLSS Models Quality Check Summary ===\n")
print(quality_summary)

write.csv(quality_summary,"Supplementary/BCT_2012_QC.csv")


plot_dir <- "Supplementary/"

for (model_name in names(list_of_models)) {
  file_path <- paste0(plot_dir, "Diagnostics_", model_name, ".png")
  
  png(filename = file_path, width = 800, height = 800, res = 120)
  
  plot(list_of_models[[model_name]])
  
  # Закрываем устройство
  dev.off()
}


#=======================================================
# Centiles calculation
#=======================================================

#--------------centiles 2025 (based on model 2012) ---------------------------
age_grid <- seq(5, 12, by = 0.1)
predict_data <- tibble::tibble(age_year = age_grid)

predict_gamlss_centiles <- function(model, age_vector) {
  
  new_data <- tibble::tibble(age_year = age_vector)
  
  pred_mu    <- predict(model, what = "mu",    newdata = new_data, type = "response")
  pred_sigma <- predict(model, what = "sigma", newdata = new_data, type = "response")
  pred_nu    <- predict(model, what = "nu",    newdata = new_data, type = "response")
  pred_tau   <- predict(model, what = "tau",   newdata = new_data, type = "response")
  
  tibble::tibble(
    age_year = age_vector,
    p15  = gamlss.dist::qBCT(0.15, mu = pred_mu, sigma = pred_sigma, nu = pred_nu, tau = pred_tau),
    p50 = gamlss.dist::qBCT(0.50, mu = pred_mu, sigma = pred_sigma, nu = pred_nu, tau = pred_tau),
    p85 = gamlss.dist::qBCT(0.85, mu = pred_mu, sigma = pred_sigma, nu = pred_nu, tau = pred_tau),
    p90 = gamlss.dist::qBCT(0.90, mu = pred_mu, sigma = pred_sigma, nu = pred_nu, tau = pred_tau),
    p95 = gamlss.dist::qBCT(0.95, mu = pred_mu, sigma = pred_sigma, nu = pred_nu, tau = pred_tau),
    p97 = gamlss.dist::qBCT(0.97, mu = pred_mu, sigma = pred_sigma, nu = pred_nu, tau = pred_tau),
    p99 = gamlss.dist::qBCT(0.99, mu = pred_mu, sigma = pred_sigma, nu = pred_nu, tau = pred_tau)
  )
}

data_2025_centiles_based_on_2012 <- dplyr::bind_rows(
  
  predict_gamlss_centiles(list_of_models$bct_boys_bmi_based_on_2012, age_grid) %>%
    dplyr::mutate(metric = "BMI", sex = 1),
  
  predict_gamlss_centiles(list_of_models$bct_girls_bmi_based_on_2012, age_grid) %>%
    dplyr::mutate(metric = "BMI", sex = 2),
  
  predict_gamlss_centiles(list_of_models$bct_boys_height_based_on_2012, age_grid) %>%
    dplyr::mutate(metric = "Height", sex = 1),
  
  predict_gamlss_centiles(list_of_models$bct_girls_height_based_on_2012, age_grid) %>%
    dplyr::mutate(metric = "Height", sex = 2)
) %>%
  
  dplyr::mutate(Source = "Centiles 2025 based on 2012 model specification") %>%
  dplyr::select(metric, sex, age_year, p15, p50, p85, p90, p95, p97, p99, Source) %>%
  dplyr::arrange(metric, sex, age_year)


#==============================================================================
#-----------------------------Relative differences in centiles-----------------
#==============================================================================

data_2025_centiles_based_on_2012_new <- data_2025_centiles_based_on_2012 %>%
  
  mutate(age_year = floor(age_year+0.5)) %>%
  
  pivot_longer(cols = starts_with("p"), names_to = "centile", values_to = "value") %>% 
  
  group_by(metric, sex, age_year, centile) %>%
  
  summarise(value = mean(value, na.rm = TRUE),.groups = "drop" )%>%
  
  mutate( name = "Based on 2012 model")

centiles_2025_new <- centiles_2025 %>%
  
  mutate(age_year = floor(age_year+0.5)) %>%
  
  pivot_longer(cols = starts_with("p"), names_to = "centile", values_to = "value") %>% 
  
  group_by(metric, sex, age_year, centile) %>%
  
  summarise(value = mean(value, na.rm = TRUE),.groups = "drop" )%>%
  
  mutate( name = "Model 2025")

empirical_centiles_new <- empirical_2025 %>%
  
  mutate(age_year = floor(age_year+0.5)) %>%
  
  pivot_longer(cols = matches("^(BMI|Height)_p"), names_to = c("metric", "centile"),
               names_pattern = "^(BMI|Height)_(p\\d+)$", values_to = "value") %>% 
  
  group_by(metric, sex, age_year, centile) %>%
  
  summarise(value = mean(value, na.rm = TRUE),.groups = "drop" ) %>%
  
  mutate( name = "Emperical centiles 2025")

centiles_2012_new <- centiles_2012 %>%
  
  pivot_longer(cols = starts_with("p"), names_to = "centile", values_to = "value")%>%
  
  mutate( name = "Centiles 2012")

upper_centiles <- c("p85", "p95", "p97")

centile_relative_diff <- centiles_2012_new %>%
  filter(centile %in% upper_centiles) %>%
  select(metric, sex, age_year, centile, value_2012 = value) %>%
  
  left_join(
    empirical_centiles_new %>%
      filter(centile %in% upper_centiles) %>%
      select(metric, sex, age_year, centile, value_empirical = value),
    by = c("metric", "sex", "age_year", "centile")
  ) %>%
  
  left_join(
    centiles_2025_new %>%
      filter(centile %in% upper_centiles) %>%
      select(metric, sex, age_year, centile, value_2025 = value),
    by = c("metric", "sex", "age_year", "centile")
  ) %>%
  
  left_join(
    data_2025_centiles_based_on_2012_new %>%
      filter(centile %in% upper_centiles) %>%
      select(metric, sex, age_year, centile,
             value_2025_based_on_2012 = value),
    by = c("metric", "sex", "age_year", "centile")
  ) %>%
  
  mutate(
    rel_diff_empirical_vs_2012 =
      100 * (value_empirical - value_2012) / value_2012,
    
    rel_diff_2025_vs_2012 =
      100 * (value_2025 - value_2012) / value_2012,
    
    rel_diff_based_on_2012_vs_2012 =
      100 * (value_2025_based_on_2012 - value_2012) /
      value_2012
  ) %>%
  
  mutate(sex = ifelse(sex == 1, "Boys", "Girls"), metric_label = ifelse(metric == "BMI", "BMI (kg/m2)", "Height (cm)"))

SF4 <- ggplot(centile_relative_diff)+
  geom_line(aes(x = age_year, y = rel_diff_empirical_vs_2012, color = "Empirical - 2012"), size=1, alpha = 0.7)+
  geom_line(aes(x = age_year, y = rel_diff_2025_vs_2012 , color = "2025 GAMLSS − 2012"), size=1, alpha = 0.7)+
  geom_line(aes(x = age_year, y = rel_diff_based_on_2012_vs_2012, color = "2025 based on 2012 model − 2012"), size=1, alpha = 0.7)+
  facet_grid(sex~metric_label~centile, scales = "free", switch = "y")+
  labs(x = "Age (years)", y = "Relative difference (%)", color= NULL)+
  
  scale_color_manual(values = c("Empirical − 2012" = "#B66A5C",
                                "2025 GAMLSS − 2012" = "#3B5B7A",
                                "2025 based on 2012 model − 2012" = "#6A8E5C"))+
  
  scale_x_continuous(breaks = 5:12)+
  
  theme(strip.placement = "outside", legend.position = "bottom")+
  geom_hline(yintercept = 0, linetype = "dashed")

output_directory <- "Supplementary/"
ggsave(
  filename = paste0(output_directory,"SF4.tiff"), 
  plot     = SF4, 
  width    = 8, 
  height   = 6, 
  dpi      = 400)

#==========================================
# Get parameters
#==========================================

age_grid <- tibble::tibble(age_year = 5:12)

get_2025_parameters <- function(model, metric_name, sex_id) {
  pred_mu    <- predict(model, what = "mu",    newdata = age_grid, type = "response")
  pred_sigma <- predict(model, what = "sigma", newdata = age_grid, type = "response")
  
  tibble::tibble(
    age_year   = age_grid$age_year,
    metric     = metric_name,
    sex        = sex_id,
    mu_2025    = pred_mu,
    sigma_2025 = pred_sigma
  )
}

params_2025 <- dplyr::bind_rows(
  get_2025_parameters(bct_boys_bmi_based_on_2012, "BMI", 1),
  get_2025_parameters(bct_girls_bmi_based_on_2012, "BMI", 2),
  get_2025_parameters(bct_boys_height_based_on_2012, "Height", 1),
  get_2025_parameters(bct_girls_height_based_on_2012, "Height", 2)
)

secular_trends_summary <- regional_reference %>%
  dplyr::select(age_year, sex, metric, mu_2012 = M, sigma_2012 = S) %>%
  
  dplyr::left_join(params_2025, by = c("age_year", "sex", "metric")) %>%
  
  dplyr::mutate(
    mu_delta    = mu_2025 - mu_2012,
    sigma_delta = sigma_2025 - sigma_2012,
    sex_label   = ifelse(sex == 1, "Boys", "Girls")
  ) %>%
  
    dplyr::select(
    metric, sex_label, age_year, 
    mu_2012, mu_2025, mu_delta, 
    sigma_2012, sigma_2025, sigma_delta
  ) %>%
  dplyr::arrange(metric, sex_label, age_year)


print(secular_trends_summary, n = Inf)



