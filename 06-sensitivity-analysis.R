#==============================================================================
#------------------------Sensitivity analysis
#==============================================================================
# Leave one out (14 - 1 region)
#GAMLSS model for 14 samples (14 - 1 region)
# calculation of CV of parameters.

#==============================================================================
# Fisrt, all parameters:

selected_families <- c(
  data_boys_bmi     = "BCPE",
  data_girls_bmi    = "BCCG",
  data_boys_height  = "BCT",
  data_girls_height = "BCT"
)

selected_lambda <- c(
  data_boys_bmi     = 0.50,
  data_girls_bmi    = 1.00,
  data_boys_height  = 2.00,
  data_girls_height = 2.00
)

selected_mu_df <- c(
  data_boys_bmi     = 4,
  data_girls_bmi    = 3,
  data_boys_height  = 5,
  data_girls_height = 5
)

selected_sigma_df <- c(
  data_boys_bmi     = 5,
  data_girls_bmi    = 2,
  data_boys_height  = 6,
  data_girls_height = 6
)

selected_nu_df <- c(
  data_boys_bmi     = 2,
  data_girls_bmi    = 1,
  data_boys_height  = NA,
  data_girls_height = NA
)

selected_tau_df <- c(
  data_boys_bmi     = 2,
  data_girls_bmi    = NA,
  data_boys_height  = 4,
  data_girls_height = 3
)

# list of data
list_of_datasets <- list(
  data_boys_bmi  = my_data_step3 %>% dplyr::filter(sex == 1, !is.na(BMI), !is.na(age_year)) %>% dplyr::select(parameter = BMI, age_year, region),
  data_girls_bmi = my_data_step3 %>% dplyr::filter(sex == 2, !is.na(BMI), !is.na(age_year)) %>% dplyr::select(parameter = BMI, age_year, region),
  data_boys_height  = my_data_step3 %>% dplyr::filter(sex == 1, !is.na(height), !is.na(age_year)) %>% dplyr::select(parameter = height, age_year, region),
  data_girls_height = my_data_step3 %>% dplyr::filter(sex == 2, !is.na(height), !is.na(age_year)) %>% dplyr::select(parameter = height, age_year, region)
)


age_grid <- tibble::tibble(
  age_year = seq(5, 12, by = 1)
)

# GAMLSS leave one out
loo_models <- list()

for (dataset_name in names(list_of_datasets)) {
  
  cat("\n========================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("========================================\n")
  
  data_input <- list_of_datasets[[dataset_name]]
  
  regions <- unique(data_input$region)
  
  loo_models[[dataset_name]] <- list()
  
  for (excluded_region in regions) {
    
    cat("Excluding region:", excluded_region, "\n")
    
    loo_models[[dataset_name]][[as.character(excluded_region)]] <-
      fit_loo_gamlss(
        data_input      = data_input,
        excluded_region = excluded_region,
        family          = selected_families[[dataset_name]],
        lambda          = selected_lambda[[dataset_name]],
        mu_df           = selected_mu_df[[dataset_name]],
        sigma_df        = selected_sigma_df[[dataset_name]],
        nu_df           = selected_nu_df[[dataset_name]],
        tau_df          = selected_tau_df[[dataset_name]]
      )
  }
}

#results

loo_parameter_results <- purrr::map_dfr(
  
  names(loo_models),
  
  function(dataset_name) {
    
    models_for_dataset <- loo_models[[dataset_name]]
    data_input <- list_of_datasets[[dataset_name]]
    
    lambda <- selected_lambda[[dataset_name]]
    family <- selected_families[[dataset_name]]
    
    purrr::map_dfr(
      
      names(models_for_dataset),
      
      function(excluded_region) {
        
        model <- models_for_dataset[[excluded_region]]
        
        # Data used for fitting this LOO model
        train_data <- data_input %>%
          dplyr::filter(region != excluded_region) %>%
          dplyr::mutate(
            age_lambda = age_year^lambda
          )
        
        # Prediction data
        new_data <- data.frame(
          age_year = age_grid,
          age_lambda = age_grid^lambda
        )
        
        # Predict distributional parameters
        pred <- predictAll(
          model,
          newdata = new_data,
          data = train_data
        )
        
        # Basic parameters
        result <- tibble::tibble(
          dataset = dataset_name,
          excluded_region = excluded_region,
          converged = model$converged,
          age_year = age_grid,
          mu = as.numeric(pred$mu),
          sigma = as.numeric(pred$sigma),
          nu = as.numeric(pred$nu)
        )
        
        # Add tau only for families that have tau
        if (family %in% c("BCPE", "BCT")) {
          
          result$tau <- as.numeric(pred$tau)
          
        } else {
          
          result$tau <- NA_real_
        }
        
        result
      }
    )
  }
)

print(
  dplyr::glimpse(loo_parameter_results)
)

print(
  loo_parameter_results %>%
    dplyr::count(dataset, excluded_region, converged)
)

# CV of parameters

cv_params_table <- loo_parameter_results %>%
  
  tidyr::pivot_longer(
    cols = c(mu, sigma, nu, tau),
    names_to = "gamlss_parameter",
    values_to = "param_value"
  ) %>%
  
  dplyr::filter(
    !is.na(param_value)
  ) %>%
  
  dplyr::group_by(
    dataset,
    age_year,
    gamlss_parameter
  ) %>%
  
  dplyr::summarise(
    Mean_Param = mean(param_value),
    SD_Param   = sd(param_value),
    CV_percent = 100 * SD_Param / abs(Mean_Param),
    .groups = "drop"
  )

print(cv_params_table$CV_percent>10)

write.csv(cv_params_table,"d:/VD/2026/VD/article_8_secular_trends/Supplementary/LOO_CV_GAMLSS_parameters.csv",row.names = FALSE)

# Output files: LOO_CV_GAMLSS_parameters.csv