#==============================================================================
#-----------------------model optimization------------------------------------
#=============================================================================

#First, formula should be start
# 1. Family
# 2. Lambda optimization
# 3. mu optimization
# 4. sigma optimization
# 5. nu optimization
# 6. tau optimization

#=============================================================================
#----------------First step is family distrub----------------------------
#=============================================================================
gc()
list_of_datasets <- list(
  data_boys_bmi  = my_data_step3 %>% dplyr::filter(sex == 1, !is.na(BMI), !is.na(age_year)) %>% dplyr::select(parameter = BMI, age_year),
  data_girls_bmi = my_data_step3 %>% dplyr::filter(sex == 2, !is.na(BMI), !is.na(age_year)) %>% dplyr::select(parameter = BMI, age_year),
  data_boys_height  = my_data_step3 %>% dplyr::filter(sex == 1, !is.na(height), !is.na(age_year)) %>% dplyr::select(parameter = height, age_year),
  data_girls_height = my_data_step3 %>% dplyr::filter(sex == 2, !is.na(height), !is.na(age_year)) %>% dplyr::select(parameter = height, age_year)
)

#----------------First step is optimization lambda----------------------------
boys_bmi_family <- fit_families(list_of_datasets$data_boys_bmi,"boys_bmi")
girls_bmi_family <- fit_families(list_of_datasets$data_girls_bmi,"girls_bmi")
boys_height_family <- fit_families(list_of_datasets$data_boys_height,"boys_height")
girls_height_family <- fit_families(list_of_datasets$data_girls_height,"girls_height")

final_aic_table <- dplyr::bind_rows(boys_bmi_family$results, girls_bmi_family$results,boys_height_family$results,girls_height_family$results)
write.csv(final_aic_table,"Supplementary/FAMILY.csv")

#WARMPLOT FOR ALL FAMLILIES
models_list <- list(
  "BCCG BMI boys" = boys_bmi_family$models$BCCG,"BCPE BMI boys" = boys_bmi_family$models$BCPE,"BCT BMI boys"  = boys_bmi_family$models$BCT,
  "BCCG BMI girls" = girls_bmi_family$models$BCCG,"BCPE BMI girls" = girls_bmi_family$models$BCPE,"BCT BMI girls"  = girls_bmi_family$models$BCT,
  "BCCG Height boys" = boys_height_family$models$BCCG,"BCPE Height boys" = boys_height_family$models$BCPE,"BCT Height boys"  = boys_height_family$models$BCT,
  "BCCG Height girls" = girls_height_family$models$BCCG,"BCPE Height girls" = girls_height_family$models$BCPE,"BCT Height girls"  = girls_height_family$models$BCT)

sapply(models_list, is.null)
par(mfrow = c(3, 4))
for (model_name in names(models_list)) {
  
  current_model <- models_list[[model_name]]
  
  if (!is.null(current_model)) {
    gamlss::wp(current_model)
    title(main = model_name)
  }
}

# После окончания работы цикла рекомендуется сбросить настройки окна к стандарту (1х1)
par(mfrow = c(1, 1))

#=============================================================================
#----------------Second step is optimization lambda----------------------------
#=============================================================================

selected_families <- c(
  data_boys_bmi       = "BCPE",
  data_girls_bmi      = "BCCG",
  data_boys_height    = "BCT",
  data_girls_height   = "BCT"
)
# choose range of lambda
lambda_grid <- seq(0.2, 3, by = 0.1)

all_lambda_results <- lapply(
  names(list_of_datasets),
  function(name) {
    
    find_best_lambda(
      data_input = list_of_datasets[[name]],
      dataset_name = name,
      family = selected_families[name],
      mu_df = 4,
      sigma_df = 3
    )
  }
)

final_lambda_table <- do.call(
  rbind,
  all_lambda_results
)

print(final_lambda_table)
output_directory <- "Supplementary/"
write.csv(final_lambda_table, file = paste0(output_directory, "lambda_optimization.csv"))
gc()

Lambda_optimization_figure <- ggplot(final_lambda_table, aes(lambda, AIC))+
  geom_point()+
  geom_line()+
  facet_wrap(~Dataset,scales = "free")+
  scale_x_continuous(breaks = seq(0.3,3,by = 0.2))

ggsave(
  filename = paste0(output_directory,  "Lambda_optimization_figure.TIFF"), 
  plot = Lambda_optimization_figure, 
  width = 10, 
  height = 8, 
  dpi = 400
)
#=============================================================================
#----------------Second step is optimization mu----------------------------
#=============================================================================
selected_lambda <- c(
  data_boys_bmi    = 0.50,  
  data_girls_bmi   = 1.00,  
  data_boys_height = 2.00,  
  data_girls_height = 2.00  
)

mu_results <- lapply(names(list_of_datasets), function(dataset_name) {
  
  find_best_mu_df(
    data_input = list_of_datasets[[dataset_name]],
    dataset_name = dataset_name,
    family = selected_families[[dataset_name]],
    lambda = selected_lambda[[dataset_name]]
  )
})

names(mu_results) <- names(list_of_datasets)
mu_AIC <- dplyr::bind_rows(mu_results) %>%
  dplyr::select(Dataset, mu_df, AIC)

output_directory <- "Supplementary/"
write.csv(mu_AIC, file = paste0(output_directory, "mu_optimization.csv"))
gc()

mu_optimization_figure <- ggplot(mu_AIC, aes(mu_df, AIC))+
  geom_point()+
  geom_line()+
  facet_wrap(~Dataset,scales = "free")+
  scale_x_continuous(breaks = seq(1,10,by = 1))

ggsave(
  filename = paste0(output_directory,  "mu_optimization_figure.TIFF"), 
  plot = mu_optimization_figure, 
  width = 10, 
  height = 8, 
  dpi = 400
)


selected_mu_df <- c(
  data_boys_bmi     = 4,
  data_girls_bmi    = 3,
  data_boys_height  = 5,
  data_girls_height = 5
)

sigma_results <- lapply(names(list_of_datasets), function(dataset_name) {
  
  find_best_sigma_df(
    data_input = list_of_datasets[[dataset_name]],
    dataset_name = dataset_name,
    family = selected_families[[dataset_name]],
    lambda = selected_lambda[[dataset_name]],
    mu_df = selected_mu_df[[dataset_name]]
  )
})

names(sigma_results) <- names(list_of_datasets)
sigma_AIC <- dplyr::bind_rows(sigma_results) %>%
  dplyr::select(Dataset, sigma_df, AIC)
output_directory <- "Supplementary/"
write.csv(sigma_AIC, file = paste0(output_directory, "sigma_optimization.csv"))

sigma_optimization_figure <- ggplot(sigma_AIC, aes(sigma_df, AIC))+
  geom_point()+
  geom_line()+
  facet_wrap(~Dataset,scales = "free")+
  scale_x_continuous(breaks = seq(1,10,by = 1))

ggsave(
  filename = paste0(output_directory,  "sigma_optimization_figure.TIFF"), 
  plot = sigma_optimization_figure, 
  width = 10, 
  height = 8, 
  dpi = 400
)

#============================================================================
#-------------------nu optimization------------------------------------------
#===========================================================================

#choose only BMI datasets, for height nu ~ 1

bmi_datasets <- c(
  "data_boys_bmi",
  "data_girls_bmi"
)

selected_sigma_df <- c(
  data_boys_bmi     = 5,
  data_girls_bmi    = 2,
  data_boys_height  = 6,
  data_girls_height = 6
)

nu_results <- lapply(bmi_datasets, function(dataset_name) {
  
  find_best_nu_df(
    data_input = list_of_datasets[[dataset_name]],
    dataset_name = dataset_name,
    family = selected_families[[dataset_name]],
    lambda = selected_lambda[[dataset_name]],
    mu_df = selected_mu_df[[dataset_name]],
    sigma_df = selected_sigma_df[[dataset_name]]
  )
})

names(nu_results) <- bmi_datasets
nu_AIC <- dplyr::bind_rows(nu_results) %>%
  dplyr::select(Dataset, nu_df, AIC)
output_directory <- "Supplementary/"
write.csv(nu_AIC, file = paste0(output_directory, "nu_optimization.csv"))

#============================================================================
#-------------------tau optimization------------------------------------------
#===========================================================================
tau_datasets <- c(
  "data_boys_bmi",
  "data_boys_height",
  "data_girls_height"
)

selected_nu_df <- c(
  data_boys_bmi    = 2,
  data_girls_bmi   = 1,
  data_boys_height = NA,
  data_girls_height = NA
)

tau_results <- lapply(tau_datasets, function(dataset_name) {
  
  find_best_tau_df(
    data_input = list_of_datasets[[dataset_name]],
    dataset_name = dataset_name,
    family = selected_families[[dataset_name]],
    lambda = selected_lambda[[dataset_name]],
    mu_df = selected_mu_df[[dataset_name]],
    sigma_df = selected_sigma_df[[dataset_name]]
  )
})

names(tau_results) <- tau_datasets
tau_AIC <- dplyr::bind_rows(tau_results) %>%
  dplyr::select(Dataset, tau_df, AIC)
output_directory <- "Supplementary/"
write.csv(tau_AIC, file = paste0(output_directory, "tau_optimization.csv"))

tau_optimization_figure <- ggplot(tau_AIC, aes(tau_df, AIC))+
  geom_point()+
  geom_line()+
  facet_wrap(~Dataset,scales = "free")+
  scale_x_continuous(breaks = seq(1,10,by = 1))

ggsave(
  filename = paste0(output_directory,  "tau_optimization_figure.TIFF"), 
  plot = tau_optimization_figure, 
  width = 10, 
  height = 8, 
  dpi = 400
)

selected_tau_df <- c(
  data_boys_bmi     = 2,
  data_girls_bmi    = NA,
  data_boys_height  = 4,
  data_girls_height = 3
)

final_models <- purrr::imap(
  list_of_datasets,
  function(data_input, dataset_name) {
    
    fit_final_gamlss(
      data_input = data_input,
      dataset_name = dataset_name,
      family = selected_families[[dataset_name]],
      lambda = selected_lambda[[dataset_name]],
      mu_df = selected_mu_df[[dataset_name]],
      sigma_df = selected_sigma_df[[dataset_name]],
      nu_df = selected_nu_df[[dataset_name]],
      tau_df = selected_tau_df[[dataset_name]]
    )
  }
)

names(final_models) <- names(list_of_datasets)
final_model_summary <- purrr::imap_dfr(
  final_models,
  ~ summarize_gamlss_model(.x, .y)
)

write.csv(
  final_model_summary,
  file = file.path(output_directory, "final_model_summary.csv"),
  row.names = FALSE
)

models_2025 <- list(
  data_boys_bmi     = final_mod_boys_bmi,
  data_girls_bmi    = final_mod_girls_bmi,
  data_boys_height  = final_mod_boys_height,
  data_girls_height = final_mod_girls_height
)

target_centiles <- c(15, 50, 85, 90, 95, 97, 99)

centiles_2025 <- purrr::imap_dfr(
  models_2025,
  function(model, dataset_name) {
    
    predict_centiles(
      model = model,
      age_vector = age_grid,
      original_data = list_of_datasets[[dataset_name]],
      lambda = selected_lambda[[dataset_name]],
      centiles = target_centiles
    ) %>%
      dplyr::mutate(
        metric = ifelse(grepl("bmi", dataset_name), "BMI", "Height"),
        sex = ifelse(grepl("boys", dataset_name), 1, 2),
        Source = "2025 GAMLSS"
      )
  }
)

data_long_centiles_2025 <- centiles_2025 %>%
  tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "percentile",
    values_to = "value"
  )%>%
  mutate(sex = ifelse(sex == 1, "Boys", "Girls"),
         metric_label = ifelse(metric == "BMI", "BMI (kg/m2)", "Height (cm)"))

cat("Okay, dataframe with centiles of new model is ready.\n")

centile_colors <- c(
  "p3"  = "#BFD7EA",
  "p50" = "#4F81BD",
  "p85" = "#F2C14E",
  "p90" = "#E89C3C",
  "p95" = "#D96C5F",
  "p97" = "#B83B5E",
  "p99" = "#7A1F3D"
)
centiles_2025_model_new <- ggplot(data_long_centiles_2025, aes(x = age_year, y = value, color = percentile)) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.8) +
  labs(
    x = "Age (years)",
    y = NULL,
    color = NULL
  ) +
  facet_grid(metric_label ~ sex, scales = "free_y", switch = "y")+
  
  scale_color_manual(values = centile_colors)+
  
  theme(panel.grid = element_blank(),strip.placement = "outside")

ggsave(
  filename = paste0(output_directory,  "centiles_2025_model_new.TIFF"), 
  plot = centiles_2025_model_new, 
  width = 10, 
  height = 8, 
  dpi = 400
)


empirical_difference <- empirical_2025 %>%
  dplyr::mutate(
    age_year = as.numeric(age_group)
  ) %>%
  dplyr::left_join(
    data_long_centiles_2025 %>%
      dplyr::select(
        age_year, sex, metric, percentile, value
      ),
    by = c(
      "age_year",
      "sex",
      "Variable" = "metric",
      "percentile"
    )
  ) %>%
  dplyr::mutate(
    difference = value_2025 - value,
    relative_difference =
      (value_2025 - value) / value * 100
  )

print(empirical_difference, n = 250)


# Output files: FAMILY.csv, lambda_optimization.csv, Lambda_optimization_figure.TIFF,  mu_optimization.csv, mu_optimization_figure.TIFF,
# sigma_optimization.csv, sigma_optimization_figure.TIFF, nu_optimization.csv, tau_optimization.csv, tau_optimization_figure.TIFF, final_model_summary.csv, centiles_2025_model_new.TIFF