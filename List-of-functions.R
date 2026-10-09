#=============================================================================
#----------------Family distribution----------------------------
#=============================================================================
fit_families <- function(data_input, dataset_name = "Dataset") {
  
  families <- c("BCCG", "BCPE", "BCT")
  
  models <- list()
  results <- data.frame(
    Dataset = dataset_name,
    Family = families,
    AIC = NA_real_
  )
  
  for (i in seq_along(families)) {
    
    mod <- try(
      gamlss(
        parameter ~ pb(age_in_months),
        sigma.fo = ~ pb(age_in_months),
        family = families[i],
        data = data_input,
        trace = FALSE
      ),
      silent = TRUE
    )
    
    if (!inherits(mod, "try-error")) {
      models[[families[i]]] <- mod
      results$AIC[i] <- AIC(mod)
    }
  }
  
  results <- results %>%
    mutate(
      delta_AIC = AIC - min(AIC, na.rm = TRUE)
    ) %>%
    arrange(AIC)
  
  list(
    results = results,
    models = models
  )
}

#========================================================================
#---------------------------Find optimal lambda--------------------------
#========================================================================

find_best_lambda <- function(data_input, 
                             dataset_name, 
                             family, 
                             mu_df = 5, 
                             sigma_df = 3) { 
  
  cat("\n========================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Family:", family, "\n")
  cat("Mu df:", mu_df, "\n")
  cat("Sigma df:", sigma_df, "\n")
  cat("Testing lambda:", paste(lambda_grid, collapse = ", "), "\n")
  cat("========================================\n")
  
  results <- data.frame( 
    Dataset = dataset_name, 
    Family = family, 
    lambda = lambda_grid, 
    AIC = NA_real_, 
    GD = NA_real_ 
  ) 
  
  for (i in seq_along(lambda_grid)) { 
    
    lambda <- lambda_grid[i] 
    
    cat(
      "  -> Fitting lambda =", lambda,
      "(", i, "of", length(lambda_grid), ")\n"
    )
    
    data_input$age_lambda <- data_input$age_year^lambda 
    
    fit <- try( 
      gamlss( 
        parameter ~ pb(age_lambda, df = mu_df), 
        sigma.fo = ~ pb(age_lambda, df = sigma_df), 
        family = family, 
        data = data_input, 
        control = gamlss.control(trace = FALSE) 
      ), 
      silent = TRUE 
    ) 
    
    if (!inherits(fit, "try-error")) { 
      
      results$AIC[i] <- AIC(fit) 
      results$GD[i] <- deviance(fit) 
      
      cat(
        "     AIC =", round(results$AIC[i], 2),
        "| GD =", round(results$GD[i], 2), "\n"
      )
      
    } else {
      
      cat("     FAILED\n")
    }
  } 
  
  results <- results[order(results$AIC), ] 
  
  cat(
    "\nBest lambda:", results$lambda[1],
    "| AIC:", round(results$AIC[1], 2),
    "| GD:", round(results$GD[1], 2), "\n"
  )
  
  return(results) 
}

#========================================================================
#---------------------------Find optimal mu--------------------------
#========================================================================

find_best_mu_df <- function(data_input,
                            dataset_name,
                            family,
                            lambda,
                            sigma_df = 3,
                            mu_df_grid = 1:10) {
  
  cat("\n========================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Family:", family, "\n")
  cat("Lambda:", lambda, "\n")
  cat("Sigma df:", sigma_df, "\n")
  cat("Testing mu df:", paste(mu_df_grid, collapse = ", "), "\n")
  cat("========================================\n")
  
  # Age transformation is fixed for all mu_df models
  data_input$age_lambda <- data_input$age_year^lambda
  
  results <- data.frame(
    Dataset = dataset_name,
    Family = family,
    lambda = lambda,
    mu_df = mu_df_grid,
    AIC = NA_real_,
    G_dev = NA_real_
  )
  
  for (i in seq_along(mu_df_grid)) {
    
    current_mu_df <- mu_df_grid[i]
    
    cat(
      "  -> Fitting mu df =", current_mu_df,
      "(", i, "of", length(mu_df_grid), ")\n"
    )
    
    fit <- try(
      if (family == "BCCG") {
        
        gamlss(
          formula = parameter ~ pb(age_lambda, df = current_mu_df),
          sigma.fo = ~ age_lambda,
          nu.fo = ~ 1,
          family = family,
          data = data_input,
          control = gamlss.control(trace = FALSE)
        )
        
      } else {
        
        gamlss(
          formula = parameter ~ pb(age_lambda, df = current_mu_df),
          sigma.fo = ~ age_lambda,
          nu.fo = ~ 1,
          tau.fo = ~ 1,
          family = family,
          data = data_input,
          control = gamlss.control(trace = FALSE)
        )
      },
      silent = TRUE
    )
    
    if (!inherits(fit, "try-error")) {
      
      results$AIC[i] <- AIC(fit)
      results$G_dev[i] <- deviance(fit)
      
      cat(
        "     AIC =", round(results$AIC[i], 2),
        "| G.dev =", round(results$G_dev[i], 2), "\n"
      )
      
    } else {
      
      cat("     FAILED\n")
    }
  }
  
  results <- results %>%
    mutate(
      delta_AIC = AIC - min(AIC, na.rm = TRUE)
    ) %>%
    arrange(AIC)
  
  cat("\nBest mu df:", results$mu_df[1],
      "| AIC:", round(results$AIC[1], 2), "\n")
  
  return(results)
}

#========================================================================
#---------------------------Find optimal sigma--------------------------
#========================================================================

find_best_sigma_df <- function(data_input,
                               dataset_name,
                               family,
                               lambda,
                               mu_df,
                               sigma_df_grid = 1:10) {
  
  cat("\n========================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Family:", family, "\n")
  cat("Lambda:", lambda, "\n")
  cat("Mu df:", mu_df, "\n")
  cat("Testing sigma df:", paste(sigma_df_grid, collapse = ", "), "\n")
  cat("========================================\n")
  
  # Age transformation is fixed for all sigma_df models
  data_input$age_lambda <- data_input$age_year^lambda
  
  results <- data.frame(
    Dataset = dataset_name,
    Family = family,
    lambda = lambda,
    mu_df = mu_df,
    sigma_df = sigma_df_grid,
    AIC = NA_real_,
    GD = NA_real_
  )
  
  for (i in seq_along(sigma_df_grid)) {
    
    current_sigma_df <- sigma_df_grid[i]
    
    cat(
      "  -> Fitting sigma df =", current_sigma_df,
      "(", i, "of", length(sigma_df_grid), ")\n"
    )
    
    fit <- try(
      if (family == "BCCG") {
        
        gamlss(
          formula = parameter ~ pb(age_lambda, df = mu_df),
          sigma.fo = ~ pb(age_lambda, df = current_sigma_df),
          nu.fo = ~ 1,
          family = family,
          data = data_input,
          control = gamlss.control(trace = FALSE)
        )
        
      } else {
        
        gamlss(
          formula = parameter ~ pb(age_lambda, df = mu_df),
          sigma.fo = ~ pb(age_lambda, df = current_sigma_df),
          nu.fo = ~ 1,
          tau.fo = ~ 1,
          family = family,
          data = data_input,
          control = gamlss.control(trace = FALSE)
        )
      },
      silent = TRUE
    )
    
    if (!inherits(fit, "try-error")) {
      
      results$AIC[i] <- AIC(fit)
      results$GD[i] <- deviance(fit)
      
      cat(
        "     AIC =", round(results$AIC[i], 2),
        "| GD =", round(results$GD[i], 2), "\n"
      )
      
    } else {
      
      cat("     FAILED\n")
    }
  }
  
  results <- results %>%
    mutate(
      delta_AIC = AIC - min(AIC, na.rm = TRUE)
    ) %>%
    arrange(AIC)
  
  cat(
    "\nBest sigma df:", results$sigma_df[1],
    "| AIC:", round(results$AIC[1], 2),
    "| GD:", round(results$GD[1], 2), "\n"
  )
  
  return(results)
}

#========================================================================
#---------------------------Find optimal nu for BMI only ----------------
#========================================================================

find_best_nu_df <- function(data_input,
                            dataset_name,
                            family,
                            lambda,
                            mu_df,
                            sigma_df,
                            nu_df_grid = 1:3) {
  
  cat("\n========================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Family:", family, "\n")
  cat("Lambda:", lambda, "\n")
  cat("Mu df:", mu_df, "\n")
  cat("Sigma df:", sigma_df, "\n")
  cat("Testing nu df:", paste(nu_df_grid, collapse = ", "), "\n")
  cat("========================================\n")
  
  # Fixed age transformation
  data_input$age_lambda <- data_input$age_year^lambda
  
  results <- data.frame(
    Dataset = dataset_name,
    Family = family,
    lambda = lambda,
    mu_df = mu_df,
    sigma_df = sigma_df,
    nu_df = nu_df_grid,
    AIC = NA_real_,
    GD = NA_real_
  )
  
  for (i in seq_along(nu_df_grid)) {
    
    current_nu_df <- nu_df_grid[i]
    
    cat(
      "  -> Fitting nu df =", current_nu_df,
      "(", i, "of", length(nu_df_grid), ")\n"
    )
    
    fit <- try(
      if (family == "BCCG") {
        
        gamlss(
          formula = parameter ~ pb(age_lambda, df = mu_df),
          sigma.fo = ~ pb(age_lambda, df = sigma_df),
          nu.fo = ~ pb(age_lambda, df = current_nu_df),
          family = family,
          data = data_input,
          control = gamlss.control(trace = FALSE)
        )
        
      } else {
        
        gamlss(
          formula = parameter ~ pb(age_lambda, df = mu_df),
          sigma.fo = ~ pb(age_lambda, df = sigma_df),
          nu.fo = ~ pb(age_lambda, df = current_nu_df),
          tau.fo = ~ 1,
          family = family,
          data = data_input,
          control = gamlss.control(trace = FALSE)
        )
      },
      silent = TRUE
    )
    
    if (!inherits(fit, "try-error")) {
      
      results$AIC[i] <- AIC(fit)
      results$GD[i] <- deviance(fit)
      
      cat(
        "     AIC =", round(results$AIC[i], 2),
        "| GD =", round(results$GD[i], 2), "\n"
      )
      
    } else {
      
      cat("     FAILED\n")
    }
  }
  
  results <- results %>%
    mutate(
      delta_AIC = AIC - min(AIC, na.rm = TRUE)
    ) %>%
    arrange(AIC)
  
  cat(
    "\nBest nu df:", results$nu_df[1],
    "| AIC:", round(results$AIC[1], 2),
    "| GD:", round(results$GD[1], 2), "\n"
  )
  
  return(results)
}

#========================================================================
#---------------------------Find optimal tau--------------------------
#========================================================================

find_best_tau_df <- function(data_input,
                             dataset_name,
                             family,
                             lambda,
                             mu_df,
                             sigma_df,
                             nu_df = NA,
                             tau_df_grid = 1:5) {
  
  cat("\n========================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Family:", family, "\n")
  cat("Lambda:", lambda, "\n")
  cat("Mu df:", mu_df, "\n")
  cat("Sigma df:", sigma_df, "\n")
  cat("Nu df:", nu_df, "\n")
  cat("Testing tau df:", paste(tau_df_grid, collapse = ", "), "\n")
  cat("========================================\n")
  
  data_input$age_lambda <- data_input$age_year^lambda
  
  results <- data.frame(
    Dataset = dataset_name,
    Family = family,
    lambda = lambda,
    mu_df = mu_df,
    sigma_df = sigma_df,
    nu_df = nu_df,
    tau_df = tau_df_grid,
    AIC = NA_real_,
    GD = NA_real_
  )
  
  for (i in seq_along(tau_df_grid)) {
    
    current_tau_df <- tau_df_grid[i]
    
    cat("  -> Fitting tau df =", current_tau_df,
        "(", i, "of", length(tau_df_grid), ")\n")
    
    fit <- try(
      
      if (is.na(nu_df)) {
        
        gamlss(
          formula = parameter ~ pb(age_lambda, df = mu_df),
          sigma.fo = ~ pb(age_lambda, df = sigma_df),
          nu.fo = ~ 1,
          tau.fo = ~ pb(age_lambda, df = current_tau_df),
          family = family,
          data = data_input,
          control = gamlss.control(trace = FALSE)
        )
        
      } else {
        
        gamlss(
          formula = parameter ~ pb(age_lambda, df = mu_df),
          sigma.fo = ~ pb(age_lambda, df = sigma_df),
          nu.fo = ~ pb(age_lambda, df = nu_df),
          tau.fo = ~ pb(age_lambda, df = current_tau_df),
          family = family,
          data = data_input,
          control = gamlss.control(trace = FALSE)
        )
      },
      
      silent = TRUE
    )
    
    if (!inherits(fit, "try-error")) {
      
      results$AIC[i] <- AIC(fit)
      results$GD[i] <- deviance(fit)
      
      cat("     AIC =", round(results$AIC[i], 2),
          "| GD =", round(results$GD[i], 2), "\n")
      
    } else {
      cat("     FAILED\n")
    }
  }
  
  results <- results %>%
    mutate(delta_AIC = AIC - min(AIC, na.rm = TRUE)) %>%
    arrange(AIC)
  
  cat("\nBest tau df:", results$tau_df[1],
      "| AIC:", round(results$AIC[1], 2),
      "| GD:", round(results$GD[1], 2), "\n")
  
  return(results)
}


#===============================================================================
#-------------------Final model-------------------------------------------------
#===============================================================================

fit_final_gamlss <- function(data_input,
                             dataset_name,
                             family,
                             lambda,
                             mu_df,
                             sigma_df,
                             nu_df = NA,
                             tau_df = NA) {
  cat("\n========================================\n")
  cat("Final GAMLSS model\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Family:", family, "\n")
  cat("Lambda:", lambda, "\n")
  cat("Mu df:", mu_df, "\n")
  cat("Sigma df:", sigma_df, "\n")
  cat("Nu df:", nu_df, "\n")
  cat("Tau df:", tau_df, "\n")
  cat("========================================\n")
  
  data_input <- data_input %>%
    dplyr::mutate(
      age_lambda = age_year^lambda
    )
  
  # BMI: age-varying nu
  if (!is.na(nu_df)) {
    
    if (!is.na(tau_df)) {
      
      model <- gamlss(
        parameter ~ pb(age_lambda, df = mu_df),
        sigma.fo = ~ pb(age_lambda, df = sigma_df),
        nu.fo = ~ pb(age_lambda, df = nu_df),
        tau.fo = ~ pb(age_lambda, df = tau_df),
        family = family,
        data = data_input,
        control = gamlss.control(trace = FALSE)
      )
      
    } else {
      
      # BCCG: no tau
      model <- gamlss(
        parameter ~ pb(age_lambda, df = mu_df),
        sigma.fo = ~ pb(age_lambda, df = sigma_df),
        nu.fo = ~ pb(age_lambda, df = nu_df),
        family = family,
        data = data_input,
        control = gamlss.control(trace = FALSE)
      )
    }
    
  } else {
    
    # Height: constant nu
    model <- gamlss(
      parameter ~ pb(age_lambda, df = mu_df),
      sigma.fo = ~ pb(age_lambda, df = sigma_df),
      nu.fo = ~ 1,
      tau.fo = ~ pb(age_lambda, df = tau_df),
      family = family,
      data = data_input,
      control = gamlss.control(trace = FALSE)
    )
  }
  
  cat("AIC:", round(AIC(model), 2), "\n")
  cat("Global deviance:", round(deviance(model), 2), "\n")
  cat("========================================\n")
  
  return(model)
}


summarize_gamlss_model <- function(model, dataset_name = NULL) {
  
  # Normalized residuals
  res <- resid(model)
  res <- res[is.finite(res)]
  
  # Kolmogorov-Smirnov test
  ks_test <- ks.test(
    res,
    "pnorm",
    mean = 0,
    sd = 1
  )
  
  data.frame(
    Dataset = dataset_name,
    Family = model$family[1],
    lambda = selected_lambda[[dataset_name]],
    mu_df = selected_mu_df[[dataset_name]],
    sigma_df = selected_sigma_df[[dataset_name]],
    nu_df = selected_nu_df[[dataset_name]],
    tau_df = selected_tau_df[[dataset_name]],
    
    AIC = AIC(model),
    GD = deviance(model),
    SBC_BIC = AIC(model, k = log(nobs(model))),
    convergence = model$converged,
    
    Residuals_Mean = mean(res),
    Residuals_SD = sd(res),
    
    KS_p_value = ks_test$p.value
  )
}


#==============================================================================
#--------------------------predict centiles------------------------------------
#==============================================================================
predict_centiles <- function(model,
                             age_vector,
                             original_data,
                             lambda,
                             centiles = target_centiles) {
  
  # Add the same age transformation used in the model
  original_data <- original_data %>%
    dplyr::mutate(
      age_lambda = age_year^lambda
    )
  
  # New data
  new_data <- tibble::tibble(
    age_year = age_vector,
    age_lambda = age_vector^lambda
  )
  
  # Predict distribution parameters
  pred <- predictAll(
    model,
    newdata = new_data,
    data = original_data
  )
  
  family <- model$family[1]
  
  params <- list(
    mu = as.numeric(pred$mu),
    sigma = as.numeric(pred$sigma),
    nu = as.numeric(pred$nu)
  )
  
  if (family %in% c("BCPE", "BCT")) {
    params$tau <- as.numeric(pred$tau)
  }
  
  qfun <- switch(
    family,
    BCPE = gamlss.dist::qBCPE,
    BCCG = gamlss.dist::qBCCG,
    BCT  = gamlss.dist::qBCT,
    stop("Unsupported family: ", family)
  )
  
  centile_values <- sapply(
    centiles,
    function(p) {
      do.call(
        qfun,
        c(
          list(p = p / 100),
          params
        )
      )
    }
  )
  
  centile_values <- as.data.frame(centile_values)
  names(centile_values) <- paste0("p", centiles)
  
  dplyr::bind_cols(
    tibble::tibble(age_year = age_vector),
    tibble::as_tibble(centile_values)
  )
}

#==============================================================================
#------------LOO---------------------------------------------------------------
#==============================================================================

fit_loo_gamlss <- function(data_input,
                          excluded_region,
                          family,
                          lambda,
                          mu_df,
                          sigma_df,
                          nu_df,
                          tau_df) {
  
  train_data <- data_input %>%
    dplyr::filter(region != excluded_region) %>%
    dplyr::mutate(
      age_lambda = age_year^lambda
    )
  
  #------------------------------------------------------------------
  # Model specification
  #------------------------------------------------------------------
  
  if (!is.na(nu_df)) {
    
    if (!is.na(tau_df)) {
      
      model <- gamlss(
        parameter ~ pb(age_lambda, df = mu_df),
        sigma.fo = ~ pb(age_lambda, df = sigma_df),
        nu.fo    = ~ pb(age_lambda, df = nu_df),
        tau.fo   = ~ pb(age_lambda, df = tau_df),
        family   = family,
        data     = train_data,
        control  = gamlss.control(trace = FALSE)
      )
      
    } else {
      
      model <- gamlss(
        parameter ~ pb(age_lambda, df = mu_df),
        sigma.fo = ~ pb(age_lambda, df = sigma_df),
        nu.fo    = ~ pb(age_lambda, df = nu_df),
        family   = family,
        data     = train_data,
        control  = gamlss.control(trace = FALSE)
      )
    }
    
  } else {
    
    model <- gamlss(
      parameter ~ pb(age_lambda, df = mu_df),
      sigma.fo = ~ pb(age_lambda, df = sigma_df),
      nu.fo    = ~ 1,
      tau.fo   = ~ pb(age_lambda, df = tau_df),
      family   = family,
      data     = train_data,
      control  = gamlss.control(trace = FALSE)
    )
  }
  
  return(model)
}

#==================================================================================
# spread of percentiles
#==================================================================================
calculate_spread <- function(data, year_label) {
  data %>%
    mutate(
      spread_upper = p97 - p50,
      spread_total = p97 - p3,
      year = year_label
    ) %>%
    select(age_year, sex, metric, year,
           p3, p50, p97,
           spread_upper, spread_total)
}


