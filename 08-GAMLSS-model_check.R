library(gamlss)
library(dplyr)

# =============================================================================
# ------------------------Quality control-------------------------------------
# =============================================================================

list_of_models <- list(
  bct_boys_bmi_based_on_2012   = bct_boys_bmi_based_on_2012,
  bct_girls_bmi_based_on_2012   = bct_girls_bmi_based_on_2012, 
  bct_boys_height_based_on_2012 = bct_boys_height_based_on_2012, 
  bct_girls_height_based_on_2012 = bct_girls_height_based_on_2012,
  final_mod_boys_bmi = final_mod_boys_bmi,
  final_mod_boys_height = final_mod_boys_height,
  final_mod_girls_bmi = final_mod_girls_bmi,
  final_mod_girls_height = final_mod_girls_height)


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

write.csv(quality_summary,"Supplementary/Quality_control_all_models.csv")


my_path <- "d:/VD/2026/VD/article_8_secular_trends/Supplementary/wormplots_comparison_BMI.png" 
full_filename <- paste0(my_path, "wormplots_comparison_BMI.png", width = 2400, height = 2400, res = 300)

# Настраиваем сетку: 2 строки (мальчики, девочки) и 2 столбца (2012, Final)
# mar задает отступы вокруг графиков
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1) + 0.1)

# --- Строка 1, Лево: Мальчики BMI 2012 ---
wp(bct_boys_bmi_based_on_2012, ylim.all = 2)
title(main = "Boys BMI: 2012 Reference", line = 1, font.main = 2)

# --- Строка 1, Право: Мальчики BMI Final ---
wp(final_mod_boys_bmi, ylim.all = 2)
title(main = "Boys BMI: Final Model (2025)", line = 1, font.main = 2)

# --- Строка 2, Лево: Девочки BMI 2012 ---
wp(bct_girls_bmi_based_on_2012, ylim.all = 2)
title(main = "Girls BMI: 2012 Reference", line = 1, font.main = 2)

# --- Строка 2, Право: Девочки BMI Final ---
wp(final_mod_girls_bmi, ylim.all = 2)
title(main = "Girls BMI: Final Model (2025)", line = 1, font.main = 2)

# Закрываем и сохраняем файл
dev.off()
cat("[Успешно] График wormplots_comparison_BMI.png сохранен.\n")

full_filename_height <- "d:/VD/2026/VD/article_8_secular_trends/Supplementary/wormplots_comparison_Height.png"

# 2. Открываем графическое устройство PNG
png(filename = full_filename_height, width = 2400, height = 2400, res = 300)

# 3. Настраиваем сетку графиков 2х2
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1) + 0.1)

# --- Строка 1, Лево: Мальчики Рост 2012 ---
wp(bct_boys_height_based_on_2012, ylim.all = 2)
title(main = "Boys Height: 2012 Reference", line = 1, font.main = 2)

# --- Строка 1, Право: Мальчики Рост Final ---
wp(final_mod_boys_height, ylim.all = 2)
title(main = "Boys Height: Final Model (2025)", line = 1, font.main = 2)

# --- Строка 2, Лево: Девочки Рост 2012 ---
wp(bct_girls_height_based_on_2012, ylim.all = 2)
title(main = "Girls Height: 2012 Reference", line = 1, font.main = 2)

# --- Строка 2, Право: Девочки Рост Final ---
wp(final_mod_girls_height, ylim.all = 2)
title(main = "Girls Height: Final Model (2025)", line = 1, font.main = 2)