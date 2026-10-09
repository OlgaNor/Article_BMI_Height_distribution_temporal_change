library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)

#================================================
# Define a consistent theme for all figures
#================================================
journal_theme <- theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(size = 11, lineheight = 1.2),
    axis.title  = element_text(size = 11),  #  11 pt
    axis.text   = element_text(size = 10),  #  10 pt
    legend.text = element_text(size = 10),  #  10 pt
    panel.spacing.y = unit(1.5, "lines"),
    panel.spacing.x = unit(1.2, "lines")
  )


theme_set(journal_theme)


#==============================================================================
# ----------------Fig. 2 (a)---------------------------------------------------
#==============================================================================

# ============================================================
# FIGURE 2 (a)
# Comparison of z-scores based on WHO and regional reference dataset
#
# 4 panels:
# Boys BMI z-score
# Girls BMI z-score
# Boys Height z-score
# Girls Height z-score
#
# X-axis: Age in years
# Y-axis: Mean z-scores
# ============================================================

# -------------------------------data-------------------------
data_plot_fig2a <- my_data_step3 %>%
  dplyr::select(sex, age_year, zhfa_RR, zbfa_RR, zhfa_WHO, zbfa_WHO) %>%
  
  tidyr::pivot_longer(
    cols = c(zhfa_RR, zbfa_RR, zhfa_WHO, zbfa_WHO),
    names_to = c("metric", "reference"),
    names_sep = "_") %>%
  
   dplyr::mutate(
    age_year = round(age_year, 1),
    sex_label = ifelse(sex == 1, "Boys", "Girls"),
    metric_label = ifelse(metric == "zhfa", "Height-for-age (Z-score)", "BMI-for-age (Z-score)"),
    reference = ifelse(reference == "RR", "Regional Reference data", "WHO")
  ) %>%
  
  dplyr::mutate(
    custom_strip = case_when(
      metric_label == "Height-for-age (Z-score)" & sex_label == "Boys"  ~ "Height-for-age (Z-score)\nBoys",
      metric_label == "Height-for-age (Z-score)" & sex_label == "Girls" ~ "Height-for-age (Z-score)\nGirls",
      metric_label == "BMI-for-age (Z-score)"    & sex_label == "Boys"  ~ "BMI-for-age (Z-score)\nBoys",
      metric_label == "BMI-for-age (Z-score)"    & sex_label == "Girls" ~ "BMI-for-age (Z-score)\nGirls"
    ),
    
    custom_strip = factor(custom_strip, levels = c(
      "Height-for-age (Z-score)\nBoys", "Height-for-age (Z-score)\nGirls",
      "BMI-for-age (Z-score)\nBoys" , "BMI-for-age (Z-score)\nGirls")))

#----------------------Figure--------------------------------------

fig2a <- ggplot(data_plot_fig2a,aes(x = age_year, y = value, color = reference, fill = reference)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey50")+
  
  geom_smooth(method = "loess", span = 0.75, linewidth = 1.2, alpha = 0.15) +
 
  facet_wrap(~custom_strip, scales = "free_y", ncol = 2)+ 
  
  scale_color_manual(values = c("Regional Reference data" = "#3B5B7A", "WHO" = "#C47A44")) +
  
  scale_fill_manual(values = c("Regional Reference data" = "#3B5B7A", "WHO" = "#C47A44")) +
  
  scale_x_continuous(breaks = 5:12) +
  
  labs(x = "Age (years)",
    y = "Z-score Value",
    color = NULL,
    fill = NULL) +
  
  theme(panel.grid = element_blank())

#=======================================================================
#-----------------------fig2B-------------------------------------------
#=======================================================================

# ============================================================
# FIGURE 2 (b) 
# Percentage of children above the reference percentile 85 and 97 (%)
#
# 4 panels:
# Boys BMI 
# Girls BMI 
# Boys Height 
# Girls Height 
#
# X-axis: Age in years
# Y-axis: Percentage of children above the reference percentile (%)
# ============================================================

# Data prepared step 1
data_2025_prepared <- my_data_step3 %>%
  
  dplyr::mutate(age_group = floor(age_year + 0.5)) %>%
  
  tidyr::pivot_longer(cols = c(height, BMI), names_to = "Variable", values_to = "empirical_value") %>%
  
  dplyr::mutate(Variable = dplyr::case_when(Variable == "height" ~ "Height",Variable == "bmi"    ~ "BMI",TRUE ~ Variable)) %>%
  
  mutate(sex = ifelse(sex == 1, "Boys", "Girls"),
         metric_label = ifelse(Variable == "BMI", "BMI (kg/m2)", "Height (cm)")) %>%
  
  dplyr::select(sex, age_group, Variable, empirical_value, metric_label)

# Data prepared step 2

centiles_2012_new <- centiles_2012_short %>%
  
  dplyr::filter(percentile %in% c("p85", "p97"))

# Data prepared step 3

comparison_result <- data_2025_prepared %>%
  
  dplyr::left_join(centiles_2012_new, by = c("Variable", "sex", "age_group", "metric_label")) %>%
  
  dplyr::mutate(is_above = empirical_value > value_2012)

# Data prepared step 4

plot_data_fig2b <- comparison_result %>%
  dplyr::group_by(Variable, metric_label, sex, age_group, percentile) %>%
  dplyr::summarise(
    N_total = dplyr::n(),                             # Всего детей в этой когорте
    Count_above = sum(is_above, na.rm = TRUE),        # Сколько человек превысили порог 2012 года
    Fact_proportion = (Count_above / N_total) * 100,
    ci_low = binom.test(Count_above, N_total)$conf.int[1] * 100,
    ci_upp = binom.test(Count_above, N_total)$conf.int[2] * 100)%>%
  dplyr::arrange(Variable, sex, age_group, percentile)

#----------------------Figure--------------------------------------

hline_data <- data.frame(
  percentile = c("p85", "p97"),
  y_intercept = c(15, 3)
)

fig2b <- ggplot(plot_data_fig2b, aes(x = age_group, y = Fact_proportion,group = sex))+
  
  geom_point(aes(color = sex), size = 2)+
  
  geom_line(aes(color = sex), linewidth = 0.7)+
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_upp), fill = "darkblue", alpha = 0.15) +
  
  facet_wrap(~metric_label+percentile, scale="free")+
  
  geom_hline(data = hline_data, aes(yintercept = y_intercept), linetype = "dashed", color = "black", linewidth = 0.6)+
  
  expand_limits(y = 0)+
  
  scale_color_manual(values = c("Boys" = "#4C9BD4", "Girls" = "#B2182B"))+
  
  scale_x_continuous(breaks = 5:12) +
  
  labs(x = "Age (years)", y = "Percentage of children 
above the reference percentile (%)",color = NULL)+
  
  theme(panel.grid = element_blank())

#=====================================================================
# --------------------------Fig2C-----------------------------
#=====================================================================

# FIGURE 2 (c) 
# Percentiles 50-97 spread
#
# 4 panels:
# Boys BMI 
# Girls BMI 
# Boys Height 
# Girls Height 
#
# X-axis: Age in years
# Y-axis: Percentiles spread
# ============================================================

# data preparation
# No1 dataset
spread_2025 <- calculate_spread(centiles_2025, "2025")%>%
  
  mutate(sex = ifelse(sex == 1, "Boys", "Girls"),
         metric_label = ifelse(metric == "BMI", "BMI (kg/m2)", "Height (cm)"))

# No2 dataset
spread_2012 <- calculate_spread(centiles_2012, "2012")%>%
  mutate(sex = ifelse(sex == 1, "Boys", "Girls"),
         metric_label = ifelse(metric == "BMI", "BMI (kg/m2)", "Height (cm)"))

# Figure
fig2c <- ggplot() +
  geom_line(data = spread_2012,aes(x = age_year,y = spread_upper,color = "2012"),linewidth = 0.8, linetype = "dashed") +
  
  geom_line(data = spread_2025, aes(x = age_year, y = spread_upper, color = "2025"), linewidth = 1.0) +
  
  facet_grid(metric_label ~ sex, scales = "free", switch = "y") +
  
  scale_x_continuous(breaks = 5:12) +
  
  scale_color_manual(name = "P50 - P85 spread", values = c( "2012" = "#3B5B7A", "2025" = "#FF5A49" )) +
  
  labs(x = "Age (years)", y = "", color = "P50 - P85 spread") +
  
  theme(strip.placement = "outside")


#=====================================================================
# ---------------------------------Fig2D-----------------------------
#=====================================================================
# ============================================================
# FIGURE 2 (d)
# Deviation in the proportion of 2025 children
# across BMI percentile bands defined by the 2012 reference
#
# 4 panels:
# Boys 5–8 years
# Girls 5–8 years
# Boys 9–12 years
# Girls 9–12 years
#
# X-axis: BMI percentile bands
# Y-axis: deviation from the expected 2012 proportion (p.p.)
# ============================================================

# Data preparation step 1

thresholds_2012_bmi <- centiles_2012 %>%
  
  dplyr::filter(metric == "BMI") %>%
  
  dplyr::select(sex,age_year, p15, p50, p85, p97,  p99)%>%
  
  dplyr::mutate(sex = ifelse(sex == 1, "Boys", "Girls"))

# Data preparation step 2

data_2025_bmi <- my_data_step3 %>%
  
  dplyr::mutate(age_year = floor(age_year + 0.5), sex = ifelse(sex == 1, "Boys", "Girls")) %>%
  
  dplyr::select(sex, age_year, BMI)

# Data preparation step 3

classified_2025 <- data_2025_bmi %>%
  
  dplyr::left_join(thresholds_2012_bmi, by = c("sex", "age_year"))

# Data preparation step 4

classified_2025 <- classified_2025 %>%
  
  dplyr::mutate(percentile_band = dplyr::case_when(BMI < p15 ~ "0–15", 
                                                   BMI >= p15 & BMI < p85 ~ "15–85",
                                                   BMI >= p85 & BMI < p97 ~ "85–97",
                                                   BMI >= p97 ~ "97–100",
                                                   TRUE ~ NA_character_), 
                age_period = dplyr::case_when(age_year >= 5 & age_year <= 8 ~ "5–8 years",
                                              age_year >= 9 & age_year <= 12 ~ "9–12 years",
                                              TRUE ~ NA_character_)) %>%
  
  dplyr::filter(!is.na(percentile_band),!is.na(age_period))


# Data preparation step 5

fig2d_plot_data <- classified_2025 %>%
  
  dplyr::count(sex, age_period, percentile_band, name = "n") %>%
  
  dplyr::group_by(sex, age_period) %>%
  
  dplyr::mutate(n_total = sum(n), proportion_2025 = 100 * n / n_total) %>%
  
  dplyr::ungroup()

# Data preparation step 6

expected_2012 <- tibble::tibble(percentile_band = c("0–15", "15–85", "85–97", "97–100"), expected_2012 = c(15, 70, 12, 3))
   

# Data preparation step 6

fig2d_plot_data <- fig2d_plot_data %>%
  
  dplyr::left_join(expected_2012, by = "percentile_band") %>%
  
  dplyr::mutate(difference_pp = proportion_2025 - expected_2012, 
                percentile_band = factor(percentile_band,
                                         levels = c("0–15", "15–85", "85–97", "97–100")),
                age_period = factor(age_period, levels = c("5–8 years", "9–12 years")),
                facet = factor(paste(sex, age_period, sep = " — "),
                               levels = c("Boys — 5–8 years","Girls — 5–8 years","Boys — 9–12 years","Girls — 9–12 years")))
      
# Figure

fig2d <- ggplot(fig2f_plot_data, aes(x = percentile_band, y = difference_pp))+
 
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "grey50") +
  
  geom_col(width = 0.7,fill = "#3B5B7A")+
  
  geom_line(aes(group = 1), linewidth = 0.9, color = "#FF5A49") +
  
  geom_point(size = 2.5, color = "#FF5A49")  +
  
  facet_wrap(~ facet,nrow = 2) +
  
  labs(x = "BMI percentile range (2012 reference)",
       y = "Deviation from 2012 expected proportion (p.p.)") +
  
  coord_cartesian(ylim = c(-5, 5))+
  
  theme(strip.background = element_blank())


# ---------------------------------Fig2E-----------------------------
#=====================================================================
# ============================================================
# FIGURE 2 (e)
# Relative difference between experimental quantiles and data 2012 (heatmap)
#
# 4 panels:
# Boys BMI
# Girls BMI
# Boys Height
# Girls Height
#
# X-axis: Age in years
# Y-axis: Percentiles
# Colors: Relative differences
# ============================================================

# Data preparation step 1

empirical_2025 <- my_data_step3 %>%
  mutate(age_year = floor(age_year+0.5)) %>%
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
    Height_p99   = quantile(height, 0.99, na.rm = TRUE)) %>%
  
  pivot_longer(cols = -c(age_year, sex), names_to = c("metric", "percentile"), names_sep = "_", values_to = "value_2025") %>%
  
  mutate(sex = ifelse(sex == 1, "Boys", "Girls"), metric_label = ifelse(metric == "BMI", "BMI (kg/m2)", "Height (cm)"))

# Data preparation step 2

centiles_2012_fig2e_and_d <- centiles_2012 %>%
  
  pivot_longer(cols = -c(metric, age_year, sex, Source), names_to = "percentile", values_to = "value_2012") %>%
  
  mutate(sex = ifelse(sex == 1, "Boys", "Girls"), metric_label = ifelse(metric == "BMI", "BMI (kg/m2)", "Height (cm)"))

# Data preparation step 3

fig2e_plot_data <- empirical_2025 %>%
  
  dplyr::inner_join(centiles_2012_fig2e_and_d %>% 
  
  dplyr::select(metric_label, sex, age_year, percentile, value_2012), by = c("metric_label", "sex", "age_year", "percentile")) %>%
  
  dplyr::mutate(Relative_diff_pct = ((value_2025 - value_2012) / value_2012) * 100,
                Absolute_diff = value_2025 - value_2012) %>%
  
  dplyr::arrange(metric_label, sex, percentile, age_year)

# Figure 

fig2e <- ggplot(fig2e_plot_data, aes(x = age_year, y = percentile, fill = Relative_diff_pct)) +
  
  geom_tile() + 
  
  facet_grid(sex ~ metric_label, scales = "fixed") +
  
  scale_fill_gradientn(colours = c("#08306B", "#6BAED6", "white", "#F4A6A6", "#D73027"),
                       values = scales::rescale(c(-20, -10, 0, 2, 5)),
                       limits = c(-20, 5),
                       oob = scales::squish,
                       name = "Experimental quantiles − centiles 2012 (Δ %)") +
  
  scale_x_continuous(breaks = 5:12) +
  
  labs(x = "Age (years)", y = "Percentile", fill = "Experimental quantiles − centiles 2012 (Δ %)")+
  
  theme(panel.grid = element_blank())



# ---------------------------------Fig2F-----------------------------
#=====================================================================
# ============================================================
# FIGURE 2 (f)
# Relative difference between fitted GAMLSS centiles and data 2012 (heatmap)
#
# 4 panels:
# Boys BMI
# Girls BMI
# Boys Height
# Girls Height
#
# X-axis: Age in years
# Y-axis: Percentiles
# Colors: Relative differences
# ============================================================

# Data preparation step 1

data_mean_centiles_2025 <- data_long_centiles_2025 %>%
  
  mutate(age_year = floor(age_year + 0.5)) %>%
  
  group_by(sex, metric_label, percentile, age_year) %>%
  
  summarise(value_2025 = mean(value))

# Data preparation step 2

fig2f_plot_data <- data_mean_centiles_2025 %>%
  
  dplyr::left_join(centiles_2012_fig2e_and_d %>%
                     
      dplyr::select(metric_label, sex, age_year, percentile, value_2012),
      by = c("metric_label", "sex", "age_year", "percentile")) %>%
  
  dplyr::mutate(relative_difference = (value_2025 - value_2012) / value_2012 * 100)

# Figure

fig2f <- ggplot(fig2f_plot_data , aes(x = age_year, y = percentile, fill = relative_difference)) +
  
  geom_tile() + 
  
  facet_grid(sex ~ metric_label, scales = "fixed") +
  
  scale_fill_gradientn(colours = c("#08306B", "#6BAED6", "white", "#F4A6A6", "#D73027"),
                       values = scales::rescale(c(-20, -10, 0, 2, 5)),
                       limits = c(-20, 5),
                       oob = scales::squish,
                       name = "GAMLSS 2025 centiles − centiles 2012 (Δ %)")+
  
  scale_x_continuous(breaks = 5:12) +
  
  labs(x = "Age (years)",y = "Percentile", fill = "GAMLSS 2025 centiles − centiles 2012 (Δ %)")+
  
  theme(panel.grid = element_blank())


#=====================================================
#---------------FIGURE 2 (combined)-------------------
#=====================================================

combined_fig2 <- ggarrange(
  fig2a, fig2b, fig2c, fig2d, fig2e, fig2f,
  ncol = 2, nrow = 3,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
  label.x = 0.02, label.y = 0.98,
  font.label = list(size = 14, face = "bold", color = "black"))


print(combined_fig2)


output_directory <- "Figures/"

ggsave(
  filename = paste0(output_directory,  "Fig2_new.TIFF"), 
  plot = combined_fig2, 
  width = 10, 
  height = 12, 
  dpi = 400
)


fig1a <- ggplot(my_data_step3, aes(x = floor(age_year + 0.5))) +
  # Добавляем after_stat(density), чтобы выровнять шкалу Y
  geom_histogram( bins = 8, 
                  fill = "#2F5B69",
                  color = "#263238", alpha = 0.7) +
  theme_minimal() +
  labs(x = "Age group", y = "Sample size")+
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,5000,1000),limits = c(0, 5000))+
  theme(
    text = element_text(family = "serif", color = "#263238",size = 12),
    axis.title = element_text(color = "#263238"),
    axis.text = element_text(color = "#263238"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

total_data <- data.frame(
  Age = 5:12,
  Total_N = c(1899, 9316, 16883, 19696, 17151, 19719, 18518, 19808)
)

fig1b <- ggplot(total_data, aes(y= Total_N, x = Age)) +
  geom_col(
    fill = "#2F5B69",
    color = "#263238", alpha = 0.7) +
  theme_minimal() +
  labs(x = "Age group", y = "Sample size")+
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,25000,10000),limits = c(0, 23000))+
  theme(
    text = element_text(family = "serif", color = "#263238",size = 12),
    axis.title = element_text(color = "#263238"),
    axis.text = element_text(color = "#263238"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#==============================================================
#------------------Figure 1S-----------------------------------
#==============================================================
obesity_prevalence <- read.csv("Obesity_prevalence.csv")

Fig1S <- ggplot(obesity_prevalence) +
  geom_line(aes(x = age_group, y = Pct_WHO, group = sex_label, color = "WHO Reference"), linewidth = 1) +
  geom_line(aes(x = age_group, y = Pct_RR, group = sex_label, color = "Regional Reference"), linewidth = 1) +
  facet_wrap(~sex_label) +
   scale_color_manual(values = c("WHO Reference" = "#C47A44","Regional Reference" = "#3B5B7A")) +
  scale_x_continuous(breaks = seq(5, 12, 1)) +
  scale_y_continuous(breaks = seq(0, 20, 2.5)) +
  labs(x = "Age (years)",
    y = "Prevalence of obesity (%)",
    color = NULL )

output_directory <- "Supplementary/"
ggsave(
  filename = paste0(output_directory,"Fig1S.tiff"), 
  plot     = Fig1S, 
  width    = 8, 
  height   = 5, 
  dpi      = 400)

#==============================================================
#------------------Figure 2S-----------------------------------
#==============================================================
plot_data_2025 <- data_2025_centiles_fixed %>%
  tidyr::pivot_longer(cols = c(p3, p50, p85, p90, p95, p97, p99), names_to = "percentile", values_to = "value")

plot_data_2025_long <- data_2025_centiles  %>%
  tidyr::pivot_longer(
    cols = c(p3, p50, p85, p90, p95, p97, p99), 
    names_to = "percentile", 
    values_to = "value") %>%
  dplyr::mutate(
    percentile = factor(percentile, levels = c("p3", "p50", "p85", "p90", "p95", "p97", "p99")),
    sex_label = ifelse(sex == 1, "Boys", "Girls"),
    metric_label = ifelse(metric == "BMI", "BMI (kg/m²)", "Height (cm)"))

data_labels_2025 <- plot_data_2025_long %>%
  group_by(metric_label, sex_label, percentile) %>%
  filter(age_year == max(age_year)) %>%
  ungroup()

Fig2S <- ggplot(plot_data_2025_long, aes(x = age_year, y = value, group = percentile)) +
  geom_smooth(method = "loess", span = 0.75, se = FALSE, linewidth = 1.0,color = "grey") +
  geom_text_repel(
    data = data_labels_2025,
    aes(label = percentile),
    direction   = "y",          
    nudge_x     = 0.3, 
    size        = 3.5,
    show.legend = FALSE) +
  
  facet_wrap(~ metric_label+sex_label, scales = "free") +
  
  scale_x_continuous(breaks = seq(5, 12, 1)) + 
  labs(x = "Age (years)",
    y = "" ) 

output_directory <- "Supplementary/"
ggsave(
  filename = paste0(output_directory,"Fig2S.tiff"), 
  plot     = Fig2S, 
  width    = 6, 
  height   = 8, 
  dpi      = 400)


#==============================================================
#------------------Figure 3S-----------------------------------
#==============================================================
new_model_fig3s <- new_model_long %>%
  dplyr::mutate(
    percentile = factor(percentile, levels = c("p3", "p50", "p85", "p90", "p95", "p97", "p99")),
    sex_label = ifelse(sex == 1, "Boys", "Girls"),
    metric_label = ifelse(metric == "BMI", "BMI (kg/m²)", "Height (cm)"))

data_labels_2025 <- new_model_fig3s %>%
  group_by(metric_label, sex_label, percentile) %>%
  filter(age_year == max(age_year)) %>%
  ungroup()

Fig3S <- ggplot(new_model_fig3s, aes(x = age_year, y = value_2025, group = percentile)) +
  geom_smooth(method = "loess", span = 0.75, se = FALSE, linewidth = 1.0,color = "grey") +
  geom_text_repel(
    data = data_labels_2025,
    aes(label = percentile),
    direction   = "y",          
    nudge_x     = 0.3, 
    size        = 3.5,
    show.legend = FALSE) +
  facet_wrap(~ metric_label+sex_label, scales = "free") +
  scale_x_continuous(breaks = seq(5, 12, 1)) + 
  labs(x = "Age (years)",
       y = "" ) 

output_directory <- "Supplementary/"
ggsave(
  filename = paste0(output_directory,"Fig3S.tiff"), 
  plot     = Fig3S, 
  width    = 6, 
  height   = 8, 
  dpi      = 400)

#==========================================================================
#-----------------------Fig4S---------------------------------------------
#=========================================================================
new_model_comparison_2025_2012 <- new_model %>%
  mutate(age_year = floor(age_year+0.5)) %>%
  tidyr::pivot_longer(cols = c(p3, p50, p85, p90, p95, p97, p99), names_to = "percentile", values_to = "value_2025") %>%
  group_by(age_year, metric, sex, percentile) %>%
  summarise(value_2025 = mean(value_2025, na.rm = TRUE)) %>%
  arrange(metric, sex, age_year, percentile)

# Объединяем и рассчитываем Δ% по вашей формуле
percentile_diff_new <- plot_data_2012 %>%
  dplyr::inner_join(new_model_comparison_2025_2012, by = c("age_year", "sex", "metric", "percentile")) %>%
  dplyr::mutate(
    diff_pct = ((value_2025 - value_2012) / value_2012) * 100,
    percentile = factor(percentile, levels = c("p3", "p50", "p85", "p90", "p95", "p97", "p99")),
    sex_label = factor(ifelse(sex == 1, "Boys", "Girls"), levels = c("Boys", "Girls")),
    metric_label = factor(ifelse(metric == "BMI", "BMI", "Height"), levels = c("BMI", "Height"))
  )


fig4S <- ggplot(percentile_diff_new, aes(x = age_year, y = percentile, fill = diff_pct)) +
  geom_tile() + 
  facet_grid(sex_label ~ metric_label, scales = "fixed") +
  scale_fill_gradient2(
    low = "#3182bd",       
    mid = "white",         
    high = "darkred",      
    midpoint = 0, 
    name = "New model − old model (Δ %)",
    limits   = c(-15, 10),           
    oob      = scales::squish ) +
  scale_x_continuous(breaks = 5:12) +
  labs(x = "Age, years",
       y = "Percentile",
       fill = "New model − old model (Δ %)")+
  theme(panel.grid = element_blank())

output_directory <- "Supplementary/"
ggsave(
  filename = paste0(output_directory,"Fig4S.tiff"), 
  plot     = Fig4S, 
  width    = 6, 
  height   = 8, 
  dpi      = 400)

#======================================================================
# Comparison of mu and sigma
#======================================================================

data_plot_deltas <- secular_trends_summary %>%
  dplyr::select(metric, sex_label, age_year, mu_delta, sigma_delta) %>%
  tidyr::pivot_longer(
    cols = c(mu_delta, sigma_delta),
    names_to = "parameter",
    values_to = "delta_value"
  ) %>%
  
  dplyr::mutate(
    parameter = factor(parameter, 
                       levels = c("mu_delta", "sigma_delta"),
                       labels = c("Delta Mu (2025 - 2012)", "Delta Sigma (2025 - 2012")),
    metric_label = factor(ifelse(metric == "BMI", "BMI (kg/m2)", "Height (cm)"), levels = c("BMI (kg/m2)", "Height (cm)")),
    sex_label = factor(sex_label, levels = c("Boys", "Girls"))
  )


fig5S <- ggplot(data_plot_deltas, aes(x = age_year, y = delta_value, color = sex_label, group = sex_label)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6, color = "grey40") +
  
  geom_line(linewidth = 1.2) +
  
  facet_wrap(~ parameter + metric_label, scales = "free", ncol = 2, strip.position = "left")  +
  
  scale_color_manual(values = c("Boys" = "#0072B2", "Girls" = "#CC79A7")) +
  scale_x_continuous(breaks = 5:12) +
  labs(x = "Age (years)",y = NULL, color = NULL)+
  theme(strip.placement = "outside")


output_directory <- "Supplementary/"
ggsave(
  filename = paste0(output_directory,"Fig5S.tiff"), 
  plot     = fig5S, 
  width    = 6, 
  height   = 8, 
  dpi      = 400)


#===============================================================
#-------------Figure 6S--------------------
#===============================================================

df <- data.frame(
  district = c(
    "Central",
    "Northwestern",
    "Southern",
    "North Caucasian",
    "Volga",
    "Ural",
    "Siberian",
    "Far Eastern"
  ),
  `2012` = c(17, 10, 5, 6, 14, 6, 10, 9),
  `2025` = c(1, 3, 1, 1, 2, 1, 3, 2)
)

df_long <- df %>%
  pivot_longer(
    cols = c(`2012`, `2025`),
    names_to = "year",
    values_to = "n"
  )


#=======================================================================
#-----------------------Fig7S------------------------------------
#-------------Differnces between 2025 centiles and 2012 centiles

data_2025_centiles_fixed <- data_2025_centiles %>%
  dplyr::mutate(age_year = floor(age_year + 0.5)) %>%
  dplyr::group_by(sex, metric, age_year) %>%
  dplyr::summarise(
    p3  = mean(p3, na.rm = TRUE),
    p50 = mean(p50, na.rm = TRUE),
    p85 = mean(p85, na.rm = TRUE),
    p90 = mean(p90, na.rm = TRUE),
    p95 = mean(p95, na.rm = TRUE),
    p97 = mean(p97, na.rm = TRUE),
    p99 = mean(p99, na.rm = TRUE),
    .groups = "drop" )

plot_data_2012 <- centiles_2012 %>%
  tidyr::pivot_longer(cols = c(p3, p50, p85, p90, p95, p97,p99), names_to = "percentile", values_to = "value_2012")

plot_data_2025 <- data_2025_centiles_fixed %>%
  tidyr::pivot_longer(cols = c(p3, p50, p85, p90, p95, p97, p99), names_to = "percentile", values_to = "value_2025")

percentile_diff <- plot_data_2012 %>%
  inner_join(plot_data_2025, by = c("age_year", "sex", "metric", "percentile")) %>%
  mutate(
    diff_value = ((value_2025 - value_2012) / value_2012) * 100,
    sex_label = ifelse(sex == 1, "Boys", "Girls"),
    metric_label = ifelse(metric == "BMI", "BMI (kg/m2)", "Height (cm)")
  ) %>%
  select(metric_label, sex_label, age_year, percentile, value_2012, value_2025, diff_value)

fig2b <- ggplot(percentile_diff, aes(x = age_year, y = percentile, fill = diff_value)) +
  geom_tile() + 
  facet_grid(sex_label ~ metric_label, scales = "fixed") +
  scale_fill_gradient2(
    low = "#3182bd",       
    mid = "white",         
    high = "darkred",      
    midpoint = 0, 
    name = "Centiles (data 2025) − centiles 2012 (Δ %)",
    limits   = c(-15, 10),           
    oob      = scales::squish ) +
  scale_x_continuous(breaks = 5:12) +
  labs(x = "Age, years",
       y = "Percentile",
       fill = "Centiles based on model 2012 (data 2025) − centiles 2012 (Δ %)")+
  theme(panel.grid = element_blank())



