if (!requireNamespace("skimr", quietly = TRUE)) install.packages("skimr")
library(dplyr)
library(skimr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(mice)
library(VIM)

#==============================================================================
#------------Distribution by geographical region-------------------------------
#==============================================================================
list_of_regions <- c("Архангельская обл.", "Калининградская обл.", "Кемеровская обл.", 
           "Краснодарский край", "Ленинградская обл.", "Московская обл.", 
           "Нижегородская обл.", "Новосибирская обл.", "Оренбургская обл.", 
           "Респ. Бурятия", "Респ. Саха", "Свердловская обл.", 
           "Ставропольский край", "Хабаровский край")

geo_ref <- tibble(list_of_regions,
  lat = c(64.54, 54.71, 55.33, 45.04, 59.94, 55.75, 56.30, 55.03, 51.77, 51.83, 62.03, 56.84, 45.04, 48.48),
  lon = c(40.54, 20.51, 86.08, 38.99, 30.31, 37.62, 43.94, 82.93, 55.10, 107.61, 129.73, 60.61, 41.97, 135.08)) %>%
  
  tidyr::unite(
    col = "coordinates", c(lat, lon),sep = "; lon ",remove = FALSE) %>%
  
    dplyr::mutate(coordinates = paste0("[lat: ", coordinates, "]"))

# Figures for supplementary
# Total counts by region

data_plot <- FigS1 <- my_data_step3 %>%
  
  dplyr::mutate(age_rounded = floor(age_year + 0.5)) %>%
  
  dplyr::mutate(
    sex_label = factor(ifelse(sex == 1, "Boys", "Girls"), levels = c("Boys", "Girls"))) %>%
  
  dplyr::group_by(region, sex_label) %>%
  
  dplyr::summarise(observations = n(), .groups = "drop") %>%
  
  dplyr::left_join(geo_ref,  by = "region")

S1 <- ggplot(data_chart, aes(x = coordinates, y = observations, fill = sex_label)) +
  
  geom_col(alpha = 0.85, show.legend = FALSE) +
  
  facet_wrap(~sex_label, nrow = 1) +
  
  scale_fill_manual(values = c("Boys" = "#3B5B7A", "Girls" = "#C47A44")) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(x = "",y = "Number of Observations", title = "Sample Size Distribution by Region, Sex, and Age Groups") +
  
  coord_flip()+
  
  geom_hline(yintercept = c(100,500,1000), linetype="dashed")

# Figure: Proportion of missing data

S2 <- VIM::aggr(
  my_data_step3, 
  col = c('#3B5B7A', '#C47A44'), 
  numbers = TRUE,                
  sortVars = TRUE,               
  labels = names(my_data_step3), 
  cex.axis = 0.8, 
  gap = 3, 
  ylab = c("Proportion of missingness", "Missingness Pattern")
)

# Figure: density plot height and BMI
S3 <- my_data_step3 %>%
  
  dplyr::select(sex, height, BMI) %>%
  
  pivot_longer(cols = c(height, BMI), 
    names_to = "Metric", 
    values_to = "Value") %>%
  
  ggplot(aes(x = Value)) +
  
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  
  geom_density(color = "red", size = 1.2) +
  
  theme_minimal() +
  
  facet_wrap(Metric ~ sex, scales = "free") +
  
  labs(x = "", y = "Density")

# Figure: distribution of height across ages

S4 <- ggplot(my_data_step3, aes(x = age_in_months, y = height)) +
  
  geom_point(alpha = 0.3, color = "darkblue") +
  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "orange", size = 1.2) +
  
  theme_minimal() +
  
  labs( x = "Age in months", y = "Height in cm")

# Figure: distribition of weight across ages

S5 <- ggplot(my_data_step3, aes(x = age_in_months, y = weight)) +
  
  geom_point(alpha = 0.3, color = "darkgreen") +
  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "orange", size = 1.2) +
  
  theme_minimal() +
  
  labs( x = "Age in months", y = "Weight in kg")

# Figure: distribution of BMI across ages

S6 <- my_data_step3 %>%
  mutate(age_years = factor(floor(age_year + 0.5))) %>%
  
  ggplot(aes(x = age_years, y = BMI, fill = sex)) +
  
  geom_boxplot(alpha = 0.7) +
  
  theme_minimal() +
  labs(x = "Age in years",
    y = "BMI in kg/m2",
    fill = "sex")

SF1 <- ggarrange(S1,S2,S3,S4,S5,S6,nrow=3,ncol=2)

output_directory <- "Supplementary/"

ggsave(
  filename = paste0(output_directory, "SF1.TIFF"), 
  plot = SF1, 
  dpi = 400,               
  width = 10,             
  height = 10,            
  units = "in"             
)


S7 <- ggplot(my_data_step3, aes(x = age_in_months, y = zhfa_WHO)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "orange", size = 1.2) +
  theme_minimal() +
  labs( x = "Age in months", y = "Z score Height")


S8 <- ggplot(my_data_step_4, aes(x = age_in_months, y = zbfa)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "orange", size = 1.2) +
  theme_minimal() +
  labs( x = "Age in months", y = "Z score BMI")

SF2 <- ggarrange(S7, S8, nrow=2,ncol=1)

output_directory <- "Supplementary/"

ggsave(
  filename = paste0(output_directory, "SF2.TIFF"), 
  plot = SF2, 
  dpi = 400,               
  width = 10,             
  height = 10,            
  units = "in"             
)

# OBESITY PREVALENCE BY WHO and Regional reference

#Prevalence by WHO

obesity_by_age_WHO <- my_data_step3 %>%
  
  dplyr::mutate(age_year = floor(age_year + 0.5)) %>%
  
  dplyr::group_by(age_year, sex) %>%
  
  dplyr::summarise(
    Total_Children = n(),
    Count_WHO      = sum(zbfa_WHO >= 2, na.rm = TRUE),
    Pct_WHO        = (Count_WHO / Total_Children) * 100,
    .groups = "drop")

#Prevalence by regional reference

obesity_by_age_RR <- my_data_step3 %>%
  
  dplyr::mutate(age_year = floor(age_year + 0.5)) %>%
  
  dplyr::group_by(age_year, sex) %>%
  
  dplyr::summarise(
    Total_Children = n(),
    Count_RR       = sum(zbfa_RR >= 2, na.rm = TRUE),
    Pct_RR         = (Count_RR / Total_Children) * 100,
    .groups = "drop")

# Differences in prevalence in obesity
obesity_comparison <- obesity_by_age_WHO %>%
  
  dplyr::left_join(dplyr::select(obesity_by_age_RR, age_year, sex, Count_RR, Pct_RR), by = c("age_year", "sex")) %>%
 
  
  dplyr::mutate(sex_label = ifelse(sex == 1, "Boys", "Girls"), 
                age_group = dplyr::case_when(
                age_year >= 5 & age_year <= 8 ~ "5–8 years",
                age_year >= 9 & age_year <= 12 ~ "9–12 years",
                TRUE ~ NA_character_)) %>%
  
  dplyr::group_by(age_group, sex_label) %>%
  
  dplyr::summarise(Total_Children = sum(Total_Children), Count_WHO = sum(Count_WHO), Count_RR = sum(Count_RR),
                   .groups = "drop") %>%
  
  dplyr::mutate(Pct_WHO = 100 * Count_WHO / Total_Children,
                Pct_RR = 100 * Count_RR / Total_Children,
                Pct_Diff = Pct_WHO - Pct_RR,
                
                effect_size_h = 
                  2 * asin(sqrt(Pct_WHO / 100)) -2 * asin(sqrt(Pct_RR / 100)),
                
                p_value = 
                  purrr::pmap_dbl(list(Count_WHO, Count_RR, Total_Children), ~ prop.test(x = c(..1, ..2),n = c(..3, ..3),correct = FALSE)$p.value)) %>%
  
  dplyr::select(age_group, sex_label, Total_Children, Count_WHO, Pct_WHO, Count_RR, Pct_RR, Pct_Diff, effect_size_h, p_value) %>%
  
  dplyr::arrange(age_group, sex_label)

S9 <- ggplot(obesity_comparison, aes(x = age_group, y = effect_size_h))+
  
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "grey50") +
  
  geom_col(width = 0.7,fill = "#6B8E7A")+
  
  facet_wrap(~ sex_label,nrow = 2) +
  
  labs(x = "Age group",
       y = "Effect size") +
  
  theme(strip.background = element_blank())

obesity_comparison_long <- obesity_comparison %>%
  
  dplyr::select(age_group, sex_label, Pct_WHO, Pct_RR) %>%
  
  tidyr::pivot_longer(cols = c(Pct_WHO, Pct_RR),names_to = "reference",values_to = "Pct") %>%
  
  dplyr::mutate(reference = dplyr::recode(reference, Pct_WHO = "WHO",  Pct_RR = "Regional reference"))
    
S10 <- ggplot(obesity_comparison_long, aes(x = age_group, y = Pct, fill =  reference))+
  
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "grey50") +
  
  geom_col(width=0.6,position = position_dodge(width = 0.9))+
  
  facet_wrap(~ sex_label,nrow = 2)+
  
  scale_fill_manual(values = c("Regional reference" = "#3B5B7A", "WHO" = "#C47A44"))+
   
  labs(x = "Age group",
       y = "Obesity prevalence (%)", fill = NULL) +
  
  theme(strip.background = element_blank(), legend.position = "right")

SF3 <- ggarrange(S9, S10, nrow=1, ncol = 2,
          widths = c(0.5, 1.5))

ggsave(
  filename = paste0(output_directory, "SF3.TIFF"), 
  plot = SF3, 
  dpi = 400,               
  width = 10,             
  height = 10,            
  units = "in"             
)

write.csv(obesity_comparison,"Obesity_prevalence.csv")


#==============================================================================
# Now the dataset is ready for the next step
#==============================================================================
rm(list = setdiff(ls(), c("my_data_step3", "regional_reference")))


#Output files: SF1.tiff, SF2.tiff, S3F.tiff,  Obesity_prevalence.csv in Supplementary