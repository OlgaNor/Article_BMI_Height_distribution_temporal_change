#===========================================================
# The first step is preprocessing
# After this step, the dataset contains no duplicate records and all city names have been standardized
#===========================================================

#installation of packages
library(dplyr)
library(stringr)
library(readxl)

# attach data
my_data <- readxl::read_xlsx("data/data.xlsx")

# Select required variables
my_data <- my_data %>% select(id,height, weight, age_in_months, age_year, sex, region, city, school)

#-------------------------------------------------------------------
# Standardization of names of the cities
#-------------------------------------------------------------------

my_data <- my_data %>%
  mutate(
    city_original = city, #copy of city variable
    city = str_to_lower(city), #rewrite city name
    
    # 3. Remove all titles except city name
    city = str_remove(city, "^г\\.\\s*|^гор\\.\\s*|^город\\s+"),
    city = str_remove(city, "^с\\.\\s*|^село\\s+"),
    city = str_remove(city, "^п\\.\\s*|^пос\\.\\s*|^пос\\.\\s*|^поселок\\s+"),
    city = str_remove(city, "^д\\.\\s*|^дер\\.\\s*|^деревня\\s+"),
    city = str_remove(city, "^пгт\\.\\s*|^пгт\\s+"),
    
    # 4. remove extra space between words
    city = str_squish(city)
  )

# Check results. Unique observations before and after
cat("Before correction:", length(unique(my_data$city)), "\n")
cat("After correction:", length(unique(my_data$city)), "\n")

#-------------------------------------------------------------------
# Remove diblicates
#-------------------------------------------------------------------

# Find all duplicates
# Create table contained only duplicates
group_vars <- c("region", "weight", "sex", "height", "city", "school", "age_in_months")

duplicates_summary <- my_data %>%
  dplyr::group_by(across(all_of(group_vars))) %>% 
  dplyr::filter(n() > 1) %>%
  dplyr::arrange(across(all_of(group_vars)))

total_dups_rows <- nrow(duplicates_summary)

unique_dups_count <- nrow(my_data) - nrow(dplyr::distinct(my_data, across(all_of(group_vars))))


cat("All observations which are duplicates:", total_dups_rows, "\n")
cat("Duplicates (excess rows):", unique_dups_count, "\n")

my_data_step1 <- my_data %>%
  distinct(across(all_of(group_vars)), .keep_all = TRUE)

# Before and after removal
cat("Inititial database:", nrow(my_data), "Number of observations.\n")
cat("After removal:", nrow(my_data_step1), "Number of observations.\n")

# =============================================================================
# remove everything except my_data_step1
# =============================================================================
rm(list = setdiff(ls(), "my_data_step1"))

# =============================================================================
# Data is ready for the next step

# Inititial database: 19895 Number of observations.
# After removal: 19765 Number of observations.
# =============================================================================