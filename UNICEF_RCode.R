# Install and load necessary libraries
#install.packages("haven")
#install.packages("dplyr")
#install.packages("labelled")
#install.packages("survey")
#install.packages("openxlsx")
#install.packages("ggplot2")
install.packages("classInt")
install.packages("units", configure.args = "--with-udunits2-lib=/usr/local/lib --with-udunits2-include=/usr/local/include/udunits2")
install.packages("sf", dependencies = TRUE)
install.packages("readxl")
library(haven)
library(dplyr)
library(labelled) # for set_value_labels and set_variable_labels
library(survey)
library(openxlsx)
library(ggplot2)
library(classInt)
library(units)
library(sf)
library(readxl)


# Task 1: Calculate survey-weighted vaccine coverage by subnational region
# Steps
###################### 1.1 ################################
# Load the .rda file
load("~/Downloads/Written_test_Health/TZ_data.rda")

# Check the loaded objects
ls()

# Inspect TZ_data
str(TZ_data)
summary(TZ_data)
glimpse(TZ_data)

###################### 1.2 ################################

# Check the column names in the dataset
names(TZ_data)

# Filter children aged 12-23 months and create age groups
children_12_23 <- TZ_data %>%
  mutate(agegroup = case_when(
    b19 >= 12 & b19 <= 23 ~ 1,
    b19 >= 24 & b19 <= 35 ~ 2
  )) %>%
  set_value_labels(agegroup = c("12-23" = 1, "24-35" = 2)) %>%
  set_variable_labels(agegroup = "Age group of child for vaccination")

# Create the Penta 1 and Penta 3 coverage variables
children_12_23 <- children_12_23 %>%
  mutate(
    ch_pent1_either = ifelse(h1 == 1 | h3 == 1, 1, 0),
    ch_pent3_either = ifelse(h5 == 1 | h7 == 1, 1, 0)
  )

# Inspect the dataset
str(children_12_23)
summary(children_12_23)
glimpse(children_12_23)

###################### 1.3 ################################

# Create the survey design object
children_12_23$weight <- children_12_23$v005 / 1000000  # Adjust the weight
svy_design <- svydesign(id = ~1, weights = ~weight, data = children_12_23)

# Calculate weighted Penta 1 coverage by subnational region
penta1_coverage <- svyby(~ch_pent1_either, ~v024, svy_design, svymean, na.rm = TRUE)
# Calculate weighted Penta 3 coverage by subnational region
penta3_coverage <- svyby(~ch_pent3_either, ~v024, svy_design, svymean, na.rm = TRUE)

# Convert to percentages
penta1_coverage$ch_pent1_either <- penta1_coverage$ch_pent1_either * 100
penta3_coverage$ch_pent3_either <- penta3_coverage$ch_pent3_either * 100

# Inspect the results
penta1_coverage
penta3_coverage


###################### 1.4 ################################

# Convert to percentages and calculate confidence intervals
penta1_coverage <- penta1_coverage %>%
  mutate(
    ch_pent1_either = ch_pent1_either * 100,
    se = se * 100,
    Coverage = round(ch_pent1_either, 1),
    CI_Lower = round(ch_pent1_either - 1.96 * se, 1),
    CI_Upper = round(ch_pent1_either + 1.96 * se, 1)
  )

penta3_coverage <- penta3_coverage %>%
  mutate(
    ch_pent3_either = ch_pent3_either * 100,
    se = se * 100,
    Coverage = round(ch_pent3_either, 1),
    CI_Lower = round(ch_pent3_either - 1.96 * se, 1),
    CI_Upper = round(ch_pent3_either + 1.96 * se, 1)
  )

# Combine the results into one data frame
coverage_results <- bind_rows(
  penta1_coverage %>% select(Region = v024, Coverage, CI_Lower, CI_Upper) %>% mutate(Indicator = "Penta 1 Coverage"),
  penta3_coverage %>% select(Region = v024, Coverage, CI_Lower, CI_Upper) %>% mutate(Indicator = "Penta 3 Coverage")
)

# Create a new workbook
wb <- createWorkbook()

# Add a worksheet
addWorksheet(wb, "Coverage")

# Write the data to the worksheet
writeData(wb, "Coverage", coverage_results)

# Save the workbook
saveWorkbook(wb, "task1_coverage.xlsx", overwrite = TRUE)

# Save the workbook to a specific path
saveWorkbook(wb, "~/Downloads/Written_test_Health/task1_coverage.xlsx", overwrite = TRUE)

# Task 2: Conduct a data quality assessment (outliers and completeness)
# Steps
###################### 2.1 ################################

# Load the .rda file
load("~/Downloads/Written_test_Health/admin_data.rda")

# Check the loaded objects
ls()

# Load 'admin_data'
str(admin_data)
summary(admin_data)
glimpse(admin_data)

###################### 2.2 ################################

# Check for missing values
missing_values <- admin_data %>%
  summarise_all(~ sum(is.na(.)))

print(missing_values)

# Calculate the percentage completeness by 'indicatortype', 'year', 'month', and 'admin1'
completeness <- admin_data %>%
  group_by(indicatortype, year, month, admin1) %>%
  summarise(
    total_entries = n(),
    missing_entries = sum(is.na(volume)),
    completeness_percentage = (total_entries - missing_entries) / total_entries * 100
  ) %>%
  arrange(completeness_percentage)  # Sort by ascending completeness_percentage

print(completeness)

###################### 2.3 ################################

# Check for outliers by 'indicatortype' and 'uid'
outliers_sd <- admin_data %>%
  group_by(indicatortype, uid) %>%
  mutate(
    mean_volume = mean(volume, na.rm = TRUE),
    sd_volume = sd(volume, na.rm = TRUE),
    z_score = (volume - mean_volume) / sd_volume,
    outlier_sd = ifelse(abs(z_score) > 3, TRUE, FALSE)
  )

# Calculate the median and MAD
outliers_mad <- admin_data %>%
  group_by(indicatortype, uid) %>%
  mutate(
    median_volume = median(volume, na.rm = TRUE),
    mad_volume = mad(volume, constant = 1.4826, na.rm = TRUE),
    mad_score = abs(volume - median_volume) / mad_volume,
    outlier_mad = ifelse(mad_score > 5, TRUE, FALSE)
  )

# Combine the results ensuring uniqueness
outliers_combined <- outliers_sd %>%
  select(indicatortype, uid, volume, admin2, year, outlier_sd) %>%
  distinct() %>%
  left_join(outliers_mad %>% select(indicatortype, uid, volume, admin2, year, outlier_mad) %>% distinct(), 
            by = c("indicatortype", "uid", "volume", "admin2", "year"))

# Print the results
print(outliers_combined)

###################### 2.4 ################################ 

# Filter for Penta 3 and year 2021 onwards
penta3_data <- admin_data %>%
  filter(indicatortype == "penta3_u1" & year >= 2021)

# Calculate the percentage completeness by 'admin1', 'year', and 'month'
completeness_penta3 <- penta3_data %>%
  group_by(admin1, year, month) %>%
  summarise(
    total_entries = n(),
    missing_entries = sum(is.na(volume)),
    completeness_percentage = (total_entries - missing_entries) / total_entries * 100
  ) %>%
  ungroup()

# Create the heatmap
ggplot(completeness_penta3, aes(x = factor(year * 100 + month), y = factor(admin1))) +
  geom_tile(aes(fill = completeness_percentage), color = "white") +
  scale_fill_gradientn(
    colours = c("#E2231A", "#F26A21", "#FFC20E", "#FFF09C", "#69DBFF"),
    values = c(0, 0.7, 0.8, 0.9, 0.94, 1),
    limits = c(0, 100),
    na.value = "grey50"
  ) +
  labs(
    title = "Completeness (%) of monthly reporting for Penta 3 by admin1",
    x = "Month-Year",
    y = "admin1",
    fill = "Completeness (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###################### 2.5 ################################

# Summarize the percentage of outliers by 'admin2', 'indicatortype', and 'year'
outlier_summary <- outliers_combined %>%
  group_by(admin2, indicatortype, year) %>%
  summarise(
    total_entries = n(),
    outliers_sd = sum(outlier_sd, na.rm = TRUE),
    outliers_mad = sum(outlier_mad, na.rm = TRUE),
    percent_outliers_sd = (outliers_sd / total_entries) * 100,
    percent_outliers_mad = (outliers_mad / total_entries) * 100
  ) %>%
  ungroup()

# Save the summary table to an Excel file
file_path <- "~/Downloads/Written_test_Health/task2_outlier_summary.xlsx"
write.xlsx(outlier_summary, file_path, sheetName = "Outlier Summary", rowNames = FALSE)

# Print the path to the file
file_path


# Task 3: Produce a merged spatial dataset and develop a map
# Steps:
  
###################### 3.1 ################################

# Load the Admin2-level map data
admin2_map <- st_read("~/Downloads/Written_test_Health/tza_adm2_map.geojson")

# Inspect the data to confirm column names
str(admin2_map)

# Aggregate to ADM1 level
admin1_map <- admin2_map %>%
  group_by(ADM1_EN) %>%
  summarise(geometry = st_union(geometry))

# Inspect the ADM1-level data
print(admin1_map)

# Create the ADM1-level map
ggplot(data = admin1_map) +
  geom_sf() +
  labs(
    title = "ADM1-level Map of Tanzania",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

###################### 3.2 ################################

# Load the weighted coverage data
coverage_data <- read_excel("~/Downloads/Written_test_Health/task1_coverage.xlsx")

# Inspect the coverage data
print(coverage_data)

###################### 3.2 ################################

# Create a lookup table for standardizing Admin 1 names
name_corrections <- data.frame(
  admin1_map = c("Name_in_Map1", "Name_in_Map2", "Name_in_Map3"),
  coverage_data = c("Name_in_Coverage1", "Name_in_Coverage2", "Name_in_Coverage3")
)

# Standardize the names in the coverage data
coverage_data <- coverage_data %>%
  mutate(ADM1_EN = recode(ADM1_EN, !!!setNames(name_corrections$coverage_data, name_corrections$admin1_map)))

# Inspect the standardized coverage data
print(coverage_data)

# Merge the ADM1-level map with the coverage data
merged_data <- admin1_map %>%
  left_join(coverage_data, by = "ADM1_EN")

# Inspect the merged data
print(merged_data)

###################### 3.2 ################################

# Plot the merged data
ggplot(data = merged_data) +
  geom_sf(aes(fill = ch_pent1_either)) +
  labs(
    title = "Penta 1 Coverage by ADM1 Region",
    fill = "Coverage (%)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Plot the merged data
ggplot(data = merged_data) +
  geom_sf(aes(fill = ch_pent1_either)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Coverage (%)") +
  labs(
    title = "Coverage of Penta 1 by Region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

