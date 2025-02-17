library(dplyr)
library(lubridate)
library(janitor)

corn_raw <- read.csv("corn_8_21.csv")#this file is the raw data with latest year (2023)
str(corn_raw)
summary (corn_raw)

#this transforms all the data to a number more than 0 if they are BDL or 0
corn <- corn_raw %>%
  mutate(vom = ifelse(vom == "BDL", 0.2, vom))%>%
  mutate(vom = ifelse(vom == 0, 0.2, vom)) %>%
  mutate(vom = ifelse(vom == "BDl", 0.2, vom)) %>%
  mutate(vom=ifelse(vom =="<0.2", 0.2, vom))%>%
  mutate(vom=ifelse(vom ==".", 0.2, vom))%>%
  filter(`lat` != "NA") %>%
  filter(`lon` != "NA") %>%
  clean_names() #will underscore all names

#check the data
head(corn)
str(corn)
summary(corn)

###################################
##reordering and renaming columns##
###################################

corn <- corn[, !names(corn) %in% "fungicide"]
corn <- corn[, !names(corn) %in% "insecticide"]
corn <- corn[, !names(corn) %in% "tillage"]
corn <- corn[, !names(corn) %in% "hybrid_company"]
corn <- corn[, !names(corn) %in% "hybrid_name"]
corn <- corn[, !names(corn) %in% "p_crop"]
corn <- corn %>%
  rename(latitude = lat, longitude = lon) 
corn <- corn %>% filter(vom!= "NA")

#convert variables to factor or numerical 

corn$year <- as.factor(corn$year)
corn$vom <- as.double(corn$vom)
corn$chu <- as.double(corn$chu)
corn$silk_chu <- as.double(corn$silk_chu)
corn$sd <- as.Date(corn$sd, format = "%m/%d/%Y")
str(corn)
summary (corn)

# Calculate average ratio of silk_chu / chu
avg_ratio <- mean(corn$silk_chu / corn$chu, na.rm = TRUE)
avg_ratio
# Impute missing silk_chu based on chu and average ratio
corn <- corn %>%
  mutate(imputed_silk_chu = ifelse(is.na(silk_chu), chu * avg_ratio, NA_real_))
# Merge imputed_silk_chu into silk_chu, keeping original values intact
corn_merged <- corn %>%
  mutate(silk_chu = coalesce(silk_chu, imputed_silk_chu)) %>%
  select(-imputed_silk_chu)  # Remove the imputed_silk_chu column if no longer needed
summary (corn_merged)

# Step 3: Merge weather data after ensuring Date is properly converted
weather <- read.csv("weather_daymet_corn.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))  # Assuming YYYY-MM-DD format

# Ensure all dates in weather are correctly formatted
stopifnot(!any(is.na(weather$Date)))

# Merge corn and weather data
combined_data <- merge(corn_merged, weather, by = "number")

# Step 4: Ensure 'sd' and 'Date' are in Date format, handle missing sd by setting it to May 1st
combined_data <- combined_data %>%
  mutate(
    sd = ifelse(is.na(sd), as.Date(paste(year, "05", "01", sep = "-")), as.Date(sd)),
    Date = as.Date(Date)
  )

# Step 4: Calculate CHU from the seeding date onward or May 1st
combined_data <- combined_data %>%
  mutate(
    ymin = ifelse(is.na(Tmin) | Tmin <= 4.4, 0, 1.8 * (Tmin - 4.4)),
    ymax = ifelse(is.na(Tmax) | Tmax <= 10, 0, 3.33 * (Tmax - 10) - 0.084 * (Tmax - 10)^2),
    daily_chu = ifelse(Date >= sd, (ymin + ymax) / 2, 0)
  )

# Step 5: Calculate cumulative CHU
combined_data <- combined_data %>%
  arrange(number, Date) %>%
  group_by(number) %>%
  mutate(cumulative_chu = cumsum(daily_chu)) %>%
  ungroup()

# Step 6: Identify silking date
combined_data <- combined_data %>%
  group_by(number) %>%
  mutate(
    silking_date = first(Date[cumulative_chu >= silk_chu & !is.na(silk_chu)], default = NA_Date_)
  ) %>%
  ungroup()

# Step 7: Summarize total CHU and silking date, then merge back into the original dataset
chu_summary <- combined_data %>%
  group_by(number) %>%
  summarize(
    total_chu = if (all(is.na(cumulative_chu))) NA_real_ else max(cumulative_chu, na.rm = TRUE),
    silking_date = first(silking_date, default = NA)
  )

corn_updated <- merge(corn, chu_summary, by = "number", all.x = TRUE)

# corn_updated already has both silk_chu and imputed_silk_chu
corn_updated <- corn_updated %>%
  mutate(
    silk_chu = coalesce(silk_chu, imputed_silk_chu)  # Merge into silk_chu, prioritizing original values
  ) %>%
  select(-imputed_silk_chu)  # Optionally remove the imputed_silk_chu column if it's no longer needed

corn_updated <- corn_updated %>%
  mutate(
    sd = as.Date(sd, format = "%Y-%m-%d"),  # Adjust the format as needed
    silking_date = as.Date(silking_date, format = "%Y-%m-%d")
  )

# Now 'silk_chu' contains the merged values, and 'imputed_silk_chu' is removed
# Check the result to ensure it looks correct
summary(corn_updated$silk_chu)
head(corn_updated)
summary (corn_updated)

# Step 8: Export the final data to a CSV file
write.csv(corn_updated, "corn_updated.csv", row.names = FALSE)

#adding weather variables 
library(dplyr)
library(tidyr)  # for pivot_wider to reshape data

# Step 1: Filter for relevant months (January to July)
relevant_months <- weather %>%
  filter(Month %in% 1:7)

# Step 2: Calculate monthly means and sum events
monthly_stats <- relevant_months %>%
  group_by(number, Month) %>%
  summarize(
    Mean_PP = mean(PP, na.rm = TRUE),
    Mean_Rad = mean(Rad, na.rm = TRUE),
    Mean_SWE = mean(SWE, na.rm = TRUE),
    Mean_VPD = mean(VPD, na.rm = TRUE),
    Mean_Tmean = mean(Tmean, na.rm = TRUE),
    Sum_EPE_i = sum(EPE_i, na.rm = TRUE),  # Sum of EPE_i events
    Sum_ETE_i = sum(ETE_i, na.rm = TRUE),  # Sum of ETE_i events
    .groups = 'drop'
  ) %>%
  # Pivot data to wide format to create separate columns for each month for each variable
  pivot_wider(
    names_from = Month,
    values_from = c(Mean_PP, Mean_Rad, Mean_SWE, Mean_VPD, Mean_Tmean, Sum_EPE_i, Sum_ETE_i),
    names_glue = "{.value}_Month_{Month}"
  )

# Step 3: Merge the aggregated data back to corn_updated
corn_updated_with_weather <- merge(corn_updated, monthly_stats, by = "number", all.x = TRUE)

# Check the final merged data
head(corn_updated_with_weather)
summary(corn_updated_with_weather)
#removing SWE for months of 6 and 7
corn_updated_with_weather <- corn_updated_with_weather %>%
  select(-Mean_SWE_Month_6, -Mean_SWE_Month_7)  # Exclude these columns

# Check the structure to confirm removal
str(corn_updated_with_weather)

# export the final data
write.csv(corn_updated_with_weather, "predction_dataset.csv", row.names = FALSE)
