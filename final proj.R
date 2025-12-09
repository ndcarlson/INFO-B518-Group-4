### -------------------------------------------------------
### Project: COVID-19 Hospitalizations in France
### Purpose: Statistical Analysis for Final Report
### -------------------------------------------------------

## Required packages
library(dplyr)
library(ggplot2)
library(readr)
library(knitr)

### -------------------------------------------------------
### 1. Load Data
### -------------------------------------------------------

df <- read_csv(
  "https://drive.google.com/uc?export=download&id=1rXHdGEDWFAMaitmkNSgehAt_e2FaC_PZ",
  show_col_types = FALSE
)

### -------------------------------------------------------
### 2. Data Cleaning
### -------------------------------------------------------

# Translate granularity labels
df <- df %>%
  mutate(
    granularity = recode(
      granularity,
      "pays" = "country",
      "departement" = "department",
      "collectivite-outremer" = "overseas_collectivity",
      "region" = "region",
      "monde" = "world"
    )
  )

# Columns expected to be numeric
num_cols <- intersect(
  c("hospitalized","icu_patients","deaths","recovered",
    "new_hospitalizations","new_icu_admissions",
    "confirmed_cases","tested"),
  names(df)
)

df <- df %>%
  mutate(
    date = as.Date(date),
    across(all_of(num_cols), ~ suppressWarnings(as.numeric(.)))
  )

# Filter to national-level
national <- df %>%
  filter(granularity == "country", location_code == "FRA") %>%
  arrange(date)

### -------------------------------------------------------
### 3. Descriptive Statistics (Table)
### -------------------------------------------------------

summary_table <- national %>%
  summarise(
    mean_hosp = mean(hospitalized, na.rm = TRUE),
    median_hosp = median(hospitalized, na.rm = TRUE),
    sd_hosp = sd(hospitalized, na.rm = TRUE),
    mean_new = mean(new_hospitalizations, na.rm = TRUE),
    median_new = median(new_hospitalizations, na.rm = TRUE),
    sd_new = sd(new_hospitalizations, na.rm = TRUE)
  )

print("Descriptive Summary Table:")
kable(summary_table)

### -------------------------------------------------------
### 4. Visualizing Trend Patterns
### -------------------------------------------------------

ggplot(national, aes(x = date, y = hospitalized)) +
  geom_line(color = "blue") +
  labs(title = "Daily Hospitalized Patients in France",
       x = "Date", y = "Patients")

ggplot(national, aes(x = date, y = new_hospitalizations)) +
  geom_line(color = "red") +
  labs(title = "Daily New Hospital Admissions in France",
       x = "Date", y = "New Admissions")


### -------------------------------------------------------
### 6. Correlation Test
### -------------------------------------------------------

temp <- national[, c("new_hospitalizations", "hospitalized")] %>%
  na.omit()

cor_result <- cor.test(
  temp$new_hospitalizations,
  temp$hospitalized
)

print("Correlation Test Between Hospital Load and Admissions:")
print(cor_result)


### -------------------------------------------------------
### 7. Simple Linear Regression
### -------------------------------------------------------

model <- lm(hospitalized ~ new_hospitalizations, data = national)
print(summary(model))

model_ci <- confint(model)
print("95% Confidence Intervals for Regression Estimates:")
print(model_ci)

### -------------------------------------------------------
### 8. Confidence Interval for Mean Hospitalization
### -------------------------------------------------------

ci_hosp <- t.test(national$hospitalized)
print("Confidence Interval for Mean Hospitalized Count:")
print(ci_hosp)

### -------------------------------------------------------
### END OF ANALYSIS
### -------------------------------------------------------

