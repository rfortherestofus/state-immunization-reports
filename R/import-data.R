# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(readr)
library(stringr)
library(dplyr)


# Measles -----------------------------------------------------------------

# CSV comes from https://publichealth.jhu.edu/ivac/resources/us-measles-tracker

# Import- measles cases dataset
measles_cases <- read_csv("data-raw/measles_cases_09_01.csv")
measles_cases <- janitor::clean_names(measles_cases)
total_measles_case <- measles_cases %>%
  select(state, total)


# Export data
write_csv(total_measles_case, "data-clean/total_measles_cases.csv")


# MMR Coverage ------------------------------------------------------------
# CSV comes from CDC's SchoolVaxView (https://data.cdc.gov/Vaccinations/Vaccination-Coverage-and-Exemptions-among-Kinderga/ijqb-a7ye/about_data)
# Import- MMR coverage data
mmr_coverage <- read_csv("data-raw/mmr_coverage.csv") |>
  clean_names()

# Desired years
target_years <- c(
  "2020-21",
  "2021-22",
  "2022-23",
  "2023-24",
  "2024-25"
)
# Filter the data
mmr_filtered <- mmr_coverage %>%
  filter(
    vaccine_exemption == "MMR",
    school_year %in% target_years,
    (geography_type == "States" | geography == "U.S. Median")
  )
# Preview filtered data
head(mmr_filtered)
# Additional target years for Montana
montana_years <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21")

montana_filtered <- mmr_coverage %>%
  filter(
    `vaccine_exemption` == "MMR",
    school_year %in% montana_years,
    geography == "Montana"
  )

# Add West Virginia for 2019-20
wv_filtered <- mmr_coverage %>%
  filter(
    `vaccine_exemption` == "MMR",
    school_year == "2019-20",
    geography == "West Virginia"
  )

mmr_combined <- bind_rows(mmr_filtered, wv_filtered, montana_filtered)

# Sort the dataset by descending years
mmr_filtered_sorted <- mmr_combined %>%
  arrange(geography, school_year)

# Export the dataset
write_csv(mmr_filtered_sorted, "data-clean/mmr_coverage_final.csv")


# Non-medical exemption rate-----------------------------------------------------------------
# Data comes from CDC's SchoolVaxView (same dataset as above)
# Filter for non-medical exemptions for 2023-2024 and 2024-2025

non_medical_exemptions <- mmr_coverage %>%
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2023-24", "2024-25"),
    (geography_type == "States" | geography == "U.S. Median")
  )
# New York: 2017-18 and 2018-19
ny_filtered <- mmr_coverage %>%
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2017-18", "2018-19"),
    geography == "New York"
  )

# Montana: 2019-20 and 2020-21
mt_filtered <- mmr_coverage %>%
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2019-20", "2020-21"),
    geography == "Montana"
  )

# California: 2015-16 and 2016-17
ca_filtered <- mmr_coverage %>%
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2015-16", "2016-17"),
    geography == "California"
  )

# Maine: 2022-23 and 2023-24
me_filtered <- mmr_coverage %>%
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2022-23", "2023-24"),
    geography == "Maine"
  )

# West Virginia: NA
wv_filtered <- mmr_coverage %>%
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2018-19", "2019-20"),
    geography == "West Virginia"
  )
non_medical_exemptions <- bind_rows(
  non_medical_exemptions,
  ny_filtered,
  mt_filtered,
  ca_filtered,
  me_filtered,
  wv_filtered
)

head(non_medical_exemptions)
# Export the dataset
write_csv(non_medical_exemptions, "data-clean/non_medical_exemption.csv")

# DTaP --------------------------------------------------------------------
# CSV comes from CDC's ChildVaxView (https://www.cdc.gov/childvaxview/about/interactive-reports.html)
# Import data set
dtap_coverage <- read_csv("data-raw/dtap_coverage.csv") |>
  clean_names()

# Filter
dtap_filtered_states <- dtap_coverage %>%
  filter(
    vaccine == "DTaP",
    dose == "≥4 Doses",
    dimension == "24 Months",
    birth_year_birth_cohort %in% c("2017", "2018", "2019", "2020", "2021")
  ) %>%
  arrange(geography, birth_year_birth_cohort)

# Export dataset
write_csv(dtap_filtered_states, "data-clean/dtap_coverage_final.csv")


# Vaccine Exemptions ------------------------------------------------------
# CSV from NCSL's brief (https://www.ncsl.org/health/state-non-medical-exemptions-from-school-immunization-requirements)

vaccine_exemptions <- read_csv("data-raw/non_medical_exemption_policies.csv")

# Export dataset
write_csv(
  vaccine_exemptions,
  "data-clean/non_medical_exemption_policies_final.csv"
)

# Health Spending ---------------------------------------------------------
# CSV from from 'America's health rankings'

# Import dataset-Public Health Spending
health_spending <- read_csv("data-raw/health_spending.csv")

# Export dataset
write_csv(health_spending, "data-clean/health_spending_final.csv")

# Universal vaccine purchase program------------------------------------------------------
# CSV from AIM resources page
universal_purchase <- read_csv("data-raw/universal_purchase.csv")

# Export dataset
write_csv(universal_purchase, "data-clean/universal_purchase_final.csv")

# State policies------------------------------------------------------
state_policies <- read_csv("data-raw/state_policies.csv")
names(state_policies) <- tolower(gsub(" ", "_", names(state_policies)))
state_policies_filtered <- state_policies %>%
  filter(`1_include_in_brief` %in% c(1, "1"))

# Export dataset
write_csv(state_policies, "data-clean/state_policies_final.csv")


# Census Data ------------------------------------------------------------

get_decennial(
  geography = "state",
  variables = "P1_001N", # Total population variable
  year = 2020,
  survey = "pl" # PL 94-171 Redistricting Data
) |>
  select(NAME, value) |>
  rename(
    state = NAME,
    total_population = value
  ) |>
  arrange(desc(total_population))
