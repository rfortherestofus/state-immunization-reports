# Load packages ----------------------------------------------------------

library(tidyverse)
library(tigris)
library(sf)
library(janitor)
library(tidycensus)

census_api_key(Sys.getenv("TIDYCENSUS_API_KEY"), install = FALSE)

total_measles_cases <- read_csv(
  "https://raw.githubusercontent.com/rfortherestofus/state-immunization-data/refs/heads/main/data-clean/total_measles_cases.csv"
)

mmr_coverage_final <- read_csv(
  "https://raw.githubusercontent.com/rfortherestofus/state-immunization-data/refs/heads/main/data-clean/mmr_coverage_final.csv"
)

dtap_coverage_final <- read_csv(
  "https://raw.githubusercontent.com/rfortherestofus/state-immunization-data/refs/heads/main/data-clean/dtap_coverage_final.csv"
)


pal <- c(
  "#002D72",
  "#68ACE5",
  "#FF6900",
  "#A7BCD6",
  "#FF9E1B",
  "#E5E2E0",
  "#6E6C6F",
  "#4A484C",
  "#E0EEF9",
  "#FFE1CC"
)


# Measles cases map ------------------------------------------------------

us_states <-
  states() |>
  clean_names() |>
  select(name)

population_by_state <-
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

df <- left_join(us_states, total_measles_cases, by = c("name" = "state"))

# Define the get_neighboring_states function outside of measles_map
get_neighboring_states <- function(state) {
  single_state <-
    df |>
    filter(name == state)

  df |>
    filter(st_touches(geometry, single_state$geometry, sparse = FALSE)[, 1]) |>
    bind_rows(single_state)
}

# Create the measles_map function that uses get_neighboring_states
measles_map <- function(state) {
  get_neighboring_states(state) |>
    st_cast("POLYGON") |>
    group_by(name) |>
    mutate(area = st_area(geometry)) |>
    slice_max(area, n = 1) |>
    ungroup() |>
    select(-area) |>
    mutate(geometry = st_simplify(geometry, dTolerance = 1000)) |>
    mutate(
      measles_category = case_when(
        total_cases <= 5 ~ "≤5",
        total_cases >= 6 & total_cases <= 15 ~ "6-15",
        total_cases >= 16 & total_cases <= 30 ~ "16-30",
        total_cases >= 31 & total_cases <= 60 ~ "31-60",
        total_cases >= 61 & total_cases <= 200 ~ "61-200",
        total_cases >= 201 ~ "200+",
        TRUE ~ NA_character_
      ),
      measles_category = factor(
        measles_category,
        levels = c("≤5", "6-15", "16-30", "31-60", "61-200", "200+")
      )
    ) |>
    ggplot(aes(fill = measles_category)) +
    geom_sf() +
    geom_sf_text(aes(label = name), color = "black", size = 3) +
    scale_fill_manual(
      values = c(
        "≤5" = "#D6E0F0",
        "6-15" = "#A9BEDC",
        "16-30" = "#7B9CC8",
        "31-60" = "#4E7AB4",
        "61-200" = "#2158A0",
        "200+" = "#002D72"
      ),
      name = "Measles Cases",
      na.value = "grey90"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.key.width = unit(1.5, "cm")
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        title.position = "top",
        label.position = "bottom"
      )
    )
}

# Usage:
measles_map("Minnesota")

#-----------------------------------------------------------------------------

df_mmr_coverage_final <- mmr_coverage_final |>
  select(geography, school_year, estimate_percent) |>
  filter(school_year == "2024-25")

df_mmr <- left_join(
  df_mmr_coverage_final,
  population_by_state,
  by = c("geography" = "state")
)


mmr_vaccination_comparison_chart <- function(state_name) {
  neighboring_data <- get_neighboring_states(state_name) |>
    st_drop_geometry() |>
    filter(name != state_name) |>
    left_join(df_mmr, by = c("name" = "geography")) |>
    filter(!is.na(total_population)) |>
    slice_max(total_population, n = 2) |>
    pull(name)

  chart_data <- df_mmr |>
    filter(geography %in% c(state_name, neighboring_data, "United States")) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent), # Convert to numeric
      geography = factor(
        geography,
        levels = c("United States", neighboring_data, state_name)
      ),
      bar_color = ifelse(geography == "United States", "#FF9E1B", "#002D72")
    )

  ggplot(chart_data, aes(x = estimate_percent, y = geography)) +
    geom_vline(
      xintercept = 95,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_col(width = 0.5, aes(fill = bar_color)) +
    scale_fill_identity() + # Use the colors we specified
    geom_text(
      aes(label = paste0(round(estimate_percent), "%")),
      hjust = 1.5,
      size = 4,
      fontface = "bold",
      color = "white",
      family = "Gentona"
    ) +
    geom_text(
      aes(label = geography, x = 2),
      hjust = 0,
      size = 4,
      color = "white",
      family = "Gentona"
    ) +
    annotate(
      "text",
      x = 91,
      y = 0.2,
      label = "HP2030 Target: 95%",
      vjust = -0.7,
      hjust = 0.9,
      size = 2.5,
      color = "gray30",
      family = "Gentona",
      fontface = "bold"
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = function(x) ifelse(x == 0, "0%", as.character(x))
    ) +
    labs(
      title = paste("Vaccination comparison (2024)"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, family = "Gentona"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(
        size = 10,
        family = "Gentona",
        margin = margin(t = 10)
      ),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}


mmr_vaccination_comparison_chart("Maryland")
mmr_vaccination_comparison_chart("Massachusetts")

#------------------------------------------------------------------------------

mmr_line_df <- mmr_coverage_final |>
  select(geography, school_year, estimate_percent)


mmr_vaccination_over_time_chart <- function(state_name) {
  state_data <- mmr_line_df |>
    filter(geography == state_name) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent),
      school_year = factor(
        school_year,
        levels = unique(school_year[order(school_year)])
      )
    )

  if (nrow(state_data) == 0) {
    stop(paste("No data found for state:", state_name))
  }

  ggplot(state_data, aes(x = school_year, y = estimate_percent, group = 1)) +
    geom_hline(
      yintercept = 95,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_line(color = "#002D72", linewidth = 1) +
    geom_point(color = "#002D72", size = 2) +
    geom_text(
      aes(label = paste0(estimate_percent, "%")),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold",
      color = "#002D72",
      family = "Gentona"
    ) +
    annotate(
      "text",
      x = length(unique(state_data$school_year)) * 0.85,
      y = 91.5,
      label = "HP2030 Target: 95%",
      vjust = 2,
      hjust = 0.5,
      size = 2.8,
      color = "gray30",
      family = "Gentona",
      fontface = "bold"
    ) +
    scale_y_continuous(
      limits = c(0, max(state_data$estimate_percent) + 3),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste(state_name, "Vaccination Rates Over Time"),
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, family = "Gentona"),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 11,
        family = "Gentona",
        color = "gray40"
      ),
      axis.text.x = element_text(size = 10, family = "Gentona", hjust = 1),
      axis.text.y = element_text(size = 10, family = "Gentona"),
      axis.title = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}


mmr_vaccination_over_time_chart("Texas")
mmr_vaccination_over_time_chart("Alabama")
mmr_vaccination_over_time_chart("California")
