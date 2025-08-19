library(tidyverse)
library(tigris)
library(sf)
library(janitor)
library(tidycensus)
library(showtext)


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

m_pal <- c(
  "0" = "#B8B8B8", # gray for zero cases
  "1-5" = "#b2e0ff",
  "6-15" = "#8ebce3",
  "16-30" = "#6b98c7",
  "31-60" = "#4775aa",
  "61-200" = "#24518e",
  "200+" = "#002d72"
)

measles_pal <- setNames(
  m_pal[c("0", "1-5", "6-15", "16-30", "31-60", "61-200", "200+")],
  c("0", "1-5", "6-15", "16-30", "31-60", "61-200", "200+")
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

get_nearest_states <- function(state, k = 5, pool = df) {
  target <- pool |> filter(name == state)
  others <- pool |> filter(name != state)

  # distance in meters (s2 great-circle if geometry is lon/lat)
  dmat <- st_distance(target, others)
  others |>
    mutate(dist_m = as.numeric(dmat[1, ])) |>
    slice_min(dist_m, n = k)
}


get_neighboring_states <- function(state, k_nearest = 5, min_neighbors = 2) {
  single_state <- df |> filter(name == state)

  if (state %in% c("Alaska", "Hawaii", "Puerto Rico")) {
    result <- bind_rows(single_state, get_nearest_states(state, k = k_nearest))
  } else {
    touching_neighbors <- df |>
      filter(sf::st_touches(geometry, single_state$geometry, sparse = FALSE)[,
        1
      ])
    if (nrow(touching_neighbors) < min_neighbors) {
      cat(
        "State",
        state,
        "has only",
        nrow(touching_neighbors),
        "touching neighbors. Adding nearest states.\n"
      )

      needed <- min_neighbors - nrow(touching_neighbors)
      nearest_states <- get_nearest_states(state, k = needed + 2) # Get extra in case some overlap

      result <- bind_rows(single_state, touching_neighbors, nearest_states) |>
        distinct(name, .keep_all = TRUE)
    } else {
      result <- bind_rows(single_state, touching_neighbors)
    }
  }

  result |> distinct(name, .keep_all = TRUE)
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
        total == 0 ~ "0",
        total >= 1 & total <= 5 ~ "1-5",
        total >= 6 & total <= 15 ~ "6-15",
        total >= 16 & total <= 30 ~ "16-30",
        total >= 31 & total <= 60 ~ "31-60",
        total >= 61 & total <= 200 ~ "61-200",
        total >= 201 ~ "200+",
        TRUE ~ NA_character_
      ),
      measles_category = factor(
        measles_category,
        levels = names(measles_pal) # puts "0" first, then ascending bins
      )
    ) |>
    ggplot(aes(fill = measles_category)) +
    geom_sf(color = "white") +
    geom_sf_text(
      aes(label = name),
      color = "black",
      size = 3,
      family = "Gentona"
    ) +
    scale_fill_manual(
      values = measles_pal,
      name = "Measles cases",
      na.value = "grey90",
      breaks = names(measles_pal),
      labels = c(
        "0",
        "1–5",
        "6–15",
        "16–30",
        "31–60",
        "61–200",
        "200+"
      )
    ) +
    theme_minimal(base_family = "Gentona") +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.key.width = grid::unit(1.3, "cm")
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        title.position = "top",
        label.position = "bottom"
      )
    )
}
# measles_map("Utah")
# measles_map("Puerto Rico")

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


# mmr_vaccination_comparison_chart("Maine")
# mmr_vaccination_comparison_chart("Alaska")

#------------------------------------------------------------------------------

mmr_line_df <- mmr_coverage_final |>
  select(geography, school_year, estimate_percent)


mmr_vaccination_over_time_chart_line <- function(state_name) {
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
    geom_line(color = "#002D72", size = 0.5) +
    geom_point(color = "#002D72", size = 1) +
    geom_text(
      aes(label = paste0(round(estimate_percent, 0), "%")),
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


# mmr_vaccination_over_time_chart_line("Delaware")
# mmr_vaccination_over_time_chart_line("Alabama")
# mmr_vaccination_over_time_chart_line("California")

## Bar chart

mmr_line_df <- mmr_coverage_final |>
  select(geography, school_year, estimate_percent)

mmr_vaccination_over_time_chart_bar <- function(state_name) {
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

  n_x <- nlevels(state_data$school_year)

  ggplot(state_data, aes(x = school_year, y = estimate_percent)) +
    geom_hline(
      yintercept = 95,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_col(fill = "#002D72", width = 0.4) +
    geom_text(
      aes(
        y = estimate_percent,
        label = paste0(round(estimate_percent, 0), "%")
      ),
      vjust = 2,
      color = "white",
      fontface = "bold",
      family = "Gentona",
      size = 3
    ) +
    annotate(
      "text",
      x = min(5, n_x),
      y = 96,
      label = "HP2030 Target: 95%",
      family = "Gentona",
      size = 3,
      color = "gray30",
      fontface = "bold",
      vjust = -1.5
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      title = glue::glue("Vaccination in {state_name} over time")
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, family = "Gentona"),
      axis.text.x = element_text(size = 10, family = "Gentona"),
      axis.text.y = element_text(size = 10, family = "Gentona"),
      axis.title = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# mmr_vaccination_over_time_chart_bar("Delaware")
# mmr_vaccination_over_time_chart_bar("Indiana")

# Lollipop

mmr_vaccination_over_time_chart_lollipop <- function(state_name) {
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

  ggplot(state_data, aes(x = school_year, y = estimate_percent)) +
    geom_hline(
      yintercept = 95,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_segment(
      aes(xend = school_year, y = 0, yend = estimate_percent),
      linewidth = 1,
      lineend = "round",
      color = "#68ACE5"
    ) +
    geom_point(
      shape = 21,
      size = 8,
      stroke = 1,
      fill = "#68ACE5",
      color = "#68ACE5"
    ) +
    geom_text(
      aes(label = paste0(round(estimate_percent, 0), "%")),
      color = "black",
      fontface = "bold",
      family = "Gentona",
      size = 3
    ) +
    annotate(
      "text",
      x = length(unique(state_data$school_year)) * 0.85,
      y = 91.5,
      label = "HP2030 Target: 95%",
      vjust = -5,
      hjust = -0.5,
      size = 2.8,
      color = "gray30",
      family = "Gentona",
      fontface = "bold"
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = glue::glue("Vaccination in {state_name} over time")
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
      axis.text.x = element_text(size = 10, family = "Gentona"),
      axis.text.y = element_text(size = 10, family = "Gentona"),
      axis.title = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}


#mmr_vaccination_over_time_chart_lollipop("Delaware")

#---------------------------------------------------------------------------------------

## DTap

df_dtap_coverage_final <- dtap_coverage_final |>
  select(geography, birth_year_birth_cohort, estimate_percent) |>
  filter(birth_year_birth_cohort == "2021")

df_dtap <- left_join(
  df_dtap_coverage_final,
  population_by_state,
  by = c("geography" = "state")
)


dtap_vaccination_comparison_chart <- function(state_name) {
  neighboring_data <- get_neighboring_states(state_name) |>
    st_drop_geometry() |>
    filter(name != state_name) |>
    left_join(df_dtap, by = c("name" = "geography")) |>
    filter(!is.na(total_population)) |>
    slice_max(total_population, n = 2) |>
    pull(name)

  # Prepare chart data
  chart_data <- df_dtap |>
    filter(geography %in% c(state_name, neighboring_data, "United States")) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent), # Convert to numeric
      geography = factor(
        geography,
        levels = unique(c("United States", neighboring_data, state_name))
      ),
      bar_color = ifelse(geography == "United States", "#FF9E1B", "#002D72")
    )

  # Create the chart
  ggplot(chart_data, aes(x = estimate_percent, y = geography)) +
    geom_vline(
      xintercept = 90,
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
      label = "HP2030 Target: 90%",
      vjust = -0.7,
      hjust = 1.2,
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
      title = paste("Vaccination comparison (2021)"),
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

# Usage:
# dtap_vaccination_comparison_chart("Montana")
# dtap_vaccination_comparison_chart("Maine")

# dtap_vaccination_comparison_chart("Massachusetts")
# dtap_vaccination_comparison_chart("Puerto Rico")

#------------------------------------------------------------------------------------------

dtap_line_df <- dtap_coverage_final |>
  select(geography, birth_year_birth_cohort, estimate_percent)

dtap_vaccination_over_time_chart_line <- function(state_name) {
  state_data <- dtap_line_df |>
    filter(geography == state_name) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent),
      birth_year_birth_cohort = factor(
        birth_year_birth_cohort,
        levels = unique(birth_year_birth_cohort[order(birth_year_birth_cohort)])
      )
    )

  n_x <- nlevels(state_data$birth_year_birth_cohort)

  ggplot(
    state_data,
    aes(x = birth_year_birth_cohort, y = estimate_percent, group = 1)
  ) +
    geom_hline(
      yintercept = 90,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_line(color = "#002D72", linewidth = 0.5) +
    geom_point(color = "#002D72", size = 3) +
    geom_text(
      aes(label = paste0(round(estimate_percent, 0), "%")),
      vjust = -1,
      color = "#002D72",
      fontface = "bold",
      family = "Gentona",
      size = 3
    ) +
    annotate(
      "text",
      x = min(5, n_x),
      y = 91,
      label = "HP2030 Target: 90%",
      family = "Gentona",
      size = 3,
      color = "gray30",
      fontface = "bold",
      vjust = -1.5
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    labs(title = glue::glue("Vaccination in {state_name} over time")) +
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
      axis.text.x = element_text(size = 10, family = "Gentona"),
      axis.text.y = element_text(size = 10, family = "Gentona"),
      axis.title = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

#dtap_vaccination_over_time_chart_line("Delaware")

# Bar chart

dtap_line_df <- dtap_coverage_final |>
  select(geography, birth_year_birth_cohort, estimate_percent)

dtap_vaccination_over_time_chart_bar <- function(state_name) {
  state_data <- dtap_line_df |>
    filter(geography == state_name) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent),
      birth_year_birth_cohort = factor(
        birth_year_birth_cohort,
        levels = unique(birth_year_birth_cohort[order(birth_year_birth_cohort)])
      )
    )

  n_x <- nlevels(state_data$birth_year_birth_cohort)

  ggplot(state_data, aes(x = birth_year_birth_cohort, y = estimate_percent)) +
    geom_hline(
      yintercept = 90,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_col(fill = "#002D72", width = 0.4) +
    geom_text(
      aes(label = paste0(round(estimate_percent, 0), "%")),
      vjust = 2,
      color = "white",
      fontface = "bold",
      family = "Gentona",
      size = 3
    ) +
    annotate(
      "text",
      x = min(5, n_x),
      y = 91,
      label = "HP2030 Target: 90%",
      family = "Gentona",
      size = 3,
      color = "gray30",
      fontface = "bold",
      vjust = -1.5
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    labs(title = glue::glue("Vaccination in {state_name} over time")) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, family = "Gentona"),
      axis.text.x = element_text(size = 10, family = "Gentona"),
      axis.text.y = element_text(size = 10, family = "Gentona"),
      axis.title = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}


#dtap_vaccination_over_time_chart_bar("Delaware")

# Lollipop Chart

dtap_vaccination_over_time_chart_lollipop <- function(state_name) {
  state_data <- dtap_line_df |>
    filter(geography == state_name) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent),
      birth_year_birth_cohort = factor(
        birth_year_birth_cohort,
        levels = unique(birth_year_birth_cohort[order(birth_year_birth_cohort)])
      )
    )

  n_x <- nlevels(state_data$birth_year_birth_cohort)

  ggplot(
    state_data,
    ggplot2::aes(x = birth_year_birth_cohort, y = estimate_percent)
  ) +
    geom_hline(
      yintercept = 90,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_segment(
      aes(xend = birth_year_birth_cohort, y = 0, yend = estimate_percent),
      linewidth = 1,
      lineend = "round",
      color = "#68ACE5"
    ) +
    geom_point(
      shape = 21,
      size = 8,
      stroke = 1,
      fill = "#68ACE5",
      color = "#68ACE5"
    ) +
    geom_text(
      aes(label = paste0(round(estimate_percent, 0), "%")),
      color = "black",
      fontface = "bold",
      family = "Gentona",
      size = 3
    ) +
    annotate(
      "text",
      x = min(5, n_x),
      y = 91,
      label = "HP2030 Target: 90%",
      family = "Gentona",
      size = 3,
      color = "gray30",
      fontface = "bold",
      vjust = -1.5
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(title = glue::glue("Vaccination in {state_name} Over Time")) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, family = "Gentona"),
      axis.text.x = element_text(size = 10, family = "Gentona"),
      axis.text.y = element_text(size = 10, family = "Gentona"),
      axis.title = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

#dtap_vaccination_over_time_chart_lollipop("Delaware")
