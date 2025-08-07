# Load packages ----------------------------------------------------------

library(tidyverse)
library(tigris)
library(sf)
library(janitor)

# Vaccination over time chart --------------------------------------------

# Vaccination comparison chart -------------------------------------------

vaccination_data <- tibble(
  name = c(
    "Alabama",
    "Alaska",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "Delaware",
    "Florida",
    "Georgia",
    "Hawaii",
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "South Dakota",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West Virginia",
    "Wisconsin",
    "Wyoming",
    "United States"
  ),
  coverage_percentage = c(
    87,
    91,
    89,
    85,
    93,
    92,
    95,
    94,
    88,
    86,
    96,
    82,
    91,
    88,
    90,
    89,
    87,
    84,
    94,
    97,
    96,
    90,
    93,
    83,
    87,
    86,
    91,
    89,
    95,
    94,
    88,
    92,
    89,
    88,
    90,
    86,
    93,
    91,
    95,
    87,
    89,
    88,
    87,
    90,
    96,
    94,
    94,
    85,
    91,
    84,
    92
  ),
  population = c(
    5024279,
    733391,
    7151502,
    3011524,
    39538223,
    5773714,
    3605944,
    989948,
    21538187,
    10711908,
    1455271,
    1839106,
    12812508,
    6785528,
    3190369,
    2937880,
    4505836,
    4657757,
    1395722,
    6177224,
    7001399,
    10037261,
    5737915,
    2961279,
    6196010,
    1084225,
    1961504,
    3104614,
    1395231,
    9288994,
    2117522,
    20201249,
    10439388,
    779094,
    11799448,
    3959353,
    4237256,
    13002700,
    1097379,
    5118425,
    909824,
    6910840,
    30029572,
    3271616,
    643077,
    8631393,
    7705281,
    1793716,
    5893718,
    576851,
    331900000
  )
)


df_vac <- left_join(vaccination_data, us_states, by = "name")

pal <- c("#FFD4B3", "#FFB580", "#FF954D", "#F56600")

create_vaccination_chart <- function(state_name, df_vac) {
  single_state <- df_vac |>
    filter(name == state_name)

  neighboring_states <- df_vac |>
    filter(st_touches(geometry, single_state$geometry, sparse = FALSE)[, 1]) |>
    filter(name != state_name) |>
    slice_max(population, n = 2) |>
    pull(name)

  chart_data <- df_vac |>
    filter(name %in% c(state_name, neighboring_states, "United States")) |>
    st_drop_geometry() |>
    mutate(
      name = factor(
        name,
        levels = c("United States", neighboring_states, state_name)
      )
    )

  ggplot(chart_data, aes(x = coverage_percentage, y = name)) +
    geom_col(width = 0.7, fill = "steelblue") +
    geom_text(
      aes(label = paste0(coverage_percentage, "%")),
      hjust = 1.5,
      size = 4
    ) +
    geom_vline(
      xintercept = 95,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.7
    ) +
    annotate(
      "text",
      x = 95,
      y = 0.5,
      label = "HP2030 Target: 95%",
      angle = 90,
      vjust = -0.5,
      size = 3,
      color = "gray50"
    ) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(
      title = paste("Vaccination comparison (2023)"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 10)
    )
}

create_vaccination_chart("Arizona", df_vac)
create_vaccination_chart("Georgia", df_vac)
create_vaccination_chart("Alaska", df_vac)


# Measles cases map ------------------------------------------------------

measles_data <- tibble(
  name = c(
    "Alabama",
    "Alaska",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "Delaware",
    "Florida",
    "Georgia",
    "Hawaii",
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "South Dakota",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West Virginia",
    "Wisconsin",
    "Wyoming"
  ),
  measles_cases = c(
    42,
    5,
    67,
    28,
    156,
    31,
    12,
    8,
    189,
    73,
    4,
    22,
    89,
    45,
    18,
    21,
    35,
    38,
    6,
    15,
    23,
    71,
    29,
    26,
    48,
    11,
    14,
    24,
    7,
    52,
    16,
    142,
    78,
    5,
    83,
    32,
    27,
    95,
    4,
    39,
    8,
    54,
    198,
    19,
    3,
    61,
    47,
    9,
    41,
    4
  )
)

us_states <-
  states() |>
  clean_names() |>
  select(name)

df <- left_join(us_states, measles_data, by = "name")

get_neighboring_states <- function(state) {
  single_state <-
    df |>
    filter(name == state)

  df |>
    filter(st_touches(geometry, single_state$geometry, sparse = FALSE)[, 1]) |>
    bind_rows(single_state) |>
    mutate(
      measles_category = case_when(
        measles_cases == 0 ~ "0",
        measles_cases >= 1 & measles_cases <= 9 ~ "1-9",
        measles_cases >= 10 & measles_cases <= 49 ~ "10-49",
        measles_cases >= 50 & measles_cases <= 99 ~ "50-99",
        measles_cases >= 100 & measles_cases <= 249 ~ "100-249",
        measles_cases >= 250 ~ "250+",
        TRUE ~ NA_character_
      ),
      # Convert to factor to control order
      measles_category = factor(
        measles_category,
        levels = c("0", "1-9", "10-49", "50-99", "100-249", "250+")
      )
    ) |>
    ggplot(aes(fill = measles_category)) +
    geom_sf() +
    geom_sf_text(aes(label = name), color = "black", size = 3) +
    scale_fill_manual(
      values = c(
        "0" = "#FFF2E6",
        "1-9" = "#FFD4B3",
        "10-49" = "#FFB580",
        "50-99" = "#FF954D",
        "100-249" = "#F56600",
        "250+" = "#CC5500"
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


get_neighboring_states("California")
get_neighboring_states("Oregon")
