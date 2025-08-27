library(tidyverse)
library(tigris)
library(sf)
library(janitor)
library(tidycensus)
library(showtext)
library(ggrepel)
library(ggfx)


pal <- c(
  "#002D72",
  #"#68ACE5",
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
  "1-9" = "#8ebce3",
  "10-49" = "#6b98c7",
  "50-99" = "#4775aa",
  "100-249" = "#24518e",
  "250+" = "#002d72"
)

measles_pal <- setNames(
  m_pal[c("0", "1-9", "10-49", "50-99", "100-249", "250+")],
  c("0", "1-9", "10-49", "50-99", "100-249", "250+")
)

get_nearest_states <- function(state, k = 5, pool = df) {
  target <- pool |> filter(name == state)
  others <- pool |> filter(name != state)

  # distance in meters (s2 great-circle if geometry is lon/lat)
  dmat <- st_distance(target, others)
  others |>
    mutate(dist_m = as.numeric(dmat[1, ])) |>
    slice_min(dist_m, n = k)
}


get_neighboring_states <- function(
  df,
  state,
  k_nearest = 5,
  min_neighbors = 2
) {
  single_state <- df |> filter(name == state)
  if (state %in% c("Alaska", "Hawaii", "Puerto Rico")) {
    result <- bind_rows(single_state, get_nearest_states(state, k = k_nearest))
  } else {
    touching_neighbors <- df |>
      filter(sf::st_touches(geometry, single_state$geometry, sparse = FALSE)[,
        1
      ])
    if (nrow(touching_neighbors) < min_neighbors) {
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
measles_map <- function(df, state) {
  df_plot <- get_neighboring_states(df, state) |>
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
        total >= 1 & total <= 9 ~ "1-9",
        total >= 10 & total <= 49 ~ "10-49",
        total >= 50 & total <= 99 ~ "50-99",
        total >= 100 & total <= 249 ~ "100-249",
        total >= 250 ~ "250+",
        TRUE ~ NA_character_
      ),
      measles_category = factor(
        measles_category,
        levels = names(measles_pal) # puts "0" first, then ascending bins
      )
    )

  if (state == "Alaska") {
    alpha <- 0.2
    scale_factor <- 2
    ak_idx <- which(df_plot$name == "Alaska")
    neigh_idx <- which(df_plot$name != "Alaska")
    ak_cent <- st_coordinates(st_centroid(st_geometry(df_plot)[ak_idx]))
    neigh_cent <- st_coordinates(st_centroid(st_union(st_geometry(df_plot)[
      neigh_idx
    ])))
    geoms <- st_geometry(df_plot)
    geoms[neigh_idx] <- lapply(geoms[neigh_idx], function(g) {
      (g - c(neigh_cent[1], neigh_cent[2])) *
        scale_factor +
        c(neigh_cent[1], neigh_cent[2])
    })
    delta <- (ak_cent - neigh_cent) * alpha
    geoms[neigh_idx] <- lapply(geoms[neigh_idx], function(g) {
      g + c(delta[1], delta[2])
    })
    st_geometry(df_plot) <- geoms
  }

  if (state == "Hawaii") {
    hi_idx <- which(df_plot$name == "Hawaii")
    neigh_idx <- which(df_plot$name != "Hawaii")
    geoms <- st_geometry(df_plot)

    hi_cent <- st_coordinates(st_centroid(st_union(geoms[hi_idx])))
    neigh_cent <- st_coordinates(st_centroid(st_union(geoms[neigh_idx])))

    scale_hi <- 4.5
    scale_neighbors <- 2.2
    pull_in <- 0.25

    geoms[hi_idx] <- lapply(geoms[hi_idx], function(g) {
      (g - c(hi_cent[1], hi_cent[2])) * scale_hi + c(hi_cent[1], hi_cent[2])
    })

    geoms[neigh_idx] <- lapply(geoms[neigh_idx], function(g) {
      (g - c(neigh_cent[1], neigh_cent[2])) *
        scale_neighbors +
        c(neigh_cent[1], neigh_cent[2])
    })

    delta <- (hi_cent - neigh_cent) * pull_in
    geoms[neigh_idx] <- lapply(geoms[neigh_idx], function(g) {
      g + c(delta[1], delta[2])
    })

    st_geometry(df_plot) <- geoms
  }

  df_centroids <- df_plot |>
    mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
    )

  med_val <- median(df_plot$total, na.rm = TRUE)

  df_centroids |>
    ggplot(aes(fill = measles_category)) +
    geom_sf(
      data = df_centroids |> filter(name != state),
      color = "white",
      size = 0.1
    ) +
    with_shadow(
      geom_sf(
        data = df_centroids |> filter(name == state),
        aes(fill = measles_category),
        linewidth = 0.5,
        color = "white"
      ),
      sigma = 0,
      x_offset = 3,
      y_offset = 3
    ) +
    ggrepel::geom_text_repel(
      aes(
        x = lon,
        y = lat,
        label = name,
        color = ifelse(total > med_val, "above", "below")
      ),
      size = 3.2,
      family = "Gentona",
      min.segment.length = 0,
      segment.color = "black",
      segment.size = 0.3,
      max.overlaps = Inf,
      force = 2,
      point.size = NA
    ) +
    scale_fill_manual(
      values = measles_pal,
      name = "Measles cases",
      na.value = "grey90",
      breaks = names(measles_pal),
      labels = c("0", "1-9", "10-49", "50-99", "100-249", "250+")
    ) +
    scale_color_manual(
      values = c("above" = "white", "below" = "black"),
      guide = "none"
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

#-----------------------------------------------------------------------------

mmr_vaccination_comparison_chart <- function(df_mmr, df, state_name) {
  neighboring_data <- get_neighboring_states(df, state_name) |>
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
      labels = scales::label_number(accuracy = 1, suffix = "%")
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

#------------------------------------------------------------------------------

mmr_vaccination_over_time_chart_line <- function(mmr_line_df, state_name) {
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

## Bar chart

mmr_vaccination_over_time_chart_bar <- function(mmr_line_df, state_name) {
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

# Lollipop

mmr_vaccination_over_time_chart_lollipop <- function(mmr_line_df, state_name) {
  state_data <- mmr_line_df |>
    filter(geography == state_name) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent),
      school_year = factor(
        school_year,
        levels = unique(school_year[order(school_year)])
      )
    )

  n_x <- nlevels(state_data$school_year)

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
      color = "#002D72"
    ) +
    geom_point(
      shape = 21,
      size = 8,
      stroke = 1,
      fill = "#002D72",
      color = "#002D72"
    ) +
    geom_text(
      aes(label = paste0(round(estimate_percent, 0), "%")),
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
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = glue::glue("Vaccination in {state_name} over time")
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
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

#---------------------------------------------------------------------------------------

## DTap

dtap_vaccination_comparison_chart <- function(df_dtap, df, state_name) {
  neighboring_data <- get_neighboring_states(df, state_name) |>
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
      labels = scales::label_number(accuracy = 1, suffix = "%")
    ) +
    labs(
      title = paste("Vaccination comparison (2023)"),
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

#------------------------------------------------------------------------------------------

dtap_vaccination_over_time_chart_line <- function(dtap_line_df, state_name) {
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

# Bar chart

dtap_vaccination_over_time_chart_bar <- function(dtap_line_df, state_name) {
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

# Lollipop Chart

dtap_vaccination_over_time_chart_lollipop <- function(
  dtap_line_df,
  state_name
) {
  state_data <- dtap_line_df |>
    filter(geography == state_name) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent),
      year = factor(
        year,
        levels = unique(year[order(year)])
      )
    )

  n_x <- nlevels(state_data$year)

  ggplot(
    state_data,
    ggplot2::aes(x = year, y = estimate_percent)
  ) +
    geom_hline(
      yintercept = 90,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_segment(
      aes(xend = year, y = 0, yend = estimate_percent),
      linewidth = 1,
      lineend = "round",
      color = "#002D72"
    ) +
    geom_point(
      shape = 21,
      size = 8,
      stroke = 1,
      fill = "#002D72",
      color = "#002D72"
    ) +
    geom_text(
      aes(label = paste0(round(estimate_percent, 0), "%")),
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
