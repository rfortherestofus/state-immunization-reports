library(tidyverse)
library(sf)
library(janitor)
library(ggfx)
library(tigris)


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

# -------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------
get_nearest_states <- function(state, k = 5, pool) {
  target <- pool |> filter(name == state)
  others <- pool |> filter(name != state)

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
    result <- bind_rows(
      single_state,
      get_nearest_states(state, k = k_nearest, pool = df)
    )
  } else {
    touching_neighbors <- df |>
      filter(sf::st_touches(geometry, single_state$geometry, sparse = FALSE)[,
        1
      ])

    if (nrow(touching_neighbors) < min_neighbors) {
      needed <- min_neighbors - nrow(touching_neighbors)
      nearest_states <- get_nearest_states(state, k = needed + 2, pool = df)

      result <- bind_rows(single_state, touching_neighbors, nearest_states) |>
        distinct(name, .keep_all = TRUE)
    } else {
      result <- bind_rows(single_state, touching_neighbors)
    }
  }

  result |> distinct(name, .keep_all = TRUE)
}

michigan_detailed_cache <- counties("MI", cb = TRUE) |>
  clean_names() |>
  filter(stusps == "MI") |>
  summarize()

# -------------------------------------------------------------------
# Main map
# df must have columns: name (state), total (cases), geometry (sfc)
# -------------------------------------------------------------------
measles_map <- function(df, state) {
  df_shifted <- shift_geometry(df, position = "outside")
  df_plot <- get_neighboring_states(df_shifted, state)

  michigan_data <- df_plot |> filter(name == "Michigan")
  other_states <- df_plot |> filter(name != "Michigan")

  if (nrow(michigan_data) > 0) {
    michigan_detailed <- michigan_detailed_cache
    michigan_detailed <- st_transform(michigan_detailed, st_crs(michigan_data))
    michigan_detailed <- michigan_detailed |>
      mutate(
        geometry = st_simplify(
          geometry,
          dTolerance = 200,
          preserveTopology = TRUE
        )
      )
    michigan_data <- michigan_data |>
      mutate(geometry = st_geometry(michigan_detailed))
  }

  if (nrow(other_states) > 0) {
    other_states <- other_states |>
      st_cast("POLYGON") |>
      group_by(name) |>
      mutate(area = sf::st_area(geometry))

    # Special handling for Hawaii - keep all islands, not just the largest
    hawaii_data <- other_states |> filter(name == "Hawaii")
    non_hawaii_data <- other_states |> filter(name != "Hawaii")

    # For Hawaii, keep all polygons but simplify gently
    if (nrow(hawaii_data) > 0) {
      hawaii_data <- hawaii_data |>
        ungroup() |>
        select(-area) |>
        mutate(
          geometry = sf::st_simplify(
            geometry,
            dTolerance = 100,
            preserveTopology = TRUE
          )
        ) |>
        group_by(name) |>
        summarize(total = first(total), .groups = "drop")
    }

    # For non-Hawaii states, keep largest polygon only
    if (nrow(non_hawaii_data) > 0) {
      non_hawaii_data <- non_hawaii_data |>
        slice_max(area, n = 1) |>
        ungroup() |>
        select(-area) |>
        mutate(geometry = sf::st_simplify(geometry, dTolerance = 1000))
    }

    other_states <- bind_rows(hawaii_data, non_hawaii_data)
  }

  df_plot <- bind_rows(michigan_data, other_states) |>
    mutate(
      measles_category = case_when(
        !is.na(total) & total == 0 ~ "0",
        !is.na(total) & total >= 1 & total <= 9 ~ "1-9",
        !is.na(total) & total >= 10 & total <= 49 ~ "10-49",
        !is.na(total) & total >= 50 & total <= 99 ~ "50-99",
        !is.na(total) & total >= 100 & total <= 249 ~ "100-249",
        !is.na(total) & total >= 250 ~ "250+",
        TRUE ~ NA_character_
      ),
      measles_category = factor(measles_category, levels = names(measles_pal))
    )

  if (state == "Alaska") {
    ak_idx <- which(df_plot$name == "Alaska")
    neigh_idx <- which(df_plot$name != "Alaska")

    ak_cent <- st_coordinates(st_centroid(st_geometry(df_plot)[ak_idx]))
    neigh_cent <- st_coordinates(st_centroid(st_union(st_geometry(df_plot)[
      neigh_idx
    ])))

    geoms <- st_geometry(df_plot)
    pull_factor <- 0.15
    delta <- (ak_cent - neigh_cent) * pull_factor

    geoms[neigh_idx] <- lapply(geoms[neigh_idx], function(g) {
      g + c(delta[1], delta[2])
    })

    st_geometry(df_plot) <- geoms
  }

  if (state == "Hawaii") {
    hi_idx <- which(df_plot$name == "Hawaii")
    neigh_idx <- which(df_plot$name != "Hawaii")

    if (length(neigh_idx) > 0) {
      hi_cent <- st_coordinates(st_centroid(st_union(st_geometry(df_plot)[
        hi_idx
      ])))
      neigh_cent <- st_coordinates(st_centroid(st_union(st_geometry(df_plot)[
        neigh_idx
      ])))

      geoms <- st_geometry(df_plot)
      pull_factor <- 0.25
      delta <- (hi_cent - neigh_cent) * pull_factor

      geoms[neigh_idx] <- lapply(geoms[neigh_idx], function(g) {
        g + c(delta[1], delta[2])
      })

      st_geometry(df_plot) <- geoms
    }
  }

  df_centroids <- df_plot |>
    mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
    )

  legend_seed <- df_centroids |>
    slice(1) |>
    slice(rep(1, length(measles_pal))) |>
    mutate(
      measles_category = factor(names(measles_pal), levels = names(measles_pal))
    )

  # Get the measles count for the selected state
  sel_row <- df_plot |> filter(name == state)
  sel_val <- sel_row$total[1]
  if (is.na(sel_val)) {
    sel_val <- 0
  }

  alt_text <- glue::glue(
    "Map showing measles cases in {state} ({sel_val}) and surrounding states between January 1, 2025 and {format(Sys.Date(), '%B %d, %Y')}."
  )

  # Create the title text
  if (sel_val == 1) {
    title_text <- paste0(sel_val, " measles case in ", state)
  } else {
    title_text <- paste0(sel_val, " measles cases in ", state)
  }

  p <- df_centroids |>
    ggplot(aes(fill = measles_category)) +
    geom_sf(
      data = df_centroids |> dplyr::filter(name != state),
      color = "white",
      size = 0.1
    )

  p <- p +
    with_shadow(
      geom_sf(
        data = df_centroids |> dplyr::filter(name == state),
        aes(fill = measles_category),
        linewidth = 0.5,
        color = "white"
      ),
      sigma = 0,
      x_offset = 2,
      y_offset = 2
    )

  p <- p +
    geom_sf(
      data = legend_seed,
      aes(fill = measles_category),
      alpha = 0,
      color = NA,
      inherit.aes = FALSE,
      show.legend = TRUE
    )

  sel_xy <- st_coordinates(st_point_on_surface(sel_row$geometry))[1, ]

  p <- p +
    scale_fill_manual(
      values = measles_pal,
      name = "Measles cases",
      na.value = "#B8B8B8",
      limits = names(measles_pal),
      breaks = names(measles_pal),
      labels = names(measles_pal),
      drop = FALSE
    ) +
    theme_minimal(base_family = "Gentona") +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.key.width = grid::unit(1.3, "cm"),
      plot.title = element_text(
        family = "Gentona",
        size = 16,
        hjust = 0.5,
        color = "black",
        margin = margin(b = 15)
      )
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        title.position = "top",
        label.position = "bottom",
        override.aes = list(alpha = 1, colour = NA)
      )
    ) +
    labs(
      title = title_text,
      alt = alt_text
    )

  return(p)
}


#-----------------------------------------------------------------------------

mmr_vaccination_comparison_chart <- function(df_mmr, df, state_name) {
  # For New Hampshire, no chart is shown here
  # https://github.com/rfortherestofus/state-immunization-reports/issues/26#issuecomment-3324023259
  if (state_name == "New Hampshire") {
    return(invisible())
  } else {
    # For other states, show neighboring states and US data
    neighboring_data <- get_neighboring_states(df, state_name) |>
      st_drop_geometry() |>
      filter(name != state_name) |>
      distinct(name) |> # ensure unique names pre-join
      left_join(df_mmr, by = c("name" = "geography")) |>
      filter(!is.na(total_population)) |>
      distinct(name, .keep_all = TRUE) |> # ensure unique names post-join
      slice_max(total_population, n = 2) |>
      pull(name) |>
      unique() # neighbors vector must be unique

    chart_data <- df_mmr |>
      filter(geography %in% c(state_name, neighboring_data, "United States")) |>
      mutate(
        estimate_percent = suppressWarnings(as.numeric(estimate_percent)),
        label_pct = janitor::round_half_up(estimate_percent, 0),
        geography = factor(
          geography,
          levels = unique(c("United States", neighboring_data, state_name)) # <-- no dups
        ),
        bar_fill = ifelse(geography == state_name, "#4E97E0", NA_character_),
        txt_col = ifelse(geography == state_name, "white", "black")
      )
  }

  alt_text <- glue::glue(
    "Bar chart showing how {state_name} compares to neighboring states and the United States as a whole in terms of proportion of kindergarteners fully protected with MMR vaccines. ",
    paste0(
      paste0(
        chart_data$geography,
        ": ",
        round(chart_data$estimate_percent),
        "%",
        collapse = ", "
      ),
      ". The HP2030 target is 95%."
    )
  )

  # Create base plot
  p <- ggplot(chart_data, aes(x = estimate_percent, y = geography)) +
    geom_col(
      aes(fill = bar_fill),
      width = 0.5,
      color = "#4E97E0",
      linewidth = 1
    ) +
    scale_fill_identity() +
    geom_text(
      aes(label = paste0(round(label_pct), "%"), color = txt_col),
      hjust = 1.5,
      size = 4,
      fontface = "bold",
      family = "Gentona"
    ) +
    geom_text(
      aes(label = geography, x = 2, color = txt_col),
      hjust = 0,
      size = 4,
      family = "Gentona"
    ) +
    scale_color_identity() +
    scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = scales::label_number(accuracy = 1, suffix = "%")
    ) +
    labs(
      title = "Vaccination comparison (2024)",
      x = NULL,
      y = NULL,
      alt = alt_text
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

  # Add target line and annotation only if state is NOT New Hampshire
  if (state_name != "New Hampshire") {
    p <- p +
      geom_vline(
        xintercept = 95,
        linetype = "dashed",
        color = "gray30",
        alpha = 0.8
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
      )
  }

  return(p)
}
#------------------------------------------------------------------------------

mmr_vaccination_over_time_chart_bar <- function(mmr_line_df, state_name) {
  state_data <- mmr_line_df |>
    filter(geography == state_name) |>
    mutate(estimate_percent = suppressWarnings(as.numeric(estimate_percent))) |>
    filter(is.finite(estimate_percent) & estimate_percent > 0) |>
    mutate(
      school_year = factor(
        school_year,
        levels = unique(school_year[order(school_year)])
      )
    )
  n_x <- nlevels(state_data$school_year)
  if (nrow(state_data) == 0) {
    stop(paste("No data with present values for state:", state_name))
  }

  alt_text <- glue::glue(
    "Bar chart showing proportion of kindergarteners fully protected with MMR vaccines in {state_name} over time. ",
    paste0(
      paste0(
        state_data$school_year,
        ": ",
        round(state_data$estimate_percent),
        "%",
        collapse = ", "
      ),
      ". The HP2030 target is 95%."
    )
  )

  # Create base plot
  p <- ggplot(state_data, aes(x = school_year, y = estimate_percent)) +
    geom_col(fill = "#4E97E0", width = 0.5) +
    geom_text(
      aes(
        y = pmin(estimate_percent + 3, 100),
        label = paste0(round(estimate_percent, 0), "%")
      ),
      vjust = 2.4,
      color = "white",
      fontface = "bold",
      family = "Gentona",
      size = 3
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = glue::glue("Vaccination in {state_name} over time"),
      alt = alt_text
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, family = "Gentona"),
      axis.text.x = element_text(size = 10, family = "Gentona"),
      axis.text.y = element_text(size = 10, family = "Gentona"),
      axis.title = element_blank(),
      plot.margin = if (state_name == "New Hampshire") {
        margin(t = 20, r = 20, b = 20, l = 20)
      } else {
        margin(t = 20, r = 60, b = 20, l = 20)
      }
    )

  # Add target line and label only if state is NOT New Hampshire
  if (state_name != "New Hampshire") {
    p <- p +
      geom_hline(
        yintercept = 95,
        linetype = "dashed",
        color = "gray30",
        alpha = 0.8
      ) +
      annotate(
        "text",
        x = n_x + 0.2,
        y = 94,
        label = "HP2030 Target: 95%",
        family = "Gentona",
        size = 3,
        color = "gray30",
        fontface = "bold",
        hjust = 0,
        vjust = -1.5
      ) +
      coord_cartesian(clip = "off")
  }

  return(p)
}

#---------------------------------------------------------------------------------------

## DTap

dtap_vaccination_comparison_chart <- function(df_dtap, df, state_name) {
  # pick two largest-pop neighbors (excluding the state itself)
  neighboring_data <- get_neighboring_states(df, state_name) |>
    st_drop_geometry() |>
    filter(name != state_name) |>
    left_join(df_dtap, by = c("name" = "geography")) |>
    filter(!is.na(total_population)) |>
    slice_max(total_population, n = 2) |>
    pull(name)

  chart_data <- df_dtap |>
    filter(geography %in% c(state_name, neighboring_data, "United States")) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent),
      geography = factor(
        geography,
        levels = c("United States", neighboring_data, state_name)
      ),
      # selected state gets fill; others are outline-only
      bar_fill = ifelse(geography == state_name, "#002D72", NA_character_),
      txt_col = ifelse(geography == state_name, "white", "black")
    )

  alt_text <- glue::glue(
    "Bar chart showing how {state_name} compares to neighboring states and the United States as a whole in terms of proportion of 2-year-olds fully protected with DTaP vaccines. ",
    paste0(
      paste0(
        chart_data$geography,
        ": ",
        round(chart_data$estimate_percent),
        "%",
        collapse = ", "
      ),
      ". The HP2030 target is 90%."
    )
  )

  ggplot(chart_data, aes(x = estimate_percent, y = geography)) +
    geom_vline(
      xintercept = 90,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_col(
      aes(fill = bar_fill),
      width = 0.5,
      color = "#002D72", # outline color for all bars
      linewidth = 1
    ) +
    scale_fill_identity() +
    # value labels: white on filled bar, black on outlined bars
    geom_text(
      aes(label = paste0(round(estimate_percent), "%"), color = txt_col),
      hjust = 1.5,
      size = 4,
      fontface = "bold",
      family = "Gentona"
    ) +
    # left-side category labels
    geom_text(
      aes(label = geography, x = 2, color = txt_col),
      hjust = 0,
      size = 4,
      family = "Gentona"
    ) +
    scale_color_identity() +
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
      title = "Vaccination comparison (2023)",
      x = NULL,
      y = NULL,
      alt = alt_text
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

dtap_vaccination_over_time_chart_bar <- function(dtap_line_df, state_name) {
  state_data <- dtap_line_df |>
    filter(geography == state_name) |>
    mutate(
      estimate_percent = as.numeric(estimate_percent),
      year = factor(year, levels = unique(year[order(year)]))
    )

  n_x <- nlevels(state_data$year)

  alt_text <- glue::glue(
    "Bar chart showing proportion of 2-year-olds fully protected with DTaP vaccines in {state_name} over time. ",
    paste0(
      paste0(
        state_data$year,
        ": ",
        round(state_data$estimate_percent),
        "%",
        collapse = ", "
      ),
      ". The HP2030 target is 90%."
    )
  )

  ggplot(state_data, aes(x = year, y = estimate_percent)) +

    geom_hline(
      yintercept = 90,
      linetype = "dashed",
      color = "gray30",
      alpha = 0.8
    ) +
    geom_col(fill = "#002D72", width = 0.5) +
    geom_text(
      aes(
        y = pmin(estimate_percent + 3, 100),
        label = paste0(round(estimate_percent, 0), "%")
      ),
      vjust = 2.3,
      color = "White",
      fontface = "bold",
      family = "Gentona",
      size = 3
    ) +
    annotate(
      "text",
      x = n_x + 0.2,
      y = 89,
      label = "HP2030 Target: 90%",
      family = "Gentona",
      size = 3,
      color = "gray30",
      fontface = "bold",
      hjust = 0,
      vjust = -1.3
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0.12))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = glue::glue("Vaccination in {state_name} over time"),
      alt = alt_text
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, family = "Gentona"),
      axis.text.x = element_text(size = 10, family = "Gentona"),
      axis.text.y = element_text(size = 10, family = "Gentona"),
      axis.title = element_blank(),
      plot.margin = margin(t = 20, r = 60, b = 20, l = 20)
    )
}

# utility function for some labels in the report
ordinal <- function(x) {
  suffix <- ifelse(
    x %% 100 %in% 11:13,
    "th",
    ifelse(
      x %% 10 == 1,
      "st",
      ifelse(x %% 10 == 2, "nd", ifelse(x %% 10 == 3, "rd", "th"))
    )
  )
  paste0(x, suffix)
}
