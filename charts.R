# Load packages ----------------------------------------------------------

library(tidyverse)
library(tigris)
library(sf)
library(janitor)

# Vaccination over time chart --------------------------------------------

# Vaccination comparison chart -------------------------------------------

# Measles cases map ------------------------------------------------------

us_states <-
  states() |>
  clean_names() |>
  select(name)

get_neighboring_states <- function(state) {
  single_state <-
    us_states |>
    filter(name == state)

  us_states |>
    filter(st_touches(geometry, single_state$geometry, sparse = FALSE)[, 1]) |>
    bind_rows(single_state)
}

get_neighboring_states("Ohio") |>
  ggplot() +
  geom_sf()
