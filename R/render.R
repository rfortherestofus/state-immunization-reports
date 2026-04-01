# Load Packages -----------------------------------------------------------
library(quarto)
library(stringr)
library(glue)
library(tidyverse)
library(here)
library(fs)
library(xfun)
library(googledrive)

# Run Typst 0.14
system("quarto typst --version") # should be typst 0.14.x

# Import Data ------------------------------------------------------------
source("R/import-data.R")

# Load Data ---------------------------------------------------------------
states_flags <- list.files(
  path = "assets/flags",
  pattern = "\\.svg$",
  full.names = TRUE
)
states <- tools::file_path_sans_ext(basename(states_flags))
states <- states[states != "puerto_rico"] # remove for now
states <- states[49:51]

# Create State QMDs --------------------------------------------------------
if (dir_exists("documents")) {
  print("Deleting documents/ and reports/")
  dir_delete("documents")
  dir_delete("reports")
}
dir_create("documents/assets/flags")
dir_create("documents/report_files")
dir_create("reports")

create_state_qmd <- function(state) {
  file_copy(
    path = "report.qmd",
    new_path = str_glue("documents/{state}.qmd"),
    overwrite = TRUE
  )
}
walk(states, create_state_qmd)

# Copy Assets -------------------------------------------------------------
file_copy(
  path = "assets/coins.svg",
  new_path = "documents/assets/coins.svg"
)

file_copy(
  path = "assets/logo.png",
  new_path = "documents/assets/logo.png"
)

file_copy(
  path = "assets/down-arrow.png",
  new_path = "documents/assets/down-arrow.png"
)

file_copy(
  path = "assets/up-arrow.png",
  new_path = "documents/assets/up-arrow.png"
)

file_copy(
  path = "typst-template.typ",
  new_path = "documents/typst-template.typ"
)

file_copy(
  path = "typst-show.typ",
  new_path = "documents/typst-show.typ"
)

file_copy(
  path = states_flags,
  new_path = "documents/assets/flags"
)

file_copy(
  path = "R/charts.R",
  new_path = "documents/charts.R"
)

# Edit Parameters YAML ---------------------------------------------------
change_parameters_yaml <- function(state) {
  gsub_file(
    file = str_glue("documents/{state}.qmd"),
    "state:.*",
    glue("state: ", gsub("_", " ", state))
  )
}

walk(states, change_parameters_yaml)

### Note about rendering:
# Since we want to pass the --pdf-standard flag to Typst CLI,
# we only use Quarto to render to .typ, and then we render ourselves
# to pdf using the typst CLI.
# This does not have a big impact of performance since Typst compilation
# is very fast anyway.
# See https://github.com/quarto-dev/quarto-cli/issues/13683

# Render Reports -----------------------------------------------------------

# Custom Typst compilation -------------------------------------------------
render_and_compile <- function(state) {
  quarto_render(str_glue("documents/{state}.qmd"))
  print(str_glue("{state} rendered"))
}
walk(states, render_and_compile)

# Move Reports -------------------------------------------------------------
all_reports <- dir_ls(path = "documents", regexp = ".pdf")

file_move(
  path = all_reports,
  new_path = "reports"
)

rename_file <- function(state) {
  state_title <- str_to_title(gsub("_", " ", state))
  old_name <- glue("reports/{state}.pdf")
  new_name <- glue(
    "reports/Status of Childhood Immunization in {state_title}.pdf"
  )

  file.rename(old_name, new_name)
}
walk(states, rename_file)

# Upload Reports -----------------------------------------------------------
if (str_detect(Sys.getenv("GOOGLE_DRIVE_EMAIL"), "rfortherestofus.com")) {
  drive_auth(Sys.getenv("GOOGLE_DRIVE_EMAIL"))

  pdf_files <- list.files("reports", pattern = "\\.pdf$", full.names = TRUE)

  upload_report <- function(report_file) {
    drive_upload(
      media = report_file,
      path = folder <- as_id("1fxoUQYyKK0ef4BRzb3Ab4w1x39s722su"),
      overwrite = TRUE
    )
  }

  walk(pdf_files, upload_report)
}
