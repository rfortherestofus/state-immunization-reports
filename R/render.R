library(quarto)
library(glue)
library(tidyverse)
library(here)
library(fs)
library(xfun)
library(googledrive)

states_flags <- list.files(
    path = "assets/flags",
    pattern = "\\.svg$",
    full.names = TRUE
)
states <- tools::file_path_sans_ext(basename(states_flags))

if (dir_exists("documents")) {
    print("Deleting documents/ and reports/")
    dir_delete("documents")
    dir_delete("reports")
}
dir_create("documents/assets/flags")
dir_create("reports")

create_state_qmd <- function(state) {
    file_copy(
        path = "report.qmd",
        new_path = str_glue("documents/{state}.qmd"),
        overwrite = TRUE
    )
}
walk(states, create_state_qmd)

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

change_parameters_yaml <- function(state) {
    gsub_file(
        file = str_glue("documents/{state}.qmd"),
        "state:.*",
        glue("state: ", gsub("_", " ", state))
    )
}
walk(states, change_parameters_yaml)

change_path_source <- function(state) {
    gsub_file(
        file = str_glue("documents/{state}.qmd"),
        "R/charts.R",
        "../R/charts.R" # Quarto uses relative paths
    )
}
walk(states, change_path_source)

change_path_data <- function(state) {
    gsub_file(
        file = str_glue("documents/{state}.qmd"),
        pattern = "data-clean/",
        replacement = "../data-clean/"
    )
}

walk(states, change_path_data)

walk(str_glue("documents/{states}.qmd"), quarto_render)

all_reports <- dir_ls(path = "documents", regexp = ".pdf")

file_move(
    path = all_reports,
    new_path = "reports"
)

drive_auth(Sys.getenv("GOOGLE_DRIVE_EMAIL"))

pdf_files <- list.files("reports", pattern = "\\.pdf$", full.names = TRUE)

upload_report <- function(report_file) {
    drive_upload(
        media = report_file,
        path = as_dribble(
            "https://drive.google.com/drive/u/1/folders/13VY2ICdG4H6ismEtSZnllTPJtBx8uUpg"
        ),
        overwrite = TRUE
    )
}

walk(pdf_files, upload_report)
