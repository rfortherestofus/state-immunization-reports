library(quarto)
library(glue)
library(tidyverse)
library(here)
library(fs)
library(xfun)

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
        path = "src/report.qmd",
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
    path = "src/typst-template.typ",
    new_path = "documents/typst-template.typ"
)
file_copy(
    path = "src/typst-show.typ",
    new_path = "documents/typst-show.typ"
)
file_copy(
    path = states_flags,
    new_path = "documents/assets/flags"
)

change_parameters_yaml <- function(state) {
    gsub_file(
        file = str_glue("documents/{state}.qmd"),
        "state: utah",
        glue("state: ", gsub("_", " ", state))
    )
}
walk(states, change_parameters_yaml)

walk(str_glue("documents/{states}.qmd"), quarto_render)

all_reports <- dir_ls(path = "documents", regexp = ".pdf")

file_move(
    path = all_reports,
    new_path = "reports"
)
pdf_files <- list.files("reports", pattern = "\\.pdf$", full.names = TRUE)
zip("reports_pdfs.zip", files = pdf_files)
