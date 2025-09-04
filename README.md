# State Immunization Reports

<br>

### Data

This repo has data on state-level immunization. The `data-raw` folder holds all raw data:

- `measles_cases_upd.csv` has measles cases for each county. Data comes from the [Johns Hopkins U.S Measles Tracker](https://publichealth.jhu.edu/ivac/resources/us-measles-tracker). Data last updated on August 7, 2025.
- `mmr1_coverage.csv` presents data on the proportion of kindergartners who received at least 1 dose of measles, mumps, and rubella (MMR) vaccine in the past school year. This data is available from the [CDC's SchoolVaxView website](https://www.cdc.gov/schoolvaxview/data/index.html). Data last updated on July 31, 2025.
- `dtap_coverage.csv` presents data on the proportion of 2-year-olds (24 months) who have received at least 4 doses of the DTaP vaccine. This data is available from the [CDC's ChildVaxView website](https://www.cdc.gov/childvaxview/about/interactive-reports.html). Data last updated on August 20, 2024.
- `non_medical_exemption_rates.csv` has non-medical exemption rates for each state. Data comes from the [CDC's SchoolVaxView website](https://www.cdc.gov/schoolvaxview/data/index.html). Data last updated on July 31, 2025.
- `non_medical_exemption_policies.csv` presents information on state policies regarding non-medical exemptions, such as those based on religious or personal beliefs. In some states, personal exemptions are also called “philosophical exemptions.” This data is available from the [National Conference of State Legislatures (NCSL) brief on 'State Non-Medical Exemptions From School Immunization Requirements'](https://www.ncsl.org/health/state-non-medical-exemptions-from-school-immunization-requirements). Data last updated on July 24, 2025.
- `health_spending.csv` has public health spending per capita for 2022-2023, available from ['America's health rankings'](https://www.americashealthrankings.org/explore/measures/PH_funding). Data from CDC, HRSA and Trust for America's Health, 2022-2023.
- `universal_purchase.csv` presents data on the 'Universal Vaccine Purchase Program', which is a state-led program in which the state government acquires all vaccines recommended by the Advisory Committee on Immunization Practices (ACIP) for every resident, regardless of their insurance coverage or ability to pay. Available from the [Association of Immunization Managers (AIM), Policy Maps – Universal Vaccine Purchase Program](https://www.immunizationmanagers.org/resources/aim-policy-maps/).Data last updated on April 2025.
- `state_policies.csv` presents data on state policies that could affect childhood immunization. This dataset is derived from multiple sources, including the [Association of Immunization Managers, Legislative Round-ups](https://www.immunizationmanagers.org/resources-toolkits/immunization-program-policy-toolkit/legislative-round-ups/); keyword search on [LegiScan and NCSL's 'State Public Health Legislation Database'](https://www.ncsl.org/health/state-public-health-legislation-database). Data last updated on May 31, 2025 (AIM Legislative Round-up)

We use the `R/import-data.R` file to import and clean all of the above
files. The `R/import-data.R` file then saves a set of clean CSV files into the `data-clean` folder.

The `data-clean` folder holds all clean data:

- `total_measles_cases.csv` has measles cases for each state
- `mmr_coverage_final.csv` has data for at least one dose of MMR vaccine coverage for kindergartners for 2021-2025 school years.
- `dtap_coverage_final.csv` has data for at least 4 doses of DTaP vaccine at 24 months for children born between 2017-2021.
- `non_medical_exemption.csv` has data on non-medical exemption rates for all states
- `non_medical_exemption_policies.csv` presents state level policies on religious and personal exemptions.
- `health_spending_final.csv` has data on public health spending per capita for 2022-2023.
- `universal_purchase_final.csv` presents data on a state's participation in the 'Universal Vaccine Purchase Program'
- `state_policies_final.csv` has data on state level immunization related policies that could strengthen or weaken vaccine safety nets.

The CSV files in the `data-clean` folder are used to generate state-level immunization reports for all 50 states, the District of Columbia, and Puerto Rico.

<br><br>

### How the report works

The report file (`report.qmd`) relies on a Typst template that lives in `typst-template.typ` and uses many functions defined in the latest for easier styling. For instance, the following snippet adds a source section at the bottom right in the current element, and adds some vertical margin:

````qmd
```{=typst}
#source("Source: SchoolVaxView")
#v(7pt)
```
````

Current functions:

- `#source()`: adds the source (some text) at the bottom right of an element
- `#connected-boxes()`: 2 boxes horizontally connected by a blue line
- `#blueline()`: adds an horizontal blue line with 100% width
- `#status-boxes()`: 2 boxes on top of each other, with text inside each of them
- `#chart-title()`: title for a group of charts

Other functions used (such as `#v()`) are built-in with Typst.

A very small amount of HTML is used for grey background sections. Note that the vast majority of html/css is **not** supported here. Background colors are [one of the few things](https://quarto.org/docs/advanced/typst/typst-css.html#supported-elements-and-properties) Quarto natively translate to typst.

`R/render.R` generates all PDF (one per state) and store them in `reports/`, as well as create a zip with all the PDFs at the root of the directory.
