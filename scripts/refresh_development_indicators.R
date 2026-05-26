#!/usr/bin/env Rscript

# Refresh the cleaned development indicators snapshot used in the summer
# assignments. Students use the CSV outputs; this script is for instructors.

options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c("dplyr", "jsonlite", "readr", "readxl", "stringr", "tidyr")

missing_packages <- required_packages[
  !(required_packages %in% rownames(installed.packages()))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(jsonlite)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

start_year <- 2000
end_year <- 2024
snapshot_year <- 2026

indicator_dictionary <- tibble::tribble(
  ~variable, ~label, ~source, ~source_indicator_code, ~unit, ~higher_is, ~notes,
  "population", "Population, total", "World Bank World Development Indicators", "SP.POP.TOTL", "people", "larger population", "Total population.",
  "gdp_per_capita_ppp", "GDP per capita, PPP", "World Bank World Development Indicators", "NY.GDP.PCAP.PP.KD", "constant international dollars", "higher income", "GDP per capita based on purchasing power parity.",
  "gdp_growth", "GDP growth", "World Bank World Development Indicators", "NY.GDP.MKTP.KD.ZG", "annual percent", "faster growth", "Annual percentage growth rate of GDP at market prices based on constant local currency.",
  "inflation", "Inflation", "World Bank World Development Indicators", "FP.CPI.TOTL.ZG", "annual percent", "higher inflation", "Inflation measured by the consumer price index.",
  "poverty_rate", "Poverty headcount ratio at $3.65 a day", "World Bank World Development Indicators", "SI.POV.LMIC", "percent of population", "higher poverty", "International poverty line for lower-middle-income countries; coverage is sparse because it depends on household surveys.",
  "gini", "Gini index", "World Bank World Development Indicators", "SI.POV.GINI", "index, 0-100", "higher inequality", "Coverage is sparse because it depends on household surveys.",
  "life_expectancy", "Life expectancy at birth", "World Bank World Development Indicators", "SP.DYN.LE00.IN", "years", "longer life expectancy", "Average number of years a newborn would live if current mortality patterns stayed constant.",
  "under5_mortality", "Under-5 mortality rate", "World Bank World Development Indicators", "SH.DYN.MORT", "deaths per 1,000 live births", "worse child mortality", "Probability of dying between birth and age 5 per 1,000 live births.",
  "secondary_school_enrollment", "Secondary school enrollment", "World Bank World Development Indicators", "SE.SEC.ENRR", "gross enrollment percent", "higher enrollment", "Gross enrollment ratio can exceed 100 because of over-age or under-age students.",
  "unemployment_total", "Unemployment, total", "World Bank World Development Indicators", "SL.UEM.TOTL.ZS", "percent of labor force", "higher unemployment", "Modeled ILO estimate.",
  "unemployment_youth", "Youth unemployment", "World Bank World Development Indicators", "SL.UEM.1524.ZS", "percent of labor force ages 15-24", "higher youth unemployment", "Modeled ILO estimate.",
  "co2_per_capita", "CO2 emissions per capita", "World Bank World Development Indicators", "EN.GHG.CO2.PC.CE.AR5", "tons of CO2 equivalent per person", "higher emissions", "Carbon dioxide emissions excluding land use, land-use change, and forestry per person.",
  "government_effectiveness", "Government effectiveness", "Worldwide Governance Indicators", "GE.EST", "estimate, approximately -2.5 to 2.5", "more effective governance", "Perception-based composite indicator; compare changes cautiously.",
  "control_of_corruption", "Control of corruption", "Worldwide Governance Indicators", "CC.EST", "estimate, approximately -2.5 to 2.5", "more control of corruption", "Perception-based composite indicator; compare changes cautiously."
)

fetch_world_bank_indicator <- function(indicator_code) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/",
    indicator_code,
    "?format=json&per_page=20000&date=",
    start_year,
    ":",
    end_year
  )

  response <- fromJSON(url, flatten = TRUE)

  if (length(response) < 2 || length(response[[2]]) == 0) {
    stop("No World Bank data returned for indicator: ", indicator_code)
  }

  response[[2]] |>
    as_tibble() |>
    transmute(
      iso3c = countryiso3code,
      country = country.value,
      year = as.integer(date),
      source_indicator_code = indicator.id,
      value = as.numeric(value)
    )
}

fetch_country_metadata <- function() {
  url <- "https://api.worldbank.org/v2/country?format=json&per_page=400"
  response <- fromJSON(url, flatten = TRUE)

  if (length(response) < 2 || length(response[[2]]) == 0) {
    stop("No World Bank country metadata returned.")
  }

  response[[2]] |>
    as_tibble() |>
    transmute(
      iso3c = id,
      country = name,
      region = region.value,
      income_group = incomeLevel.value
    ) |>
    filter(region != "Aggregates") |>
    mutate(
      is_lmic = income_group %in% c(
        "Low income",
        "Lower middle income",
        "Upper middle income"
      )
    )
}

fetch_wgi_indicator <- function(sheet, variable, indicator_code) {
  wgi_url <- paste0(
    "https://datacatalogfiles.worldbank.org/ddh-published/0038026/",
    "DR0095947/wgidataset_with_sourcedata-2025.xlsx"
  )

  wgi_file <- tempfile(fileext = ".xlsx")
  download.file(wgi_url, wgi_file, mode = "wb", quiet = TRUE)

  read_excel(wgi_file, sheet = sheet) |>
    transmute(
      iso3c = `Economy (code)`,
      country = `Economy (name)`,
      year = as.integer(Year),
      source_indicator_code = indicator_code,
      value = as.numeric(`Governance estimate (approx. -2.5 to +2.5)`),
      variable = variable
    ) |>
    filter(year >= start_year, year <= end_year)
}

wdi_indicator_dictionary <- indicator_dictionary |>
  filter(source != "Worldwide Governance Indicators")

wdi_indicator_data <- wdi_indicator_dictionary |>
  filter(source != "Worldwide Governance Indicators") |>
  select(variable, source_indicator_code) |>
  split(seq_len(nrow(wdi_indicator_dictionary))) |>
  lapply(function(row) {
    message("Fetching ", row$source_indicator_code, " (", row$variable, ")")

    fetch_world_bank_indicator(row$source_indicator_code) |>
      mutate(variable = row$variable)
  }) |>
  bind_rows()

wgi_indicator_data <- bind_rows(
  fetch_wgi_indicator("ge", "government_effectiveness", "GE.EST"),
  fetch_wgi_indicator("cc", "control_of_corruption", "CC.EST")
)

indicator_data <- bind_rows(wdi_indicator_data, wgi_indicator_data)

country_metadata <- fetch_country_metadata()

development_indicators <- indicator_data |>
  select(iso3c, year, variable, value) |>
  group_by(iso3c, year, variable) |>
  summarise(
    value = if (all(is.na(value))) NA_real_ else value[which(!is.na(value))[1]],
    .groups = "drop"
  ) |>
  pivot_wider(names_from = variable, values_from = value) |>
  inner_join(country_metadata, by = "iso3c") |>
  select(
    country,
    iso3c,
    region,
    income_group,
    is_lmic,
    year,
    all_of(indicator_dictionary$variable)
  ) |>
  arrange(country, year) |>
  mutate(
    assignment_sample = is_lmic &
      !is.na(population) &
      population >= 500000
  ) |>
  select(
    country,
    iso3c,
    region,
    income_group,
    is_lmic,
    assignment_sample,
    year,
    everything()
  )

data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(
  development_indicators,
  file.path(data_dir, paste0("development_indicators_", snapshot_year, ".csv")),
  na = ""
)

write_csv(
  indicator_dictionary,
  file.path(data_dir, paste0("development_indicators_dictionary_", snapshot_year, ".csv")),
  na = ""
)

message("Rows written: ", nrow(development_indicators))
message("Countries/economies written: ", n_distinct(development_indicators$iso3c))
message("Years written: ", min(development_indicators$year), "-", max(development_indicators$year))
