# Routine package libraries and datasets to load
library(readxl)
library(janitor)
library(tidyverse)
library(knitr)

# Tables
tbl_crop_data <- readxl::read_xlsx("data/fp5-crop-data.xlsx", sheet = "data")

tbl_yield <- tbl_crop_data |> 
  select(crop, uscs_units, lb_per_unit, kg_per_uscs_unit, standard_moisture)

# Emissions authority table
# 2025-04-03
authority_table <- 
  readr::read_csv("data/fp5-emissions-authority-table.csv")
  
# GWP factors
tbl_gwp_factors <- 
  readr::read_csv("data/fp5-GWP.csv") |>
  tidyr::pivot_wider(names_from = Gas,
                     values_from = `Global Warming Potential`,
                     names_prefix = "gwp_") |> 
  janitor::clean_names() |> 
  dplyr::rename(gwp_assessment_report = assessment_report_ar,
                gwp_time_horizon = time_horizon)

# Helper functions
# Prep versions of authority tables specific to operations
prep_authority <- function(data, component) {
  data |> 
    # simplify
    select(where(is.character), CO2_fossil:MJ, -Unit, -Notes) |> 
    # best guess on how to preserve the gas column names to match authority table
    rename_with(~ str_replace_all(str_to_lower(.), " ", "_"),
                .cols = where(is.character)) |> 
    filter(str_detect(source_detail, component))
}