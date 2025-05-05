#-------------------------------------------------------------------------------
# Energy Use from pesticides production
#-------------------------------------------------------------------------------

calc_EU_GHG_pesticides <- function(data) {
  
  require(tidyverse)
  require(sf)

  ghg_category <- "GHG emissions associated with production of pesticides" 
  eu_category <- "Energy use associated with production of pesticides"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ref_eu <- authority_table |> 
    filter(`Source Category` == !!eu_category)
  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  eu_cols <- ref_eu |> clean_names() |> select(metric:source_category) |> distinct()

  # Get columns needed for calculation
  data_red <- data |> 
    st_drop_geometry() |> 
    select(scn_id, crop, field_area, ends_with("_app")) 
  
  # Get GWP factors
  gwp_ref_table <- data |> 
    st_drop_geometry() |> 
    select(
      scn_id, starts_with("gwp"), -gwp_time_horizon, -gwp_assessment_report
    ) |> 
    pivot_longer(starts_with("gwp"), names_to = "GHG", values_to = "GWP") |> 
    mutate(GHG = str_remove(GHG, "gwp_"))

  # Add pest rates and MJ / GHG factors
  pest_factors <- pest_ref_rates |> 
    select(crop = 1, source_detail = 2, avg_app_rate = 3) |>     
    left_join(
      ref_eu |> select(source_detail = "Source Detail", MJ)
    ) |> 
    left_join(
      ref_ghg |> select(source_detail = "Source Detail", CO2_fossil:SF6) |> 
        pivot_longer(-source_detail, names_to = "GHG", values_to = "factor"),
      relationship = "many-to-many"
    ) |> 
    mutate(GHG2 = str_to_lower(GHG)) |> 
    left_join(gwp_ref_table, by = c("GHG2" = "GHG"), relationship = "many-to-many") |> 
    select(-GHG2)
    
  pest_dict <- c(
    "herbicides_app" = "Herbicides",
    "insecticides_app" = "Insecticides",
    "fungicides_app" = "Fungicides",
    "inoculant_app" = "Inoculant",
    "seed_treatment_app" = "Seed Treatment",
    "herbicides_sulfuric_acid_app" = "Herbicides (sulfuric acid)",
    "growth_reg_app" = "Growth Regulators",
    "fumigants_app" = "Fumigants"
  )
  
  # Compute the amount of products appliedsyntethic fertilizers applied
  data_red <- data_red |> 
    pivot_longer(
      -c(scn_id, crop, field_area), 
      values_to = "app_num",
      names_to = "source_detail"
    ) |>
    mutate(source_detail = str_replace_all(source_detail, pest_dict)) 
  
  # Attach fert factors and convert to kg_GHG
  eu_df <- left_join(
    data_red, pest_factors |> distinct(source_detail, avg_app_rate, MJ),
    relationship = "many-to-many"
  ) |>
    mutate(MJ_total = app_num * avg_app_rate * MJ) |>
    group_by(scn_id, source_detail) |>
    summarise(MJ = sum(MJ_total, na.rm = TRUE), units = "MJ") |>
    bind_cols(eu_cols) |> 
    ungroup() |>
    group_nest(scn_id, .key = "eu_df")
  
  # Attach fert factors and convert to kg_GHG
  ghg_df <- left_join(
    data_red, pest_factors |> select(-MJ), 
    relationship = "many-to-many"
  ) |> 
    mutate(
      # GHG = str_remove(GHG, "_fossil|_biogenic"),
      kg_gas = app_num * avg_app_rate * factor,
      kg_CO2e = kg_gas * GWP
    ) |>     
    group_by(scn_id, source_detail, GHG) |> 
    summarise(across(c(kg_gas, kg_CO2e), ~ sum(.x, na.rm = TRUE))) |> 
    ungroup() |> 
    drop_na()

  # Pivot wider for collect results later
  ghg_df <- bind_rows(
    ghg_df |> 
      pivot_wider(
        id_cols = c(scn_id, source_detail),
        names_from = GHG, values_from = kg_gas) |> 
      mutate(units = "kg")
    , 
    ghg_df |> 
      pivot_wider(
        id_cols = c(scn_id, source_detail),
        names_from = GHG, values_from = kg_CO2e) |> 
      mutate(units = "kg_CO2e")
  ) |> 
    bind_cols(ghg_cols) |> 
    group_nest(scn_id, .key = "ghg_df")

  # Combine outputs
  eu_ghg_df <- left_join(eu_df, ghg_df) |> 
    mutate(eu_ghg_pest_results = map2(
      eu_df, ghg_df, ~ {
        bind_rows(.x, .y) |> 
        relocate(metric:source_category, source_detail,  CO2_fossil:SF6, MJ, units)
      }
    )) |> 
    select(-eu_df, -ghg_df)    
    
  # Attach results to the output table
  data <- left_join(data |> select(-any_of("eu_ghg_df")), eu_ghg_df)
  return(data)
}
