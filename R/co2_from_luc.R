#-------------------------------------------------------------------------------
# CO2 from direct land use change
#-------------------------------------------------------------------------------

calc_CO2_from_luc <- function(data) {

  require(tidyverse)
  require(sf)

  ghg_category <- "Direct land use change emissions"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  
  # Get columns needed for calculation
  data_red <- data |>
    select(
      scn_id, crop, field_area, county, state,
      starts_with("previous"),
      starts_with("current"),
      time_since_change, tillage, dluc_area
    ) 
  
  # Derivate current land use and management information
  data_red <- data_red |>
    mutate(

      # Derive current land use
      current_landuse = if_else(crop == "Alfalfa", "Perennial crop", "Cultivated"),

      # Derive current vegetation
      current_vegetation = if_else(crop == "Alfalfa", "Perennial crop", "Cropland"),

      # Derive current management
      current_management = case_when(
        tillage == "Conventional tillage" ~ "Full-tillage",
        tillage == "Reduced till" ~ "Reduced tillage",
        TRUE ~ "No till"
      ),

      # Derive current ecozone
      current_ecozone = if_else(crop == "Alfalfa", "Temperate (all moisture regimes)", "Not applicable"),
      current_species = "Not applicable"

      # Here we can add some rules for inputs levels based on fertilization and manure
    )

  # Attach dominant soil unit and soil class data
  data_red <- st_join(
    data_red,
    soilmap |> select(soil_subunit = DOMSOI)
  ) |>
    st_drop_geometry()

  # Attach soil class, climate class
  data_red <- list(
    data_red,
    soil_classes,
    climate_by_county |> select(-climate),
    climate_dict,
    soc_ref
  ) |>
    reduce(left_join)

  # Pivot current land use and management information
  data_red <- data_red |>
    pivot_longer(
      cols = starts_with(c("previous", "current")), 
      names_to = "state_var", values_to = "value"
    ) |> 
    separate(state_var, into = c("landuse_state", "var")) |>
    pivot_wider(names_from = "var")

  data_red <- left_join(
    data_red, c_veg,
    #  |> mutate(species = if_else(state == "current", "Not applicable", species)),
    by = c(
      "vegetation" = "Type of vegetation",
      "ecozone" = "Ecological/Climate Zone",
      "species" = "Species or Age"
    )
  )

  # Soil and vegetation carbon stocks for reference and current states
  data_red <- left_join(
    data_red, soc_factors, 
    by = c("climate_region", "landuse" = "land_use", "management", "inputs")
  ) |>
    mutate(
      CS_soil = SOC * FLU * FMG * FI,
      CS_veg = CS_veg,
      CS_total = (CS_soil + CS_veg ) * 1e3,
    )

  # C to CO2 ratio
  CO2_MW <- 44/12

  # Compute CO2 fluxes
  ghg_df  <- data_red |> 
    group_by(scn_id, field_area, time_since_change) |> 
    mutate(
      delta_CS = CS_total[landuse_state == "previous"] - CS_total[landuse_state == "current"]
    ) |> 
    ungroup() |> 
    filter(landuse_state == "current") |> 
    mutate(
      AEF = case_when(
        between(time_since_change, 1, 20) ~ (0.1025 - 0.005 * time_since_change),
        TRUE ~0
      ),
      CO2_biogenic = delta_CS * CO2_MW * AEF * dluc_area,
      units = "kg"
    )
  ghg_df <- ghg_df |> bind_rows(ghg_df |> mutate(units = "kg_CO2e"))

  # Collect results later
  ghg_df <- ghg_df |> 
    bind_cols(ghg_cols)|>
    select(scn_id, metric:source_category, CO2_biogenic, units) |> 
    group_nest(scn_id, .key = "co2_luc_results")

  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)
  return(data)

}
