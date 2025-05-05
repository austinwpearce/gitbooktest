#-------------------------------------------------------------------------------
# CO2 from direct land use change
#-------------------------------------------------------------------------------

calc_CO2_from_dluc <- function(data) {

  require(tidyverse)
  require(sf)

  ghg_category <- "Direct land use change emissions"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  
  # Get columns needed for calculation
  data_red <- data |>
    select(
      scn_id, crop, field_area, manure_kg_applied,
      starts_with("previous"),
      starts_with("current"),
      time_since_change, tillage, dluc_area
    ) 

  # Derivate current land use and management information
  data_red <- data_red |>
    mutate(

      # Derive current land use
      current_landuse = "Cultivated",
  
      # Derive current vegetation
      current_vegetation = "Cropland",
  
      # Set species to Not applicable
      current_species = "Not applicable",

      # Derive current management
      current_management = case_when(
        tillage == "Conventional tillage" ~ "Full-tillage",
        tillage == "Reduced till" ~ "Reduced tillage",
        TRUE ~ "No till"
      ),

      # Here we can add some rules for inputs levels based on fertilization and manure
      current_inputs = case_when(
        !is.na(manure_kg_applied) | manure_kg_applied > 0 ~ "High with manure",
        .default = "High without manure"
      ),

      previous_inputs = case_when(
        previous_landuse == "Grassland" & previous_management == "Improved" ~
          "High",
        previous_landuse == "Grassland" & previous_management != "Improved" ~
          "Medium",
        .default = "Not applicable"
      ),

      previous_management = case_when(
        previous_landuse %in% c("Managed forest", "Shrubland") ~ "Not applicable",
        .default = previous_management
      ),

      has_changed = previous_landuse != current_landuse
    ) |> 
      filter(!is.na(time_since_change), has_changed)
      

  # Attach climate region, eco-zone and SOC ref value
  data_red <- data_red |> 
    mutate(
      climate_region = extract(climate_zone, data_red) |> 
        pull(category) |> str_to_sentence(),
      SOC = extract(soc_ref, data_red) |>
        pull(category)
    ) |> 
    st_join(eco_zone |> select(eco_zone = gez_name)) |> 
    st_drop_geometry()

  # Pivot for attaching state-specific factors
  data_red <- data_red |> 
    pivot_longer(
      cols = starts_with(c("previous", "current")), 
      names_to = "luc_state", 
      values_to = "value"
    ) |> 
    separate(luc_state, into = c("state", "var")) |> 
    pivot_wider(names_from = "var") 

  data_red <- bind_rows(

    #  ACA NO ESTÁ UNIENDO BIEN!!!!! chequear que se den las combinaciones de cveg!!!S
    
    # Climate region only CS_veg values
    left_join(
      data_red, c_veg1, 
      by = c("vegetation" = "veg_type", "climate_region")
    ) |>
      drop_na(CS_veg),
  
    # ecozone/species only CS_veg values
    left_join(
      data_red, c_veg2, 
      by = c("vegetation" = "veg_type", "eco_zone", "species" = "species_age")
    ) |> 
      drop_na(CS_veg)
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
      delta_CS = CS_total[state == "previous"] - CS_total[state == "current"],
      delta_CS = if_else(has_changed, delta_CS, 0, missing = 0)
    ) |> 
    ungroup() |> 
    filter(state == "current") |> 
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
    group_nest(scn_id, .key = "co2_dluc_results")

  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)
  return(data)

}
