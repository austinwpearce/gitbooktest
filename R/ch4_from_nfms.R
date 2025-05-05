#-------------------------------------------------------------------------------
# CH4 from non flooded soils
#-------------------------------------------------------------------------------

calc_CH4_from_nfms <- function(data) {
  
  require(tidyverse)
  require(sf)
  
  ghg_category <- "CH4 flux from non-flooded soils"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  
  # Get columns needed for calculation
  data_red <- data |> 
    st_drop_geometry() |> 
    select(scn_id, crop, field_area, state, county, gwp_ch4_biogenic)
    
  # Join tables
  data_red <- left_join(data_red, natural_vegetation) |> 
    mutate(
      land_use = if_else(crop == "Alfalfa", "Perennial cropland", "Annual cropland")
    ) |> 
    left_join(CH4B_MF) |> 
    # Replacing CH4B for rice to 0
    mutate(CH4_base = if_else(crop == "Rice", 0, CH4_base))

  # Compute CH4 fluxes
  ghg_df <- data_red |>    
    mutate(CH4_biogenic = CH4_base * MF * field_area * 1e3, units = "kg")
  ghg_df <- ghg_df |> 
    bind_rows(
      mutate(ghg_df, CH4_biogenic = CH4_biogenic * gwp_ch4_biogenic, units = "kg_CO2e")
    )
  
  # Collect results later
  ghg_df <- ghg_df |> 
    bind_cols(ghg_cols)|>
    select(scn_id, metric:source_category, CH4_biogenic, units) |>
    group_nest(scn_id, .key = "ch4_nmfs_results")
  
  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)
  return(data)
 
}
