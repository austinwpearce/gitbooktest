#-------------------------------------------------------------------------------
# Non-CO2 from burning biomass
#-------------------------------------------------------------------------------

calc_non_CO2_from_burning <- function(data) {
  
  require(tidyverse)
  require(sf)

  ghg_category <- "Non-CO2 emissions from biomass burning"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  
  # Get columns needed for calculation
  data_red <- data |> 
    st_drop_geometry() |> 
    select(
      scn_id, crop, field_area, 
      biomass_dead, biomass_prop, area_burned, stage_of_burning,
      crop_burned, crop_burned_yield, 
      gwp_ch4_biogenic, gwp_n2o
    ) 
  
  # Join ref tables
  data_red <- list(
    data_red,
    crop_factors |> rename(crop_burned = crop),
    combustion_efficiency,
    tbl_crop_data |> select(crop_burned = crop, DM = standard_dm)
  ) |> 
    reduce(left_join)
  
  # Create simple rule for factors
  data_red <- data_red |>
    mutate(
      EF_CH4 = if_else(crop == "Alfalfa", 2.3, 2.7),
      EF_N2O = if_else(crop == "Alfalfa", 0.21, 0.07),
      C = if_else(str_detect(biomass_dead, "No significant"), 0.47, 0.44),
      S_db = if_else(str_detect(biomass_dead, "No significant"), 1, 2)
    )

  # Carbon fraction factor for computing Hpeak
  F_c <- 0.45

  data_red <- data_red |> 
    mutate(
      M  = case_when(
        crop_burned == "Alfalfa" ~ (((crop_burned_yield / HI) * (DM) * F_c) * (C^-1) * (biomass_prop)) * S_db,  
        crop_burned != "Alfalfa" ~ (crop_burned_yield * (1/ HI - 1) * DM),
        .default = NA_real_
      )
    )
  
  # Compute GHG fluxes
  ghg_df <- data_red |> 
    mutate(
      CH4_biogenic = M * area_burned * C_e * EF_CH4 * 1e-3,
      N2O = M * area_burned * C_e * EF_N2O * 1e-3,
      units = "kg"
    )
  
  ghg_df <- ghg_df |> 
    bind_rows(
      ghg_df |> 
        mutate(
          CH4_biogenic = CH4_biogenic * gwp_ch4_biogenic,
          N2O = N2O * gwp_n2o,
          units = "kg_CO2e"
      )
    )

  # Collect results later
  ghg_df <- ghg_df |> 
    bind_cols(ghg_cols)|>
    select(scn_id, metric:source_category, CH4_biogenic, N2O, units) |> 
    group_nest(scn_id, .key = "non_co2_burning_results")
  
  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)
  return(data)

}
