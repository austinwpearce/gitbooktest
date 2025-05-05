#-------------------------------------------------------------------------------
# CO2 from lime
#-------------------------------------------------------------------------------

calc_CO2_from_lime <- function(data) {
  
  require(tidyverse)
  require(sf)

  ghg_category <- "CO2 from carbonate lime applications to soils"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  
  # Get columns needed for calculation
  data_red <- data |> 
    st_drop_geometry() |> 
    select(scn_id, field_area,  matches("lime.*.(rate|source)"))

  # Calculate Lime amount
  data_red <- data_red |> 
    pivot_longer(
      -c(scn_id, field_area), 
      values_transform = as.character
    ) |>
    separate(name, into = c("fertID", "name"), sep = "_") |>
    pivot_wider(id_cols = c(scn_id, field_area, fertID)) |>
    mutate(rate = as.numeric(rate)) |> 
    drop_na() |> 
    group_by(scn_id, field_area, source) |>
    summarise(rate = sum(rate, na.rm = T)) |> 
    mutate(amount = rate * field_area) |> 
    rename(source_detail = "source") |> 
    ungroup()
  
  # Define factors and constants
  MW <- 44/12

  # Compute CO2 fluxes
  ghg_df <- data_red |> 
    mutate(
      EF = if_else(source_detail == "Lime (calcitic)", 0.059, 0.064),
      CO2 = amount * EF * MW, units = "kg"
    ) |> 
    group_by(scn_id, source_detail, units) |> 
    summarise(CO2_fossil = sum(CO2, na.rm = T)) |> 
    ungroup()  
  ghg_df <- ghg_df |> 
    bind_rows(
      ghg_df |> mutate(units = "kg_CO2e")
    )
  
  # Collect results later
  ghg_df <- ghg_df |> 
    bind_cols(ghg_cols)|> 
    select(scn_id, metric:source_category, CO2_fossil, units) |> 
    group_nest(scn_id, .key = "co2_lime_results")

  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)
  return(data)

}
