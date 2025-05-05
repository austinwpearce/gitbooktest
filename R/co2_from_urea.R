#-------------------------------------------------------------------------------
# CO2 from urea
#-------------------------------------------------------------------------------

calc_CO2_from_urea <- function(data) {
  
  require(tidyverse)
  require(sf)

  ghg_category <- "CO2 from urea fertilizer applications"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  
  # Get columns needed for calculation
  data_red <- data |> 
    st_drop_geometry() |> 
    select(scn_id, field_area, matches("fert.*.(rate|source|slow|inhibitor)"))
  
  # Ref table
  urea_props <- authority_table |> 
    filter(`Metric` == "GHG Emissions") |> 
    select(source_detail = `Source Detail`, urea_prop = `Urea fraction`) |>
    drop_na(urea_prop)

  # Compute total amount of urea 
  data_red <- data_red |> 
    pivot_longer(
      -c(scn_id, field_area), 
      values_transform = as.character
    ) |>
    separate(name, into = c("fertID", "name"), sep = "_") |>
    pivot_wider(id_cols = c(scn_id, field_area, fertID)) |>
    rename(source_detail = "source") |> 
    mutate(rate = as.numeric(rate)) |> 
    drop_na(rate) |> 
    group_by(scn_id, field_area, source_detail) |>
    summarise(rate = sum(rate, na.rm = T)) |> 
    ungroup() |> 
    left_join(urea_props) |> 
    mutate(
      # For US vg, app to urea = 0.5 * 0.42 + 0.25 = 0.465
      # N prop urea = 0.46
      rate = rate * if_else(
        condition = source_detail == "US average nitrogen fertilizer",
        true = urea_prop / 0.46,
        false = 1
      ),
      amount = rate * field_area * urea_prop    
    )  
    
  # Define factors and constants
  EF <- 0.2
  MW <- 44/12
    
  # Compute CO2 fluxes
  ghg_df <- data_red |>
    mutate(CO2 = amount * EF * MW, units = "kg") |> 
    group_by(scn_id, units) |>
    summarise(CO2_fossil = sum(CO2, na.rm = T)) |> 
    ungroup()
  ghg_df <- ghg_df |> 
    bind_rows(
      ghg_df |> mutate(units = "kg_CO2e")
    )

  # Collect results later
  ghg_df <- ghg_df |> 
    bind_cols(ghg_cols) |> 
    select(scn_id, metric:source_category, CO2_fossil, units) |> 
    group_nest(scn_id, .key = "co2_urea_results")

  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)
  return(data)

}
