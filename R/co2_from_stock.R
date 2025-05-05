#-------------------------------------------------------------------------------
# CO2 from soil carbon stock changes
#-------------------------------------------------------------------------------

calc_CO2_from_stock <- function(data) {
  
  require(tidyverse)
  require(sf)

  ghg_category <- "Soil carbon stock changes"

  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
    
  # Get columns needed for calculation
  data_red <- data |> 
    st_drop_geometry() |> 
    select(scn_id, year,field_area) 
      
  # Join tables
  ghg_df <- left_join(data_red, swat) |> 
    mutate(CO2_biogenic = -CO2_stock_change * field_area, units = "kg")
  ghg_df <- ghg_df |> 
    bind_rows(ghg_df |> mutate(units = "kg_CO2e"))
  
  # Collect results later
  ghg_df <- ghg_df |> 
    bind_cols(ghg_cols) |> 
    select(scn_id, metric:source_category, CO2_biogenic, units) |> 
    group_nest(scn_id, .key = "co2_stock_results")

  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)
  return(data)
 
}
