#' @title calc_input_transport
#' @description
#' Returns a list of tables of energy and emissions for ag input transportation
#' @param data crop input data table
#' @param lookup lookup table of energy and emission values

# Primary Function ============================================================
calc_input_transport <- function(data, lookup) {
  
  if (missing(data)) {
    stop("Please provide the field input data.")
  }
  
  if (missing(lookup)) {
    stop("Please provide the lookup table with manure transport emissions factors.")
  }
  
  # Process input data and create new column for output
  data |>
    mutate(input_transport_results = pmap(
      list(report = gwp_assessment_report,
           horizon = gwp_time_horizon),  # List of inputs changing per row
      ~ tabulate_it_emissions(lookup = lookup,
                              report = ..1,
                              horizon = ..2)))
  
}

# Supporting function ==========================================================

# called inside calc_input_transport()
# This function calculates a table of energy and emissions for input transportation
tabulate_it_emissions <- function(lookup, report, horizon) {
  
  # Hard-coded factors
  distance <- 122 # miles
  mpg <- 6.9 # mpg same as crop transport
  
  # derive volume of fuel
  v_diesel <- distance / mpg # gallons of diesel
  
  # Calculate kg of gas and MJ per unit of fuel
  tbl_kg_gas <- lookup |> 
    # Multiply each component by the volume of fuel
    mutate(across(.cols = c(CO2_fossil:MJ),
                  .fns = ~ .x * v_diesel),
           units = case_when(metric == "Energy Use" ~ "MJ",
                             metric == "GHG Emissions" ~ "kg"))
  
  # Convert gas totals from kg to kg CO2e using GWP factors
  gwp <- tbl_gwp_factors |> 
    filter(gwp_assessment_report == report,
           gwp_time_horizon == horizon)
  
  tbl_kg_co2e <- tbl_kg_gas |> 
    filter(metric == "GHG Emissions") |> 
    mutate(
      CO2_fossil   = CO2_fossil * gwp$gwp_co2_fossil,
      CO2_biogenic = CO2_biogenic * gwp$gwp_co2_biogenic,
      CH4_fossil   = CH4_fossil * gwp$gwp_ch4_fossil,
      CH4_biogenic = CH4_biogenic * gwp$gwp_ch4_biogenic,
      N2O          = N2O * gwp$gwp_n2o,
      NF3          = NF3 * gwp$gwp_nf3,
      SF6          = SF6 * gwp$gwp_sf6,
      units = "kg_CO2e")
  
  # join the rows into one neat table
  return(bind_rows(tbl_kg_gas, tbl_kg_co2e))
}
