#' @title calc_manure_transport
#' @description
#' Returns a list of tables of energy and emissions for manure transportation
#' @param data input data table for fields receiving manure applications
#' @param lookup lookup table of energy and emission values

# Primary Function ============================================================
calc_manure_transport <- function(data, lookup) {
  
  if (missing(data)) {
    stop("Please provide the field input data.")
  }
  
  if (missing(lookup)) {
    stop("Please provide the lookup table with manure transport emissions factors.")
  }
  
  # Hard-coded variables
  # density <- 8.34  # lb/gal
  greet_constant <- 10416 # BTU/ton/mi (2024 FD-CIC value)
  
  # Process input data
  data |>
    # Step 3 Determine transportation distance
    mutate(manure_distance = case_when(manure_pct_solids > 10 ~ 30, 
                                       manure_pct_solids <= 10 ~ 3,
                                       .default = NA_real_)) |> 
    # Step 4: Calculate direct energy use of manure transportation (mt)
    # involves converting from BTU to MJ
    mutate(manure_transport_dir_energy = 
             manure_ton_applied * manure_distance * greet_constant * btu_mj,
           manure_transport_dir_energy_units = "MJ") |> 
    # Step 5 calculate the gallons of diesel
    mutate(manure_transport_diesel = manure_transport_dir_energy * diesel_mj_gal,
           manure_transport_diesel_units = "gallon") |> 
    # Step 6 Calculate emissions
    # create a new list of result tables called tbl_manure_transport_emissions
    mutate(
      manure_transport_results = pmap(
        list(v_diesel = manure_transport_diesel,
             report = gwp_assessment_report,
             horizon = gwp_time_horizon),  # List of inputs changing per row
        # function uses the irrigation lookup data from the authority table
        ~ tabulate_mt_emissions(lookup = lookup, 
                                v_diesel = ..1,
                                report = ..2,
                                horizon = ..3)))
  
}

# Supporting function ==========================================================
# called inside calc_manure_transport()
tabulate_mt_emissions <- function(lookup, v_diesel, report, horizon) {
  
  if (is.na(v_diesel)) {
    tbl_tmp <- lookup |> 
      # fill output with NA values
      summarise(across(where(is.character), ~ NA_character_),
                across(where(is.numeric), ~ NA_real_)) |> 
      mutate(units = NA_character_)
    
    return(tbl_tmp)
  }
  
    # Calculate kg of gas and MJ per unit of fuel
    # Simplified Feb 20 because we will do total summaries at the very end now instead.
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
    
    return(bind_rows(tbl_kg_gas, tbl_kg_co2e))
}
