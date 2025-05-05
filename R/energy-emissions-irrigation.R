#' @title calc_irrigation
#' @description
#' Calculates a table of energy and emissions for irrigation pumping
#' @param data input scenario data table
#' @param lookup lookup table of energy and emissions factors (authority.csv)

# Primary Function =============================================================

calc_irrigation <- function(data, lookup) {
  
  if (missing(data)) {
    stop("Please provide the field input data.")
  }
  
  if (missing(lookup)) {
    stop("Please provide the lookup table with pumping emissions factors.")
  }
  
  # Hard-coded variables
  # None
  
  # Process input data
  data |> 
    #st_drop_geometry() |> 
    # join in missing information on thermal efficiency values from Hoffman table
    left_join(tbl_pump_performance, by = "pumping_energy_source") |> 
    # Calculate direct energy use of irrigation pumping
    mutate(
      pumping_dir_energy = calc_dir_energy_pump(
        hectares = field_area,
        gross_water_pumped = gross_water_pumped,
        lift = pumping_lift,
        pressure = pumping_pressure,
        energy_source = pumping_energy_source,
        eff_power_unit = thermal_efficiency),
      
      pumping_dir_energy_units = "MJ",
      
      # units of total pumping energy source = 
      total_pumping_energy_source = case_when(
        is_irrigated == "No" ~ NA_real_,
        # convert MJ to gallons of diesel
        pumping_energy_source == "Diesel (ag equipment)" ~ pumping_dir_energy * diesel_mj_gal,
        # convert MJ to MWh of electricity (for next problem)
        pumping_energy_source == "Electricity (grid)" ~ pumping_dir_energy * mj_mwh,
        # convert MJ to gallons of Gasoline
        pumping_energy_source == "Gasoline" ~ pumping_dir_energy * gasoline_mj_gal,
        # convert MJ to MWh of electricity (for next problem)
        pumping_energy_source == "LPG" ~ pumping_dir_energy * lpg_mj_gal,
        # convert MJ to MWh of electricity (for next problem)
        pumping_energy_source == "Natural gas" ~ pumping_dir_energy * natgas_mj_cf),
      # add units
      pumping_energy_source_units = case_when(
        pumping_energy_source %in% c("Diesel (ag equipment)", "Gasoline", "LPG") ~ "gallon",
        pumping_energy_source == "Electricity (grid)" ~ "MWh",
        pumping_energy_source == "Natural gas" ~ "scf"), 
      # Step 6 Calculate emissions
      # create a new list of result tables called tbl_pumping_emissions
      irrigation_results = pmap(
        # list of arguments in the make_table_pumping_emissions() function
        list(
          energy_source = pumping_energy_source,
          q_energy = total_pumping_energy_source,
          egrid_subregion = egrid_subregion,
          report = gwp_assessment_report,
          horizon = gwp_time_horizon
        ),
        # function uses the irrigation lookup data from the authority table
        ~ tabulate_pumping_emissions(lookup = lookup, 
                                     energy_source = ..1,
                                     q_energy = ..2,
                                     egrid_subregion = ..3,
                                     report = ..4,
                                     horizon = ..5)))
}


# Supporting Functions =========================================================
# returns MJ of direct energy use
# This value will help calculate the amount of fuel or electricity used
calc_dir_energy_pump <- function(hectares, # hectares
                                 gross_water_pumped, # mm
                                 lift, # m
                                 pressure, # m
                                 energy_source,
                                 eff_power_unit # percent (0-1)
){
  
  # Step 1 Total Head (meters)
  total_head <- lift + pressure
  
  # Step 2 Ideal Energy (MJ)
  E_ideal <- (total_head * hammmhead_MJ * gross_water_pumped * hectares)
  
  # Step 3 Efficiency
  eff_pump <- 0.75  # standard value
  # gear/belt, elec = 1, else 0.95
  eff_drive <- if_else(energy_source == "Electricity (grid)",
                       true = 1,
                       false = 0.95)
  
  eff_overall <- eff_pump * eff_drive * eff_power_unit
  
  # Step 4 Direct Energy Use of Pump (MJ)
  dir_energy <- if_else(condition = is.na(gross_water_pumped),
                        true = NA_real_, 
                        false = E_ideal / eff_overall)
  
  return(dir_energy)
  }

# energy_source: source of pumping energy (diesel, electricity, etc)
# q_energy: the quantity of the energy source (gal, cft, MWh)
# egrid_subregion: the four-letter eGRID subregion in which the electricity was produced

tabulate_pumping_emissions <- function(lookup, energy_source, q_energy,
                                       egrid_subregion = NULL,
                                       report, horizon) {
  
  if (is.na(energy_source)) {
    tbl_tmp <- lookup |> 
      # don't need these columns anymore
      select(-subregion, -pumping_energy_source) |> 
      # fill output with NA values
      summarise(across(where(is.character), ~ NA_character_),
                across(where(is.numeric), ~ NA_real_)) |> 
      mutate(units = NA_character_)
    
    return(tbl_tmp)
  }
  
  if (missing(q_energy)) {
    stop("Please provide a value for the quantity of energy source")
  }
  
  # Validate the energy_source input
  valid_details <- c("Diesel (ag equipment)", "Electricity (grid)", "LPG", "Gasoline", "Natural gas")
  if (!energy_source %in% valid_details) {
    stop("Error: 'energy_source' must be 'Diesel (ag equipment)', 'Electricity (grid)', 'Gasoline', 'LPG', or 'Natural gas'.")
  }
  
  # Require egrid_subregion only for electricity
  if (energy_source == "Electricity (grid)" && is.null(egrid_subregion)) {
    stop("eGRID subregion is required when energy_source is 'Electricity'.")
  }
  
  if (energy_source == "Electricity (grid)") {
    tbl_tmp <- lookup |> 
      filter(pumping_energy_source == energy_source & subregion == egrid_subregion) |> 
      # don't need these columns anymore
      select(-subregion, -pumping_energy_source)
    
  } else {
    tbl_tmp <- lookup |> 
      filter(pumping_energy_source == energy_source) |> 
      # don't need these columns anymore
      select(-subregion, -pumping_energy_source)
  }
  
  
  # Calculate kg of gas and MJ per unit of fuel
  # Simplified Feb 20 because we will do total summaries at the very end now instead.
  tbl_kg_gas <- tbl_tmp |> 
    # Multiply each component by the volume of fuel
    mutate(across(.cols = c(CO2_fossil:MJ),
                  .fns = ~ .x * q_energy),
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

