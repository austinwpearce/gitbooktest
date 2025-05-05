#' @title calc_drying
#' @description
#' Calculates a table of energy and emissions for crop drying
#' @param data input scenario data table
#' @param lookup lookup table of energy and emissions factors (authority.csv)

# Primary Function =============================================================

calc_drying <- function(data, lookup) {
  
  if (missing(data)) {
    stop("Please provide the field input data.")
  }
  
  # if (missing(lookup)) {
  #   stop("Please provide the lookup table with pumping emissions factors.")
  # }
  
  # Hard-coded variables
  
  data |>
    # st_drop_geometry() |> 
    # select(scn_id, field_name, egrid_subregion, crop, field_area, yield_adjusted,
    #        harvest_moisture,
    #        is_crop_dried, is_crop_dried_onfarm,
    #        drying_system, crop_transport_total, moisture_removed,
    #        cotton_moisture_level, cotton_region) |> 
    # join in missing information on thermal efficiency values from Hoffman table
    left_join(tbl_drying_systems, by = "drying_system") |>
    left_join(tbl_cotton, by = c("cotton_moisture_level", "cotton_region")) |> 
    # Calculate direct energy use of irrigation pumping
    mutate(
      kg_water_removed_per_kg_crop = case_when(
        crop %in% c("Corn (silage)", "Cotton", "Peanuts",
                    "Potatoes", "Sugar beets") ~ NA_real_, 
        str_detect(crop, "Wheat") ~ 
          0.0106173 * moisture_removed + 0.0001592 * moisture_removed ^ 2,
        .default = 0.0113483 * moisture_removed + 0.0001711 * moisture_removed ^ 2),
      
      # calculate total water removed for the whole crop
      total_kg_water_removed = kg_water_removed_per_kg_crop * crop_transport_total,
      
      # calculate total cotton lint
      cotton_lint_total = case_when(
        crop == "Cotton" ~ yield_adjusted * field_area,
        .default = NA_real_),
      
      # peanut energy per kg
      peanut_gas_mj_per_kg = case_when(
        crop == "Peanuts" ~ (62618 * harvest_moisture - 578344) * btu_mj / ton_kg,
        .default = NA_real_),
      
      peanut_electric_mj_per_kg = case_when(
        crop == "Peanuts" ~ (2.991 * harvest_moisture - 27.7) * kwh_mj / ton_kg,
        .default = NA_real_),
      
      # total MJ from gas
      drying_total_mj_gas = case_when(
        crop == "Cotton" ~ gas_mj_per_kg_lint * cotton_lint_total,
        crop == "Peanuts" ~ peanut_gas_mj_per_kg * crop_transport_total,
        .default =  total_kg_water_removed * gas_mj_per_kg_water),
      
      # total MJ from electricity
      drying_total_mj_electric = case_when(
        crop == "Cotton" ~ electric_mj_per_kg_lint * cotton_lint_total,
        crop == "Peanuts" ~ peanut_electric_mj_per_kg * crop_transport_total,
        .default = total_kg_water_removed * electric_mj_per_kg_water),
      
      # Convert MJ of gas to volume of gas (gallons for LP, scf for Nat.Gas)
      drying_gas_source = case_when(
        crop == "Cotton" ~ cotton_gas_source,
        .default = "LPG"),
      
      drying_gas_volume = case_when(
        crop == "Cotton" & cotton_region == "SW" ~ drying_total_mj_gas * natgas_mj_cf,
        # default includes Cotton in SE which is LPG
        .default = drying_total_mj_gas * lpg_mj_gal),
      
      drying_gas_volume_units = case_when(
        drying_gas_source == "LPG" ~ "gallon",
        drying_gas_source == "Natural gas" ~ "scf"),
      
      # electricity
      drying_mwh_electricity = drying_total_mj_electric * mj_mwh,
      
      # Step 6 Calculate emissions
      # Create two lists of tables, one for gas portion and one for electric portion
      drying_gas_results = pmap(
        # list of arguments in the make_table_pumping_emissions() function
        list(
          is_crop_dried = is_crop_dried,
          on_farm = is_crop_dried_onfarm,
          energy_source = drying_gas_source,
          q_energy = drying_gas_volume,
          report = gwp_assessment_report,
          horizon = gwp_time_horizon
        ),
        # function uses the irrigation lookup data from the authority table
        ~ tabulate_drying_emissions(
          lookup = lookup,
          is_crop_dried = ..1,
          on_farm = ..2,
          energy_source = ..3,
          q_energy = ..4,
          report = ..5,
          horizon = ..6
        )
      ),
      
      drying_electric_results = pmap(
        # list of arguments in the make_table_pumping_emissions() function
        list(
          is_crop_dried = is_crop_dried,
          on_farm = is_crop_dried_onfarm,
          energy_source = "Electricity",
          q_energy = drying_mwh_electricity,
          egrid_subregion = egrid_subregion,
          report = gwp_assessment_report,
          horizon = gwp_time_horizon
        ),
        # function uses the irrigation lookup data from the authority table
        ~ tabulate_drying_emissions(
          lookup = lookup,
          is_crop_dried = ..1,
          on_farm = ..2,
          energy_source = ..3,
          q_energy = ..4,
          egrid_subregion = ..5,
          report = ..6,
          horizon = ..7
        )
      )
      )
  
}

# Supporting Functions =========================================================

# energy_source: source of pumping energy (diesel, electricity, etc)
# q_energy: the quantity of the energy source (gal, cft, MWh)
# egrid_subregion: the four-letter eGRID subregion in which the electricity was produced

tabulate_drying_emissions <- function(lookup, 
                                      is_crop_dried,
                                      on_farm, 
                                      energy_source,
                                      q_energy,
                                      egrid_subregion = NULL,
                                      report, 
                                      horizon) {
  
  if (is_crop_dried == "No") {
    tbl_tmp <- lookup |> 
      # don't need these columns anymore
      select(-subregion) |> 
      # fill output with NA values
      summarise(across(where(is.character), ~ NA_character_),
                across(where(is.numeric), ~ NA_real_)) |> 
      mutate(units = NA_character_)
    
    return(tbl_tmp)
  }
  
  # Define system boundaries based on on_farm argument
  boundaries <- if (on_farm == "Yes") {
    c("Upstream", "On-Farm Mechanical")
  } else {
    c("Upstream", "Post-Harvest")
  }
  
  if (missing(q_energy)) {
    stop("Please provide a value for the quantity of energy source")
  }
  
  # Validate the energy_source input
  valid_details <- c("Electricity", "LPG", "Natural gas")
  if (!energy_source %in% valid_details) {
    stop("Error: 'energy_source' must be 'Electricity', 'LPG', or 'Natural gas'.")
  }
  
  # Require egrid_subregion only for electricity
  if (energy_source == "Electricity" && is.null(egrid_subregion)) {
    stop("eGRID subregion is required when energy_source is 'Electricity'.")
  }
  
  if (energy_source == "Electricity") {
    tbl_tmp <- lookup |> 
      filter(str_detect(source_detail, energy_source),
             subregion == egrid_subregion) |> 
      # don't need these columns anymore
      select(-subregion)
    
  } else {
    tbl_tmp <- lookup |> 
      filter(str_detect(source_detail, energy_source),
             system_boundary %in% boundaries) |> 
      # don't need these columns anymore
      select(-subregion)
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

