#' @title calc_crop_transport
#' @description
#' Returns a list of tables of energy and emissions for crop transportation
#' @param data input data table of scenarios
#' @param lookup lookup table of energy and emission values associated with fuels

# Primary Function ============================================================
calc_crop_transport <- function(data, lookup) {
  
  if (missing(data)) {
    stop("Please provide the field input data.")
  }
  
  if (missing(lookup)) {
    stop("Please provide the lookup table with manure transport emissions factors.")
  }
  
  # hard-coded 
  mpg <- 6.9 # One mpg for both diesel and biodiesel at the moment
  
  round_trip <- 1.8 # the return trip uses 25% less fuel.
  # previously calculated as E + 0.8E (which = 1.8E)
  
  # Process input data and create new column for output
  data |>
    # Step 2 calculate total kg production to transport for the whole field 
    mutate(crop_transport_total = yield_harvest * field_area) |> 
    # Step 3 Determine transportation capacity
    left_join(tbl_crop_transport_capacity |> 
                select(crop, crop_transport_capacity = "kg_per_load")) |> 
    # Step 4: Determine the volume of fuel burned
    mutate(
      # Calculate the number of trips, rounding up
      crop_transport_n_trip = ceiling(crop_transport_total / crop_transport_capacity),
      # Return the volume of fuel used for round trip (1.8x includes loaded and empty)
      crop_transport_fuel = round_trip * crop_transport_n_trip * crop_transport_distance / mpg,
      crop_transport_fuel_units = "gallon") |> 
    # Step 5 Calculate energy and emissions
    # create a new list of result tables called crop_transport_results
    mutate(
      crop_transport_results = pmap(
        list(v_fuel = crop_transport_fuel,
             fuel_type = crop_transport_fuel_type,
             # where the crop is dried determines crop transport being assigned
             # to on-farm mechanical or postharvest
             on_farm = is_crop_dried_onfarm,
             # which GWP factors are used
             report = gwp_assessment_report,
             horizon = gwp_time_horizon),  # List of inputs changing per row
        ~ tabulate_ct_emissions(lookup = lookup,
                                v_fuel = ..1,
                                fuel_type = ..2,
                                on_farm = ..3,
                                report = ..4,
                                horizon = ..5)))
  
}

# Supporting function ==========================================================
# called inside calc_crop_transport()
# This function calculates a table of energy and emissions for crop transportation
# on_farm is an argument for where the crop was taken after harvest.
# It affects the system boundaries
tabulate_ct_emissions <- function(lookup, v_fuel, fuel_type, on_farm,
                                  report, horizon) {
  
  # probably won't need the following as all crops are transported
  if (is.na(v_fuel)) {
    tbl_tmp <- lookup |>
      # fill output with NA values
      summarise(across(where(is.character), ~ NA_character_),
                across(where(is.numeric), ~ NA_real_)) |>
      mutate(units = NA_character_)
    
    return(tbl_tmp)
  }
  
  if (missing(v_fuel)) {
    stop("Please provide a value for v_fuel.")
  }
  
  # Validate the fuel_type input
  valid_fuel_types <- c("Diesel", "Biodiesel")
  if (!fuel_type %in% valid_fuel_types) {
    stop("Error: 'fuel_type' must be either 'Diesel' or 'Biodiesel'.")
  }
  
  # Validate the on_farm input
  valid_on_farm <- c("Yes", "No")
  if (!on_farm %in% valid_on_farm) {
    stop("Error: 'on_farm' must be either 'Yes' or 'No'.")
  }
  
  # Define system boundaries based on on_farm argument
  boundaries <- if (on_farm == "Yes") {
    c("Upstream", "On-Farm Mechanical")
  } else {
    c("Upstream", "Post-Harvest")
  }
  
  # Calculate kg of gas and MJ per unit of fuel
  tbl_kg_gas <- lookup |> 
    # Keep only rows where source_detail matches the specified fuel type
    filter(str_detect(source_detail, fuel_type),
           system_boundary %in% boundaries) |> 
    # Multiply each component by the volume of fuel
    mutate(across(.cols = c(CO2_fossil:MJ),
                  .fns = ~ .x * v_fuel),
           units = case_when(metric == "Energy Use" ~ "MJ",
                             metric == "GHG Emissions" ~ "kg"))
  
  # Convert gas totals from kg to kg CO2e using GWP factors
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

# The twilight zone ===========================================================
# The two options for fuel type are "diesel" (default) and "biodiesel" (B100)
# Returns gallons of diesel or biodiesel
# total_crop in units of kg/field
# capacity in units of kg/load

# calc_transport_fuel <- function(total_crop, capacity, distance){
#   
#   # Calculate the number of trips, multiplying yield and area and divide by capacity, rounding up
#   n_trip <- ceiling(total_crop / capacity)
#   
#   # One mpg for both diesel and biodiesel at the moment
#   mpg <- 6.9 
#   
#   # Return the volume of fuel used for round trip (1.8x includes loaded and empty)
#   1.8 * n_trip * distance / mpg
#   
# }