#-------------------------------------------------------------------------------
# CH4 from rice cultivation
#-------------------------------------------------------------------------------

calc_CH4_from_rice <- function(data) {
  
  require(tidyverse)
  require(sf)

  ghg_category <- "CH4 emissions from flooded rice cultivation"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  
  # Get columns needed for calculation
  data_red <- data |> 
    st_drop_geometry() |> 
    select(
      scn_id, field_area, rice_ratooning, state, county,
      starts_with(c("fert", "lime", "gypsum")), 
      rice_water_mgt, rice_water_mgt_preseason, rice_residue,
      seeding_method, planting_date, harvest_date, clay,
      rice_previous_yield, rice_straw_inc,
      manure_kg_applied, green_manure, compost_rate,
      gwp_ch4_biogenic
    )
  
  # Ref table
  s_fert <- authority_table |> 
    filter(Metric == "GHG Emissions") |> 
    select(source = `Source Detail`, Sprop = "S fraction") |> 
    drop_na(Sprop)

  # COMPUTE SULFUR RATES AND SCALING FACTOR
  s_df <- bind_rows(
    # Inorganic fertilizers
    data_red |> 
      pivot_longer(starts_with("fert"), values_transform = as.character) |> 
      separate(name, into = c("fertID", "name")) |> 
      pivot_wider(id_cols = c(scn_id, fertID)) |> 
      mutate(rate = as.numeric(rate)),
    # Gypsum
    data_red |> 
      select(scn_id, rate = gypsum_rate) |> mutate(source = "Gypsum")
  ) |> 
    left_join(s_fert) |> 
    drop_na(rate) |> 
    transmute(scn_id, SR = rate * Sprop) |> 
    
    # Summarize S Rates
    group_by(scn_id) |> 
    summarise(SR = sum(SR, na.rm = T)) |> 
    ungroup() |> 
  
    # Compute SF_s factor
    mutate(
      SF_s = case_when(
        between(SR, 0.01, 338) ~ 1 - (SR * 0.00133),
        SR > 338 ~ 0.55, 
        TRUE ~ 1
      ),
    )

  
  # Compute Organic Amendment rates by field
  manure_CFOA <- 0.21
  compost_CFOA <- 0.17
  green_manure_CFOA <- 0.45
  rice_dm <- 0.86
  rice_hi <- 0.42

  org_df <- data_red |> 
    select(
      scn_id, field_area, rice_straw_inc, manure_kg_applied, compost_rate, green_manure, rice_previous_yield
    ) |> 
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |> 
    mutate(
      
      manure_roa = manure_kg_applied / field_area * manure_CFOA,
      compost_roa = compost_rate * compost_CFOA,
      green_manure_roa = green_manure * green_manure_CFOA,
      
      # Add method for estimate straw
      straw_roa = (rice_previous_yield * (1 / rice_hi - 1) * rice_dm) *
        if_else(rice_straw_inc == "> 30 days before cultivation", 0.19, 1),

      # Organic amendment combined rate
      ROA = (manure_roa + compost_roa + green_manure_roa + straw_roa) * 1e-3,

      # COmpute SF_o factors
      SF_o = (1 + ROA)^0.59

    ) 

  # Join factors by region
  data_red <- list(
    data_red, 
    tier2_counties, 
    rice_emission_factors,
    s_df, 
    org_df
  ) |> 
    reduce(left_join) |> 
    mutate(across(starts_with("SF_"), ~ replace_na(.x, 1)))

  # Add Management factors
  data_red <- data_red |> 
    mutate(
      SF_e = if_else(str_detect(seeding_method, "high_residue") & region == "California", 0.4, 1),
      SF_w = case_when(
        rice_water_mgt == "Continuously flooded" ~ 1,
        rice_water_mgt == "Intermittently flooded, single aereation" ~ 0.61,
        rice_water_mgt == "Intermittently flooded, multiple aereation" ~ 0.17
      ),
      SF_p = case_when(
        region == "Mid-South" & rice_water_mgt_preseason == "Flooded preseason > 30 days" ~ 2.41,
        region == "California" & rice_water_mgt_preseason == "Nonflooded preseason" ~ 0.41,
        TRUE ~ 1
      ),
      SF_r = case_when(
        region == "California" & rice_residue == "Low or medium residue" ~ 0.46,
        region == "Mid-South" & rice_residue == "High residue" ~ 2.16,
        TRUE ~ 1
      )
    )

  # Combute EF base scenarios
  ghg_df <- data_red |> 
    mutate(

      # Cap clay values to 54%
      clay = if_else(clay > 54, 54, clay),

      t_days = as.numeric(as_date(harvest_date) - as_date(planting_date)),
      i = if_else(rice_ratooning == "Yes", 2, 1),

      # Compute base emission factor and scaled emission
      EF_base = (EF_sa - ((clay - BPC) * C_f)) / CP,
      EF_i = EF_base * SF_w * SF_p * SF_o * SF_s * SF_r * SF_e, 

      # Total, by area and by crop unit CH4 Emissions
      CH4_biogenic = EF_i * t_days * field_area,
      units = "kg"
    )
  
  ghg_df <- ghg_df |> 
    bind_rows(
      mutate(ghg_df, CH4_biogenic = CH4_biogenic * gwp_ch4_biogenic, units = "kg_CO2e")
    )
    
  # Collect results later
  ghg_df <- ghg_df |> 
    bind_cols(ghg_cols)|>
    select(scn_id, metric:source_category, CH4_biogenic, units) |> 
    group_nest(scn_id, .key = "ch4_rice_results")
  
  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)
  return(data)
}
