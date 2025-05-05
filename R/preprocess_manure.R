#' @title prep_manure
#' @description
#' User entered manure values are prepped for SI conversion and later calculations
#' @param data crop input dataset
#' @param lookup lookup table with the crop standard yields and moistures

prep_manure <- function(data, lookup) {
  
  # Hard-coded variables
  density <- 8.34  # lb/gal
  
  # Process input data
  data |>
    # fill in missing information on manure_region
    # (for looking up default content later if needed)
    left_join(tbl_manure_regions, by = "state") |> 
    # percent N per unit of manure
    mutate(manure_n_pct = manure_n / 2000 * 100,
           # manure_n_dry = manure_n / (manure_pct_solids / 100),
           .after = manure_n) |> 
    mutate(manure_p2o5_pct = manure_p2o5 / 2000 * 100, .after = manure_p2o5) |> 
    # Step 2: Determine total tons of manure transported to the field
    # field_area is assumed to be hectares prior to this calculation
    mutate(
      manure_ton_applied = case_when(
        is.na(manure_rate) ~ NA_real_,

        manure_type %in% c("Liquid", "Slurry") & manure_pct_solids <= 10 ~
          (manure_rate * density / 2000) * field_area,
        
        manure_type %in% c("Semi-solid", "Solid") & manure_pct_solids > 10 ~
          manure_rate * field_area),
      # manure_ton_dry = manure_ton_applied * (manure_pct_solids / 100),
      # Manure SI variables whole field basis
      manure_kg_applied = manure_ton_applied * ton_kg,
      manure_n_applied = manure_kg_applied * manure_n_pct / 100,
      manure_p2o5_applied = manure_kg_applied * manure_p2o5_pct / 100
      )
  
}
