#-------------------------------------------------------------------------------
# Convert rates, yields and areas
#-------------------------------------------------------------------------------

USCS_to_SI <- function(data) {

  # Load conversion factors
  here(base_dir, "R/conversion-factors.R") |> source()

  # Load crops units lookup
  data <- data |> 
    left_join(
      here(base_dir, "data/crop_units.csv") |>
        read_csv() |> 
        select(crop = 1, factor = 4),
      by = "crop"
    ) |> 
    mutate(
      # Convert yields
      across(matches("yield"), ~ .x * factor * (lb_kg / ac_ha)),
      
      # Convert areas
      across(matches("area"), ~ .x * ac_ha),
      
      # Convert lb/ac rates
      across(
        c(
          seeding_rate, lime_rate, gypsum_rate,
          green_manure, compost_rate, biosolids_rate
        ),
        .fns = ~ .x * lb_kg / ac_ha
      ),
      
      # Conversions for pumping system parameters
      gross_water_pumped = gross_water_pumped * inch_mm, # acre-inches -> mm
      pumping_lift = pumping_lift * ft_mhead, # feet -> m_head
      pumping_pressure = pumping_pressure * psi_mhead,
      
      # convert field_ops_diesel from gal/ac to gal/ha
      field_ops_diesel = field_ops_diesel / ac_ha 

    ) |> 
      select(-factor)
  
  liq_ferts <- c(
    "Ammonia (aqueous)",
    "Urea ammonium nitrate",
    "Urea ammonium nitrate (green ammonia)",
    "Ammonia (aqueous) (green ammonia)"
  )
  
  data <- data |> 
    mutate(      
      # Convert rates
      across(
        matches("fert\\d+_rate"),
        ~ {
          col <- cur_column()
          src_col <- get(str_replace(col, "_rate", "_source"))
          .x * case_when(
              str_detect(src_col, "Ammonia (aqueous)") ~ 7.6,
              str_detect(src_col, "Urea ammonium nitrate") ~ 11.1,
              TRUE ~ 1) * lb_kg / ac_ha
        }
      )
    )
  
  return(data)
}
