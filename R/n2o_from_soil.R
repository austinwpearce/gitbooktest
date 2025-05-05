#-------------------------------------------------------------------------------
# N2O from soil
#-------------------------------------------------------------------------------

calc_N2O_from_soil <- function(data) {

  require(tidyverse)
  require(sf)
  
  ghg_category <- "Soil N2O"
  
  ref_ghg <- authority_table |> 
    filter(`Source Category` == !!ghg_category)  
  ghg_cols <- ref_ghg |> clean_names() |> select(metric:source_category) |> distinct()
  
  # Get columns needed for calculation
  data_red <- data |>
    select(
      scn_id, crop, field_area, yield, state, county,
      tillage, rice_water_mgt, biochar_yr1, cover_crop_type, 
      residue_mgt, starts_with(c("fert")), 
      compost_rate, biosolids_rate, green_manure, manure_n_applied,
      livestock,
      gwp_n2o
    ) |>
    st_drop_geometry()
   
  n_fert <- authority_table |> 
    select(source_detail = `Source Detail`, Nprop, FR_sn) |>
    drop_na(Nprop) |> 
    distinct()
  
  # Other factors
  FR_on <- 0.21
  S_bc <- -0.23
  N2O_mw <- 44/28
  EF_leach <- 0.011
  green_manure_nprop <- 0.0325
  compost_nprop <- 0.0125
  biosolids_nprop <- 0.03

  # Map tillage categories
  data_red <- data_red |>
    mutate(
      tillage = if_else(str_detect(tillage, "Conventional|Reduced"), "Conventional or reduced till", tillage)
    )

  # Synthetic fertilizers
  syn_fert_df <- data_red |>
    pivot_longer(starts_with("fert"), values_transform = as.character) |>
    separate(name, into = c("fertID", "name")) |>
    pivot_wider(id_cols = c(scn_id, field_area, fertID)) |>
    mutate(rate = as.numeric(rate)) |>
    left_join(n_fert |> select(source = source_detail, Nprop)) |> 
    drop_na(rate) |> 
    mutate(syn_fert_n_app = field_area * rate * Nprop) |> 
    select(-rate, -Nprop)

  # Organic fertilizers
  org_fert_df <- data_red |>
    select(
      scn_id, field_area, manure_n_applied, green_manure, 
      biosolids_rate, compost_rate
    ) |> 
    mutate(
      green_manure_n_app = green_manure * green_manure_nprop * field_area,
      compost_n_app = compost_rate * compost_nprop * field_area,
      biosolids_n_app = biosolids_rate * biosolids_nprop * field_area
    ) |> 
    transmute(
      across(everything(), ~ replace_na(.x, 0)),
      scn_id, 
      # Appling Nprop for other sources fo manure
      org_fert_n_app = manure_n_applied + 
        green_manure_n_app + compost_n_app + biosolids_n_app
    )

  # Attach factors
  fert_df <- list(
    data_red |> select(scn_id, state, county, tillage, livestock),
    syn_fert_df,
    org_fert_df,
    climate_by_county |> select(-code),
    climate_factors,
    tillage_factors,
    # livestock_factors, # actually not used, prp factors are hard-coded later
    n_fert |> select(source = source_detail, FR_sn)
  ) |> 
    reduce(left_join) |> 
    mutate(
      FR_sn = replace_na(FR_sn, 0), 
      across(starts_with("S_"), ~ replace_na(.x, 0)),
      across(starts_with("EF_"), ~ replace_na(.x, 1)),
    )
    
  # Compute Fertilizers contribution
  fert_df <- fert_df |> 
    mutate(
      
      # Update scaling factors
      S_sr = if_else(slow %in% c("no", NA_character_), 0, S_sr),
      S_inh = if_else(inhibitor %in% c("no", NA_character_), 0, S_inh),

      # Split and scale by source
      Fin_sn = syn_fert_n_app * (1 + S_sr) * (1 + S_inh),
      Fin_on = org_fert_n_app,

      # Compute Amount of volatilization by source. FR_on is constant for all organic
      Fvol_sn = Fin_sn * FR_sn,
      Fvol_on = Fin_on * FR_on

    ) |> 
      select(-syn_fert_n_app, -org_fert_n_app)
    
  fert_df <- fert_df |> 
      group_by(scn_id, EF_sn, EF_on) |> 
      summarise(
        across(c(Fin_sn, Fin_on, Fvol_sn, Fvol_on), ~ sum(.x, na.rm = TRUE))
      ) |> 
      ungroup() |> 
      mutate(
        across(ends_with(c("_sn", "_on")), ~ replace_na(.x, 0))    
      )
    
  # Attach factors
  resid_df <- list(
    data_red |> select(scn_id, crop, field_area, yield, residue_mgt),
    n_residues,
    crop_factors |> select(-DM),
    tbl_crop_data |> select(crop, DM = standard_dm)
  ) |> 
    reduce(left_join) 

  # Compute residue contribution
  resid_df <- resid_df |> 
    mutate(
      Rm = case_when(
        residue_mgt %in% c("grazing", "baling") ~ 0.75,
        residue_mgt == "burning" ~ 0.9,
        .default = 0
      ),
      CB_a = yield / HI * field_area * DM,
      # add DM below because the USDA eq 3-17 is wrong (workspace issue #25)
      CRN_a = (CB_a - yield * field_area * DM) * N_a * (1 - Rm),
      CRN_b = CB_a * (1 + R) * N_b,
      Fin_cr = (CRN_a + CRN_b)
    ) 

  # Inputs from direct deposition
  animal_df <- data_red |> 
    transmute(scn_id, Fin_prp = 0, EF_prp = 0, Fvol_prop = Fin_prp * FR_on)

  # Combine sources
  data_red <- list(
    data_red,
    fert_df,
    resid_df,
    animal_df,
    climate_by_county |> select(-code),
    climate_factors |> select(-S_sr, -S_inh),
    tillage_factors,
    leaching_factors
  ) |> 
    reduce(left_join)
    
  # Rice fix factors
  data_red <- data_red |> 
    mutate(
      # Modify EF for rice
      EF_on = case_when(
        rice_water_mgt == "Continuously flooded" ~ 0.003,
        rice_water_mgt != "Continuously flooded" ~ 0.005,
        TRUE ~ EF_on
      ),
      EF_sn = case_when(
        rice_water_mgt == "Continuously flooded" ~ 0.003,
        rice_water_mgt != "Continuously flooded" ~ 0.005,
        TRUE ~ EF_sn
      ),

      # Fill Updatre biochar Scaling factor
      S_bc = if_else(biochar_yr1 == "Yes", S_bc, 0, missing = 0)

    )
    
  # Compute N2O emissions
  ghg_df <- data_red |>
      mutate(
    
        # N2O from inputs
        N2O_input = (
          (Fin_sn * EF_sn) +
          ((Fin_on + Fin_cr) * EF_on) +
          (Fin_prp * EF_prp)
        ) * (1 + S_till) * (1+ S_bc),
    
        # N2O volatilization and leaching
        N2O_vol = (Fvol_sn + Fvol_on) * EF_vol,
        N2O_leach = (Fin_sn + Fin_on + Fin_prp +  Fin_cr) * FR_leach * EF_leach,
    
        # Direct emissions
        N2O_direct = N2O_input * N2O_mw,
          
        # Indirect emissions
        N2O_indirect = (N2O_vol + N2O_leach) * N2O_mw,
        
        # Total emissions
        N2O_total = N2O_direct + N2O_indirect,        
      )
  
  ghg_df <- ghg_df |> 
    select(scn_id, N2O_direct, N2O_indirect) |> 
    pivot_longer(
      N2O_direct:N2O_indirect, 
      names_to = "source_detail", values_to = "N2O"
    ) |> 
    mutate(
      source_detail = case_when(
        source_detail == "N2O_direct" ~ "Direct emissions",
        source_detail == "N2O_indirect" ~ "Indirect emissions"
      ),
      units = "kg"
    )
  
  ghg_df <- ghg_df |>
    bind_rows(
      ghg_df |> mutate(N2O = N2O * gwp_n2o, units = "kg_CO2e")
    )
  

  # Collect results later
  ghg_df <- ghg_df |> 
    bind_cols(ghg_cols) |> 
    select(scn_id, metric:source_category, source_detail, N2O, units) |> 
    group_nest(scn_id, .key = "n2o_soil_results")

  # Attach results to the output table
  data <- left_join(data |> select(-any_of("ghg_df")), ghg_df)  
  return(data)

}
