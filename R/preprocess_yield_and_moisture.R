#' @title add_yields
#' @description
#' User entered yield values are not necessarily the yield at standard moisture
#' or at harvest
#' @param data crop input dataset
#' @param lookup lookup table with the crop standard yields and moistures

prep_yields <- function(data, lookup) {
  
  # Hard-coded variables
  seed_lint <- 0.365 # lb to lb for cotton (also in conversion-factors.R)
  lint_seed <- 1 / seed_lint # lb lint -> lb seed cotton (lint + seed + trash)
  
  # Process input data and create new column for output
  data |>
    # add standard moisture (remember it is in decimal)
    left_join(lookup |> select(crop, standard_moisture),
              relationship = "many-to-many") |> 
    mutate(
      harvest_moisture = if_else(!is.na(harvest_moisture),
                                 true = harvest_moisture,
                                 false = standard_moisture * 100 + moisture_removed),
      pct_sugar = pct_sugar / 100) |>
    # add yield at standard moisture
    mutate(
      yield_adjusted = case_when(
        crop == "Alfalfa" ~ yield * (1 - harvest_moisture / 100) / (1 - standard_moisture),
        crop == "Cotton" ~ yield, # lint-yield
        crop == "Rice" ~ yield * (1 - harvest_moisture / 100) / (1 - standard_moisture),
        crop == "Sugar beets" ~ yield * pct_sugar,
        .default = yield),
      .after = yield) |> 
    # add yield at harvest
    mutate(
      yield_harvest = case_when(
        crop == "Alfalfa" ~ yield, # yield entered is the yield at baling
        crop == "Sugar beets" ~ yield,
        crop == "Cotton" ~ yield_adjusted * lint_seed,
        # for corn, grains, etc.
        .default = yield * (1 - standard_moisture) / (1 - harvest_moisture / 100)),
      .after = yield_adjusted)
}
