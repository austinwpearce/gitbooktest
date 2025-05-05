#' @title add_gwp_factors
#' @author Austin
#' @description
#' Returns new columns that store GWP factors for CO2, CH4, N2O, etc. so that
#' factors can vary by scenario
#' @param data input scenarios table

# Primary Function ============================================================
add_gwp_factors <- function(data, lookup) {
  
  if (missing(data)) {
    stop("Please provide the field input data.")
  }
  
    # Process input data and create new column for output
  left_join(data, lookup)
  
}