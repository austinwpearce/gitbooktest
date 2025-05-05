# print result helper function
print_this <- function(x, units) {
  
  if (x < 0.01) {
    output <- format(round(x, 4), scientific = FALSE)
  } else if (x < 1) {
    output <- round(x, 3)
  } else if (x < 10){
    output <- round(x, 2)
  } else if (x < 100){
    output <- round(x, 1)
  } else {
    output <- format(round(x), big.mark = ",")
  }
  paste(output, units)
}

# Test
# print_this(0.0012345678, "units")
# 
# print_this(0.012345678, "units")
# 
# print_this(0.12345678, "units")
# 
# print_this(1.2345678, "units")
# 
# print_this(12.345678, "units")
# 
# print_this(1234.5678, "units")

