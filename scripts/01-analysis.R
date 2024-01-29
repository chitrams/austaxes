# Start here ----

# Understand how map2_dbl works; map the if statements to using map2_dbl
# Then make a shiny app out of it where you can enter deductions and select Medicare levy
# HECS debt?

# Using if statements -----

calculate_tax <- function(income) {
  if (income <= 18200) {
    tax_amount = 0
  } else if (income <= 45000) {
    tax_amount = (income - 18200) * 0.19
  } else if (income <= 120000) {
    tax_amount = (45000 - 18200) * 0.19 + (income - 45000) * 0.325
  } else if (income <= 180000) {
    tax_amount = (45000 - 18200) * 0.19 + (120000 - 45000) * 0.325 + (income - 120000) * 0.37
  } else {
    tax_amount = (45000 - 18200) * 0.19 + (120000 - 45000) * 0.325 + (180000 - 120000) * 0.37 +
      (income - 180000) * 0.45
  }
  
  levy_amount <- income * 0.02
  total_amount <- tax_amount + levy_amount
  
  paste0("Total tax is $", total_amount, ", inclusive Medicare levy (2%) of $", levy_amount, ".")
}

102898*0.8
calculate_tax(102808*0.8)
calculate_tax(112645*0.8)

# Using purrr's map -----

#%% map #1 -----

calculate_tax <- function(taxable_income, tax_data){
  cut_points <- cut(taxable_income, breaks = c(-Inf, tax_data$bracket, Inf),
                    labels = FALSE, right = FALSE)
  tax_amount <- sum(map2_dbl(cut_points, tax_data$rate,
                             ~ if (.x > 1) (taxable_income - tax_data$bracket[.x - 1]) * .y))
  return(tax_amount)
}

calculate_tax_more <- function(income, deductions, medicarelevy = TRUE){
  
  # Data for 2023-2024
  tax_brackets <- c(0, 18200, 45000, 120000, 180000, Inf)
  tax_rates <- c(0, 0.19, 0.325, 0.37, 0.45)
  
  # Calculations
  taxable_income <- income - deductions
  
  cut_points <- cut(income, breaks = tax_brackets, labels = FALSE, right = FALSE)
  
  tax_amount <- sum(map2_dbl(cut_points, tax_rates, 
                             ~ if (.x > 1) (income - tax_brackets[.x - 1]) * .y))
  
  if (medicarelevy) {
    levy_amount <- taxable_income * 0.02
    total_amount <- tax_amount + levy_amount
    
    paste0("Total tax is $", total_amount, " , inclusive Medicare levy (2%) of $", levy_amount, ".")
    
  } else {
    paste0("Total tax is $", tax_amount, "without the Medicare levy.")
  }
  
}

# Example usage:
calculate_tax_more(120000, 4000, medicarelevy = TRUE)
calculate_tax_simple(120000)

#%% Using map2dbl -------
# Copied directly, need to understand how it works

calculate_tax <- function(income, include_medicare_levy = TRUE) {
  tax_brackets <- c(0, 18200, 45000, 120000, 180000, Inf)
  tax_rates <- c(0, 0.19, 0.325, 0.37, 0.45)
  
  cut_points <- cut(income, breaks = tax_brackets, labels = FALSE, right = FALSE)
  
  # If include_medicare_levy is TRUE, add 2% Medicare levy
  if (include_medicare_levy) {
    tax_amount <- sum(map2_dbl(cut_points, tax_rates, 
                               ~ if (.x > 1) (income - tax_brackets[.x - 1]) * .y else 0))
    medicare_levy <- income * 0.02
    tax_amount <- tax_amount + medicare_levy
  } else {
    # If include_medicare_levy is FALSE, calculate tax without the Medicare levy
    tax_amount <- sum(map2_dbl(cut_points, tax_rates, 
                               ~ if (.x > 1) (income - tax_brackets[.x - 1]) * .y else 0))
  }
  
  return(tax_amount)
}

calculate_tax(120000, include_medicare_levy = FALSE)
calculate_tax_simple(120000)

#%% Using mapply -----

calculate_tax <- function(income, include_medicare_levy = TRUE) {
  tax_brackets <- c(0, 18200, 45000, 120000, 180000, Inf)
  tax_rates <- c(0, 0.19, 0.325, 0.37, 0.45)
  
  cut_points <- cut(income, breaks = tax_brackets, labels = FALSE, right = FALSE)
  
  # Calculate tax without Medicare levy
  tax_amount <- sum(mapply(function(.x, .y) {
    if (.x > 1) (income - tax_brackets[.x - 1]) * .y
    else 0
  }, cut_points, tax_rates))
  
  # Add Medicare levy if include_medicare_levy is TRUE
  if (include_medicare_levy) {
    medicare_levy <- income * 0.02  # 2% Medicare levy
    tax_amount <- tax_amount + medicare_levy
  }
  
  return(tax_amount)
}
