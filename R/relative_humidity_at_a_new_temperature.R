# Function to calculate saturation vapor pressure using Tetens equation
saturation_vapor_pressure <- function(temperature) {
  # Constants
  A1 <- 17.27
  B1 <- 237.7
  A2 <- 21.87
  B2 <- 265.5
  
  # Different equations for different temperature ranges
  if (temperature < 0) {
    alpha <- A2 * temperature / (B2 + temperature)
  } else {
    alpha <- A1 * temperature / (B1 + temperature)
  }
  
  return(exp(alpha))
}

# Function to calculate the relative humidity at a new temperature
relative_humidity_new_temp <- function(current_humidity, current_temp, new_temp) {
  
  # Calculating saturation vapor pressure for current and new temperatures
  saturation_vapor_pressure_current <- saturation_vapor_pressure(current_temp)
  saturation_vapor_pressure_new <- saturation_vapor_pressure(new_temp)
  
  # Calculating actual vapor pressure
  actual_vapor_pressure_current <- current_humidity * saturation_vapor_pressure_current / 100
  
  # Calculating new relative humidity
  new_humidity <- 100 * actual_vapor_pressure_current / saturation_vapor_pressure_new
  
  return(new_humidity)
}

# Example usage
current_humidity <- 40 # in percentage
current_temp <- 29     # in Celsius
new_temp <- 23         # in Celsius

new_humidity <- relative_humidity_new_temp(current_humidity, current_temp, new_temp)
cat("The estimated relative humidity at", new_temp, "°C is", new_humidity, "%\n")
