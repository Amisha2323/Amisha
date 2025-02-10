library(tidyverse)
library(tictoc)

# Read the data
df <- readxl::read_excel("clinics.xls")

# Ensure lat/long are numeric
df <- df %>%
  mutate(
    locLat = as.numeric(locLat),
    locLong = as.numeric(locLong)
  )

# Haversine function
haversine <- function(lat1, lon1, lat2, lon2) {
  # Convert to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  km <- 6371 * c  # Earth's radius in km
  return(km)
}

# Reference point (St. Louis)
ref_lat <- 38.6270
ref_lon <- -90.1994

# Function to time execution in seconds
time_execution <- function(func) {
  start_time <- Sys.time()
  result <- func()
  end_time <- Sys.time()
  return(as.numeric(end_time - start_time))
}

# 1. Basic for loop
loop_time <- time_execution(function() {
  distances <- numeric(nrow(df))
  for(i in 1:nrow(df)) {
    distances[i] <- haversine(ref_lat, ref_lon, 
                              df$locLat[i], df$locLong[i])
  }
  return(distances)
})

# 2. Using apply
apply_time <- time_execution(function() {
  mapply(haversine, 
         ref_lat, ref_lon,
         df$locLat, df$locLong)
})

# 3. Using dplyr
dplyr_time <- time_execution(function() {
  df %>%
    mutate(distance = haversine(ref_lat, ref_lon, locLat, locLong))
})


cat("\nExecution times (in seconds):\n")
cat("For loop:", loop_time, "seconds\n")
cat("Apply method:", apply_time, "seconds\n")
cat("Dplyr method:", dplyr_time, "seconds\n")

