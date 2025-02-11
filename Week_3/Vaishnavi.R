# Load necessary libraries
library(readxl)
library(geosphere)
library(microbenchmark)

# Read the dataset
clinic_data <- read_excel("clinics.xls")

# Convert locLat and locLong to numeric
clinic_data$locLat <- as.numeric(clinic_data$locLat)
clinic_data$locLong <- as.numeric(clinic_data$locLong)

# Define Haversine function (fixed)
haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  deg_to_rad <- pi / 180
  
  lat1 <- lat1 * deg_to_rad
  lon1 <- lon1 * deg_to_rad
  lat2 <- lat2 * deg_to_rad
  lon2 <- lon2 * deg_to_rad
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  return(R * c)
}

# Method 1: For-loop approach
haversine_loop <- function(data) {
  distances <- numeric(nrow(data))
  for (i in 1:nrow(data)) {
    distances[i] <- haversine(40.671, -73.985, data$locLat[i], data$locLong[i])
  }
  return(distances)
}

# Method 2: Using apply function
haversine_apply <- function(data) {
  distances <- apply(data, 1, function(row) {
    haversine(40.671, -73.985, as.numeric(row["locLat"]), as.numeric(row["locLong"]))
  })
  return(distances)
}

# Method 3: Vectorized approach
haversine_vectorized <- function(data) {
  return(haversine(40.671, -73.985, data$locLat, data$locLong))
}

# Benchmarking the three methods
benchmark_results <- microbenchmark(
  loop = haversine_loop(clinic_data),
  apply = haversine_apply(clinic_data),
  vectorized = haversine_vectorized(clinic_data),
  times = 10
)

# Print benchmark results
print(benchmark_results)
