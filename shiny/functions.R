library(dplyr)


#Function to select the number of missing workers for a specifiy job_cat for each of the regions
get_numMissing_perCountry <- function(data, occup_name) {
  #occup_name = "Potters and related workers"
  result <- filter(data, DES_OCCUP_L3_NAME == occup_name) %>%
            select(region, DES_OCCUP_L3_NAME, numMissing) %>%
            distinct()
  return(result)
}

# Function to get the Top5 Missing Job-Categorys for a specific region
get_top_perOccup <- function(data, regionName, n = 5) {
  #regionName <- "ČESKÁ REPUBLIKA"
  result <- select(data, region, DES_OCCUP_L3_NAME, numMissing) %>%
          filter(region == regionName) %>%
          distinct() %>%
          top_n(n, numMissing)
  return(result)
}

# Function to get the numer of missing workers for a specific region & specific occup
get_numMissing_occup_country <- function(data, occup_name, regionName) {
  result <- get_numMissing_perCountry(data, occup_name) %>%
            filter(region == regionName)
  return(result)
}

# Create a list of all Region Names in the data
get_regionNames <- function(data) {
  result <- unique(data$region)
  return(result)
}

# Function to enhance a top5-dataset with an additional occup group
add_occup <- function(data, top5_set, occup_name) {
  add_occup <- get_numMissing_occup_country(data, occup_name, regionName)
  result <- dplyr::union(add_occup,top5_set)  
  
  return(result)
}






