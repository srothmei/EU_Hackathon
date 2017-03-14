library(dplyr)


#Function to select the number of missing workers for a specifiy job_cat for each of the regions
get_numMissing_perCountry <- function(occup_name) {
  #occup_name = "Potters and related workers"
  result <- filter(jobs_per_region_suggestions, DES_OCCUP_L3_NAME == occup_name) %>%
            select(region, DES_OCCUP_L3_NAME, numMissing) %>%
            distinct()
  return(result)
}

# Function to get the Top5 Missing Job-Categorys for a specific region
get_top_perOccup <- function(regionName, n = 5) {
  #regionName <- "ČESKÁ REPUBLIKA"
  result <- select(jobs_per_region_suggestions, region, DES_OCCUP_L3_NAME, numMissing) %>%
          filter(region == regionName) %>%
          distinct() %>%
          top_n(n, numMissing)
  return(result)
}

# 
