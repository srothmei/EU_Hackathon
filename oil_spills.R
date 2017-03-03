# The tools
library(tidyverse) 
library(highcharter) 
library(lubridate)
library(rvest)
library(janitor)
library(stringr)
library(jsonlite)
library(countrycode)
options(highcharter.debug = TRUE)

# Load the data
json <- read_lines("https://ourworldindata.org/wp-content/uploads/nvd3/nvd3_multiBarChart_Oil/multiBarChart_Oil.html")
json <- json[seq(
  which(str_detect(json, "var xxx")),
  first(which(str_detect(json, "\\}\\]\\;")))
)]

json <- fromJSON(str_replace_all(json, "var xxx = |;$", ""))
json <- transpose(json)
#str(json)

#Transform data
dspills <- map_df(json, function(x) {
  df <- as.data.frame(x[["values"]])
  df$key <- x[["key"]]
  tbl_df(df)
  df
})
# Data is ready!

# Staked Area Chart
hcspills <- hchart(dspills, "areaspline", hcaes(x, y, group = "key")) %>%
            hc_plotOptions(series = list(stacking = "normal")) %>%
            hc_xAxis(type = "datetime") %>%
            hc_title(text = "Number of Oil Spills Over the Past 4 Decades")
hcspills

# Modify Stuff
hcspills2 <- hcspills %>%
              hc_colors(c("#000000", "#222222")) %>%
              hc_title(
                align = "left",
                style = list(color = "black")
              ) %>%
              hc_credits (
                enabled = TRUE,
                text = "Data from ITOPF.com",
                href = "http://www.itopf.com/knowledge-resources/data-statistics/statistics/"
              ) %>%
              hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
              hc_chart(divBackgroundImage = "http://www.drodd.com/images14/ocean-wallpaper30.jpg",
                       backgroundColor = hex_to_rgba("white", 0.50)) %>%
              hc_tooltip(sort = TRUE, table = TRUE) %>%
              hc_legend(align = "right", verticalAlign = "top", layout = "horizontal") %>%
              hc_xAxis(opposite = TRUE, gridLineWidth = 0,
                       title = list(text = "Time", style = list(color = "black")),
                       lineColor = "black", tickColor = "black",
                       labels = list(style = list(color = "black"))) %>% 
              hc_yAxis(reversed = TRUE, gridLineWidth = 0, lineWidth = 1, lineColor = "black",
                       tickWidth = 1, tickLength = 10, tickColor = "black",
                       title = list(text = "Oil Spills", style = list(color = "black")),
                       labels = list(style = list(color = "black"))) %>% 
              hc_add_theme(hc_theme_elementary())
hcspills2
  
  
  
  
  
  
  
  
  
  
  


