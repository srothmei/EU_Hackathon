library(ggplot2)
library(broom)

rm(list = ls())

# Load map data
map_data_ger <- readRDS("data/DEU_adm1.rds")

# Tidy map data for ggplot2
map_data_ger_tidy <- tidy(map_data_ger, region = "ID_1")

# Plot
ggplot(data = map_data_ger_tidy, aes(x=long, y=lat, group=group, fill=piece)) +
    geom_polygon() +
    coord_quickmap()

