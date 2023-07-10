# Load required packages
library(terra)
library(rasterVis)
library(tidyverse)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(cowplot)
library(flipPlots)
library(magrittr)
library(gridExtra)


# Set the directory where the raster files are located
raster_dir <- "data/LC/"

# Read in the legend for raster data
subset_legend <- read_csv("data/labura_lookup_lc.csv")
# rcl <- 
#   read_csv("data/kalbar/tabular/lc_rcl_klhk.csv")

# Read raster files
raster_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE) %>% 
  purrr::map(~rast(.x))

# Create a list to store the rasters
raster_list <- list()


# Generate color table for raster data
n_classes <- nrow(subset_legend)
coltb <- data.frame(t(col2rgb(rainbow(n_classes, end=.9), alpha=TRUE)))

# Loop through the raster files, classify them and apply color table
# Use purrr and pipes to read, classify, and apply color table to raster files
raster_list <- purrr::map(raster_files, function(raster_file) {
  r <- raster_file %>% 
    # classify(rcl) %>%
    as.factor()
  levels(r) <- subset_legend
  coltab(r) <- coltb
  r <- setNames(r,names(raster_file))
  r
  
})

# Create a function to generate ggplot objects for each raster
create_ggplot <- function(raster_object) {
  ggplot() + 
    geom_spatraster(data = raster_object) +
    scale_fill_hypso_d() +
    theme_bw() +
    labs(title = names(raster_object))
}

# Use purrr and pipes to create ggplot objects for each raster in the list
map_plots_list <- purrr::map(raster_list, create_ggplot)


# Subset the list to select the first, middle, and last rasters
subset_list <- raster_list[c(1, floor(length(raster_list)/2), length(raster_list))]
landuse_stack <- rast(raster_stack)

# Create a frequency table using crosstab
crosstab_ <- crosstab(landuse_stack) %>% 
  as.data.frame()
crosstab_ <- crosstab_[crosstab_$Freq != 0, ]

# Create input for Sankey diagram
input_sankey <- crosstab_ %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(Freq>mean(Freq))
  

# Generate Sankey diagram
input_sankey %>% 
  select(-Freq) %>% 
  SankeyDiagram(
    max.categories = 15,
    font.size = 10,
    sinks.right = T,
    label.show.varname = F,
    weights = input_sankey[["Freq"]],
    node.position.automatic = T,
    hovertext.show.percentages = FALSE,
    label.show.percentages = T,
    label.show.counts = F,
    link.color = "Source",
    variables.share.values = T
  )

map_viz <- rasterVis::levelplot(landuse_stack)

# Bind freq table
freq_tables <- lapply(raster_list, function(r) {
  table(as.factor(values(r)))
})
freq_df <- data.frame(freq_tables) %>% 
  # slice(1:nrow(subset_legend))%>%
  select(matches("Freq")) 

# save the files
write_csv(freq_df, "result/freq_df.csv")
write_csv(crosstab_, "result/crosstab_preques.csv")
save.image("result/pre_ques.RData")
# save RDS files
# save(crosstab_, landuse_stack, file = "preques_out.Rda")
# load("preques_out.Rda")
