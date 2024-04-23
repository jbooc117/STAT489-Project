#library(sf)
#library(GWmodel)
#library(ggplot2)
#library(tidyverse)
set.seed(1234)

finalCVD = read_sf("./Data/finalCVDshapes.shp")

# Remove features with empty geometries
merge2015GW = finalCVD[!st_is_empty(finalCVD), ]
merge2015GW = merge2015GW%>%na.omit(merge2015GW) #1 county missing

# Calculate centroids of the MULTIPOLYGON geometries
mergedf_centroids <- st_centroid(merge2015GW)

# Extract coordinates
coords <- st_coordinates(mergedf_centroids)

# SpatialPolygonsDataFrame for bw.gwr
mergedf_spatial = merge2015GW%>%as_Spatial() #change name


#change formula
opt_bandwidth <- bw.gwr(Data_Vl ~ 
                          prc_wht + prc_frc + prc_hsp + perc_sn + 
                          p2_5_20 + estimat + U__2015, 
                        data = mergedf_spatial, 
                        kernel = "gaussian", 
                        parallel.method = "omp")

# Running GWR with the optimal bandwidth for seed 1234: 128251.7
gwr_results <- gwr.basic(Data_Vl ~ 
                           prc_wht + prc_frc + prc_hsp + perc_sn + 
                           p2_5_20 + estimat + U__2015, 
                         data = mergedf_spatial, 
                         bw = opt_bandwidth, 
                         kernel = "gaussian", 
                         adaptive = FALSE, 
                         parallel.method = "omp")
#Plot GWR
# Convert SDF into SF 
gwr_results_sf = gwr_results$SDF%>%as("sf")

gwr_results_sf%>%ggplot() +
  geom_sf(aes(fill = gwr_results_sf$y),
          color = scales::alpha("black",
                                alpha = 0.1
                                )) +
  scale_fill_gradientn(colours = terrain.colors(8),
                       #limits = c(19,530),
                       breaks = c(100,250,350)) +
  theme(text = element_text(size = 20), 
        legend.position = "bottom") + 
  labs(title = "GWR for particulate concentrations, 2015",
       fill = "Deaths from CVD per 100,000 people")

#Parameter Analysis
gwr_results

