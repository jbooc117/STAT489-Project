library(sf)
library(GWmodel)
library(ggplot2)
set.seed(1234)
# Remove features with empty geometries
merge2015GW <- merge2015[!st_is_empty(merge2015), ]

# Calculate centroids of the MULTIPOLYGON geometries
mergedf_centroids <- st_centroid(merge2015GW)

# Extract coordinates
coords <- st_coordinates(mergedf_centroids)

# SpatialPolygonsDataFrame for bw.gwr
mergedf_spatial = merge2015GW%>%as_Spatial() #change name


#change forumla
opt_bandwidth <- bw.gwr(Data_Vl ~ 
                          prc_wht + prc_frc + prc_hsp + perc_sn + 
                          DaysCO + DaysNO2 + DaysOzone + DaysPM2_5 + DaysPM10, 
                        data = mergedf_spatial, 
                        kernel = "gaussian", 
                        parallel.method = "omp")

# Running GWR with the optimal bandwidth
gwr_results <- gwr.basic(Data_Vl ~ 
                           prc_wht + prc_frc + prc_hsp + perc_sn + 
                           DaysCO + DaysNO2 + DaysOzone + DaysPM2_5 + DaysPM10, 
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
                       limits = c(0, 100),
                       breaks = c(25, 50, 7)) +
  theme(text = element_text(size = 20), 
        legend.position = "bottom")
