---
title: "Simulation"
format: html
editor: visual
---

```{r}
library(GWmodel)
library(dplyr)
library(ggplot2)
library(MASS)
library(sf)# For mvrnorm
set.seed(123) # For reproducibility


finalCVD <- read_sf("C:/Users/Shombit Roy/OneDrive/Desktop/finalCVDshapes.shp")
finalCVD <- na.omit(finalCVD)

covariates <- subset(finalCVD, select = c(prc_wht, prc_frc, prc_hsp, perc_sn, p2_5_20, estimat, U__2015, geometry))


covariates_cleaned <- st_drop_geometry(covariates)
covariates_cleaned <- na.omit(covariates_cleaned)


cov_matrix <- cov(covariates_cleaned)
simulated_covariates <- as.data.frame(mvrnorm(n = nrow(covariates_cleaned), mu = colMeans(covariates_cleaned), Sigma = cov_matrix))
names(simulated_covariates) <- names(covariates_cleaned)


covariates_centroids <- st_centroid(finalCVD)
covariates$lon <- st_coordinates(covariates_centroids)[,1]
covariates$lat <- st_coordinates(covariates_centroids)[,2]


#piecewise
median_lat <- mean(finalCVD$lat)
median_lon <- mean(finalCVD$lon)

get_coefficients <- function(lat, lon, median_lat, median_lon) {
  if (lat >= median_lat && lon < median_lon) {
    c(63.96, 213.1, 63.37, -52.57, 4.244, -0.001999, 3.194)
  } else if (lat >= median_lat && lon >= median_lon) {
    c(-85.72, 37.2, -113.7, -117.3, 7.714, -0.002183, 4.078)
  } else if (lat < median_lat && lon < median_lon) {
    c(-152.9, -16.67, -151.6, -411.2, 6.369, -0.001991, 4.248)
  } else if (lat < median_lat && lon >= median_lon) {
    c(-95.24, 35.8, -121.7, -212.8, 6.821, -0.002247, 4.13)
  } else {
    c(-81.158, 55.26, -93.21, -209.0, 6.311, -.00212, 3.73)  # Default coefficients if no conditions are met
  }
}

# Applying the piecewise coefficients for the simulated variable to not make it fixed 
simulated_covariates$Data_Vl <- mapply(function(idx) {
  coefs <- get_coefficients(covariates$lat[idx], covariates$lon[idx], median_lat, median_lon)
  sum(coefs * simulated_covariates[idx, 1:7]) + rnorm(1, mean = 0, sd = 1)
}, idx = 1:nrow(simulated_covariates))


finalCVD_simulated <- bind_cols(st_sf(covariates), simulated_covariates)

# Convert to Spatial Dataframe if needed and perform GWR
finalCVD_spatial = as_Spatial(finalCVD_simulated)

opt_bandwidth_2 <- bw.gwr(Data_Vl ~ prc_wht...11 + prc_frc...12 + prc_hsp...13 + perc_sn...14 + p2_5_20...15 + estimat...16 + U__2015...17, 
                          data = finalCVD_spatial, 
                          kernel = "gaussian", 
                          adaptive = FALSE)

gwr_results <- gwr.basic(Data_Vl ~ prc_wht...11 + prc_frc...12 + prc_hsp...13 + perc_sn...14 + p2_5_20...15 + estimat...16 + U__2015...17, 
                         data = finalCVD_spatial, 
                         bw = opt_bandwidth_2, 
                         kernel = "gaussian", 
                         adaptive = FALSE)

# Convert to sf and plot
gwr_results_sf = as(gwr_results$SDF, "sf")




gwr_results_sf%>%ggplot() +
  geom_sf(aes(fill = residual),
          color = scales::alpha("black",
                                alpha = 0.1
                                ))





library(sf)
library(spdep)
gwr_results_spatial <- as(gwr_results_sf, "Spatial")

nb <- poly2nb(gwr_results_spatial)

listw <- nb2listw(nb, style="W", zero.policy=TRUE)

class(gwr_results_spatial$residual)

moran_test <- moran.test(gwr_results_spatial$residual, listw, alternative = "two.sided")

moran_test

gwr_results_sf %>%
  ggplot() +
  geom_sf(aes(fill = residual),
          color = scales::alpha("black", alpha = 0.1)) +
  scale_fill_viridis_c()

```
