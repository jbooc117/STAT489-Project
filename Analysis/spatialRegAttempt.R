library(tidyverse)
library(spdep)
library(spatialreg)
library(rgdal)
library(rgeos)

cvdCoords = cbind(data.Tx$lon, data.Tx$lat)
cvd.5nn = knearneigh(cvdCoords, k = 5, longlat = TRUE) #identifies 5 nearest neighbors
cvd.5nn.nb = knn2nb(cvd.5nn)
plot(cvd.5nn.nb, cvdCoords) #displays 5 nearest neighbors

# Above code should display a plot showing the 5 nearrest neighbors in the state of Texas.
# I am currently unsure how to have the outline of Texas show over the map.