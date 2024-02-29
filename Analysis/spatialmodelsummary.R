# Attempt to do GWR model summary on spatial data (errors with current data set)

library(sf)
library(sp)
library(spatialreg)


sp_data.Tx = as(data.Tx, "Spatial") # convert the data to spatial 
bw = bw.gwr(formula = TX~, approach = "AIC", adaptive = "T", data = sp_data.Tx) # kernel bandwidtch calculation
gwr_data.Tx = gwr.basic(formula = TX~, approach = "AIC", adaptive = "T", data = sp_data.TX) # fit the GWR model 
gwr_data.Tx
