---
  title: "Final GW model"
format: html
editor: visual
---
  
  ## Quarto
  
  Quarto enables you to weave together content and executable code into a finished document. To learn more about Quarto see <https://quarto.org>.

## Running Code

When you click the **Render** button a document will be generated that includes both content and the output of embedded code. You can embed code like this:
  
  ```{r}
library(sf)
library(GWmodel)
library(ggplot2)
set.seed(123)

finalCVD <- read_sf("finalCVDshapes.shp") #relative file path


# Remove features with empty geometries
merge2015GW = finalCVD[!st_is_empty(finalCVD), ]
merge2015GW = merge2015GW%>%na.omit(merge2015GW)

mergedf_centroids <- st_centroid(merge2015GW)

coords <- st_coordinates(mergedf_centroids)

mergedf_spatial = merge2015GW%>%as_Spatial()

opt_bandwidth <- bw.gwr(Data_Vl ~ 
                          prc_wht + prc_frc + prc_hsp + perc_sn + 
                          p2_5_20 + estimat + U__2015, 
                        data = mergedf_spatial, 
                        kernel = "gaussian", 
                        parallel.method = "omp")

gwr_results <- gwr.basic(Data_Vl ~ 
                           prc_wht + prc_frc + prc_hsp + perc_sn + 
                           p2_5_20 + estimat + U__2015, 
                         data = mergedf_spatial, 
                         bw = opt_bandwidth, 
                         kernel = "gaussian", 
                         adaptive = FALSE, 
                         parallel.method = "omp")

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

```

```{r}
gwr_results_sf %>% 
  mutate("Death Abs Err" = abs(yhat - y)) %>%
  ggplot() +
  geom_sf(aes(fill = `Death Abs Err`),
          color = scales::alpha("black",
                                alpha = 0.1)) +
  scale_fill_viridis_c(limits = c(0, 60),
                       breaks = c(10, 30, 50)) +
  theme(text = element_text(size = 20), 
        legend.position = "bottom") +
  labs(title = "Absolute Prediction Error of Death by GWR w/gaussian kernel")
```

```{r}
#significance
local_t_values <- gwr_results_sf$prc_wht_TV
local_p_values <- 2 * pt(-abs(local_t_values), df = 2480.149) #df number cam from the Gw diagnostic


gwr_results_sf$prc_wht_p_values <- local_p_values


gwr_results_sf$prc_wht_signif_coef <- ifelse(gwr_results_sf$prc_wht_p_values < 0.05, 
                                             gwr_results_sf$prc_wht, 
                                             NA)  # Set to NA if not significant

library(ggplot2)
ggplot(gwr_results_sf) +
  geom_sf(aes(fill = prc_wht_signif_coef), color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA, 
                      name = "Coefficient Magnitude", limits = NULL) +  
  labs(title = "Local Significance and Magnitude of 'prc_wht' Parameter",
       subtitle = "Geographically Weighted Regression (GWR)",
       caption = "Red indicates larger magnitude of effect;\nwhite indicates no significant effect") +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "right")


```

```{r}
#significance
local_t_values_2 <- gwr_results_sf$prc_frc_TV
local_p_values_2 <- 2 * pt(-abs(local_t_values_2), df = 2480.149) #df number cam from the Gw diagnostic


gwr_results_sf$prc_af_p_values <- local_p_values_2


gwr_results_sf$prc_af_signif_coef <- ifelse(gwr_results_sf$prc_af_p_values < 0.05, 
                                            gwr_results_sf$prc_frc, 
                                            NA)  # Set to NA if not significant

library(ggplot2)
ggplot(gwr_results_sf) +
  geom_sf(aes(fill = prc_af_signif_coef), color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA, 
                      name = "Coefficient Magnitude", limits = NULL) +  
  labs(title = "Local Significance and Magnitude of 'prc_af' Parameter",
       subtitle = "Geographically Weighted Regression (GWR)",
       caption = "Red indicates larger magnitude of effect;\nwhite indicates no significant effect") +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "right")
```

```{r}
#significance
local_t_values_3 <- gwr_results_sf$prc_hsp_TV
local_p_values_3 <- 2 * pt(-abs(local_t_values_3), df = 2480.149) #df number cam from the Gw diagnostic



gwr_results_sf$prc_hsp_p_values <- local_p_values_3


gwr_results_sf$prc_hsp_signif_coef <- ifelse(gwr_results_sf$prc_hsp_p_values < 0.05, 
                                             gwr_results_sf$prc_hsp, 
                                             NA)  # Set to NA if not significant

library(ggplot2)
ggplot(gwr_results_sf) +
  geom_sf(aes(fill = prc_hsp_signif_coef), color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA, 
                      name = "Coefficient Magnitude", limits = NULL) +  
  labs(title = "Local Significance and Magnitude of 'prc_hsp' Parameter",
       subtitle = "Geographically Weighted Regression (GWR)",
       caption = "Red indicates larger magnitude of effect;\nwhite indicates no significant effect") +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "right")
```

```{r}
#significance
local_t_values_4 <- gwr_results_sf$perc_sn_TV
local_p_values_4 <- 2 * pt(-abs(local_t_values_4), df = 2480.149) #df number cam from the Gw diagnostic


gwr_results_sf$prc_sn_p_values <- local_p_values_4


gwr_results_sf$prc_sn_signif_coef <- ifelse(gwr_results_sf$prc_sn_p_values < 0.05, 
                                            gwr_results_sf$perc_sn, 
                                            NA)  # Set to NA if not significant

library(ggplot2)
ggplot(gwr_results_sf) +
  geom_sf(aes(fill = prc_sn_signif_coef), color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA, 
                      name = "Coefficient Magnitude", limits = NULL) +  
  labs(title = "Local Significance and Magnitude of 'prc_sn' Parameter",
       subtitle = "Geographically Weighted Regression (GWR)",
       caption = "Red indicates larger magnitude of effect;\nwhite indicates no significant effect") +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "right")
```

```{r}
#significance
local_t_values_5 <- gwr_results_sf$p2_5_20_TV
local_p_values_5 <- 2 * pt(-abs(local_t_values_5), df = 2480.149) #df number cam from the Gw diagnostic



gwr_results_sf$prc_p2_p_values <- local_p_values_5


gwr_results_sf$prc_p2_signif_coef <- ifelse(gwr_results_sf$prc_p2_p_values < 0.05, 
                                            gwr_results_sf$p2_5_20, 
                                            NA)  # Set to NA if not significant

library(ggplot2)
ggplot(gwr_results_sf) +
  geom_sf(aes(fill = prc_p2_signif_coef), color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA, 
                      name = "Coefficient Magnitude", limits = NULL) +  
  labs(title = "Local Significance and Magnitude of 'prc_p2_5' Parameter",
       subtitle = "Geographically Weighted Regression (GWR)",
       caption = "Red indicates larger magnitude of effect;\nwhite indicates no significant effect") +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "right")
```

```{r}
#significance
local_t_values_6 <- gwr_results_sf$estimat_TV
local_p_values_6 <- 2 * pt(-abs(local_t_values_6), df = 2480.149) #df number cam from the Gw diagnostic


gwr_results_sf$prc_est_p_values <- local_p_values_6


gwr_results_sf$prc_est_signif_coef <- ifelse(gwr_results_sf$prc_est_p_values < 0.05, 
                                             gwr_results_sf$estimat, 
                                             NA)  # Set to NA if not significant

library(ggplot2)
ggplot(gwr_results_sf) +
  geom_sf(aes(fill = prc_est_signif_coef), color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA, 
                      name = "Coefficient Magnitude", limits = NULL) +  
  labs(title = "Local Significance and Magnitude of 'estimat' Parameter",
       subtitle = "Geographically Weighted Regression (GWR)",
       caption = "Red indicates larger magnitude of effect;\nwhite indicates no significant effect") +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "right")
```

```{r}
#significance
local_t_values_7 <- gwr_results_sf$U__2015
local_p_values_7 <- 2 * pt(-abs(local_t_values_7), df = 2480.149) #df number cam from the Gw diagnostic


gwr_results_sf$prc_U_2015_p_values <- local_p_values_7


gwr_results_sf$prc_U_2015_signif_coef <- ifelse(gwr_results_sf$prc_U_2015_p_values < 0.05, 
                                                gwr_results_sf$U__2015, 
                                                NA)  # Set to NA if not significant

library(ggplot2)
ggplot(gwr_results_sf) +
  geom_sf(aes(fill = prc_U_2015_signif_coef), color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA, 
                      name = "Coefficient Magnitude", limits = NULL) +  
  labs(title = "Local Significance and Magnitude of 'estimat' Parameter",
       subtitle = "Geographically Weighted Regression (GWR)",
       caption = "Red indicates larger magnitude of effect;\nwhite indicates no significant effect") +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "right")
```