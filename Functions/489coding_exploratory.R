library(tidyverse)
library(sf)
library(ggfortify)

df.heartDisease = filter(heartDisease, LocationAbbr == "TX" & Data_Value != "NA")
#cleans data down to approx. 267,000 rows from 5.7 million

lm.year = lm(df.heartDisease$Data_Value~df.heartDisease$Year)
#plot(lm.year)

# Initial data set contained too many points so by using filtering, the focus can be set to the heart disease prevalence in TX. Further filtering can be put in place in the future on specific counties, then variables such as race, income, and gender can be analyzed in these geographical units. 
