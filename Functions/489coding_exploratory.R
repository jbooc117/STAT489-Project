library(tidyverse)
library(sf)
library(ggfortify)

df.heartDisease = filter(heartDisease, LocationAbbr == "TX" & Data_Value != "NA")
#cleans data down to approx. 267,000 rows from 5.7 million

lm.year = lm(df.heartDisease$Data_Value~df.heartDisease$Year)
#plot(lm.year)

