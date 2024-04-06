library(tidyverse)
library(sf)
library(ggfortify)
library(readr)

#Shombit code, used to start the creation of finalCVD

Rates_and_Trends_in_Heart_Disease_and_Stroke_Mortality_Among_US_Adults_35_by_County_Age_Group_Race_Ethnicity_and_Sex_2000_2019_20240208 <- read_csv("Rates_and_Trends_in_Heart_Disease_and_Stroke_Mortality_Among_US_Adults__35___by_County__Age_Group__Race_Ethnicity__and_Sex___2000-2019_20240208.csv")
data.Tx <- subset(Rates_and_Trends_in_Heart_Disease_and_Stroke_Mortality_Among_US_Adults_35_by_County_Age_Group_Race_Ethnicity_and_Sex_2000_2019_20240208)
data.CD <- subset(data.Tx, Topic == "Cardiovascular disease (CVD)")
data.CD2 = subset(data.CD, select = -c(GeographicLevel, DataSource,Topic, Data_Value_Type,Data_Value_Footnote_Symbol,Data_Value_Footnote, Confidence_limit_High,Confidence_limit_Low, StratificationCategory1, StratificationCategory2, StratificationCategory3))
Data.CD4 <- filter(data.CD2, Data_Value != "NA")
#Data.CD4 = subset(Data.CD4, select = -c(Stratification3))
Data.CD4 = Data.CD4%>%filter(Year == c("2015"))
Data.CD4 = Data.CD4%>%filter(Stratification1 == "Ages 35-64 years",
                  Stratification2 == "Overall",
                  Stratification3 == "Overall",
                  LocationAbbr != "AK",
                  LocationAbbr != "HI")
View(Data.CD4)

#Our data does not have longitude/latitude, but we were able to add it by matching the GEOID to the data from
#the US_Conus_VacSocial 
load("VacData.Rdata")
mergedData = merge(Data.CD4, US_Conus_VacSocial, by.x = "LocationID", by.y = "GEOID", all.x = TRUE) 
View(mergedData)

marchCVD = subset(mergedData, select = c(LocationID, Year, County, State,
                                         lon, lat, perc_white,
                                         perc_africanamerican,
                                         perc_hispanic,
                                         perc_asian,
                                         Data_Value,
                                         Data_Value_Unit,
                                         geometry))
View(marchCVD)
#marchCVD = marchCVD%>%filter(Data_Value_Unit != "%") #removes year ranges
na.omit(marchCVD)


st_write(marchCVD, "newFinalCVD.shp", geom_name = marchCVD$geometry)

savedCVD = read_sf("newFinalCVD.shp") #testing to see if .shp works
savedCVD = na.omit(savedCVD)
View(savedCVD)

#Merge with PM2.5 data
pm2015 = read_sf("pm25-2015.shp")
pm2015_nogeom = st_drop_geometry(pm2015)
View(pm2015_nogeom)

cvd2015 = left_join(savedCVD, pm2015_nogeom, by = join_by("LoctnID" == "GEOID"))

cvd2015 = cvd2015%>%subset(select = -c(NAMELSAD))
View(cvd2015)

st_write(cvd2015, "particulate2015CVD.shp", geom_name = cvd2015$geometry)
testCVD = read_sf("particulate2015CVD.shp")

#Merge with Median Income Data
cvdIncome2015 = left_join(cvd2015, df_22, by = join_by("LoctnID" == "GEOID"))
cvdIncome2015 = cvdIncome2015%>%subset(select = -c(NAME, variable, moe))

#Merge with unemployment Data
unemployment2015 = read_csv("unemploymentRate2015.csv")
cvdFinale2015 = left_join(cvdIncome2015, unemployment2015[, c("FIPS_Code", "Unemployment_rate_2015")], 
                          by = join_by("LoctnID" == "FIPS_Code"))
st_write(cvdFinale2015, "finalCVDshapes.shp", geom_name = cvdFinale2015$geometry)

####Should only need this once you have downloaded the shapefile 

finalCVD = read_sf("finalCVDshapes.shp")
View(finalCVD)


### Past Attempt

## Filtering 2015 AQI Data
annual_aqi_by_county_2015 <- read_csv("annual_aqi_by_county_2015.csv")
aqi2015 = subset(annual_aqi_by_county_2015,
                 select = c(CountyName, State,
                            DaysWithAQI, DaysCO,
                            DaysNO2, DaysOzone,
                            DaysPM2_5, DaysPM10))
aqi2015 = filter(aqi2015, State != "Alaska")
aqi2015 = filter(aqi2015, State != "Hawaii")
aqi2015 = aqi2015%>%rename("County" = "CountyName")
View(aqi2015)

## Merging 2015 to Dataset
merge2015 = left_join(savedCVD, aqi2015, by = c("County", "State"))
View(merge2015)
#merge2015 = na.omit(merge2015)

st_write(merge2015, "mergeNA2015CVD.shp", geom_name = merge2015$geometry)
cvd2015 = read_sf("mergeNA2015CVD.shp")
cvd2015NAO = na.omit(cvd2015)
View(cvd2015)
View(cvd2015NAO)

