library(tidyverse)
library(sf)
library(ggfortify)

#Shombit code, used to start the creation of finalCVD

data.Tx <- subset(Rates_and_Trends_in_Heart_Disease_and_Stroke_Mortality_Among_US_Adults_35_by_County_Age_Group_Race_Ethnicity_and_Sex_2000_2019_20240208)
data.CD <- subset(data.Tx, Topic == "Cardiovascular disease (CVD)")
data.CD2 = subset(data.CD, select = -c(GeographicLevel, DataSource,Topic, Data_Value_Type,Data_Value_Footnote_Symbol,Data_Value_Footnote, Confidence_limit_High,Confidence_limit_Low, StratificationCategory1, StratificationCategory2, StratificationCategory3))
Data.CD4 <- filter(data.CD2, Data_Value != "NA")
Data.CD4 = subset(Data.CD4, select = -c(Stratification3))
Data.CD4 = Data.CD4%>%filter(Year == c("2015"))
Data.CD4 = Data.CD4%>%filter(Stratification1 == "Ages 35-64 years",
                  Stratification2 == "Overall")
View(Data.CD4)
#wideCVD = pivot_wider(Data.CD4)
#Data.CD5 = bind_cols(Data.CD4, US_Conus_VacSocial)


#Our data does not have longitude/latitude, but we were able to add it by matching the GEOID to the data from
#the US_Conus_VacSocial 
load("VacData.Rdata")
mergedData = merge(Data.CD4, US_Conus_VacSocial, by.x = "LocationID", by.y = "GEOID", all.x = TRUE) 
View(mergedData)

marchCVD = subset(mergedData, select = c(LocationID, County, State,
                                         lon, lat, perc_white,
                                         perc_africanamerican,
                                         perc_hispanic,
                                         perc_asian,
                                         Year, Data_Value,
                                         Data_Value_Unit,
                                         Stratification2,
                                         geometry))
View(marchCVD)
#marchCVD = marchCVD%>%filter(Data_Value_Unit != "%") #removes year ranges
na.omit(marchCVD)


st_write(marchCVD, "newFinalCVD.shp", geom_name = marchCVD$geometry)

savedCVD = read_sf("newFinalCVD.shp") #testing to see if .shp works
savedCVD = na.omit(savedCVD)
View(savedCVD)

## Filtering 2015 AQI Data
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
merge2015 = na.omit(merge2015)




### Previous Attempts
#Initial testing for what variables we could filter out
df.largeCVD = filter(Rates_and_Trends_in_Heart_Disease_and_Stroke_Mortality_Among_US_Adults_35_by_County_Age_Group_Race_Ethnicity_and_Sex_2000_2019_20240208
                     , LocationAbbr == "TX" & Data_Value != "NA")
View(df.largeCVD)
df.largeCVD = subset(df.largeCVD, select = -c(StratificationCategory1,StratificationCategory2,StratificationCategory3))
df.largeCVD = subset(df.largeCVD, select = -c(Topic, Data_Value_Footnote_Symbol, Data_Value_Footnote))

count(df.largeCVD, LocationDesc == "Anderson" & Year == 2009)
df.largeCVD = filter(df.largeCVD, Stratification3 != "Overall")

View(df2.largeCVD)
count(df.largeCVD, Stratification2 != "Overall")

df.largeCVD = subset(df.largeCVD, select = -c(GeographicLevel, DataSource))


