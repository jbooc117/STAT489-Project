library(tidyverse)
library(sf)
library(ggfortify)

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

df.wideCVD = 
  
#Shombit code, used to start the creation of finalCVD
  
data.Tx <- subset(Rates_and_Trends_in_Heart_Disease_and_Stroke_Mortality_Among_US_Adults_35_by_County_Age_Group_Race_Ethnicity_and_Sex_2000_2019_20240208)
data.CD <- subset(data.Tx, Topic == "Cardiovascular disease (CVD)")
data.CD2 = subset(data.CD, select = -c(GeographicLevel, DataSource,Topic, Data_Value_Type,Data_Value_Footnote_Symbol,Data_Value_Footnote, Confidence_limit_High,Confidence_limit_Low, StratificationCategory1, StratificationCategory2, StratificationCategory3))
Data.CD3 = filter(data.CD2, Stratification2 != "Overall")
Data.CD4 <- filter(Data.CD3, Data_Value != "NA")
Data.CD4 = subset(Data.CD4, select = -c(Stratification3))
View(Data.CD4)
wideCVD = pivot_wider(Data.CD4)


#Our data does not have longitude/latitude, but we were able to add it by matching the GEOID to the data from
#the US_Conus_VacSocial 
load("VacData.Rdata")
mergedData = merge(Data.CD4, US_Conus_VacSocial, by.x = "LocationID", by.y = "GEOID", all.x = TRUE) 

finalCVD = slice(mergedData, 1:248883)
View(finalCVD)

#Removing the extra variables added by the merging (probably could have just selected the original columns)
finalCVD = subset(finalCVD, select = -c(LocationID, County, State, perc_vac, population,
                                          population.persqkm, perc_white, perc_africanamerican,
                                          perc_hispanic, perc_asian, median_householdincome,
                                          perc_HealthInsurance))
View(finalCVD)
write_csv(finalCVD, file = "C:/Users/jbooc/Documents/R Coding/STAT489-Project/finalCVD2.csv")

