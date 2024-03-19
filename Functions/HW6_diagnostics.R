# load the data (code from homework.pdf in simulations folder) 
load("C:/Users/Christina Kim/Downloads/VacData.Rdata")
data <- read.csv("C:/Users/Christina Kim/Downloads/finalCVD (2).csv")

indices = data %>% select(LocationID) %>% mutate(LocationID = as.character(LocationID)) %>% mutate(Digits = str_length(LocationID)) %>% select(Digits) %>% `==`(4) %>% which()
mergedData_2 <- merge(US_Conus_VacSocial, data, by.x = "GEOID", by.y = "LocationID") 
my_data_2 <- subset(mergedData_2, select = -c(perc_vac, population, population.persqkm, median_householdincome, perc_HealthInsurance, LocationAbbr, LocationDesc, Class, Stratification1, Year, Data_Value_Unit))

data <- my_data_2[,c("Data_Value", "perc_asian", "perc_white", "perc_hispanic", "perc_africanamerican")]
train_indices <- sample(1:nrow(data), size = floor(0.8 * nrow(data)))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

rf_model <- randomForest(Data_Value ~ perc_asian + perc_white + perc_hispanic + perc_africanamerican, data = train_data, ntree = 10)

# use the ggfortify library to produce diagnostic plots 

library("ggfortify")
autoplot(rf_model, which = 1:6, ncol = 3, label_size = 3)

# Comments on diagnostic plots 
# residual vs fitted values - shows random distribution 
# Q-Q plot - checks normality assumption of errors 
# scale vs location - checks the assumption of equal variance 
# residuals vs leverage - identify any influential observations 
