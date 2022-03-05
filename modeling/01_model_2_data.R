
##If you don't have pacman first run: 
#install.packages("pacman") before running this script
pacman::p_load(readxl,tidyverse,  scales, data.table, stringr, lubridate, zoo, odbc, parsedate, DBI,httr,readxl, sandwich, lmtest, stargazer)

#####################Data from model 1 to join with our new covariates###############################################
trips_and_vac  <-read_rds("Lab-2/01_county_level/model_1.rds")


#####################Education###############################################
#Source: https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
Education <- read_excel("Lab-2/01_county_level/USDA/Education.xls")
Education <- Education[4:nrow(Education),]
colnames(Education) <- Education[1,]
Education <- Education[2:nrow(Education),]

Education<- Education %>% 
  select(colnames(Education)[1], colnames(Education[, (ncol(Education)-3):ncol(Education)])) %>% 
  rename( "pct_less_hdiploma" =`Percent of adults with less than a high school diploma, 2015-19`,    
"pct_hdiploma_only" =`Percent of adults with a high school diploma only, 2015-19`,
"pct_some_college" =`Percent of adults completing some college or associate's degree, 2015-19`,
"pct_college_higher" =`Percent of adults with a bachelor's degree or higher, 2015-19`           )

Education[,2:5] <- sapply(Education[,2:5], as.numeric)

#####################Poverty Estimates###############################################
#Source: https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
Poverty <- read_excel("Lab-2/01_county_level/USDA/PovertyEstimates.xls")
Poverty <- Poverty[4:nrow(Education),]
colnames(Poverty) <- Poverty[1,]
Poverty <- Poverty[2:nrow(Poverty),]

#Description of selected variables
" 
FIPStxt= State-county FIPS code
PCTPOVALL_2019= Estimated percent of people of all ages in poverty 2019
MEDHHINC_2019= Estimate of median household income 2019
"
Poverty <- Poverty %>% select(FIPStxt,PCTPOVALL_2019,MEDHHINC_2019)
Poverty[,2:ncol(Poverty)] <- sapply(Poverty[,2:ncol(Poverty)], as.numeric) 



#####################Population Estimates###############################################
#Source: https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
Population <- read_excel("Lab-2/01_county_level/USDA/PopulationEstimates.xls")
Population <- Population[2:nrow(Population),]
colnames(Population) <- Population[1,]
Population <- Population[2:nrow(Population),]

Population <- Population %>% select(FIPStxt, POP_ESTIMATE_2019)
Population$POP_ESTIMATE_2019 <- as.numeric(Population$POP_ESTIMATE_2019)


#####################Unenployment Estimates###############################################
#Source: https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
Unemployment <- read_excel("Lab-2/01_county_level/USDA/Unemployment.xlsx")
Unemployment <- Unemployment[4:nrow(Unemployment),]
colnames(Unemployment) <- Unemployment[1,]
Unemployment <- Unemployment[2:nrow(Unemployment),]

Unemployment <-Unemployment %>% select(FIPS_Code,
Unemployment_rate_2019,
Unemployment_rate_2020) 
Unemployment[, 2:3] <- sapply(Unemployment[, 2:3], as.numeric)
Unemployment <- Unemployment%>%
  mutate(
  change_unemployment_pandemic= Unemployment_rate_2020- Unemployment_rate_2019)


###Union of all data
colnames(Education)[1] <- "FIPS"
colnames(Poverty)[1] <- "FIPS"
colnames(Population)[1] <- "FIPS"
colnames(Unemployment)[1] <- "FIPS"


full_data <- left_join(Education, Poverty, by= "FIPS")
full_data <- left_join(full_data, Population, by="FIPS")
full_data <- left_join(full_data, Unemployment, by="FIPS")
full_data <- left_join(trips_and_vac, full_data, by="FIPS" )


write_rds(full_data, "Lab-2/01_county_level/model_2.rds")

colnames(data_model2)



