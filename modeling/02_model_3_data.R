


pacman::p_load(readxl,tidyverse,  scales, data.table, stringr, lubridate, zoo, odbc, parsedate, DBI,httr,readxl, sandwich, lmtest, stargazer,fastDummies)

###################################Recreating the FIPS code from the US Census data###################################
#Source for age group data: https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
age_data_us <- read.csv("C:/Users/Mi Pc/Desktop/cc-est2019-alldata.csv")


age_data_FIPS <- age_data_us %>% select(STATE, COUNTY, STNAME, CTYNAME) %>% distinct()

##Extracting the FIPS code to join with the CENSUS data
Education <- read_excel("Lab-2/01_county_level/USDA/Education.xls")
Education <- Education[4:nrow(Education),]
colnames(Education) <- Education[1,]
Education <- Education[2:nrow(Education),]
FIPS_CODE <- Education %>% select(colnames(Education[,1:3])) 
colnames(FIPS_CODE)[1] <- "FIPS"


##county is the last three numbers and State is last two numbers  
age_data_FIPS$CountyCodeFIPS <- ifelse(nchar(age_data_FIPS$COUNTY)==1, paste0("00",age_data_FIPS$COUNTY), 
                                       ifelse(nchar(age_data_FIPS$COUNTY)==2, paste0("0",age_data_FIPS$COUNTY),
                                              age_data_FIPS$COUNTY))
age_data_FIPS$StateCodeFIPS <- ifelse(nchar(age_data_FIPS$STATE)==1, paste0("0",age_data_FIPS$STATE), 
                                              age_data_FIPS$STATE)
age_data_FIPS$FIPS <- paste0(age_data_FIPS$StateCodeFIPS,age_data_FIPS$CountyCodeFIPS)

verification <- left_join(FIPS_CODE, age_data_FIPS, by="FIPS")

##Final FIPS table for age data
final_FIPS <- na.omit(verification)
write_rds(final_FIPS, "Lab-2/01_county_level/fips_codebook.rds")




####################################Working on the age table######################################################

age_data_us <- age_data_us %>% select(STATE, YEAR, COUNTY, CTYNAME, TOT_POP, AGEGRP, TOT_MALE, TOT_FEMALE) %>% 
  filter(YEAR==12)


##Definitions from the codebook/ see cc-est2019-alldata.pdf
#12 = 7/1/2019 population estimate
# 0 = "Total"
# 1 = "Age 0 to 4 years"
# 2 = "Age 5 to 9 years"
# 3 = "Age 10 to 14 years"
# 4 = "Age 15 to 19 years"
# 5 = "Age 20 to 24 years"
# 6 = "Age 25 to 29 years"
# 7 = "Age 30 to 34 years"
# 8 =" Age 35 to 39 years"
# 9 = "Age 40 to 44 years"
# 10 = "Age 45 to 49 years"
# 11 = "Age 50 to 54 years"
# 12 = "Age 55 to 59 years"
# 13 = "Age 60 to 64 years"
# 14 = "Age 65 to 69 years"
# 15 = "Age 70 to 74 years"
# 16 = "Age 75 to 79 years"
# 17 = "Age 80 to 84 years"
# 18 =" Age 85 years or older"

children <- c(1,2,3,4)
adults <- c(5,6,7,8,9)
adults45 <- c(10,11,12,13)
adults65over <- c(14,15,16,17,18)

age_data_us$coded_age_group <- ifelse(age_data_us$AGEGRP %in% children, 'children', 
                                      ifelse(age_data_us$AGEGRP %in% adults, 'adults18',
                                             ifelse(age_data_us$AGEGRP %in% adults45, 'adults45',
                                                    ifelse(age_data_us$AGEGRP  %in% adults65over, 'adults65over', 
                                                           'Total'))))


age_data_us_transformed <- age_data_us %>% select(STATE, COUNTY, CTYNAME, coded_age_group, TOT_POP, TOT_MALE, TOT_FEMALE) %>%
  group_by(STATE, COUNTY, coded_age_group) %>% 
  summarise(total= sum(TOT_POP),
            total_male= sum(TOT_MALE),
            total_female= sum(TOT_FEMALE))


age_data_us_transformed2 <- age_data_us_transformed %>% select(STATE,COUNTY, coded_age_group, total) %>% 
  spread(coded_age_group, total)


##Getting the FIPS with the strategy implemented at the beginning of the script
age_data_us_transformed2$CountyCodeFIPS <- ifelse(nchar(age_data_us_transformed2$COUNTY)==1, paste0("00",age_data_FIPS$COUNTY), 
                                       ifelse(nchar(age_data_us_transformed2$COUNTY)==2, paste0("0",age_data_FIPS$COUNTY),
                                              age_data_us_transformed2$COUNTY))
age_data_us_transformed2$StateCodeFIPS <- ifelse(nchar(age_data_us_transformed2$STATE)==1, paste0("0",age_data_FIPS$STATE), 
                                                 age_data_us_transformed2$STATE)
age_data_us_transformed2$FIPS <- paste0(age_data_us_transformed2$StateCodeFIPS,age_data_FIPS$CountyCodeFIPS)

age_data_us_transformed2 <- age_data_us_transformed2 %>% mutate(
pct_children = children/Total,
pct_adults18 = adults18/Total,
pct_adults45= adults45/Total,
pct_adults65over = adults65over/Total) %>%
  select(FIPS, StateCodeFIPS,
         pct_children,
         pct_adults18,
         pct_adults45,
         pct_adults65over) %>%
  mutate(verification= pct_children+ pct_adults18+ pct_adults45 + pct_adults65over)


#######################Regional data##################
regional_data <- read_excel("Lab-2/01_county_level/CENSUS/state-geocodes-v2017.xlsx")
regional_data <- regional_data[5:nrow(regional_data),]
colnames(regional_data) <- regional_data[1,]
regional_data <- regional_data[2:nrow(regional_data),]
colnames(regional_data)[3] <- "StateCodeFIPS"


model_3 <- left_join(age_data_us_transformed2, regional_data, by="StateCodeFIPS")


model_3$USRegion <- ifelse(model_3$Region==1, 'Northeast Region',
                           ifelse(model_3$Region==2, 'Midwest Region', 
                                  ifelse(model_3$Region==3, 'South Region',
                                         'West Region')))


model_3 <- model_3 %>%
select("FIPS", "StateCodeFIPS", "pct_children", "pct_adults18", "pct_adults45", "pct_adults65over","USRegion")

##Joining with model 2 data
data_model2 <- read_rds("Lab-2/01_county_level/model_2.rds")

model_3_data <- left_join(data_model2, model_3, by="FIPS")
write_rds(model_3_data, "Lab-2/01_county_level/model_3.rds")
# extra_analysis <- as.data.frame(model_3 %>% select(FIPS, USRegion))
# 
# ##Creating Dummies 
# model_3 <- dummy_cols(model_3, select_columns = 'USRegion')
# 
# ##Joining with trips and vac
# trips_and_vac  <-read_rds("Lab-2/01_county_level/model_1.rds")
# 
# model_3 <- left_join(trips_and_vac, model_3, by="FIPS")
# 
# model_3 <- model_3 %>% select(
# -c(STATE, COUNTY, StateCodeFIPS, USRegion))
# 


write_rds(model_3, 'Lab-2/01_county_level/model_3.rds')





