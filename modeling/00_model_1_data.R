

pacman::p_load(readxl,tidyverse,  scales, data.table, stringr, lubridate, zoo, odbc, parsedate, DBI,httr,readxl, sandwich, lmtest, stargazer)


#Source for vac: https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc
vac_data <- read.csv("C:/Users/Mi Pc/Google Drive/00_MIDS_BERKELEY/203-Statistics for Data Science/lab2-data/CDC- VacCOVID- July 13/COVID-19_Vaccinations_in_the_United_States_County.csv")
colnames(vac_data)[1] <- "Date"
vac_data$Date <- as.Date(vac_data$Date, format="%m/%d/%Y")

##As of June 1, 2021
vac_data <- vac_data %>% filter(Date=="2021-06-01") %>% select(FIPS
                                                               ,Administered_Dose1_Pop_Pct)

################BTS Data#######
remove_comma <- function(column) {
  column<-gsub(",","",as.character(column))
  return(column)
}

##Community data report, 
##Percent baseline report
bts_data <- read.csv("C:/Users/Mi Pc/Google Drive/00_MIDS_BERKELEY/203-Statistics for Data Science/lab2-data/BTS- Transportation-  July 12/Trips_by_Distance.csv")
bts_data <- bts_data %>% filter(ï..Level=="County") 
bts_data[,7:19] <- sapply(bts_data[,7:19], remove_comma)
bts_data[,7:19] <- sapply(bts_data[,7:19], as.numeric)


trips_county <- bts_data %>% 
  mutate(
    new_dates = ymd(Date),
    total_population = Population.Staying.at.Home + Population.Not.Staying.at.Home,
    trips_100_or_greater = Number.of.Trips.100.250 + Number.of.Trips.250.500 + Number.of.Trips...500,
    trips_per_capita = trips_100_or_greater / total_population
    
  ) %>%
  filter(year(new_dates) == '2021' & month(new_dates) == '6')


trips_county <- trips_county %>%
  group_by(County.FIPS) %>%
  summarize(sum_of_trips = sum(trips_per_capita, na.rm = TRUE)) 

colnames(trips_county)[1] <- "FIPS"


##Puting the information together
vac_data$FIPS <- as.factor(vac_data$FIPS)
trips_county$FIPS <- as.factor(trips_county$FIPS)
county_data <- left_join(vac_data, trips_county, by="FIPS")
county_data <- na.omit(county_data)



##Using the Administered Dose 1 (%)
ggplot(county_data %>% filter(Administered_Dose1_Pop_Pct>0), aes(x=Administered_Dose1_Pop_Pct, y=sum_of_trips)) + geom_point() + geom_smooth() + 
  labs(title="Amount of Trips (per capita) vs. Administered Dose 1 (%) - County Level", x="Administered Dose (%)", y="Amount of Trips")



###The NA's assessment could be done in this part of the script. 


county_data2 <- county_data %>% filter(Administered_Dose1_Pop_Pct >0) 
cor(county_data2$Administered_Dose1_Pop_Pct, county_data2$sum_of_trips)

summary(lm(sum_of_trips ~ Administered_Dose1_Pop_Pct, county_data2))
write_rds(county_data2, "Lab-2/01_county_level/model_1.rds")








