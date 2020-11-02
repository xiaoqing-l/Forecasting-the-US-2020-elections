library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/qianchengchen/Desktop/Sta304-PS3")
raw_data <- read_dta("usa_00002.dta.gz")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

names(raw_data)
# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(perwt,
         stateicp,
         educ,
         sex, 
         age,
         race,
         hhincome) 

head(reduced_data)
reduced_data$region%>%table()
reduced_data$educ%>%table()
reduced_data$sex%>%table()
reduced_data$race%>%table()
reduced_data$stateicp%>%table()


#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

#reduced_data <- 
#  reduced_data %>%
#  count(age) %>%
#  group_by(age) 

reduced_data$age <- as.integer(reduced_data$age)

#Drop data who can not vote
filtered_census_data <- 
  reduced_data %>% filter(age >=18)

#Adjust some NAs
filtered_census_data$hhincome<-ifelse(filtered_census_data$hhincome==9999999,
                                      NaN,filtered_census_data$hhincome)

#Drop data which is NA
filtered_census_data<-na.omit(filtered_census_data)

rm(raw_data,reduced_data)

#grouping age
filtered_census_data<-filtered_census_data %>% 
  mutate(age = case_when(age <=17 ~ '17 or less',
                         age >17  & age <= 30 ~ '18 to 30',
                         age >30  & age <= 40 ~ '31 to 40',
                         age >40  & age <= 50 ~ '41 to 50',
                         age >50  & age <= 60 ~ '51 to 60',
                         age >60  & age <= 70 ~ '61 to 70',
                         age >70  & age <= 80 ~ '71 to 80',
                         age >80 ~ 'above 80' )) 
unique(filtered_census_data$age)

#change variable name of sex 
filtered_census_data$sex<-ifelse(filtered_census_data$sex=="female","Female","Male")
unique(filtered_census_data$sex)
#rename column of sex to gender
names(filtered_census_data)[names(filtered_census_data) == 'sex'] <-'gender'


#grouping educ
filtered_census_data<-filtered_census_data %>% 
  mutate(educ = case_when(educ=="n/a or no schooling" ~ '3rd Grade or less',
                          educ=="nursery school to grade 4"~'3rd Grade or less',
                          educ=="grade 5, 6, 7, or 8" ~ 'Middle School - Grades 4 - 8',
                          educ=="grade 9" ~ "Completed some high school",
                          educ=="grade 10" ~ "Completed some high school",
                          educ=="grade 11" ~ 'Completed some high school',
                          educ=="grade 12" ~ 'Completed some high school',
                          educ=="1 year of college" ~ 'High school graduate',
                          educ=="2 years of college" ~ 'High school graduate',
                          educ=="4 years of college" ~ 'High school graduate',
                          educ=="5+ years of college" ~ 'College Degree')) 
#rename column of educ to education
names(filtered_census_data)[names(filtered_census_data) == 'educ'] <-'education'

#rename obs of stateicp
unique(filtered_census_data$stateicp)
filtered_census_data<-filtered_census_data %>% 
  mutate(stateicp = case_when(stateicp=="alabama"~"AL",
                              stateicp=="alaska"~"AK",
                              stateicp=="arizona"~"AZ",
                              stateicp=="arkansas"~"AR",
                              stateicp=="california"~"CA",
                              stateicp=="colorado"~"CO",
                              stateicp=="connecticut"~"CT",
                              stateicp=="delaware"~"DE",
                              stateicp=="florida"~"FL",
                              stateicp=="georgia"~"GA",
                              stateicp=="hawaii"~"HI",
                              stateicp=="idaho"~"ID",
                              stateicp=="illinois"~"IL",
                              stateicp=="indiana"~"IN",
                              stateicp=="iowa"~"IA",
                              stateicp=="kansas"~"KS",
                              stateicp=="kentucky"~"KY",
                              stateicp=="louisiana"~"LA",
                              stateicp=="maine"~"ME",
                              stateicp=="maryland"~"MD",
                              stateicp=="massachusetts"~"MA",
                              stateicp=="michigan"~"MI",
                              stateicp=="minnesota"~"MN",
                              stateicp=="mississippi"~"MS",
                              stateicp=="missouri"~"MO",
                              stateicp=="montana"~"MT",
                              stateicp=="nebraska"~"NE",
                              stateicp=="nevada"~"NV",
                              stateicp=="new hampshire"~"NH",
                              stateicp=="new jersey"~"NJ",
                              stateicp=="new mexico"~"NM",
                              stateicp=="new york"~"NY",
                              stateicp=="north carolina"~"NC",
                              stateicp=="north dakota"~"ND",
                              stateicp=="ohio"~"OH",
                              stateicp=="oklahoma"~"OK",
                              stateicp=="oregon"~"OR",
                              stateicp=="pennsylvania"~"PA",
                              stateicp=="rhode island"~"RI",
                              stateicp=="south carolina"~"SC",
                              stateicp=="south dakota"~"SD",
                              stateicp=="tennessee"~"TN",
                              stateicp=="texas"~"TX",
                              stateicp=="utah"~"UT",
                              stateicp=="vermont"~"VT",
                              stateicp=="virginia"~"VA",
                              stateicp=="washington"~"WA",
                              stateicp=="west virginia"~"WV",
                              stateicp=="wisconsin"~"WI",
                              stateicp=="wyoming"~"WY",
                              stateicp=="district of columbia"~"DC")) 
##rename column of stateicp to state
names(filtered_census_data)[names(filtered_census_data) == 'stateicp'] <-'state'


#grouping hhincome as in survey date
min(filtered_census_data$hhincome)
max(filtered_census_data$hhincome)
filtered_census_data<-filtered_census_data %>% 
  mutate(hhincome = case_when(hhincome<=14999 ~ "Less than $14,999",
                              hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                              hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                              hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                              hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                              hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                              hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                              hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",                                hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                              hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                              hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                              hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                              hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                              hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                              hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",                                hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                              hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                              hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                              hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                              hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                              hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                              hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                              hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                              hhincome>=250000~"$250,000 and above")) 
#rename column of hhincome to household_income
names(filtered_census_data)[names(filtered_census_data) == 'hhincome'] <-'household_income'

#to see the category of race
unique(filtered_census_data$race)
filtered_census_data<-filtered_census_data %>% 
  mutate(race = case_when(race=="white"~"White",
                          race=="black/african american/negro"~"Black, or African American",
                          race=="american indian or alaska native"~"American Indian or Alaska Native",
                          race=="other asian or pacific islander"~"other asian or pacific islander",
                          race=="other race, nec"~"Other race",
                          race=="two major races"~"Other race",
                          race=="three or more major races"~"Other race",
                          race=="chinese"~"Chinese",
                          race=="japanese"~"Japanese")) 



# Saving the census data as a csv file in my
# working directory
write_csv(filtered_census_data, "outputs/census_data.csv")

