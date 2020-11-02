#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/qianchengchen/Desktop/Sta304-PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(registration,
         vote_intention,
         vote_2020,
         state,
         education,
         gender,
         age,
         race_ethnicity,
         household_income
  )

#Adjust Data types
reduced_data$age<-as.numeric(reduced_data$age)

#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

#Filter data, keep data who will vote
filtered_survey_data<-reduced_data %>% 
  filter(registration=="Registered"&
           vote_intention=="Yes, I will vote"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden"))

#Drop data which is NA
filtered_survey_data<-na.omit(filtered_survey_data)

rm(raw_data,reduced_data)


#grouping age
filtered_survey_data<-filtered_survey_data %>% 
  mutate(age = case_when(age <=17 ~ '17 or less',
                         age >17  & age <= 30 ~ '18 to 30',
                         age >30  & age <= 40 ~ '31 to 40',
                         age >40  & age <= 50 ~ '41 to 50',
                         age >50  & age <= 60 ~ '51 to 60',
                         age >60  & age <= 70 ~ '61 to 70',
                         age >70  & age <= 80 ~ '71 to 80',
                         age >80 ~ 'above 80' )) 
unique(filtered_survey_data$age)


##rename obs of education
filtered_survey_data$education[filtered_survey_data$education==
                                 "Other post high school vocational training"
]<-"High school graduate"
filtered_survey_data$education[filtered_survey_data$education==
                                 "Completed some college, but no degree"
]<-"High school graduate"
filtered_survey_data$education[filtered_survey_data$education==
                                 "Completed some graduate, but no degree"
]<-"College Degree (such as B.A., B.S.)"

unique(filtered_survey_data$education)

# to see the category of household_income
unique(filtered_survey_data$household_income)

# to see the category of race_ethnicity
unique(filtered_survey_data$race_ethnicity)
#rename obs of race_ethnicity to a new column
filtered_survey_data<-filtered_survey_data %>% 
  mutate(race = case_when(race_ethnicity =="Asian (Chinese)"~'Chinese',
                          race_ethnicity =="Asian (Japanese)"~'Japanese',
                          race_ethnicity =="Asian (Vietnamese)"~'other asian or pacific islander',
                          race_ethnicity =="Asian (Korean)"~'other asian or pacific islander',
                          race_ethnicity =="Asian (Filipino)"~'other asian or pacific islander',
                          race_ethnicity =="Asian (Asian Indian)"~'other asian or pacific islander',
                          race_ethnicity =="Asian (Other)"~'other asian or pacific islander',
                          race_ethnicity =="Pacific Islander (Native Hawaiian)"~'other asian or pacific islander',
                          race_ethnicity =="Pacific Islander (Samoan)"~'other asian or pacific islander',
                          race_ethnicity =="Pacific Islander (Guamanian)"~'other asian or pacific islander',
                          race_ethnicity =="Pacific Islander (Other)"~'other asian or pacific islander',
                          race_ethnicity =="White"~'White',
                          race_ethnicity =="Black, or African American"~'Black, or African American',
                          race_ethnicity =="Some other race"~'Other race',
                          race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native")) 
filtered_survey_data$race_ethnicity<-NULL
unique(filtered_survey_data$race)



# Saving the survey/sample data as a csv file in my
# working directory
write_csv(filtered_survey_data, "outputs/survey_data.csv")

