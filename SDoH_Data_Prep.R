# Load necessary libraries
library(haven)
library(dplyr)

# Load the data from Wave 1
data_wave1 <- WHO_SAGE_GH_Wave1

# Generate new variables
data_wave1 <- data_wave1 %>%
  mutate(Year = 2007,
         ID1 = id,
         ID2 = id2ndwave) %>%
  arrange(ID1, Year)

# Save the modified Wave 1 data
#write_dta(data_wave1, "C:/Users/pma311/Box/Mentoring/PQAR/Summer-2024/WHO_SAGE_GH_Wave1.dta")

# Load the data from Wave 2
data_wave2 <- WHO_SAGE_GH_Wave2
  
# Generate new variables for Wave 2
data_wave2 <- data_wave2 %>%
  mutate(ID1 = id) %>%
  arrange(ID1, Year)

# Save the modified Wave 2 data
#write_dta(data_wave2, "C:/Users/pma311/Box/Mentoring/PQAR/Summer-2024/WHO_SAGE_GH_Wave2.dta")

# Reload the modified Wave 1 data
#data_wave1 <- read_dta("C:/Users/pma311/Box/Mentoring/PQAR/Summer-2024/WHO_SAGE_GH_Wave1.dta")

# Append Wave 2 data to Wave 1 data
data_combined <- bind_rows(data_wave1, data_wave2)

# Save the combined data
#write_dta(data_combined, "C:/Users/pma311/Box/Mentoring/PQAR/Summer-2024/WHO_SAGE_GH_Waves_1_2.dta")




##############################################################################################################################
rm(list = ls())
gc(reset=TRUE)



library(haven)
library(tidyr)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(labelled)
library(sjmisc)

DF <- WHO_SAGE_GH_Waves_1_2

# Frequency 
table(DF$Year, useNA = "ifany")
table(DF$q1011, useNA = "ifany")
table(DF$q4040, useNA = "ifany")
table(DF$q4041a, useNA = "ifany")
table(DF$q4041b, useNA = "ifany")
table(DF$q4042, useNA = "ifany")
table(DF$q4043, useNA = "ifany")
table(DF$q4044, useNA = "ifany")
table(DF$q4045, useNA = "ifany")

var_label(DF$q4040)
var_label(DF$q4041a)
var_label(DF$q4041b)
var_label(DF$q4042)
var_label(DF$q4043)
var_label(DF$q4044)
var_label(DF$q4045)

# Frequency for sadness variables
sadness_vars <- c("q4046", "q4047", "q4048", "q4049", "q4050", "q4051", "q4052", "q4053", "q4054", "q4055", "q4056", "q4057", "q4058", "q4059")
for (var in sadness_vars) {
  print(table(DF[[var]], useNA = "ifany"))
}

labels <- sapply(sadness_vars, function(var) var_label(DF[[var]]))
labels

# in wave 2 dperession question is a follow-up hence NA may refer to people not interviewed in wave 1
DF <- DF %>%
  mutate(Wave = case_when(Year==2007 ~ 'Wave 1',
                          Year==2014 ~ 'Wave 2')) %>%
  mutate_at(vars(q1011), ~ ifelse(. == -8, NA, .)) %>%
  mutate_at(vars(q4040:q4059, q6001:q6009, q6013, q6014, q6017,q6018, q0409, q0408), ~ ifelse(. >= 8, NA, .)) %>%
  mutate_at(vars(q3014:q3015, q7002, q4060), ~ ifelse(. >= 8, NA, .))





# Count the number of occurrences of each ID
DF <- DF %>%
  group_by(ID1) %>%
  mutate(count = n()) %>%
  ungroup() 

# Order columns
DF<- DF %>%
  select(-ID1, everything(), ID1) #total observation from wave 1 and 2 added 10,308


# Keep observations with information from both waves
DF <- DF %>%
  filter(count == 2) #total observations with information from both waves 4,444


# Replace values 8 or greater with NA for variables q4040 to q4059
DF$q4045 <- ifelse(is.na(DF$q4045), 2, DF$q4045)  # If NA, set to 2 (No)
DF$q4046 <- ifelse(is.na(DF$q4046), 2, DF$q4046)  # If NA, set to 2 (No)
DF$q4047 <- ifelse(is.na(DF$q4047), 2, DF$q4047)  # If NA, set to 2 (No)
DF$q4048 <- ifelse(is.na(DF$q4048), 2, DF$q4048)  # If NA, set to 2 (No)
DF$q4049 <- ifelse(is.na(DF$q4049), 2, DF$q4049)  # If NA, set to 2 (No)
DF$q4050 <- ifelse(is.na(DF$q4050), 2, DF$q4050)  # If NA, set to 2 (No)
DF$q4051 <- ifelse(is.na(DF$q4051), 2, DF$q4051)  # If NA, set to 2 (No)
DF$q4052 <- ifelse(is.na(DF$q4052), 2, DF$q4052)  # If NA, set to 2 (No)
DF$q4053 <- ifelse(is.na(DF$q4053), 2, DF$q4053)  # If NA, set to 2 (No)
DF$q4054 <- ifelse(is.na(DF$q4054), 2, DF$q4054)  # If NA, set to 2 (No)
DF$q4055 <- ifelse(is.na(DF$q4055), 2, DF$q4055)  # If NA, set to 2 (No)
DF$q4056 <- ifelse(is.na(DF$q4056), 2, DF$q4056)  # If NA, set to 2 (No)
DF$q4057 <- ifelse(is.na(DF$q4057), 2, DF$q4057)  # If NA, set to 2 (No)
DF$q4058 <- ifelse(is.na(DF$q4058), 2, DF$q4058)  # If NA, set to 2 (No)
DF$q4059 <- ifelse(is.na(DF$q4059), 2, DF$q4059)  # If NA, set to 2 (No)

criteria_c_vars <- grep("^q404[7-9]$|^q405[0-9]$", names(DF), value = TRUE)

# Create helper functions to count symptoms
count_criteria_b <- function(df) {
  rowSums(df[, c("q4042", "q4043", "q4044", "q4045", "q4046")] == 1, na.rm = TRUE)
}

count_criteria_c <- function(df) {
  rowSums(df[, criteria_c_vars] == 1, na.rm = TRUE)
}

# Apply the helper functions to count symptoms for each respondent
DF <- DF %>%
  mutate(
    count_b = count_criteria_b(.),
    count_c = count_criteria_c(.)
  )

# Define mild, moderate, and severe depression based on the criteria based ICD-10 DCR codes F32.0–32.2
DF <- DF %>%
  mutate(
    mild_depression = (q4045 == 1 & count_b >= 2 & count_c >= 1 & count_b + count_c >= 4 # Criterion A, B, and C
                       ),
    moderate_depression = (
      q4045 == 1 & count_b >= 2 & count_c >= 3 & count_b + count_c >= 6 # Criterion A, B, and C
    ),
    severe_depression = (
      q4045 == 1 & count_b == 3 & count_c >= 5 & count_b + count_c >= 8 # Criterion A, B, and C
    ),
    depression_type = case_when(
      severe_depression ~ "Severe",
      moderate_depression ~ "Moderate",
      mild_depression ~ "Mild",
      TRUE ~ "No Symptoms"
    )
  )

table(DF$mild_depression, useNA = "ifany")
table(DF$moderate_depression, useNA = "ifany")
table(DF$severe_depression, useNA = "ifany")

prop.table(table(DF$mild_depression, useNA = "ifany"))
prop.table(table(DF$moderate_depression, useNA = "ifany"))
prop.table(table(DF$severe_depression, useNA = "ifany"))


############### Independent Variable
#Q6013 = Have someone to trust in
#Q6014 = Trust in neighbours
#Q6017 = Feel safe at home
#Q6018 = Feel safe on street
#CIN_10 = Social cohesion index ((q6001 + q6002 + q6003 + q6004 + q6005 + q6006 + q6007 + q6008 + q6009)
#Unmet Healthcare need = q5002
##Functional disabilities (q2037, q2038, q2042, q2043, q2044)
#bathing= Q2037
#dressing= Q2038
#eating= Q2042
#transferring= Q2043
#walking= Q2041
#going outside= Q2046
#toileting=  Q2044

#AGE_10 = Age of Respondents (q1011)
#SEX_10 = Sex (q0406)
#EDU_10 = Education (q0409)
#INC_10 = Income Index (qUINTILE_C)
# Chronic Health = (q4010 q4022 q4001 q4033 q4060)
#Isolation or loneliness (q6011a,q6011b, q6011c)
#Food & finance security -  Food = q3014 q3015
# Finance = Q7002, Q8066 


require(misty)

item.reverse(DF$q6013)
## recoding or coding social capital and social needs factors including ADL
DF <- DF %>%
  mutate(rev_q6013 = item.reverse(q6013), rev_q6014 = item.reverse(q6014),
    rev_q6017 = item.reverse(q6017), rev_q6018 = item.reverse(q6018),
    rev_q6001 = item.reverse(q6001), rev_q6002 = item.reverse(q6002),
    rev_q6003 = item.reverse(q6003), rev_q6004 = item.reverse(q6004),
    rev_q6005 = item.reverse(q6005), rev_q6006 = item.reverse(q6006),
    rev_q6007 = item.reverse(q6007), rev_q6008 = item.reverse(q6008),
    rev_q6009 = item.reverse(q6009)) %>%
  mutate(SOL = q6011a + q6011b + q6011c) %>%
  mutate(FIN = case_when(q7002 %in% 1:2 ~ 1, q7002 == 3 ~ 2, q7002 == 4 ~ 3,  
                         q7002 == 5 ~ 0),
         levels = c(0, 1, 2, 3), labels = c("none at all", "completely or mostly", "moderately", "little"))  %>%
  mutate(FOOD_10 = case_when(q3014 %in% 1:2 ~ 0, q3014 %in% 3:4 ~ 1,  q3014 == 5 ~ 2,  
                             q3014 %in% 8:9 ~ NA_real_ ),
         levels = c(0, 1, 2), labels = c("every month or almost every month", "occasional or some months", "never")) %>%
  mutate(FOOD_20 = case_when(q3015 %in% 1:2 ~ 0,  q3015 %in% 3:4 ~ 1,  q3015 == 5 ~ 2,     
      q3015 %in% 8:9 ~ NA_real_ ), 
      levels = c(0, 1, 2), labels = c("every month or almost every month", "occasional or some months", "never")) %>%
  mutate(ADL = interaction(q2037, q2038, q2042, q2043, q2044, drop = TRUE))%>%
  mutate(ADL2 = case_when(ADL == "1" ~ 0,
                          TRUE ~ 1)) %>%
  mutate(ADL2 = factor(ADL2, levels = c(0, 1), labels = c("None", "1+ ADL")))


############ Neighbourhood level factors
require(forcats)
# Generate neighbourhood safety index (NSAF) by summing Q6017 and Q6018 &
# Generate neighbood social support (CIN_10) by summing Q6001, Q6003, Q6004, Q6007, Q6008, and Q6009
DF <- DF %>%  mutate(NSAF = Q6017 + Q6018) %>%
  mutate(CIN_10 = q6001 + q6003 + q6004 + q6007 + q6008 + q6009)


# Proportion of Trust in Neighbors by Neighborhoods (NTR),Neighborhood Safety (NSAF_10) & Neighborhood Social Participation (SOP_10)
DF <- DF %>%
  group_by(q0101b) %>%
  mutate(NTR = mean(100 * (q6014 %in% c(1, 2)), na.rm = TRUE)) %>%
  mutate(NSAF_10 = mean(100 * (NSAF %in% 1:4), na.rm = TRUE)) %>%
  mutate(SOP_10 = mean(100 * (CIN_10 %in% 12:18), na.rm = TRUE)) %>% ungroup()

#column not found


############### coding sociodemographic information
# Recode q1011 into AGE_10
DF <- DF %>%
  mutate(AGE = case_when(q1011 >= 18 & q1011 <= 49 ~ 1,
                         q1011 >= 50 & q1011 <= 59 ~ 2,
                         q1011 >= 60 & q1011 <= 69 ~ 3,
                         q1011 >= 70 & q1011 <= 79 ~ 4,
                         q1011 >= 80 & q1011 <= 120 ~ 5,
                         TRUE ~ NA_real_),
         AGE = factor(AGE, 
                      labels = c("18 to 49 years", "50 to 59 years", "60 to 69 years", 
                                 "70 to 79 years", "80 years and above")))


# Recode q1016 into EDU
DF <- DF %>%
  mutate(EDU = case_when(
      q1016 == 1 ~ 1, q1016 == 2 ~ 2, 
      q1016 %in% 3:4 ~ 3, q1016 %in% 5:6 ~ 0, 
      TRUE ~ NA_real_),
      EDU = factor(EDU, 
                      labels = c("Post-secondary education", "No formal education", 
                                 "Primary education", "Secondary education")))


# Recode q1009 into SEX
DF <- DF %>%
  mutate(SEX_10 = case_when(
      q1009 == 1 ~ 0,  q1009 == 2 ~ 1,  TRUE ~ NA_real_),
    SEX_10 = factor(SEX_10, labels = c("Male", "Female")))

# Encode q0105a into REG
DF <- DF %>%
  mutate(REG = as.factor(q0105a)) %>%
  mutate(REG_10 = case_when(
      REG %in% c(1, 2, 3, 4, 9, 10) ~ 1,  
      REG %in% 6:8 ~ 2, 
      REG == 5 ~ 0,  TRUE ~ NA_real_),
    REG_10 = factor(REG_10, labels = c("Accra", "Southern", "Northern")))


# Recode q1012 into MAR
DF <- DF %>%
  mutate(MAR = case_when(
      q1012 == 1 ~ 1, q1012 %in% 2:3 ~ 0,  
      q1012 == 4 ~ 2, q1012 == 5 ~ 3, TRUE ~ NA_real_),
      MAR = factor(MAR, labels = c("Married or cohabiting", "Never married", 
                                   "Separated or divorced", "Widowed")))



##Predictors:
##Outcomes: Depression and Hypertension















