library(tidyverse)

# Questions
# 1) Do variables StartDate, EndDate, Status, Progress, Duration, Finished, RecordedDate, Distributionchannel stays in the cleaned version?
# 2) We have some people above 70 years of age. Should we set a limit or leave everybody? The oldest person is 110
d %>% filter(Dem_age > 70) %>% ggplot(aes (x = Dem_age)) + geom_histogram()
# 3) Should we reode "- other" in Country to NA?
# 4) What is with number 5 in Dem_martial?



# Log:
# 1) I removed all participants that were below 18 for the cleaned version
# 2) I removed all participants that did not their consent for the cleaned version
# 3) In Brunei from 3894 people, 3887 speak Bulgarian (probably Bulgarians), 6 speak English, 1 speaks LT
d %>% filter(Country == "Bulgaria") %>% group_by(UserLanguage) %>% summarize(n())
# I recoded all people from Brunei who speak Bulgarian to Bulgaria
# 4) Detected that there's "-other" option in country
# Recoded "- other" to NA



# Set the names for the variables
name <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)", 
          "Finished", "RecordedDate", "ResponseId", "DistributionChannel", 
          "UserLanguage", "Consent", "Dem_age", "Dem_gender", "Dem_edu", 
          "Dem_edu_mom", "Dem_employment", "Country", "Dem_Expat", "Dem_state", 
          "Dem_maritalstatus", "Dem_dependents", "Dem_riskgroup", "Dem_islolation", 
          "Dem_isolation_adults", "Dem_isolation_kids", "AD_gain", "AD_loss", 
          "AD_check", "Scale_PSS10_UCLA_1", "Scale_PSS10_UCLA_2", "Scale_PSS10_UCLA_3", 
          "Scale_PSS10_UCLA_4", "Scale_PSS10_UCLA_5", "Scale_PSS10_UCLA_6", 
          "Scale_PSS10_UCLA_7", "Scale_PSS10_UCLA_8", "Scale_PSS10_UCLA_9", 
          "Scale_PSS10_UCLA_10", "Scale_PSS10_UCLA_11", "Scale_PSS10_UCLA_12", 
          "Scale_PSS10_UCLA_13", "OECD_people_1", "OECD_people_2", "OECD_insititutions_1", 
          "OECD_insititutions_2", "OECD_insititutions_3", "OECD_insititutions_4", 
          "OECD_insititutions_5", "OECD_insititutions_6", "Corona_concerns_1", 
          "Corona_concerns_2", "Corona_concerns_3", "Corona_concerns_4", 
          "Corona_concerns_5", "Trust_countrymeasure", "Compliance_1", 
          "Compliance_2", "Compliance_3", "Compliance_4", "Compliance_5", 
          "Compliance_6", "BFF_15_1", "BFF_15_2", "BFF_15_3", "BFF_15_4", 
          "BFF_15_5", "BFF_15_6", "BFF_15_7", "BFF_15_8", "BFF_15_9", "BFF_15_10", 
          "BFF_15_11", "BFF_15_12", "BFF_15_13", "BFF_15_14", "BFF_15_15", 
          "Expl_Distress_1", "Expl_Distress_2", "Expl_Distress_3", "Expl_Distress_4", 
          "Expl_Distress_5", "Expl_Distress_6", "Expl_Distress_7", "Expl_Distress_8", 
          "Expl_Distress_9", "Expl_Distress_10", "Expl_Distress_11", "Expl_Distress_12", 
          "Expl_Distress_13", "Expl_Distress_14", "Expl_Distress_15", "Expl_Distress_16", 
          "Expl_Distress_17", "Expl_Distress_18", "Expl_Distress_19", "Expl_Distress_20", 
          "Expl_Distress_21", "Expl_Distress_22", "Expl_Distress_23", "Expl_Distress_24", 
          "Expl_Distress_txt", "SPS_1", "SPS_2", "SPS_3", "SPS_4", "SPS_5", 
          "SPS_6", "SPS_7", "SPS_8", "SPS_9", "SPS_10", "Expl_Coping_1", 
          "Expl_Coping_2", "Expl_Coping_3", "Expl_Coping_4", "Expl_Coping_5", 
          "Expl_Coping_6", "Expl_Coping_7", "Expl_Coping_8", "Expl_Coping_9", 
          "Expl_Coping_10", "Expl_Coping_11", "Expl_Coping_12", "Expl_Coping_13", 
          "Expl_Coping_14", "Expl_Coping_15", "Expl_Coping_16", "Expl_coping_txt", 
          "Expl_media_1", "Expl_media_2", "Expl_media_3", "Expl_media_4", 
          "Expl_media_5", "Expl_media_6", "Final_open")


# Import the raw dataset
d <- read_csv("COVIDiSTRESS data extraction -_April 13 (choice values).csv", col_names = name, skip = 3) 


# Filter participants who gave their consent to participate
d <- d %>%
  filter(Consent == "Yes")

# Filter participants who are younger than 18
d <- d %>%
  filter(Dem_age > 18)

# Exclude data that is not from Denmark before March 29th. The survey was launched in other countries later, thus these data
# are not valid
d <- d %>% 
  fi



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Data recoding ##########################################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Recoding edu variables 
d <- d %>% mutate_at(
  .vars = vars(contains("Dem_edu")),
  .funs = recode, 
  "- PhD/Doctorate" = "PHD", 
  "- Up to 12 years of school" = "12 years",
  "- Up to 9 years of school" = "9 years", 
  "- Up to 6 years of school" = "6 years",
  "- College degree" = "Col_degree",
  "- None" = "None",
  "- Some College or equivalent" = "Col",
  "1" = "1")





# Recoding people from Brunei who speak Bulgarian to Bulgaria => KNOWN ERROR!
d <- d %>% mutate(
  Country = case_when(
    Country == "Brunei" & UserLanguage == "BG" ~ "Bulgaria",
    TRUE ~ Country
  )
  )

# Recoded "- other" from variable Country to NA
d <- d %>% mutate(
  Country = na_if(Country, "- other")
)



# Recode marital status
d <- d %>% mutate_at(
  .vars = vars(contains("Dem_maritalstatus")),
  .funs = recode, 
  "Single" = "single", 
  "Married/cohabiting" = "mar/cohab",
  "Divorced/widowed" = "div/widow", 
  "Other or would rather not say" = "other/notSay")

### Load function 'recode_if'(Aden-Buie & Gerke, 2018)
recode_if <- function(x, condition, ...) {
  if_else(condition, recode(x, ...), x)
}

### Fix differences in scoring between english and other languages 
d <- d %>%
  mutate(Dem_maritalstatus = 
           recode_if(Dem_maritalstatus, UserLanguage != "EN", 
                     "single" = "other/notSay",
                     "mar/cohab" = "single",
                     "div/widow"= "mar/cohab",
                     "other/notSay" = "div/widow"))







# Recoding isolation variable
d <- d %>% mutate(
  Dem_islolation = recode(Dem_islolation,
                          "Life carries on as usual" = "Usual",
                          "Life carries on with minor change" = "Min_change",
                          "Isolated" = "Iso",
                          "Isolated in medical facility of similar location" = "Iso_med",
                          "1" = "1")
)




# Recoding AD_gain
d <- d %>% mutate(
  AD_gain = recode(AD_gain,
                   "� If Program A is adopted, 200 people will be saved." = "Program A",
                   "� If Program B is adopted, there is 1/3 probability that 600 people will be saved, and 2/3 probability that no people will be saved" = "Program B")
)

# Recoding AD_loss
d <- d %>% mutate(
  AD_loss = recode(AD_loss,
                   "� If Program C is adopted 400 people will die." = "Program C",
                   "� If Program D is adopted there is 1/3 probability that nobody will die, and 2/3 probability that 600 people will die." = "Program D")
)



# Recoding PSS
d <- d %>% mutate_at(
  .vars = vars(contains("PSS10")),
  .funs = recode, 
  "Never" = 1, 
  "Almost never" = 2,
  "Sometimes" = 3, 
  "Fairly often" = 4,
  "Very often" = 5
)

# Recoding Concerns, Compliance, BFF, Distress, SPS, Coping, Expl_media
d <- d %>% mutate_at(
  .vars = vars(matches("Corona_concerns|Compliance|BFF|Distress|SPS|Coping|Expl_media")),
  .funs = recode, 
  "Strongly disagree" = 1, 
  "Disagree" = 2,
  "Slightly disagree" = 3, 
  "Slightly agree" = 4,
  "Agree" = 5,
  "Strongly agree" = 6
)

# Recoding Trust_countryrmeasure
d <- d %>% mutate(
  Trust_countrymeasure = recode(Trust_countrymeasure,
                                "Too little" = 0,
                                "1" = 1,
                                "2" = 2,
                                "3" = 3,
                                "4" = 4,
                                "Appropriate" = 5,
                                "6" = 6,
                                "7" = 7,
                                "8" = 8,
                                "9" = 9,
                                "Too much" = 10))




