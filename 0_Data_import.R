library(tidyverse)
library(psych)
library(ggplot2)
library(multicon)

# Load dataset
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
d <- read_csv("COVIDiSTRESS import April 6 2020 (choice values).csv", col_names = name, skip = 3) 
da <- read_csv("COVIDiSTRESS import April 6 2020 (choice values).csv", col_names = name, skip = 3)

# View data structure
glimpse(d)

# Filter participants who want to participate 
d <- d %>%
  filter(Consent == "Yes")


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Different checks of the data ###########################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Completeness rate is 54% / How many participants displayed the last survey page. 
sum(d$Finished) / nrow(d)

# How many participants who agrred to participate in the study displayed  the last page - 64% (it's really high!)
d %>%
  filter(Consent == "Yes") %>%
  summarise(sum(Finished)/length(Finished))

# What was the average percentege of completeness in the survey for each individual?

participant_completeness_rate <- apply(X = d[,11:135], MARGIN = 1, FUN = function(x){
  
  sum(!is.na(x))/length(x)
})

describe(participant_completeness_rate)

# How many persons answered only 1 question? 
length(which(participant_completeness_rate < (2/125)))

# What was the averge completeness rate within each question?
question_completeness_rate <- apply(X = d[,11:135], MARGIN = 2, FUN = function(x){
  sum(!is.na(x))/length(x)
})

describe(question_completeness_rate)

# this plot is very crude but we can spot some certain issues with our survey. The general trend is similar to other online surveys however those strange drops need to be examined furhter
# I'm leaving it for your consideration
qplot(seq_along(question_completeness_rate),question_completeness_rate) + geom_line()


# On Dem_edu and Dem_edu_mom variable - we have answer "1"? That's strange. => I am not sure what to to with these values
unique(d$Dem_edu)
unique(d$Dem_edu_mom)
unique(d$Dem_islolation)

# Similarly Dem_Marital status has "5" => I am not sure what to to with these values
unique(d$Dem_maritalstatus)


# Number of cases from each country
d %>% group_by(Country) %>% summarize(n()) %>% print(n=1000)

# Number of participants in each language
d %>% group_by(UserLanguage) %>% summarize(n()) %>% print(n=1000)



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Data recoding ##########################################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Dominik: I think we should leave some demographics unrecoded for the sake of visualizations. Maybe turn them to factors

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
                   "· If Program A is adopted, 200 people will be saved." = "Program A",
                   "· If Program B is adopted, there is 1/3 probability that 600 people will be saved, and 2/3 probability that no people will be saved" = "Program B")
)

# Recoding AD_loss
d <- d %>% mutate(
  AD_loss = recode(AD_loss,
                   "· If Program C is adopted 400 people will die." = "Program C",
                   ". If Program D is adopted there is 1/3 probability that nobody will die, and 2/3 probability that 600 people will die." = "Program D")
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



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Creating composite scores ################################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# 1) PSS10
PSS10set <- d[, grep("PSS10", names(d))]
PSS10list <- list(PSS10_avg = c(1:3, -4, -5, 6, -7, -8, 9:13)) 
PSS10score <- scoreTest(PSS10set, PSS10list, nomiss = 0.01, rel = F)
d <- data.frame(d, PSS10score)

# 2) Compliance
Compset <- d[, grep("Compliance", names(d))]
complist <- list(Compliance_avg = c(1:3, -4, 5, -6)) #Not sure if buying large groceries is against the recommendations, I'd say yes?
Compscore <- scoreTest(Compset, complist, nomiss = 0.01, rel = F)
d <- data.frame(d, Compscore)

# 3) BFI
BFF15set <- d[, grep("BFF_15", names(d))]
BFF15list <- list(BFI_n = c(1, 2, -3), 
                  BFI_e = c(4, 5, -6),
                  BFI_o = c(7, 8, 9),
                  BFI_a = c(10, 11, -12),
                  BFI_c = c(13, -14, 15)) 
BFF15score <- scoreTest(BFF15set, BFF15list, nomiss = 0.01, rel = F)
d <- data.frame(d, BFF15score)

# 4) SPS10
SPS10set <- d[, grep("SPS", names(d))]
SPS10list <- list(SPS_avg = c(1:3, -4, 5, -6)) 
SPS10score <- scoreTest(SPS10set, SPS10list, nomiss = 0.01, rel = F)
d <- data.frame(d, SPS10score)