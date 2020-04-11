library(tidyverse)
library(psych)
library(ggplot2)
library(multicon)
library(expss)


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
# Dominik: The first two drops are the experimental questions (half participant got 1 and half got 2)
# Dominik: The 3rd, 4th and 5th are open ended questions, if I am not mistaken

qplot(seq_along(question_completeness_rate),question_completeness_rate) + geom_line()


# On Dem_edu and Dem_edu_mom variable - we have answer "1"? That's strange. => I am not sure what to to with these values
unique(d$Dem_edu)
unique(d$Dem_edu_mom)
unique(d$Dem_islolation)

# Similarly Dem_Marital status has "5" => I am not sure what to to with these values
unique(d$Dem_maritalstatus)


# Number of cases from each country 
# Brunei needs to be recoded to Bulgaria!
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



# Recode marital status
d <- d %>% mutate_at(
  .vars = vars(contains("Dem_maritalstatus")),
  .funs = recode, 
  "Single" = "single", 
  "Married/cohabiting" = "mar/cohab",
  "Divorced/widowed" = "div/widow", 
  "Other or would rather not say" = "other/notSay")

# Load function 'recode_if'(Aden-Buie & Gerke, 2018)
recode_if <- function(x, condition, ...) {
  if_else(condition, recode(x, ...), x)
}

# Fix differences in scoring between english and other languages 
d <- d %>%
  mutate(Dem_maritalstatus = 
           recode_if(Dem_maritalstatus, UserLanguage != "EN", 
                     "single" = "other/notSay",
                     "mar/cohab" = "single",
                     "div/widow"= "mar/cohab",
                     "other/notSay" = "div/widow"))


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

#5) Corona_Concerns
corCset <- d[, grep("Corona_concerns", names(d))]
corClist <- list(corC_avg = c(1:5)) 
corCscore <- scoreTest(corCset, corClist, nomiss = 0.01, rel = F)
d <- data.frame(d, corCscore)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Data Visualization########################################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

dsub<-subset(d, Country %in% c("Italy", "Canada", "France", "Germany", "Japan", "United Kingdom","United States")) #subset countries needed

#load function to calculate means and SD
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#create dataframe for the plot
df_bar<- data_summary(dsub, varname="PSS10_avg", 
                    groupnames="Country")

head(df_bar) #check the stats

#plot with basic layout. Customize at will
ggplot(df_bar, aes(x=fct_reorder(Country, PSS10_avg), y=PSS10_avg, width=.6)) + 
  geom_bar(stat="identity", color="black", fill="darkred", position=position_dodge()) +
  geom_errorbar(aes(ymin=PSS10_avg, ymax=PSS10_avg+sd), width=.2, #to obtain also the lower part of the SD bar use <ymin=PSS10_avg-sd>
                 position=position_dodge(.9))+
  ggtitle("Stress Levels G7 Countries (Bars represent the upper limit of the SD)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()+
  xlab("Country")+
  ylab("Stress")+
  ylim(0,5)



# Visualization of the distress scale

d %>% summarize_at(.vars = dplyr::vars(matches("Distress_\\d")), .funs = mean, na.rm= T) %>% 
  pivot_longer(cols = everything(), names_to = "Source", values_to = "Value") %>% 
  mutate(Source = dplyr::recode(Source,
         "Expl_Distress_1"="Income",
         "Expl_Distress_2"="Work",
         "Expl_Distress_3"="Children's education",
         "Expl_Distress_4"="Job prospects",
         "Expl_Distress_5"="Access to necessities (food etc.)",
         "Expl_Distress_6"="No social activities",
         "Expl_Distress_7"="No religious activities",
         "Expl_Distress_8"="Behavior of adults I live with",
         "Expl_Distress_9"="Behavior of children I live with",
         "Expl_Distress_10"="National economy",
         "Expl_Distress_11"="Civil services (police, sanitation...)",
         "Expl_Distress_12"="Risk of catching coronavirus",
         "Expl_Distress_13"="Risk of being hospitalized or dying",
         "Expl_Distress_14"="Worry over friends and relatives who live far away",
         "Expl_Distress_15"="Adapt work to digital platforms",
         "Expl_Distress_16"="Adapt to social life on digital platforms",
         "Expl_Distress_17"="Feeling ashamed for acting differently",
         "Expl_Distress_18"="Loneliness",
         "Expl_Distress_19"="Time I spend inside",
         "Expl_Distress_20"="Time I spend in proximity to others",
         "Expl_Distress_21"="Not knowing about developments with COVID",
         "Expl_Distress_22"="Not knowing how to stop COVID",
         "Expl_Distress_23"="Not knowing how long the measures will last",
         "Expl_Distress_24"="No travel outside my country")
  ) %>% 
  ggplot(aes(x = fct_reorder(Source, Value), y = Value)) + 
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill="darkred")+
  coord_flip(ylim = c(1,5))+
  theme_minimal()+
  xlab("Source of distress")+
  ylab("Level of distress")




# Visualization of the coping scale

d %>% summarize_at(.vars = dplyr::vars(matches("Coping_\\d")), .funs = mean, na.rm= T) %>% 
  pivot_longer(cols = everything(), names_to = "Source", values_to = "Value") %>% 
  mutate(Source = dplyr::recode(Source,
                                "Expl_Coping_1"="Information from the government",
                                "Expl_Coping_2"="Face-to-face interactions friends/family",
                                "Expl_Coping_3"="Phonecalls/long-range interactions friends/family",
                                "Expl_Coping_4"="Face-to-face interactions colleagues",
                                "Expl_Coping_5"="Phonecalls/long-range interactions colleagues",
                                "Expl_Coping_6"="Social media",
                                "Expl_Coping_7"="Video games (alone)",
                                "Expl_Coping_8"="Video games (online)",
                                "Expl_Coping_9"="Watching TV-shows or movies",
                                "Expl_Coping_10"="Helping others",
                                "Expl_Coping_11"="Preparing for the crisis",
                                "Expl_Coping_12"="Following government's advice",
                                "Expl_Coping_13"="My work/vocation",
                                "Expl_Coping_14"="Hobby",
                                "Expl_Coping_15"="God or Religion",
                                "Expl_Coping_16"="Knowledge of actions take by government or civil service")) %>% 
  ggplot(aes(x = fct_reorder(Source, Value), y = Value)) + 
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill="darkred")+
  coord_flip(ylim = c(1,5))+
  theme_minimal()+
  xlab("Source of coping")+
  ylab("Level of coping")



# Visualization of trust

d %>% summarize_at(.vars = dplyr::vars(matches("OECD")), .funs = mean, na.rm= T) %>% 
  pivot_longer(cols = everything(), names_to = "Source", values_to = "Value") %>% 
  mutate(Source = dplyr::recode(Source,
                                "OECD_people_1"="Majority of people",
                                "OECD_people_2"="Majority of people I know personally",
                                "OECD_insititutions_1"="Country's Parliament/government",
                                "OECD_insititutions_2"="Country's Police",
                                "OECD_insititutions_3"="Country's Civil service",
                                "OECD_insititutions_4"="Country's Healthcare system",
                                "OECD_insititutions_5"="WHO",
                                "OECD_insititutions_6"="Government's measures against COVID")) %>% 
  ggplot(aes(x = fct_reorder(Source, Value), y = Value)) + 
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill="darkred")+
  coord_flip(ylim = c(1,10))+
  scale_y_continuous(breaks = seq(1,10,1))+
  theme_minimal()+
  xlab("Source")+
  ylab("Level of trust")



# Visualization of concern

d %>% summarize_at(.vars = dplyr::vars(matches("concern")), .funs = mean, na.rm= T) %>% 
  pivot_longer(cols = everything(), names_to = "Source", values_to = "Value") %>% 
  mutate(Source = dplyr::recode(Source,
                                "Corona_concerns_1"="... me personally",
                                "Corona_concerns_2"="... my family",
                                "Corona_concerns_3"="... my close friends",
                                "Corona_concerns_4"="... my country",
                                "Corona_concerns_5"="... other countries")) %>% 
  ggplot(aes(x = fct_reorder(Source, Value), y = Value)) + 
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill="darkred")+
  coord_flip(ylim = c(1,6))+
  scale_y_continuous(breaks = seq(1,6,1))+
  theme_minimal()+
  xlab("I worry for...")+
  ylab("Level of worry")


# Visualization of compliance

d %>% summarize_at(.vars = dplyr::vars(matches("Compliance_\\d")), .funs = mean, na.rm= T) %>% 
  pivot_longer(cols = everything(), names_to = "Source", values_to = "Value") %>% 
  mutate(Source = dplyr::recode(Source,
                                "Compliance_1"="I am well informed how I can stop\nthe spread of coronavirus",
                                "Compliance_2"="I have done everything to reduce\nthe spread of coronavirus",
                                "Compliance_3"="I have done everything to stop keep\nthe phyisical distance",
                                "Compliance_4"="I feel that keeping distance\nwould have a high personal cost",
                                "Compliance_5"="I trust others follow guidelines\nto stop the spread of coronavirus",
                                "Compliance_6"="I have bought large extra supplies")) %>% 
  ggplot(aes(x = fct_reorder(Source, Value), y = Value)) + 
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill="darkred")+
  coord_flip(ylim = c(1,6))+
  scale_y_continuous(breaks = seq(1,6,1))+
  theme_minimal()+
  xlab("Compliance")+
  ylab("Level of agreement")


# Visualization of media

d %>% summarize_at(.vars = dplyr::vars(matches("media_\\d")), .funs = mean, na.rm= T) %>% 
  pivot_longer(cols = everything(), names_to = "Source", values_to = "Value") %>% 
  mutate(Source = dplyr::recode(Source,
                                "Expl_media_1"="... the government",
                                "Expl_media_2"="... independent news outlets in the coutnry",
                                "Expl_media_3"="... news outlets outside the country",
                                "Expl_media_4"="... friends and family",
                                "Expl_media_5"="... social media",
                                "Expl_media_6"="I have heard more positive than negative\nstories about people's behavior")) %>% 
  ggplot(aes(x = fct_reorder(Source, Value), y = Value)) + 
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill="darkred")+
  coord_flip(ylim = c(1,6))+
  scale_y_continuous(breaks = seq(1,6,1))+
  theme_minimal()+
  xlab("I have sought information from...")+
  ylab("Level of agreement")
