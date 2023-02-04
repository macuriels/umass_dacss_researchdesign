##install libraries needed
library(tidyverse) #used for most data wrangling and visualizations
library(naniar) #used for missing values visualization
library(mediation) #used for mediator analysis

##optional: correct/set working directory
# setwd("/Users/mcurielinteros.ai/Documents/UMass/umass_dacss_researchdesign")

##read in data
#data is downloaded as is from Qualtrics
df <- as_tibble(read.csv("UMASS_data_AsOfDec1.csv")[-c(1:2),]) #the last bit is to remove top 2 rows (useless metadata)

##select variables of interest
df <- dplyr::select(
  df 
  ,EndDate
  ,Finished
  ,ResponseId
  ,Q1
  ,Q42
  ,Q42_7_TEXT
  ,Q2
  ,Q2_8_TEXT
  ,Q23
  ,Q23_4_TEXT
  ,Q24
  ,Q43
  ,Q44
  ,Q44_4_TEXT
  ,Q48.1
  ,Q46.1
  ,G5.climatechangenarratives_DO_Q52
  ,G5.climatechangenarratives_DO_Q53
  ,Q58
  ,Q37
  ,Q41_1_1
  ,Q41_1_2
  ,Q41_2_1
  ,Q41_2_2
)

##rename columns
df <- df %>%
  rename(
    date = EndDate
    ,survey_completed = Finished
    ,id = ResponseId
    ,consent = Q1
    ,education = Q42
    ,education_other = Q42_7_TEXT
    ,major = Q2
    ,major_other = Q2_8_TEXT
    ,gender = Q23
    ,gender_other = Q23_4_TEXT
    ,age = Q24
    ,income = Q43
    ,political_affiliation = Q44
    ,political_affiliation_other = Q44_4_TEXT
    ,affiliation_alt_question = Q48.1
    ,living_area = Q46.1
    ,group_control = G5.climatechangenarratives_DO_Q52
    ,group_experiment = G5.climatechangenarratives_DO_Q53
    ,emotion_type = Q58
    ,emotion_intensity = Q37
    ,plastic_current = Q41_1_1
    ,plastic_intention = Q41_1_2
    ,refills_current = Q41_2_1
    ,refills_intention = Q41_2_2
  )

##convert columns from characters to numbers
df <- df %>% mutate_at(c('plastic_current', 'plastic_intention', 'refills_current', 'refills_intention'), as.numeric)

##create column subtracting current use minus intention
df <- df %>% 
  mutate(plastic_difference = ifelse(is.na(plastic_current)==TRUE, 0, plastic_current) - ifelse(is.na(plastic_intention)==TRUE, 0, plastic_intention)
         ,refills_difference = ifelse(is.na(refills_current)==TRUE, 0, refills_current) - ifelse(is.na(refills_intention)==TRUE, 0, refills_intention))

##exclude null responses
df <- df[!(is.na(df$plastic_current) | is.na(df$plastic_intention) | is.na(df$refills_current) | is.na(df$refills_current)), ] 

##exclude outlier responses (negative differences in plastic and positive differences in refills)
df <- df %>% 
  filter(plastic_difference >= 0) %>% 
  filter(refills_difference <= 0)

##once nulls and outliers have been removed, convert difference columns to absolute values
df <- df %>% 
  mutate(refills_difference = abs(refills_difference))

##merge control versus experimental id into one column
df <- df %>% 
  mutate(group = case_when(
    group_control=="1" ~ "control"
    ,group_experiment=="1" ~ "experiment"
  ))

##convert columns from characters to factors
df$emotion_intensity <- as.numeric(factor(df$emotion_intensity, levels=c("Not intense at all", "Mildly intense emotions", "Very intense emotions")))
df$income <- factor(df$income, levels=c("$25,000 or less", "$25,001 - $50,000", "$50,001 - $90,000", "$90,001 - $120,000", "$120,001 or more"))
df$group <- factor(df$group)

##H1
#refills box plot
df %>% 
  ggplot(aes(x=group, y=refills_difference, fill=group)) +
  geom_boxplot()
#refills line plot (another way of looking at the info)
df %>% 
  ggplot(aes(x=refills_current, y=refills_intention, group=group, colour=group)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#platic line plot (another way of looking at the info)
df %>% 
  ggplot(aes(x=plastic_current, y=plastic_intention, group=group, colour=group)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  scale_y_continuous(limits =c(from=0, to=10),breaks = seq (from=0, to= 100, by= 5))
#refills statistical test
wilcox.test(df$refills_difference ~ df$group, paired=FALSE, exact=FALSE)

##H2
#mediation test
model.M <- lm(emotion_intensity ~ group, df)
model.Y <- lm(refills_difference ~ group + emotion_intensity, df)
results <- mediate(model.M, model.Y, treat='group', mediator='emotion_intensity', boot=TRUE, sims=500, control.value = "control", treat.value = "experiment")
summary(results)
#ACME = average causal mediation effects = effect of the IV on the DV when it goes through the mediator
#ADE = average direct effects = effect of the IV on the DV after removing the mediator (but it is considered)
#total effect = effect of the IV on the DV without considering the mediator
#read more: https://crumplab.com/psyc7709_2019/book/docs/bootstrapped-mediation-tutorial.html
#read more: https://data.library.virginia.edu/introduction-to-mediation-analysis/
#read more: https://towardsdatascience.com/doing-and-reporting-your-first-mediation-analysis-in-r-2fe423b92171

##H3
#political affiliation box plots (one plot)
df %>% 
  ggplot(aes(x=political_affiliation, y=refills_difference, fill=group)) +
  geom_boxplot()
#political affiliation box plots (facet grid)
df %>%
ggplot(aes(x=group, y=refills_difference, fill=group)) +
  geom_boxplot() +
  facet_wrap(~political_affiliation)




##additional plots
#count participants per condition
table(df$group)

#count current use of plastics and refills
plastic_current <- df %>% count(plastic_current)
write_csv(plastic_current, "plastic_current")
refills_current <- df %>% count(refills_current)
write_csv(refills_current, "refills_current")

#visualize missing values
df %>%
  select(plastic_current, plastic_intention, refills_current, refills_intention) %>% 
  vis_miss()