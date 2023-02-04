##set up libraries neewded

library(tidyverse)
library(ggplot2)

##read in the data
ps328.cols<-read_csv("Group Projects 2019 Numeric.csv")
ps328.text<-read_csv("Group Projects 2019 Text.csv", col_names = FALSE, skip = 3)
ps328.numeric<-read_csv("Group Projects 2019 Numeric.csv", col_names = FALSE, skip = 3)
colnames(ps328.text)<-colnames(ps328.cols)
colnames(ps328.numeric)<-colnames(ps328.cols)
rm(ps328.cols)

##read in second data set
ps328.cols<-read_csv("Group Projects 2nd 2019 Numeric.csv")
ps328.text2<-read_csv("Group Projects 2nd 2019 Text.csv", col_names = FALSE, skip = 3)
ps328.numeric2<-read_csv("Group Projects 2nd 2019 Numeric.csv", col_names = FALSE, skip = 3)
colnames(ps328.text2)<-colnames(ps328.cols)
colnames(ps328.numeric2)<-colnames(ps328.cols)
rm(ps328.cols)

##############################################
##this file presents example code for each group
##go to the section for your group
##############################################


##############################################
##trade group
##trade group q20, 22, 24 (wage, job price), 18 (overall), q49 (text)
##increase, decrease, no diff (1,2,3), 
##############################################

trade19<-mutate(ps328.text,
                trade.treat=FL_31_DO=="TradeSpecific|TradeOverall",
                trade.overall=recode(Q18,`Very bad` =0,
                                     `Somewhat bad` = 1,
                                     `Somewhat good` =  2,
                                     `Very good` = 3),
                trade.wage=recode(Q20, Increase=2,
                                  Decrease=0,
                                  `Does not make a difference`=1),
                trade.price=recode(Q24, Increase=0,
                                  Decrease=2,
                                  `Does not make a difference`=1),
                trade.job=recode(Q22, `Job creation`=2,
                                   `Job losses`=0,
                                   `Does not make a difference`=1))%>%
  select(ResponseId,`Duration (in seconds)`,contains("trade"))

trade19<-mutate(trade19,trade.all=trade.price+trade.wage+trade.job)

#how many specific with trade good
trade19<-mutate(trade19,trade.good=(trade.price==2)+(trade.wage==2)+(trade.job==2))

#analysis
t.test(trade.overall~trade.treat,trade19)
t.test(trade.job~trade.treat,trade19)
table(trade19$trade.good,trade19$trade.overall)


##############################################
##example recode and analysis - humanitarian aid
##############################################
group19<-select(ps328.text,ResponseId,`Duration (in seconds)`)
group19$ideology = ps328.numeric$Q16
group19$aid.moral<-5-ps328.numeric$Q55_1
group19$aid.fund<-3-ps328.numeric$Q58
group19$aid.treat<-ps328.text$HumanitarianAid_DO
group19<-mutate(group19,
                aid.treat =recode(aid.treat,
                        `Q53|Q55|Q58` = "Conflict",
                        `Q52|Q55|Q58` = "Civil War",
                        `Q54|Q55|Q58` = "State Block",
                        `Q60|Q55|Q58` = "Pathos"),
                aid.treat = factor(aid.treat, ordered= FALSE),
                aid.treat = relevel(aid.treat, ref="Conflict"))

#descriptives
table(group19$aid.treat)

table(ps328.text$Q55_1)
table(group19$aid.moral)

table(ps328.text$Q58)
table(group19$aid.fund)

##anova model - not significant
summary(aov(aid.fund~aid.treat,group19))
summary(aov(aid.moral~aid.treat,group19))

#but borderline significance for all treatments in regression
#note - appears to be mediated by change in moral imperative
summary(lm(aid.fund~aid.treat, group19))
summary(lm(aid.moral~aid.treat, group19))

#t.test not significant - treatments not strong enough effect size
t.test(aid.fund~aid.treat, filter(group19, aid.treat%in%c("Conflict","State Block")))
t.test(aid.moral~aid.treat, filter(group19, aid.treat%in%c("Conflict", "State Block")))

#is there an interaction with ideology?
summary(lm(aid.fund~aid.treat*(ideology>3), group19))
summary(lm(aid.moral~aid.treat*(ideology>3), group19))

#is there an interaction with ideology?
summary(lm(aid.fund~aid.treat*ideology, group19))
summary(lm(aid.moral~aid.treat*ideology, group19))

##graphical inspection
p <- ggplot(group19, aes(aid.treat,aid.moral))
p+ geom_boxplot()+
  xlab("Treatment") +
  ylab("Moral Imperative")

p <- ggplot(group19, aes(aid.treat,aid.fund))
p+ geom_boxplot()+
  xlab("Treatment") +
  ylab("Increase Funding")

df <- group19 %>%
  select(aid.treat,aid.fund,ideology) %>%
  filter(is.na(ideology)==FALSE)%>%
  group_by(aid.treat, ideology>3) %>%
  summarise(mean=mean(aid.fund, na.rm=TRUE), se=sd(aid.fund, na.rm=TRUE)/sqrt(n()))%>%
  mutate(liberal = `ideology > 3` ==FALSE)

ggplot(df, aes(aid.treat, mean, colour = liberal)) +
  geom_line(aes(group = liberal)) +
  geom_errorbar(aes(ymin = mean-1.95*se, ymax = mean+1.95*se), width = 0.2)+
  ylim(0,2) +
  xlab("Treatment") +
  ylab("Increase Funding")

##############################################
##example recode and analysis - social media
##############################################
##motivation to control prejudice - use hi.imcp as interaction for prej. experiments
##needed for social media experiment, maybe other

group19<-select(ps328.text,ResponseId,`Duration (in seconds)`)
group19$mcpr1<-ps328.numeric$Q180_1
group19$mcpr2<-ps328.numeric$Q180_2
group19$mcpr3<-ps328.numeric$Q180_3
group19$mcpr4<-ps328.numeric$Q180_4
group19$mcpr5<-ps328.numeric$Q180_5
group19$mcpr6<-ps328.numeric$Q180_6

psych::scoreItems(list(imcp=c("mcpr1","mcpr2"), emcp=c("mcpr3","mcpr4")),
              items=group19[,grep("mcpr", colnames(group19))])
                
factanal(select(group19,contains("mcpr")),2,
         covmat=cor(select(group19,contains("mcpr")),use="pairwise.complete.obs"))

#this creates composite internal motivation to control prejudice item
group19$imcp<-rowMeans(group19[,c("mcpr1","mcpr2","mcpr3")], na.rm=TRUE)
#sometimes you use threshold of 4.5, or use median split like this
group19$hi.imcp<-group19$imcp>4


group19<-mutate(group19,
                socmed.out1 = 5 - ps328.numeric$Q49_1,
                socmed.out2 = 5 - ps328.numeric$Q55,
                socmed.out3 = recode(ps328.numeric$Q70,
                                     `7`=0, `6`=1, `4`=2, `2`=3, `1`=4),
                socmed.out4 = recode(ps328.numeric$Q75,
                                     `7`=0, `4`=1, `3`=2, `2`=3, `1`=4),
                socmed.lib1 = ps328.text$CensorshiponSocialMedia_DO=="Q47|Q49",
                socmed.lib2 = ps328.text$SocialMedia2_DO=="Q52|Q55",
                socmed.lib3 = ps328.text$SocialMediaSlurs_DO=="Q79|Q70",
                socmed.lib4 = ps328.text$SocMediaStereotypes_DO=="Q84|Q75",
                ideology = ps328.numeric$Q16,
                partyid = case_when(
                  ps328.numeric$Q42 == 2 ~ "Democrat",
                  ps328.numeric$Q42 == 1 ~ "Republican",
                  ps328.numeric$Q42 == 3 ~ "Independent",
                  ps328.numeric$Q48_1 == 1  ~ "Democrat",
                  ps328.numeric$Q48_1 == 2  ~ "Republican",
                  ps328.numeric$Q48_1 == 3  ~ "Independent",
                  TRUE ~ NA_character_)
                )

##reshape data to consider treatments simultaneously
##need to loop as need to gather treatment and outcome

socmedia<-select(group19, ResponseId, contains("socmed"), hi.imcp, ideology, partyid) %>%
  gather(.,"cond","socmed.out",contains("socmed.out"), )%>%
  gather(.,"cond2","socmed.treat",contains("socmed.lib"))%>%
  filter(substring(cond, 11,11) == substring(cond2, 11,11))%>%
  filter(is.na(socmed.out)==FALSE)%>%
  select(ResponseId,hi.imcp,socmed.treat,socmed.out,ideology,cond,partyid)%>%
  mutate(socmed.treat = ifelse(socmed.treat,FALSE,TRUE),
         repub = partyid=="Republican",
         dem = partyid == "Democrat")

t.test(socmed.out~socmed.treat, socmedia)
t.test(socmed.out~socmed.treat, filter(socmedia, hi.imcp==TRUE))

lm1<-lm(socmed.out~hi.imcp*socmed.treat, socmedia)
summary(lm1)
summary(lm(socmed.out~ideology*socmed.treat, socmedia))
summary(lm(socmed.out~repub*socmed.treat, socmedia))
summary(lm(socmed.out~(ideology>2)*socmed.treat, socmedia))
summary(lm(socmed.out~hi.imcp*socmed.treat+cond, socmedia))
summary(lm(socmed.out~dem*socmed.treat+cond, socmedia))

summary(lm(socmed.out~ideology + hi.imcp*socmed.treat, socmedia))

plot(effects::allEffects(lm1), multiline=TRUE, ci.style="bars")

lm1<-lm(socmed.out~repub*socmed.treat*cond+hi.imcp, socmedia)
summary(lm1)
plot(effects::allEffects(lm1), multiline=TRUE, ci.style="bars")

# this does the same thing using ggplot

df <- socmedia %>%
  select(socmed.treat,socmed.out,hi.imcp) %>%
  filter(is.na(socmed.out)==FALSE)%>%
  group_by(socmed.treat, hi.imcp) %>%
  summarise(mean=mean(socmed.out, na.rm=TRUE), se=sd(socmed.out, na.rm=TRUE)/sqrt(n()))%>%
  mutate(imcp = ifelse(hi.imcp, "High Internal Motivation", "Low Internal Motivation"))

ggplot(df, aes(socmed.treat, mean, colour = imcp)) +
  geom_line(aes(group = imcp)) +
  geom_errorbar(aes(ymin = mean-1.95*se, ymax = mean+1.95*se), width = 0.2)+
  ylim(0,4) +
  xlab("Blacks Targeted") +
  ylab("Support for Censorship")

df <- socmedia %>%
  select(socmed.treat,socmed.out,repub) %>%
  filter(is.na(socmed.out)==FALSE)%>%
  group_by(socmed.treat, repub) %>%
  summarise(mean=mean(socmed.out, na.rm=TRUE), se=sd(socmed.out, na.rm=TRUE)/sqrt(n()))%>%
  mutate(imcp = ifelse(repub, "Republican", "Independent or Democrat"))

ggplot(df, aes(socmed.treat, mean, colour = repub)) +
  geom_line(aes(group = repub)) +
  geom_errorbar(aes(ymin = mean-1.95*se, ymax = mean+1.95*se), width = 0.2)+
  ylim(0,4) +
  xlab("Blacks Targeted") +
  ylab("Support for Censorship")


##############################################
##example recode and analysis - climate change group
##############################################

climate <- select(ps328.numeric, ResponseId)%>%
  mutate(gender = ps328.text$Q36,
         treat = recode(ps328.text$ClimateChange_DO,
                        `Q63|Q198|Q196` = "Greta Thunberg",
                        `Q63|Q62|Q196` = "James Mattis"), 
         out = ps328.numeric$Q196,
         ideology = ps328.numeric$Q16,
         partyid = case_when(
           ps328.text$Q42 == 2 ~ "Democrat",
           ps328.text$Q42 == 1 ~ "Republican",
           ps328.text$Q42 == 3 ~ "Independent",
           ps328.text$Q48_1 == 1  ~ "Democrat",
           ps328.text$Q48_1 == 2  ~ "Republican",
           ps328.text$Q48_1 == 3  ~ "Independent",
           TRUE ~ NA_character_))

climate2 <-select(ps328.numeric2, ResponseId)%>%
  mutate(gender = ps328.text2$Q36,
         treat = recode(ps328.text2$ClimateChange_DO,
                        `Q198` = "Greta Thunberg",
                        `Q62` = "James Mattis",
                        `Q63` = "Control",
                        .missing= "Control"),
         out = ps328.numeric2$Q196,
         ideology = ps328.numeric2$Q16,
         partyid = case_when(
           ps328.text2$Q42 == 2 ~ "Democrat",
           ps328.text2$Q42 == 1 ~ "Republican",
           ps328.text2$Q42 == 3 ~ "Independent",
           ps328.text2$Q48_1 == 1  ~ "Democrat",
           ps328.text2$Q48_1 == 2  ~ "Republican",
           ps328.text2$Q48_1 == 3  ~ "Independent",
           TRUE ~ NA_character_))

climate<-rbind(climate,climate2)
rm(climate2)

##analysis & plots
t.test(out~gender, data=climate)
summary(lm(out~treat, climate))
summary(lm(out~treat*gender, climate))
summary(lm(out~treat*gender+ideology, climate))

df <- climate %>%
  filter(is.na(out)==FALSE)%>%
  group_by(treat, gender) %>%
  summarise(mean=mean(out, na.rm=TRUE), se=sd(out, na.rm=TRUE)/sqrt(n()))

ggplot(df, aes(treat, mean, colour = gender)) +
  geom_line(aes(group = gender)) +
  geom_errorbar(aes(ymin = mean-1.95*se, ymax = mean+1.95*se), width = 0.2)+
  ylim(0,10) +
  xlab("Treatment") +
  ylab("Climate Change is a Problem")


##############################################
##example recode and analysis - gun control
##############################################

gun <- select(ps328.numeric, ResponseId)%>%
  mutate(gender = ps328.text$Q36,
         treat = "News", 
         out = 3-ps328.numeric$Q48, 
         reduce = 3-ps328.numeric$Q46,
         under25 = ifelse(ps328.text$Q37=="18-25", "Yes", "No"),
         student = "No",
         ideology = ps328.numeric$Q16,
         partyid = case_when(
           ps328.text$Q42 == 2 ~ "Democrat",
           ps328.text$Q42 == 1 ~ "Republican",
           ps328.text$Q42 == 3 ~ "Independent",
           ps328.text$Q48_1 == 1  ~ "Democrat",
           ps328.text$Q48_1 == 2  ~ "Republican",
           ps328.text$Q48_1 == 3  ~ "Independent",
           TRUE ~ NA_character_))

gun2 <-select(ps328.numeric2, ResponseId)%>%
  mutate(gender = ps328.text2$Q36,
         treat = recode(ps328.text2$GunControl_DO,
                        `Q42` = "News",
                        `Q32` = "Control",
                        .missing= "Control"),
         out = 3-ps328.numeric2$Q48, 
         reduce = 3-ps328.numeric2$Q46,
         under25 = ifelse(ps328.text2$Q37=="18-25", "Yes", "No"),
         student = ps328.text2$Q33,
         ideology = ps328.numeric2$Q16,
         partyid = case_when(
           ps328.text2$Q42 == 2 ~ "Democrat",
           ps328.text2$Q42 == 1 ~ "Republican",
           ps328.text2$Q42 == 3 ~ "Independent",
           ps328.text2$Q48_1 == 1  ~ "Democrat",
           ps328.text2$Q48_1 == 2  ~ "Republican",
           ps328.text2$Q48_1 == 3  ~ "Independent",
           TRUE ~ NA_character_))

gun<-rbind(gun,gun2)
rm(gun2)

##analysis & plots
t.test(out~treat, data=gun)
t.test(reduce~treat, data=gun)

##no effect - it may make folks depressed and less optimistic?
##also high level of initial support - hard to go much higher
summary(lm(out~treat, gun))
summary(lm(out~treat*student, gun))
summary(lm(out~treat*under25+ideology, gun))
summary(lm(out~treat*(ideology>4)+student, gun))
summary(lm(out~treat*reduce + ideology+student, gun))
summary(lm(out~treat*as.factor(reduce) + ideology, gun))

df <- gun %>%
  filter(is.na(out)==FALSE)%>%
  group_by(treat, under25) %>%
  summarise(mean=mean(out, na.rm=TRUE), se=sd(out, na.rm=TRUE)/sqrt(n()))

ggplot(df, aes(treat, mean, colour = under25)) +
  geom_line(aes(group = under25)) +
  geom_errorbar(aes(ymin = mean-1.95*se, ymax = mean+1.95*se), width = 0.2)+
  ylim(0,2) +
  xlab("Treatment") +
  ylab("Support for Stricter Gun Laws")

##health media
library(googlesheets4)
health<-read_sheet(ss = "1xl8fO1JLEZ6k_R3xEKCezwVwr-V1wo-HXCdmTWRstfk")

health.sm<-health%>%
  mutate(year = as.numeric(sub("\\*","", unlist(health$Year))),
         partisan= grepl("yes", health$`# Partisanship yes or no...26`),
         universal = grepl("yes", health$`#universal health care      yes or no- if yes count number...27`),
         words = `Total Numbers of  Words Count`,
         era = case_when(
           year<1984 ~"1970-1984",
           year==1984 ~"1984",
           year>1984 & year<1991 ~ "1985-1990",
           year>1990 ~ "1991-1995"))%>%
  select(year, contains("Average"), words, partisan, universal,era)
health.sm<-group_by(health.sm, era)%>%
  summarise(individuals = sum(`Average Individuals`, na.rm=TRUE),
            govt = sum(`Average Government`, na.rm=TRUE),
            words = sum(words, na.rm=TRUE),
            ind.words = individuals/words*100,
            govt.words = govt/words*100)

##############################################
##example recode and analysis - police military
##############################################

police.mil <- select(ps328.numeric, ResponseId)%>%
  mutate(gender = ps328.text$Q36,
         treat = recode(ps328.text$PoliceMilitary_DO,
                        `Q66|Q64` = "Military",
                        `Q63|Q64` = "Control"),
         out.cat = factor(ps328.text$Q64, levels = c("Definitely would not ask them", "Unlikely I would ask them",
                                                 "Likely I would ask them", "Definitely would ask them")),
         out = as.numeric(out.cat),
         ideology = ps328.numeric$Q16,
         partyid = case_when(
           ps328.numeric$Q42 == 2 ~ "Democrat",
           ps328.numeric$Q42 == 1 ~ "Republican",
           ps328.numeric$Q48_1 == 1  ~ "Democrat",
           ps328.numeric$Q48_1 == 2  ~ "Republican",
           ps328.numeric$Q48_1 == 3  ~ "Independent",
           ps328.numeric$Q42 == 3 ~ "Independent",
           TRUE ~ NA_character_),
         party.orig = case_when(
           ps328.numeric$Q42 == 2 ~ "Democrat",
           ps328.numeric$Q42 == 1 ~ "Republican",
           ps328.numeric$Q42 == 3 ~ "Independent",
           TRUE ~ NA_character_),)

##analysis & plots
t.test(out~treat, data=police.mil)
chisq.test(police.mil$treat, police.mil$out.cat)

##no effect - it may make folks depressed and less optimistic?
##also high level of initial support - hard to go much higher
summary(lm(out~treat, police.mil))
summary(lm(out~treat*ideology, police.mil))
summary(lm(out~treat+I(partyid=="Republican"), police.mil))
summary(lm(out~treat*partyid, police.mil))
summary(lm(out~treat*I(partyid=="Republican"), police.mil))
summary(lm(out~treat*party.orig, police.mil))

temp<-filter(police.mil,is.na(partyid)==FALSE, is.na(out.cat)==FALSE) %>%
  count(out.cat, treat) %>% 
  group_by(treat) %>% 
  mutate(Sum=sum(n),
         proportion = n/Sum)
  ggplot(aes(y=proportion, x=out.cat,fill=treat)) +
  geom_col(position = "dodge")

ggplot(temp,aes(y=proportion, x=out.cat,fill=treat)) + 
  geom_col(position="dodge")+
  labs(x="\n Would you approach the Police?", y="Proportion of Subjects who say...",
       fill=expression(paste("Treatment \n Photo")))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_discrete(labels=c(expression(paste("Definitely \n would NOT")), 
                      "Unlikely\n", "Likely\n", expression(paste("Definitely \n would"))))+
  theme(axis.ticks.x = element_blank(), axis.text.x=element_text(vjust=-2))
  

ggplot(filter(police.mil, is.na(partyid)==FALSE), aes(x=partyid,y=out,fill=treat)) + 
  geom_violin(position="dodge")+
  scale_y_discrete(
    labels = levels(police.mil$out.cat))

df <- police.mil %>%
  filter(is.na(out)==FALSE, is.na(partyid)==FALSE)%>%
  group_by(treat, partyid) %>%
  summarise(mean=mean(out, na.rm=TRUE), se=sd(out, na.rm=TRUE)/sqrt(n()))

ggplot(df, aes(treat, mean, colour = under25)) +
  geom_line(aes(group = under25)) +
  geom_errorbar(aes(ymin = mean-1.95*se, ymax = mean+1.95*se), width = 0.2)+
  ylim(0,2) +
  xlab("Treatment") +
  ylab("Support for Stricter Gun Laws")

