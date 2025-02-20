########################
#LOAD DATA AND PACKAGES#
########################

setwd("~/Desktop/Dissertation/JSPP - Positive Characteristics") #insert your own working directory here
library(readr)
library(tidyr)
library(tidytext)
library(dplyr)
library(textstem)
library(SnowballC)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(corrplot)
library(psych)
library(ggplot2)
library(stargazer)
library(webshot)

data <- read.csv('focusing_on_the_good_data.csv.csv')
data <- subset(data, Finished == "TRUE") # remove people who didn't complete survey

##################################################
#PARTISANSHIP, TREATMENT GROUPS, ATTENTION CHECKS#
##################################################

#PARTISANSHIP
#count leaners as Democrats and Republicans
data$partisanship[data$lean == "Closer to Democratic"] <- "Democrat"
data$partisanship[data$lean == "Closer to Republican"] <- "Republican"
#how many of each party?
#369 Democrats, 86 Republicans
table(data$partisanship)
#filter for only Democrats and Republicans
subjectpool <- data %>% filter(partisanship %in% c("Republican", "Democrat"))

#TREATMENT GROUPS
#treatment
table(subjectpool$treatment)
#control
#t1 - outpartisan
subjectpool$treatment_combined <- subjectpool$treatment
subjectpool$treatment_combined[subjectpool$treatment_combined == "treatment1d"] <- "outpartisan"
subjectpool$treatment_combined[subjectpool$treatment_combined == "treatment1r"] <- "outpartisan"
#t2 - positive
subjectpool$treatment_combined[subjectpool$treatment_combined == "treatment2"] <- "positive"
#t3 - outpartisan and positive
subjectpool$treatment_combined[subjectpool$treatment_combined == "treatment3d"] <- "both"
subjectpool$treatment_combined[subjectpool$treatment_combined == "treatment3r"] <- "both"

#ATTENTION CHECKS
table(subjectpool$name) #443 said Sam out of 455
table(subjectpool$relationship) #428 said work out of 455
table(subjectpool$name, subjectpool$relationship) #420 out of 455
#keep the people who passed both attention checks
subjectpool_attentive <- subjectpool %>%
  filter(subjectpool$name == "Sam" & subjectpool$relationship == "Work")

####################################
#INDIVIDUAL-LEVEL OUTCOME VARIABLES#
####################################

#SAM FEELING THERMOMETER

#Only attentive
#There does not really seem to be a difference between attentive and entire sample

#subjectpool_attentive$sam_ft_1 <- as.numeric(subjectpool_attentive$sam_ft_1)
#mean(subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "control"], na.rm = TRUE) #
#mean(subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "outpartisan"], na.rm = TRUE) #54.54082
#mean(subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "positive"], na.rm = TRUE) #76.40909
#mean(subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "both"], na.rm = TRUE) #

#t.test(subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "outpartisan"], 
#       subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "control"]) # ***p<0.01
#t.test(subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "positive"], 
#       subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "control"]) # ***p<0.01
#t.test(subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "both"], 
#       subjectpool_attentive$sam_ft_1[subjectpool_attentive$treatment_combined == "control"]) # not significant

#Everyone
subjectpool$sam_ft_1 <- as.numeric(subjectpool$sam_ft_1)
mean(subjectpool$sam_ft_1[subjectpool$treatment_combined == "control"], na.rm = TRUE) #63.45133
mean(subjectpool$sam_ft_1[subjectpool$treatment_combined == "outpartisan"], na.rm = TRUE) #54.54867
mean(subjectpool$sam_ft_1[subjectpool$treatment_combined == "positive"], na.rm = TRUE) #75.67826
mean(subjectpool$sam_ft_1[subjectpool$treatment_combined == "both"], na.rm = TRUE) #65.95614

#SAM TRAITS

#Trait scales

subjectpool$sam_trait_positive <- rowSums(subjectpool[ , c('sam_generous', 'sam_honest', 'sam_openminded', 'sam_intelligent')])
subjectpool$sam_trait_negative <- rowSums(subjectpool[ , c('sam_mean', 'sam_selfish', 'sam_closedminded', 'sam_hypocritical')])
subjectpool <- subjectpool %>% mutate(sam_trait_scale = sam_trait_positive - sam_trait_negative)

subjectpool$sam_closedminded_recoded <- ifelse(subjectpool$sam_closedminded == 1, 0, 1)
subjectpool$sam_hypocritical_recoded <- ifelse(subjectpool$sam_hypocritical == 1, 0, 1)
subjectpool$sam_selfish_recoded <- ifelse(subjectpool$sam_selfish == 1, 0, 1)
subjectpool$sam_mean_recoded <- ifelse(subjectpool$sam_mean == 1, 0, 1)

sam_traits_recoded_df <- subjectpool[,c('sam_honest', 'sam_intelligent', 'sam_generous', 'sam_openminded', 'sam_closedminded_recoded', 'sam_hypocritical_recoded', 'sam_selfish_recoded', 'sam_mean_recoded')]


###############################
#GROUP-LEVEL OUTCOME VARIABLES#
###############################

#Group feeling thermometer

# People who vote for the opposite party

# transforming data frame 
# declare col4 where if col1 is equal 
# to col3, replace by col1+col3 value, 
# otherwise by col1+col2 value 
#data_frame$col4 <- with( 
#  data_frame, ifelse(col1+col3>5, col1+col3, col1+col2))

subjectpool <- transform(
  subjectpool, group_ft_outparty_voters = ifelse(partisanship == "Republican", group_ft_1, group_ft_2)
)

subjectpool[,c('partisanship','group_ft_1','group_ft_2','group_ft_outparty_voters')] %>% View()

subjectpool$group_ft_outparty_voters <- as.numeric(subjectpool$group_ft_outparty_voters)

subjectpool <- transform(
  subjectpool, group_ft_inparty_voters = ifelse(partisanship == "Democrat", group_ft_1, group_ft_2)
)
subjectpool$group_ft_inparty_voters <- as.numeric(subjectpool$group_ft_inparty_voters)
subjectpool[,c('partisanship','group_ft_1','group_ft_2','group_ft_inparty_voters')] %>% View()

#subjectpool$group_ft_1 - People who vote Democrat

subjectpool$group_ft_1 <- as.numeric(subjectpool$group_ft_1)

#subjectpool$group_ft_2 #People who vote Republican

subjectpool$group_ft_2 <- as.numeric(subjectpool$group_ft_2)

#group_ft_politicians

subjectpool <- transform(
  subjectpool, group_ft_outparty_politicians = ifelse(partisanship == "Republican", group_ft_3, group_ft_4)
)

subjectpool[,c('partisanship','group_ft_3','group_ft_4','group_ft_outparty_politicians')] %>% View()

subjectpool$group_ft_outparty_politicians <- as.numeric(subjectpool$group_ft_outparty_politicians)

subjectpool <- transform(
  subjectpool, group_ft_inparty_politicians = ifelse(partisanship == "Democrat", group_ft_3, group_ft_4)
)
subjectpool$group_ft_inparty_politicians <- as.numeric(subjectpool$group_ft_inparty_politicians)
subjectpool[,c('partisanship','group_ft_3','group_ft_4','group_ft_inparty_politicians')] %>% View()

#subjectpool$group_ft_3 #Democratic politicians

subjectpool$group_ft_3 <- as.numeric(subjectpool$group_ft_3)

#subjectpool$group_ft_4 #Republican politicians

subjectpool$group_ft_4 <- as.numeric(subjectpool$group_ft_4)

#subjectpool$group_ft_5 #Your coworkers

subjectpool$group_ft_5 <- as.numeric(subjectpool$group_ft_5)

#subjectpool$group_ft_6 #Men

subjectpool$group_ft_6 <- as.numeric(subjectpool$group_ft_6)

#subjectpool$group_ft_7 #Women

subjectpool$group_ft_7 <- as.numeric(subjectpool$group_ft_7)

#subjectpool$group_ft_8 #LGBTQ+

subjectpool$group_ft_8 <- as.numeric(subjectpool$group_ft_8)

#subjectpool$group_ft_9 #Americans

subjectpool$group_ft_9 <- as.numeric(subjectpool$group_ft_9)

#subjectpool$group_ft_10 #Immigrants

subjectpool$group_ft_10 <- as.numeric(subjectpool$group_ft_10)

#Group friend

#subjectpool$group_friend_1 #People who vote Democrat
subjectpool$group_friend_1[subjectpool$group_friend_1 == "Very likely"] <- 5
subjectpool$group_friend_1[subjectpool$group_friend_1 == "LIkely"] <- 4
subjectpool$group_friend_1[subjectpool$group_friend_1 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_1[subjectpool$group_friend_1 == "Unlikely"] <- 2
subjectpool$group_friend_1[subjectpool$group_friend_1 == "Very unlikely"] <- 1
subjectpool$group_friend_1<- as.numeric(subjectpool$group_friend_1)

#subjectpool$group_friend_2 #People who vote Republican
subjectpool$group_friend_2[subjectpool$group_friend_2 == "Very likely"] <- 5
subjectpool$group_friend_2[subjectpool$group_friend_2 == "LIkely"] <- 4
subjectpool$group_friend_2[subjectpool$group_friend_2 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_2[subjectpool$group_friend_2 == "Unlikely"] <- 2
subjectpool$group_friend_2[subjectpool$group_friend_2 == "Very unlikely"] <- 1
subjectpool$group_friend_2<- as.numeric(subjectpool$group_friend_2)

#subjectpool$group_friend_voters

subjectpool <- transform(
  subjectpool, group_friend_outparty_voters = ifelse(partisanship == "Republican", group_friend_1, group_friend_2)
)

subjectpool[,c('partisanship','group_friend_1','group_friend_2','group_friend_outparty_voters')] %>% View()

subjectpool <- transform(
  subjectpool, group_friend_inparty_voters = ifelse(partisanship == "Democrat", group_friend_1, group_friend_2)
)
subjectpool[,c('partisanship','group_friend_1','group_friend_2','group_friend_inparty_voters')] %>% View()

#subjectpool$group_friend_3 #Democratic politicians
subjectpool$group_friend_3[subjectpool$group_friend_3 == "Very likely"] <- 5
subjectpool$group_friend_3[subjectpool$group_friend_3 == "LIkely"] <- 4
subjectpool$group_friend_3[subjectpool$group_friend_3 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_3[subjectpool$group_friend_3 == "Unlikely"] <- 2
subjectpool$group_friend_3[subjectpool$group_friend_3 == "Very unlikely"] <- 1
subjectpool$group_friend_3<- as.numeric(subjectpool$group_friend_3)

#subjectpool$group_friend_4 #Republican politicians
subjectpool$group_friend_4[subjectpool$group_friend_4 == "Very likely"] <- 5
subjectpool$group_friend_4[subjectpool$group_friend_4 == "LIkely"] <- 4
subjectpool$group_friend_4[subjectpool$group_friend_4 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_4[subjectpool$group_friend_4 == "Unlikely"] <- 2
subjectpool$group_friend_4[subjectpool$group_friend_4 == "Very unlikely"] <- 1
subjectpool$group_friend_4<- as.numeric(subjectpool$group_friend_4)

#subjectpool$group_friend_politicians

subjectpool <- transform(
  subjectpool, group_friend_outparty_politicians = ifelse(partisanship == "Republican", group_friend_3, group_friend_4)
)

subjectpool[,c('partisanship','group_friend_3','group_friend_4','group_friend_outparty_politicians')] %>% View()

subjectpool <- transform(
  subjectpool, group_friend_inparty_politicians = ifelse(partisanship == "Democrat", group_friend_3, group_friend_4)
)

#subjectpool$group_friend_5 #Your coworkers
subjectpool$group_friend_5[subjectpool$group_friend_5 == "Very likely"] <- 5
subjectpool$group_friend_5[subjectpool$group_friend_5 == "LIkely"] <- 4
subjectpool$group_friend_5[subjectpool$group_friend_5 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_5[subjectpool$group_friend_5 == "Unlikely"] <- 2
subjectpool$group_friend_5[subjectpool$group_friend_5 == "Very unlikely"] <- 1
subjectpool$group_friend_5<- as.numeric(subjectpool$group_friend_5)

#subjectpool$group_friend_6 #Men
subjectpool$group_friend_6[subjectpool$group_friend_6 == "Very likely"] <- 5
subjectpool$group_friend_6[subjectpool$group_friend_6 == "LIkely"] <- 4
subjectpool$group_friend_6[subjectpool$group_friend_6 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_6[subjectpool$group_friend_6 == "Unlikely"] <- 2
subjectpool$group_friend_6[subjectpool$group_friend_6 == "Very unlikely"] <- 1
subjectpool$group_friend_6<- as.numeric(subjectpool$group_friend_6)

#subjectpool$group_friend_7 #Women
subjectpool$group_friend_7[subjectpool$group_friend_7 == "Very likely"] <- 5
subjectpool$group_friend_7[subjectpool$group_friend_7 == "LIkely"] <- 4
subjectpool$group_friend_7[subjectpool$group_friend_7 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_7[subjectpool$group_friend_7 == "Unlikely"] <- 2
subjectpool$group_friend_7[subjectpool$group_friend_7 == "Very unlikely"] <- 1
subjectpool$group_friend_7<- as.numeric(subjectpool$group_friend_7)

#subjectpool$group_friend_8 #LGBTQ+
subjectpool$group_friend_8[subjectpool$group_friend_8 == "Very likely"] <- 5
subjectpool$group_friend_8[subjectpool$group_friend_8 == "LIkely"] <- 4
subjectpool$group_friend_8[subjectpool$group_friend_8 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_8[subjectpool$group_friend_8 == "Unlikely"] <- 2
subjectpool$group_friend_8[subjectpool$group_friend_8 == "Very unlikely"] <- 1
subjectpool$group_friend_8<- as.numeric(subjectpool$group_friend_8)

#subjectpool$group_friend_9 #Americans
subjectpool$group_friend_9[subjectpool$group_friend_9 == "Very likely"] <- 5
subjectpool$group_friend_9[subjectpool$group_friend_9 == "LIkely"] <- 4
subjectpool$group_friend_9[subjectpool$group_friend_9 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_9[subjectpool$group_friend_9 == "Unlikely"] <- 2
subjectpool$group_friend_9[subjectpool$group_friend_9 == "Very unlikely"] <- 1
subjectpool$group_friend_9<- as.numeric(subjectpool$group_friend_9)

#subjectpool$group_friend_10 #Immigrants
subjectpool$group_friend_10[subjectpool$group_friend_10 == "Very likely"] <- 5
subjectpool$group_friend_10[subjectpool$group_friend_10 == "LIkely"] <- 4
subjectpool$group_friend_10[subjectpool$group_friend_10 == "Neither likely nor unlikely"] <- 3
subjectpool$group_friend_10[subjectpool$group_friend_10 == "Unlikely"] <- 2
subjectpool$group_friend_10[subjectpool$group_friend_10 == "Very unlikely"] <- 1
subjectpool$group_friend_10<- as.numeric(subjectpool$group_friend_10)

#Group marry
#subjectpool$group_marry_1 #People who vote Democrat
subjectpool$group_marry_1[subjectpool$group_marry_1 == "Excited"] <- 3
subjectpool$group_marry_1[subjectpool$group_marry_1 == "Neutral"] <- 2
subjectpool$group_marry_1[subjectpool$group_marry_1 == "Upset"] <- 1
subjectpool$group_marry_1<- as.numeric(subjectpool$group_marry_1)

#subjectpool$group_marry_2 #People who vote Republican
subjectpool$group_marry_2[subjectpool$group_marry_2 == "Excited"] <- 3
subjectpool$group_marry_2[subjectpool$group_marry_2 == "Neutral"] <- 2
subjectpool$group_marry_2[subjectpool$group_marry_2 == "Upset"] <- 1
subjectpool$group_marry_2<- as.numeric(subjectpool$group_marry_2)

#subjectpool$group_marry_voters

subjectpool <- transform(
  subjectpool, group_marry_outparty_voters = ifelse(partisanship == "Republican", group_marry_1, group_marry_2)
)
subjectpool[,c('partisanship','group_marry_1','group_marry_2','group_marry_outparty_voters')] %>% View()

subjectpool <- transform(
  subjectpool, group_marry_inparty_voters = ifelse(partisanship == "Democrat", group_marry_1, group_marry_2)
)

#subjectpool$group_marry_3 #Democratic politicians
subjectpool$group_marry_3[subjectpool$group_marry_3 == "Excited"] <- 3
subjectpool$group_marry_3[subjectpool$group_marry_3 == "Neutral"] <- 2
subjectpool$group_marry_3[subjectpool$group_marry_3 == "Upset"] <- 1
subjectpool$group_marry_3<- as.numeric(subjectpool$group_marry_3)

#subjectpool$group_marry_4 #Republican politicians
subjectpool$group_marry_4[subjectpool$group_marry_4 == "Excited"] <- 3
subjectpool$group_marry_4[subjectpool$group_marry_4 == "Neutral"] <- 2
subjectpool$group_marry_4[subjectpool$group_marry_4 == "Upset"] <- 1
subjectpool$group_marry_4 <- as.numeric(subjectpool$group_marry_4)

#subjectpool$group_marry_politicians


subjectpool <- transform(
  subjectpool, group_marry_outparty_politicians = ifelse(partisanship == "Republican", group_marry_3, group_marry_4)
)

subjectpool[,c('partisanship','group_marry_3','group_marry_4','group_marry_politicians')] %>% View()

subjectpool <- transform(
  subjectpool, group_marry_inparty_politicians = ifelse(partisanship == "Democrat", group_marry_3, group_marry_4)
)


#subjectpool$group_marry_5 #Your coworkers
subjectpool$group_marry_5[subjectpool$group_marry_5 == "Excited"] <- 3
subjectpool$group_marry_5[subjectpool$group_marry_5 == "Neutral"] <- 2
subjectpool$group_marry_5[subjectpool$group_marry_5 == "Upset"] <- 1
subjectpool$group_marry_5<- as.numeric(subjectpool$group_marry_5)

#subjectpool$group_marry_6 #Men
subjectpool$group_marry_6[subjectpool$group_marry_6 == "Excited"] <- 3
subjectpool$group_marry_6[subjectpool$group_marry_6 == "Neutral"] <- 2
subjectpool$group_marry_6[subjectpool$group_marry_6 == "Upset"] <- 1
subjectpool$group_marry_6<- as.numeric(subjectpool$group_marry_6)

#subjectpool$group_marry_7 #Women
subjectpool$group_marry_7[subjectpool$group_marry_7 == "Excited"] <- 3
subjectpool$group_marry_7[subjectpool$group_marry_7 == "Neutral"] <- 2
subjectpool$group_marry_7[subjectpool$group_marry_7 == "Upset"] <- 1
subjectpool$group_marry_7<- as.numeric(subjectpool$group_marry_7)

#subjectpool$group_marry_8 #LGBTQ+
subjectpool$group_marry_8[subjectpool$group_marry_8 == "Excited"] <- 3
subjectpool$group_marry_8[subjectpool$group_marry_8 == "Neutral"] <- 2
subjectpool$group_marry_8[subjectpool$group_marry_8 == "Upset"] <- 1
subjectpool$group_marry_8<- as.numeric(subjectpool$group_marry_8)

#subjectpool$group_marry_9 #Americans
subjectpool$group_marry_9[subjectpool$group_marry_9 == "Excited"] <- 3
subjectpool$group_marry_9[subjectpool$group_marry_9 == "Neutral"] <- 2
subjectpool$group_marry_9[subjectpool$group_marry_9 == "Upset"] <- 1
subjectpool$group_marry_9<- as.numeric(subjectpool$group_marry_9)

#subjectpool$group_marry_10 #Immigrants
subjectpool$group_marry_10[subjectpool$group_marry_10 == "Excited"] <- 3
subjectpool$group_marry_10[subjectpool$group_marry_10 == "Neutral"] <- 2
subjectpool$group_marry_10[subjectpool$group_marry_10 == "Upset"] <- 1
subjectpool$group_marry_10<- as.numeric(subjectpool$group_marry_10)

#Group traits
#subjectpool$group_traits_voters
subjectpool <- transform(
  subjectpool, group_traits_voters = ifelse(partisanship == "Republican", group_traits_1, group_traits_2)
)

subjectpool[,c('partisanship','group_traits_1','group_traits_2','group_traits_voters')] %>% View()


#subjectpool$group_traits_politicians
subjectpool <- transform(
  subjectpool, group_traits_politicians = ifelse(partisanship == "Republican", group_traits_3, group_traits_4)
)

subjectpool[,c('partisanship','group_traits_3','group_traits_4','group_traits_politicians')] %>% View()

#GROUP TRAIT SCALES

#Trait scale for dem and rep
subjectpool$dem_voter_trait_positive <- rowSums(subjectpool[ , c('dem_voter_generous', 'dem_voter_honest', 'dem_voter_openminded', 'dem_voter_intelligent')])
subjectpool$dem_voter_trait_negative <- rowSums(subjectpool[ , c('dem_voter_mean', 'dem_voter_selfish', 'dem_voter_closedminded', 'dem_voter_hypocritical')])
subjectpool <- subjectpool %>% mutate(dem_voter_trait_scale = dem_voter_trait_positive - dem_voter_trait_negative)

subjectpool$rep_voter_trait_positive <- rowSums(subjectpool[ , c('rep_voter_generous', 'rep_voter_honest', 'rep_voter_openminded', 'rep_voter_intelligent')])
subjectpool$rep_voter_trait_negative <- rowSums(subjectpool[ , c('rep_voter_mean', 'rep_voter_selfish', 'rep_voter_closedminded', 'rep_voter_hypocritical')])
subjectpool <- subjectpool %>% mutate(rep_voter_trait_scale = rep_voter_trait_positive - rep_voter_trait_negative)

subjectpool$dem_politician_trait_positive <- rowSums(subjectpool[ , c('dem_politician_generous', 'dem_politician_honest', 'dem_politician_openminded', 'dem_politician_intelligent')])
subjectpool$dem_politician_trait_negative <- rowSums(subjectpool[ , c('dem_politician_mean', 'dem_politician_selfish', 'dem_politician_closedminded', 'dem_politician_hypocritical')])
subjectpool <- subjectpool %>% mutate(dem_politician_trait_scale = dem_politician_trait_positive - dem_politician_trait_negative)

subjectpool$rep_politician_trait_positive <- rowSums(subjectpool[ , c('rep_politician_generous', 'rep_politician_honest', 'rep_politician_openminded', 'rep_politician_intelligent')])
subjectpool$rep_politician_trait_negative <- rowSums(subjectpool[ , c('rep_politician_mean', 'rep_politician_selfish', 'rep_politician_closedminded', 'rep_politician_hypocritical')])
subjectpool <- subjectpool %>% mutate(rep_politician_trait_scale = rep_politician_trait_positive - rep_politician_trait_negative)

#People who vote for the opposite party

subjectpool <- transform(
  subjectpool, outparty_voter_trait_scale = ifelse(partisanship == "Republican", dem_voter_trait_positive, rep_voter_trait_scale)
)
subjectpool[,c('partisanship','dem_voter_trait_positive','rep_voter_trait_scale','outparty_voter_trait_scale')] %>% View()

subjectpool <- transform(
  subjectpool, inparty_voter_trait_scale = ifelse(partisanship == "Democrat", dem_voter_trait_positive, rep_voter_trait_scale)
)


#Politicians of the opposite party

subjectpool <- transform(
  subjectpool, outparty_politician_trait_scale = ifelse(partisanship == "Republican", dem_politician_trait_positive, rep_politician_trait_scale)
)
subjectpool[,c('partisanship','dem_politician_trait_positive','rep_politician_trait_scale','outparty_politician_trait_scale')] %>% View()

subjectpool <- transform(
  subjectpool, inparty_politician_trait_scale = ifelse(partisanship == "Democrat", dem_politician_trait_positive, rep_politician_trait_scale)
)
subjectpool[,c('partisanship','dem_politician_trait_positive','rep_politician_trait_scale','inparty_politician_trait_scale')] %>% View()


#subjectpool$group_traits_5 #Your coworkers
subjectpool$coworker_trait_positive <- rowSums(subjectpool[ , c('coworker_generous', 'coworker_honest', 'coworker_openminded', 'coworker_intelligent')])
subjectpool$coworker_trait_negative <- rowSums(subjectpool[ , c('coworker_mean', 'coworker_selfish', 'coworker_closedminded', 'coworker_hypocritical')])
subjectpool <- subjectpool %>% mutate(coworker_trait_scale = coworker_trait_positive - coworker_trait_negative)

#subjectpool$group_traits_6 #Men

subjectpool$men_trait_positive <- rowSums(subjectpool[ , c('men_generous', 'men_honest', 'men_openminded', 'men_intelligent')])
subjectpool$men_trait_negative <- rowSums(subjectpool[ , c('men_mean', 'men_selfish', 'men_closedminded', 'men_hypocritical')])
subjectpool <- subjectpool %>% mutate(men_trait_scale = men_trait_positive - men_trait_negative)

#subjectpool$group_traits_7 #Women
subjectpool$women_trait_positive <- rowSums(subjectpool[ , c('women_generous', 'women_honest', 'women_openminded', 'women_intelligent')])
subjectpool$women_trait_negative <- rowSums(subjectpool[ , c('women_mean', 'women_selfish', 'women_closedminded', 'women_hypocritical')])
subjectpool <- subjectpool %>% mutate(women_trait_scale = women_trait_positive - women_trait_negative)

#subjectpool$group_traits_8 #LGBTQ+
subjectpool$lgbtq_trait_positive <- rowSums(subjectpool[ , c('lgbtq_generous', 'lgbtq_honest', 'lgbtq_openminded', 'lgbtq_intelligent')])
subjectpool$lgbtq_trait_negative <- rowSums(subjectpool[ , c('lgbtq_mean', 'lgbtq_selfish', 'lgbtq_closedminded', 'lgbtq_hypocritical')])
subjectpool <- subjectpool %>% mutate(lgbtq_trait_scale = lgbtq_trait_positive - lgbtq_trait_negative)

#subjectpool$group_traits_9 #Americans
subjectpool$americans_trait_positive <- rowSums(subjectpool[ , c('americans_generous', 'americans_honest', 'americans_openminded', 'americans_intelligent')])
subjectpool$americans_trait_negative <- rowSums(subjectpool[ , c('americans_mean', 'americans_selfish', 'americans_closedminded', 'americans_hypocritical')])
subjectpool <- subjectpool %>% mutate(americans_trait_scale = americans_trait_positive - americans_trait_negative)

#subjectpool$group_traits_10 #Immigrants
subjectpool$immigrants_trait_positive <- rowSums(subjectpool[ , c('immigrants_generous', 'immigrants_honest', 'immigrants_openminded', 'immigrants_intelligent')])
subjectpool$immigrants_trait_negative <- rowSums(subjectpool[ , c('immigrants_mean', 'immigrants_selfish', 'immigrants_closedminded', 'immigrants_hypocritical')])
subjectpool <- subjectpool %>% mutate(immigrants_trait_scale = immigrants_trait_positive - immigrants_trait_negative)



##############################
### AFFECTIVE POLARIZATION ###
##############################

#Lelkes and Westwood (2017, 489): “The difference in the feeling thermometer score for a respondent’s own party and the feeling thermometer score for the opposition party” 
#(i.e., out-party feeling – in-party feeling = affective polarization). 

#For example, Republican has in-party FT score of 50 and 30 for Democrats. Another has 50 in-party and 10 for Democrats. 
#30 - 50 = -20.
#10 - 50 = -40. 
#Second person has more affective polarization than first. 

subjectpool$affective_polarization_voters <- subjectpool$group_ft_outparty_voters - subjectpool$group_ft_inparty_voters
subjectpool$affective_polarization_politicians <- subjectpool$group_ft_outparty_politicians - subjectpool$group_ft_inparty_politicians



#################################################
### REGRESSIONS TO ESTIMATE TREATMENT EFFECTS ###
#################################################

subjectpool$treatment_combined[subjectpool$treatment_combined == "both"] <- "positive_outpartisan" #reorder

sam1 <- lm(subjectpool$sam_ft_1 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
sam2 <- lm(subjectpool$sam_trait_scale ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

subjectpool$affpol_traits_voters <- subjectpool$outparty_voter_trait_scale - subjectpool$inparty_voter_trait_scale
subjectpool$affpol_traits_politicians <- subjectpool$outparty_politician_trait_scale - subjectpool$inparty_politician_trait_scale
subjectpool$affpol_friend_voters <- subjectpool$group_friend_outparty_voters - subjectpool$group_friend_inparty_voters
subjectpool$affpol_friend_politicians <- subjectpool$group_friend_outparty_politician - subjectpool$group_friend_inparty_politician
subjectpool$affpol_marry_voters <- subjectpool$group_marry_outparty_voters - subjectpool$group_marry_inparty_voters
subjectpool$affpol_marry_politicians <- subjectpool$group_marry_outparty_politician - subjectpool$group_marry_inparty_politician

voter1 <- lm(subjectpool$affective_polarization_voters ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
voter2 <- lm(subjectpool$affpol_traits_voters ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
voter3 <- lm(subjectpool$affpol_friend_voters ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
voter4 <- lm(subjectpool$affpol_marry_voters ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

politician1 <- lm(subjectpool$affective_polarization_politicians ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
politician2 <- lm(subjectpool$affpol_traits_politicians ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
politician3 <- lm(subjectpool$affpol_friend_politicians ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
politician4 <- lm(subjectpool$affpol_marry_politicians ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

sam_list <- list(sam1, sam2)
voter_list <- list(voter1, voter2, voter3, voter4)
politician_list <- list(politician1, politician2, politician3, politician4)

covariate_labels <- c("outpartisan" = "Outpartisan Treatments (1 and 3)",
                      "positive" = "Positive Treatments (2 and 3)",
                      "both" = "Outpartisan and Positive Treatment (3)")

outcome_labels_sam <- c("sam_ft_1" = "Sam Feeling Thermometer",
                    "sam_trait_scale" = "Sam Trait Scale")

outcome_labels_voter <- c("affective_polarization_voters" = "Feeling Thermometer",
                          "affpol_traits_voters" = "Trait Scale",
                          "affpol_friend_voters" = "Friend",
                          "affpol_marry_voters" = "In-Law")

outcome_labels_politician <- c("affective_polarization_politician" = "Feeling Thermometer",
                          "affpol_traits_politicians" = "Trait Scale",
                          "affpol_friend_politicians" = "Friend",
                          "affpol_marry_politicians" = "In-Law")


sam_table <- stargazer(sam_list, type = 'html', out = "sam_table.html", dep.var.labels = outcome_labels_sam, covariate.labels = covariate_labels,
                       star.char = c("*", "**", "***"),
                       star.cutoffs = c(.05, .01, .001))

voter_table <- stargazer(voter_list, type = 'html', out = "voter_table.html", dep.var.labels = outcome_labels_voter, covariate.labels = covariate_labels,
                         star.char = c("*", "**", "***"),
                         star.cutoffs = c(.05, .01, .001))

politician_table <- stargazer(politician_list, type = 'html', out = "politician_table.html", dep.var.labels = outcome_labels_politician, covariate.labels = covariate_labels,
                              star.char = c("*", "**", "***"),
                              star.cutoffs = c(.05, .01, .001))

#Sam traits mentioned versus not#

subjectpool$sam_trait_mentioned <- subjectpool$sam_honest + subjectpool$sam_generous - subjectpool$sam_selfish - subjectpool$sam_mean
subjectpool$sam_trait_notmentioned <- subjectpool$sam_intelligent + subjectpool$sam_openminded - subjectpool$sam_hypocritical - subjectpool$sam_closedminded

sam3 <- lm(subjectpool$sam_trait_mentioned ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
sam4 <- lm(subjectpool$sam_trait_notmentioned ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

sam_list2 <- list(sam3, sam4)

outcome_labels_sam2 <- c("sam_trait_mentioned" = "Traits Mentioned",
                        "sam_trait_notmentioned" = "Traits Not Mentioned")

stargazer(sam_list2, type = 'html', out = "sam_table2.html", dep.var.labels = outcome_labels_sam2, covariate.labels = covariate_labels,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))


### OTHER GROUPS ####

# 5. Your coworkers #

coworkers_ft <- lm(subjectpool$group_ft_5 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
coworkers_friend <- lm(subjectpool$group_friend_5 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
coworkers_inlaw <- lm(subjectpool$group_marry_5 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
coworkers_traits <- lm(subjectpool$coworker_trait_scale ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

coworkers_list <- list(coworkers_ft, coworkers_friend, coworkers_inlaw, coworkers_traits)

dv_labels_coworkers <- c("group_ft_5" = "Feeling Thermometer",
                         "coworker_trait_scale" = "Trait Scale",
                         "group_friend_5" = "Friend",
                         "group_marry_5" = "In-Law")

stargazer(coworkers_list, type = 'html', out = "coworkers.html", dep.var.labels = dv_labels_coworkers, covariate.labels = covariate_labels,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

# 6. Men #

men_ft <- lm(subjectpool$group_ft_6 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
men_friend <- lm(subjectpool$group_friend_6 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
men_inlaw <- lm(subjectpool$group_marry_6 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
men_traits <- lm(subjectpool$men_trait_scale ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

men_list <- list(men_ft, men_friend, men_inlaw, men_traits)

dv_labels_men <- c("group_ft_6" = "Feeling Thermometer",
                         "men_trait_scale" = "Trait Scale",
                         "group_friend_6" = "Friend",
                         "group_marry_6" = "In-Law")

stargazer(men_list, type = 'html', out = "men.html", dep.var.labels = dv_labels_men, covariate.labels = covariate_labels,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

# 7. Women # 
women_ft <- lm(subjectpool$group_ft_7 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
women_friend <- lm(subjectpool$group_friend_7 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
women_inlaw <- lm(subjectpool$group_marry_7 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
women_traits <- lm(subjectpool$women_trait_scale ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

women_list <- list(women_ft, women_friend, women_inlaw, women_traits)

dv_labels_women <- c("group_ft_7" = "Feeling Thermometer",
                   "women_trait_scale" = "Trait Scale",
                   "group_friend_7" = "Friend",
                   "group_marry_7" = "In-Law")

stargazer(women_list, type = 'html', out = "women.html", dep.var.labels = dv_labels_women, covariate.labels = covariate_labels,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

# 8. LGBTQ+ #

lgbtq_ft <- lm(subjectpool$group_ft_8 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
lgbtq_friend <- lm(subjectpool$group_friend_8 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
lgbtq_inlaw <- lm(subjectpool$group_marry_8 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
lgbtq_traits <- lm(subjectpool$lgbtq_trait_scale ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

lgbtq_list <- list(lgbtq_ft, lgbtq_friend, lgbtq_inlaw, lgbtq_traits)

dv_labels_lgbtq <- c("group_ft_8" = "Feeling Thermometer",
                     "lgbtq_trait_scale" = "Trait Scale",
                     "group_friend_8" = "Friend",
                     "group_marry_8" = "In-Law")

stargazer(lgbtq_list, type = 'html', out = "lgbtq.html", dep.var.labels = dv_labels_lgbtq, covariate.labels = covariate_labels,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

# 9. Americans #

americans_ft <- lm(subjectpool$group_ft_9 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
americans_friend <- lm(subjectpool$group_friend_9 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
americans_inlaw <- lm(subjectpool$group_marry_9 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
americans_traits <- lm(subjectpool$americans_trait_scale ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

americans_list <- list(americans_ft, americans_friend, americans_inlaw, americans_traits)

dv_labels_americans <- c("group_ft_9" = "Feeling Thermometer",
                     "americans_trait_scale" = "Trait Scale",
                     "group_friend_9" = "Friend",
                     "group_marry_9" = "In-Law")

stargazer(americans_list, type = 'html', out = "americans.html", dep.var.labels = dv_labels_americans, covariate.labels = covariate_labels,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

# 10. Immigrants #

immigrants_ft <- lm(subjectpool$group_ft_10 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
immigrants_friend <- lm(subjectpool$group_friend_10 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
immigrants_inlaw <- lm(subjectpool$group_marry_10 ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)
immigrants_traits <- lm(subjectpool$immigrants_trait_scale ~ subjectpool$outpartisan + subjectpool$positive + subjectpool$both)

immigrants_list <- list(immigrants_ft, immigrants_friend, immigrants_inlaw, immigrants_traits)

dv_labels_immigrants <- c("group_ft_10" = "Feeling Thermometer",
                     "immigrants_trait_scale" = "Trait Scale",
                     "group_friend_10" = "Friend",
                     "group_marry_10" = "In-Law")

stargazer(immigrants_list, type = 'html', out = "immigrants.html", dep.var.labels = dv_labels_immigrants, covariate.labels = covariate_labels,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

### DEM vs REP ####
library(dplyr)
dem <- subjectpool %>%
  filter(partisanship == "Democrat")

dem_voter1 <- lm(dem$affective_polarization_voters ~ dem$outpartisan + dem$positive + dem$both)
dem_voter2 <- lm(dem$affpol_traits_voters ~ dem$outpartisan + dem$positive + dem$both)
dem_voter3 <- lm(dem$affpol_friend_voters ~ dem$outpartisan + dem$positive + dem$both)
dem_voter4 <- lm(dem$affpol_marry_voters ~ dem$outpartisan + dem$positive + dem$both)
dem_voter_list <- list(dem_voter1, dem_voter2, dem_voter3, dem_voter4)

dem_politician1 <- lm(dem$affective_polarization_politicians ~ dem$outpartisan + dem$positive + dem$both)
dem_politician2 <- lm(dem$affpol_traits_politicians ~ dem$outpartisan + dem$positive + dem$both)
dem_politician3 <- lm(dem$affpol_friend_politicians ~ dem$outpartisan + dem$positive + dem$both)
dem_politician4 <- lm(dem$affpol_marry_politicians ~ dem$outpartisan + dem$positive + dem$both)
dem_politician_list <- list(dem_politician1, dem_politician2, dem_politician3, dem_politician4)

dem_voter_table <- stargazer(dem_voter_list, type = 'html', out = "dem_voter_table.html", dep.var.labels = outcome_labels_voter, covariate.labels = covariate_labels,
                         star.char = c("*", "**", "***"),
                         star.cutoffs = c(.05, .01, .001))

dem_politician_table <- stargazer(dem_politician_list, type = 'html', out = "dem_politician_table.html", dep.var.labels = outcome_labels_politician, covariate.labels = covariate_labels,
                              star.char = c("*", "**", "***"),
                              star.cutoffs = c(.05, .01, .001))

rep <- subjectpool %>%
  filter(partisanship == "Republican")

rep_voter1 <- lm(rep$affective_polarization_voters ~ rep$outpartisan + rep$positive + rep$both)
rep_voter2 <- lm(rep$affpol_traits_voters ~ rep$outpartisan + rep$positive + rep$both)
rep_voter3 <- lm(rep$affpol_friend_voters ~ rep$outpartisan + rep$positive + rep$both)
rep_voter4 <- lm(rep$affpol_marry_voters ~ rep$outpartisan + rep$positive + rep$both)
rep_voter_list <- list(rep_voter1, rep_voter2, rep_voter3, rep_voter4)

rep_politician1 <- lm(rep$affective_polarization_politicians ~ rep$outpartisan + rep$positive + rep$both)
rep_politician2 <- lm(rep$affpol_traits_politicians ~ rep$outpartisan + rep$positive + rep$both)
rep_politician3 <- lm(rep$affpol_friend_politicians ~ rep$outpartisan + rep$positive + rep$both)
rep_politician4 <- lm(rep$affpol_marry_politicians ~ rep$outpartisan + rep$positive + rep$both)
rep_politician_list <- list(rep_politician1, rep_politician2, rep_politician3, rep_politician4)

rep_voter_table <- stargazer(rep_voter_list, type = 'html', out = "rep_voter_table.html", dep.var.labels = outcome_labels_voter, covariate.labels = covariate_labels,
                             star.char = c("*", "**", "***"),
                             star.cutoffs = c(.05, .01, .001))

rep_politician_table <- stargazer(rep_politician_list, type = 'html', out = "rep_politician_table.html", dep.var.labels = outcome_labels_politician, covariate.labels = covariate_labels,
                                  star.char = c("*", "**", "***"),
                                  star.cutoffs = c(.05, .01, .001))


