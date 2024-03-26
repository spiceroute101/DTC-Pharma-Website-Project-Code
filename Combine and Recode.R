library(strengejacke)
library(tidyverse)
library(haven)
library(dplyr)
library(sjmisc)
library(readxl)

### IMPORT DATASETS

poststudy_clean = as.data.frame(Post.Study.May.10_Edited_TotalRiskRecall)
poststudy = poststudy_clean[-c(1),]

prestudy_clean = as.data.frame(Pre_study_may_10)
prestudy = prestudy_clean[-c(1),]


controlfix = as.data.frame(control.aoi.fixation.stats)
pointsfix = as.data.frame(points.aoi.fixation.stats)
bubblefix = as.data.frame(bubble.aoi.fixation.stats)
boxfix = as.data.frame(box.aoi.fixation.stats)
tablefix = as.data.frame(table.aoi.fixation.stats)

### COMBINE AOI DATASETS

## Combine fixation stats datatsets

combined = rbind(controlfix, pointsfix)
combined = rbind(combined, bubblefix)
combined = rbind(combined, boxfix)
combined = rbind(combined, tablefix)



## Differences between control and treatment over total fixation count

frq(combined$item_filename)
frq(combined$aoi_name)
frq(poststudy$OpenRecall_benefits)

## Combining questionnaires 

poststudy$tester_id = poststudy$realeye.tester.id
jointdata1 = merge(combined, poststudy, by.x = 'participant_id', by.y = 'tester_id')

frq(poststudy$realeye.tester.id)

prestudy$RID = prestudy$QSEDResponseID
jointdata1 = merge(jointdata1, prestudy, by = 'RID')

frq(jointdata1$participant_quality_grade)

jointdata1 = jointdata1 %>%
  filter(participant_quality_grade != 1) %>%
  filter(participant_quality_grade != 2)


jointdata_mm = jointdata1

#### Open Recall  

frq(jointdata_mm$item_filename)

frq(jointdata_mm$QID11_16)
jointdata_mm$benrec1 = ifelse(jointdata_mm$QID11_1.x == "Mentioned", 1, 0)
jointdata_mm$benrec2 = ifelse(jointdata_mm$QID11_2 == "Mentioned", 1, 0)
jointdata_mm$riskrec1 = ifelse(jointdata_mm$QID11_3 == "Mentioned", 1, 0)
jointdata_mm$riskrec2 = ifelse(jointdata_mm$QID11_4 == "Mentioned", 1, 0)
jointdata_mm$riskrec3 = ifelse(jointdata_mm$QID11_5 == "Mentioned", 1, 0)
jointdata_mm$benrec3 = ifelse(jointdata_mm$QID11_6 == "Mentioned", 1, 0)
jointdata_mm$benrec4 = ifelse(jointdata_mm$QID11_7 == "Mentioned", 1, 0)
jointdata_mm$benrec5 = ifelse(jointdata_mm$QID11_10 == "Mentioned", 1, 0)
jointdata_mm$benrec6 = ifelse(jointdata_mm$QID11_11 == "Mentioned", 1, 0)
jointdata_mm$benrec7 = ifelse(jointdata_mm$QID11_12 == "Mentioned", 1, 0)


jointdata_mm$benrec8 = ifelse(jointdata_mm$QID11_13 == "Not Mentioned", 1, 0)
jointdata_mm$benrec9 = ifelse(jointdata_mm$QID11_14 == "Not Mentioned", 1, 0)
jointdata_mm$benrec10 = ifelse(jointdata_mm$QID11_15 == "Not Mentioned", 1, 0)
jointdata_mm$riskrec4 = ifelse(jointdata_mm$QID11_16 == "Not Mentioned", 1, 0)
jointdata_mm$riskrec5 = ifelse(jointdata_mm$QID11_17 == "Not Mentioned", 1, 0)
jointdata_mm$riskrec6 = ifelse(jointdata_mm$QID11_18 == "Not Mentioned", 1, 0)

jointdata_mm$riskfoil1 = ifelse(jointdata_mm$QID11_3 == "Not Mentioned", 1, 0)
jointdata_mm$riskfoil2 = ifelse(jointdata_mm$QID11_4 == "Not Mentioned", 1, 0)
jointdata_mm$riskfoil3 = ifelse(jointdata_mm$QID11_5 == "Not Mentioned", 1, 0)
jointdata_mm$riskfoil4 = ifelse(jointdata_mm$QID11_16 == "Mentioned", 1, 0)
jointdata_mm$riskfoil5 = ifelse(jointdata_mm$QID11_17 == "Mentioned", 1, 0)
jointdata_mm$riskfoil6 = ifelse(jointdata_mm$QID11_18 == "Mentioned", 1, 0)

jointdata_mm$riskfoiltotal = jointdata_mm$riskfoil1 + jointdata_mm$riskfoil2 + jointdata_mm$riskfoil3 + jointdata_mm$riskfoil4 + jointdata_mm$riskfoil5 + jointdata_mm$riskfoil6
jointdata_mm$trueriskrec = jointdata_mm$riskrec1 + jointdata_mm$riskrec2 + jointdata_mm$riskrec3 + jointdata_mm$riskrec4 + jointdata_mm$riskrec5 + jointdata_mm$riskrec6
jointdata_mm$risk_sensitivity = jointdata_mm$riskfoiltotal - jointdata_mm$trueriskrec

frq(jointdata_mm$risk_sensitivity)

jointdata_mm$truebenrec = jointdata_mm$benrec1 + jointdata_mm$benrec2 + jointdata_mm$benrec3 + 
  jointdata_mm$benrec4 + jointdata_mm$benrec5 + jointdata_mm$benrec6 + jointdata_mm$benrec7 + 
  jointdata_mm$benrec8 + jointdata_mm$benrec9 + jointdata_mm$benrec10

jointdata_mm$trueriskrec_weighted = plyr::mapvalues(jointdata_mm$trueriskrec, c(0:6), c(0,3.333,6.667,10,13.333,16.667, 20))
jointdata_mm$truebenrec_weighted = plyr::mapvalues(jointdata_mm$truebenrec, c(0:8), c(0,2.5,5,7.5,10,12.5,15,17.5,20))

frq(jointdata_mm$truebenrec_weighted)

frq(jointdata_mm$cuedbenefit_weighted)
jointdata_mm$cuedrisk_weighted = plyr::mapvalues(jointdata_mm$CuedRecall_risks, c(0:8), c(0,2.5,5,7.5,10,12.5,15,17.5,20))
jointdata_mm$cuedbenefit_weighted = plyr::mapvalues(jointdata_mm$CuedRecall_Benefits, c(0:4), c(0,5,10,15,20))

frq(jointdata_mm$OpenRecall_Risks)
jointdata_mm$openbenefit_weighted = plyr::mapvalues(jointdata_mm$OpenRecall_benefits, c(0:4), c(0,5,10,15,20))
jointdata_mm$openrisk_weighted = plyr::mapvalues(jointdata_mm$OpenRecall_Risks, c(0:6), c(0,3.333,6.667,10,13.333,16.667, 20))

jointdata_mm$rec_risk_minus_ben = jointdata_mm$trueriskrec_weighted - jointdata_mm$truebenrec_weighted
jointdata_mm$cued_difference = jointdata_mm$cuedrisk_weighted - jointdata_mm$cuedbenefit_weighted
jointdata_mm$open_difference = jointdata_mm$openrisk_weighted - jointdata_mm$openbenefit_weighted

#Total risk recall

frq(jointdata_mm$TotalRecall_Risks)

## Safety perceptions
class(jointdata_mm$QID2_1)

jointdata_mm$riskperceptions = (as.numeric(jointdata_mm$QID1_1.x) + as.numeric(jointdata_mm$QID1_2) + as.numeric(jointdata_mm$QID1_3) +
                                  as.numeric(jointdata_mm$QID1_4) + as.numeric(jointdata_mm$QID1_5) + as.numeric(jointdata_mm$QID1_6) + as.numeric(jointdata_mm$QID1_7))/7 

jointdata_mm$rpp1 = as.numeric(jointdata_mm$QID1_1.x)
jointdata_mm$rpp1 = plyr::mapvalues(jointdata_mm$rpp1, c(1:7), c(7:1))

jointdata_mm$rpp2 = as.numeric(jointdata_mm$QID1_2)
jointdata_mm$rpp2 = plyr::mapvalues(jointdata_mm$rpp2, c(1:7), c(7:1))

jointdata_mm$rpp3 = as.numeric(jointdata_mm$QID1_3)
jointdata_mm$rpp3 = plyr::mapvalues(jointdata_mm$rpp3, c(1:7), c(7:1))

jointdata_mm$rpp4 = as.numeric(jointdata_mm$QID1_4)
jointdata_mm$rpp4 = plyr::mapvalues(jointdata_mm$rpp4, c(1:7), c(7:1))

jointdata_mm$rpp5 = as.numeric(jointdata_mm$QID1_5)
jointdata_mm$rpp5 = plyr::mapvalues(jointdata_mm$rpp5, c(1:7), c(7:1))

jointdata_mm$rpp6 = as.numeric(jointdata_mm$QID1_6)
jointdata_mm$rpp6 = plyr::mapvalues(jointdata_mm$rpp6, c(1:7), c(7:1))

jointdata_mm$rpp7 = as.numeric(jointdata_mm$QID1_7)
jointdata_mm$rpp7 = plyr::mapvalues(jointdata_mm$rpp7, c(1:7), c(7:1))

psych::alpha(tibble(as.numeric(jointdata_mm$QID1_1.x), as.numeric(jointdata_mm$QID1_2), as.numeric(jointdata_mm$QID1_3),
                    as.numeric(jointdata_mm$QID1_4), as.numeric(jointdata_mm$QID1_5), as.numeric(jointdata_mm$QID1_6), as.numeric(jointdata_mm$QID1_7)))

jointdata_mm$RiskPerceptions = (jointdata_mm$rpp1 + jointdata_mm$rpp2 + jointdata_mm$rpp3 + jointdata_mm$rpp4 + jointdata_mm$rpp5 + jointdata_mm$rpp6 + jointdata_mm$rpp7)/7

frq(jointdata_mm$QID2_2)

frq(jointdata_mm$RiskPerceptions)

jointdata_mm$rboutweigh1 = plyr::mapvalues(jointdata_mm$QID2_1, c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neither Agree Nor Disagree", "Somewhat Agree", "Agree", "Strongly Agree"), c(7:1)) %>% remove_all_labels()
frq(jointdata_mm$rboutweigh1)

jointdata_mm$rboutweigh2 = plyr::mapvalues(jointdata_mm$QID2_2, c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neither Agree Nor Disagree", "Somewhat Agree", "Agree", "Strongly Agree"), c(7:1)) %>% remove_all_labels()
frq(jointdata_mm$rboutweigh2)

jointdata_mm$risks_outweigh_benefits = (as.numeric(jointdata_mm$rboutweigh1) + as.numeric(jointdata_mm$rboutweigh2))/2
cor1 = cor.test(as.numeric(jointdata_mm$rboutweigh1),as.numeric(jointdata_mm$rboutweigh2), method = 'pearson')
cor1

frq(jointdata_mm$QID3_2)
jointdata_mm$rp1 = plyr::mapvalues(jointdata_mm$QID3_2, c(1:7), c(7:1))  %>% remove_all_labels()
frq(jointdata_mm$rp1)

mean(as.numeric(jointdata_mm$rp1))
mean(as.numeric(jointdata_mm$QID3_1))

jointdata_mm$riskperceptions2 = (as.numeric(jointdata_mm$rp1) + as.numeric(jointdata_mm$QID3_1))/2
frq(jointdata_mm$riskperceptions2)
cor.test(as.numeric(jointdata_mm$rp1),as.numeric(jointdata_mm$QID3_1), method = 'pearson')

psych::alpha(tibble(jointdata_mm$RiskPerceptions, jointdata_mm$riskperceptions2, jointdata_mm$risks_outweigh_benefits))

jointdata_mm$RiskPer_Combined = (jointdata_mm$RiskPerceptions + jointdata_mm$riskperceptions2 + jointdata_mm$risks_outweigh_benefits)/3

## Ease of use perceptions

frq(jointdata_mm$QID21_3)
jointdata_mm$ease1 = plyr::mapvalues(jointdata_mm$QID21_1.x, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7)) %>% remove_all_labels()
jointdata_mm$ease2 = plyr::mapvalues(jointdata_mm$QID21_2, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7)) %>% remove_all_labels()
jointdata_mm$ease3 = plyr::mapvalues(jointdata_mm$QID21_3, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7)) %>% remove_all_labels()
jointdata_mm$ease4 = plyr::mapvalues(jointdata_mm$QID21_4, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7)) %>% remove_all_labels()
jointdata_mm$ease5 = plyr::mapvalues(jointdata_mm$QID21_5, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7)) %>% remove_all_labels()

jointdata_mm$easeofuse = (as.numeric(jointdata_mm$ease4) + as.numeric(jointdata_mm$ease5))/2
cor.test(as.numeric(jointdata_mm$ease4), as.numeric(jointdata_mm$ease5), method = 'pearson')


## Site Credibility 

frq(jointdata_mm$QID27_3)
jointdata_mm$sitecred = (as.numeric(jointdata_mm$QID27_1) + as.numeric(jointdata_mm$QID27_2) + as.numeric(jointdata_mm$QID27_3))/3
psych::alpha(tibble(as.numeric(jointdata_mm$QID27_1), as.numeric(jointdata_mm$QID27_2), as.numeric(jointdata_mm$QID27_3)))
frq(jointdata_mm$sitecred)
jointdata_mm$sitetrust1 = plyr::mapvalues(jointdata_mm$QID21_1.x, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7)) %>% remove_all_labels()
jointdata_mm$sitetrust2 = plyr::mapvalues(jointdata_mm$QID21_2, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7)) %>% remove_all_labels()
jointdata_mm$sitetrust = (as.numeric(jointdata_mm$sitetrust2) + as.numeric(jointdata_mm$sitetrust1))/2
cor.test(as.numeric(jointdata_mm$sitetrust2), as.numeric(jointdata_mm$sitetrust1), method = 'pearson')
cor.test(jointdata_mm$sitetrust, jointdata_mm$sitecred, method = 'pearson')
jointdata_mm$sitecred_trust = (jointdata_mm$sitetrust + jointdata_mm$sitecred)/2

## brand Trust 

jointdata_mm$brandtrust1 = as.numeric(plyr::mapvalues(jointdata_mm$QID5_1, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7))) %>% remove_all_labels()
jointdata_mm$brandtrust2 = as.numeric(plyr::mapvalues(jointdata_mm$QID5_2, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7))) %>% remove_all_labels()
jointdata_mm$brandtrust3 = as.numeric(plyr::mapvalues(jointdata_mm$QID5_3, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7))) %>% remove_all_labels()
jointdata_mm$brandtrust4 = as.numeric(plyr::mapvalues(jointdata_mm$QID5_4, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7))) %>% remove_all_labels()
jointdata_mm$brandtrust5 = as.numeric(plyr::mapvalues(jointdata_mm$QID5_5, c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), c(1:7))) %>% remove_all_labels()

jointdata_mm$brandtrust = (jointdata_mm$brandtrust1 + jointdata_mm$brandtrust2 + jointdata_mm$brandtrust3 +
                             jointdata_mm$brandtrust4 + jointdata_mm$brandtrust5)/5

psych::alpha(tibble(jointdata_mm$brandtrust1, jointdata_mm$brandtrust2, jointdata_mm$brandtrust3, 
                    jointdata_mm$brandtrust4, jointdata_mm$brandtrust5))

## brand attitude

frq(jointdata_mm$QID6_3)
jointdata_mm$brandatt1 = as.numeric(jointdata_mm$QID6_1.x)
jointdata_mm$brandatt2 = as.numeric(jointdata_mm$QID6_2)
jointdata_mm$brandatt3 = as.numeric(jointdata_mm$QID6_3)
frq(jointdata_mm$brandatt3)

psych::alpha(tibble(jointdata_mm$brandatt1, jointdata_mm$brandatt2, jointdata_mm$brandatt3))
jointdata_mm$Brand_Attitude = (jointdata_mm$brandatt1 + jointdata_mm$brandatt2 + jointdata_mm$brandatt3)/3

cor.test(jointdata_mm$Brand_Attitude, jointdata_mm$brandtrust, method = 'pearson')
jointdata_mm$Brand_Trust_Attitude = (jointdata_mm$Brand_Attitude + jointdata_mm$brandtrust)/2

## Emotional Arousal

frq(jointdata_mm$QID30_1)
jointdata_mm$emo1 = as.numeric(plyr::mapvalues(jointdata_mm$QID30_1, c(1:7), c(7:1)))
frq(jointdata_mm$emo1)
jointdata_mm$emo2 = as.numeric(jointdata_mm$QID30_2) 
jointdata_mm$emo3 = as.numeric(jointdata_mm$QID30_3) 
jointdata_mm$emo4 = as.numeric(plyr::mapvalues(jointdata_mm$QID30_4, c(1:7), c(7:1)))
jointdata_mm$emo5 = as.numeric(plyr::mapvalues(jointdata_mm$QID30_5, c(1:7), c(7:1)))
jointdata_mm$emo6 = as.numeric(jointdata_mm$QID30_6)
jointdata_mm$emo7 = as.numeric(plyr::mapvalues(jointdata_mm$QID30_7, c(1:7), c(7:1)))
jointdata_mm$emo8 = as.numeric(jointdata_mm$QID30_8)
jointdata_mm$emo9 = as.numeric(jointdata_mm$QID30_9)
jointdata_mm$emo10 = as.numeric(jointdata_mm$QID30_10)
jointdata_mm$emo11 = as.numeric(plyr::mapvalues(jointdata_mm$QID30_11, c(1:7), c(7:1)))

psych::alpha(tibble(jointdata_mm$emo1, jointdata_mm$emo2, jointdata_mm$emo3, jointdata_mm$emo4, jointdata_mm$emo5, jointdata_mm$emo6, 
                    jointdata_mm$emo7, jointdata_mm$emo8, jointdata_mm$emo9, jointdata_mm$emo10, jointdata_mm$emo11))

jointdata_mm$positive_affective_arousal = (jointdata_mm$emo1 + jointdata_mm$emo2 + jointdata_mm$emo3 + jointdata_mm$emo4 + jointdata_mm$emo5 + jointdata_mm$emo6 + 
                                             jointdata_mm$emo7 + jointdata_mm$emo8 + jointdata_mm$emo9 + jointdata_mm$emo10 + jointdata_mm$emo11)/11

frq(jointdata_mm$previous_diagnosis)

jointdata_mm$aoi_fixation_average_total_time_spent_secs = jointdata_mm$aoi_fixation_average_total_time_spent_ms/1000

#Gender
frq(jointdata_mm$QID32)
jointdata_mm = jointdata_mm %>%
  filter(QID32 == "Female" | QID32 == "Male")
jointdata_mm$female = as.numeric(ifelse(jointdata_mm$QID32 == "Female", 1, 0))
#jointdata_mm$gender = jointdata_mm$QID32
frq(jointdata_mm$female)

#Age
frq(jointdata_mm$QID34_1)
jointdata_mm$age = as.numeric(jointdata_mm$QID34_1)

#Previous diagnosis
frq(jointdata_mm$QID63)

jointdata_mm$previous_diagnosis = as.numeric(ifelse(jointdata_mm$QID63 == "Yes", 1, 0))
frq(jointdata_mm$previous_diagnosis)

#Ever taken prescription medication for allergies - 2467 no responses 
frq(jointdata_mm$QID64)
jointdata_mm$taken_prescriptionmeds = as.numeric(ifelse(jointdata_mm$QID64 == "Yes", 1, 0))

#Currently taking prescription medication for allergies 
frq(jointdata_mm$QID65)
jointdata_mm$currently_prescriptionmeds = as.numeric(ifelse(jointdata_mm$QID65 == "Yes", 1, 0))

#Optimism Bias

frq(jointdata_mm$QID23)
jointdata_mm$QID23 = plyr::mapvalues(jointdata_mm$QID23, from = c("2", "3", "5", "6", "About the same likelihood\n4", "Far less likely\n1", "Far more likely\n7"), c("6", "5", "3", "2", "4", "7", "1" )) %>% remove_all_labels()
jointdata_mm$ob1 = as.numeric(jointdata_mm$QID23)

frq(jointdata_mm$QID44)
jointdata_mm$ob2 = as.numeric(plyr::mapvalues(jointdata_mm$QID44, from = c("2", "3", "5", "6", "About the same likelihood\n4", "Far less likely\n1", "Far more likely\n7"), c("6", "5", "3", "2", "4", "7", "1" )) %>% remove_all_labels())
frq(jointdata_mm$ob2)

jointdata_mm$optimism_bias = (jointdata_mm$ob1 + jointdata_mm$ob2)/2

# Prevenntion & Promotion

jointdata_mm$prevention = (as.numeric(jointdata_mm$QID1_1.y) + as.numeric(jointdata_mm$QID6_1.y) + as.numeric(jointdata_mm$QID8_1) + as.numeric(jointdata_mm$QID11_1.y)
                           + as.numeric(jointdata_mm$QID13_1) + as.numeric(jointdata_mm$QID14_1) + as.numeric(jointdata_mm$QID15_1) + as.numeric(jointdata_mm$QID17_1)
                           + as.numeric(jointdata_mm$QID19_1))/9

frq(jointdata_mm$prevention)

jointdata_mm$promotion = (as.numeric(jointdata_mm$QID7_1) + as.numeric(jointdata_mm$QID9_1) + as.numeric(jointdata_mm$QID10_1) + as.numeric(jointdata_mm$QID12_1)
                          + as.numeric(jointdata_mm$QID16_1) + as.numeric(jointdata_mm$QID18_1) + as.numeric(jointdata_mm$QID20_1) + as.numeric(jointdata_mm$QID21_1.y)
                          + as.numeric(jointdata_mm$QID22_1))/9

# PRISM

frq(jointdata_mm$QID36_1)
jointdata_mm$benefits_knowledge = as.numeric(jointdata_mm$QID35_1)

jointdata_mm$risks_knowledge = as.numeric(jointdata_mm$QID36_1)

jointdata_mm$percieved_condition_threat = as.numeric(jointdata_mm$QID37_1)

jointdata_mm$percieved_condition_threat = as.numeric(jointdata_mm$QID37_1)

jointdata_mm$percieved_risk_threat = as.numeric(jointdata_mm$QID38_1)

jointdata_mm$condition_worry = as.numeric(jointdata_mm$QID39_1)

jointdata_mm$risk_worry = as.numeric(jointdata_mm$QID39_2)

jointdata_mm$riskinfo_seeking_norm = (as.numeric(jointdata_mm$QID41_5) + as.numeric(jointdata_mm$QID41_6) + as.numeric(jointdata_mm$QID41_7) + as.numeric(jointdata_mm$QID41_8))/4

jointdata_mm$benefitsinfo_seeking_norm = (as.numeric(jointdata_mm$QID41_1) + as.numeric(jointdata_mm$QID41_2) + as.numeric(jointdata_mm$QID41_3) + as.numeric(jointdata_mm$QID41_4))/4

jointdata_mm$benefitsinfo_seeking_attitude = (as.numeric(jointdata_mm$QID42_1) + as.numeric(jointdata_mm$QID42_2) + as.numeric(jointdata_mm$QID42_3) + as.numeric(jointdata_mm$QID42_4)
                                              + as.numeric(jointdata_mm$QID42_5) + as.numeric(jointdata_mm$QID42_6) + as.numeric(jointdata_mm$QID42_7))/7

jointdata_mm$risksinfo_seeking_attitude = (as.numeric(jointdata_mm$QID43_1) + as.numeric(jointdata_mm$QID43_2) + as.numeric(jointdata_mm$QID43_3) + as.numeric(jointdata_mm$QID43_4)
                                           + as.numeric(jointdata_mm$QID43_5) + as.numeric(jointdata_mm$QID43_6) + as.numeric(jointdata_mm$QID43_7))/7

jointdata_pd = jointdata_mm %>%
  filter(previous_diagnosis ==1)

## Normalizing 

myvars = c("item_filename" , "aoi_name" , "age", "female", "previous_diagnosis",  "aoi_fixation_total_count", "aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks", "risk_sensitivity", "RiskPer_Combined",
           "easeofuse", "sitecred_trust", "positive_affective_arousal", "Brand_Attitude", "brandtrust", "riskperceptions", "riskperceptions2", "risks_outweigh_benefits")

myvars2 = c("aoi_fixation_total_count", "aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks", "risk_sensitivity", "RiskPer_Combined",
            "easeofuse", "sitecred_trust", "positive_affective_arousal", "Brand_Attitude", "brandtrust", "riskperceptions", "riskperceptions2", "risks_outweigh_benefits")

jointdata_norm = jointdata_mm[myvars]
jointdata_norm2 = jointdata_mm[myvars2]

jointdata_norm2$aoi_fixation_total_count == 0

jointdata_norm2$aoi_fixation_average_total_time_spent_secs[jointdata_norm2$aoi_fixation_total_count == 0] = 0


minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

norm_jointdata = as.data.frame(lapply(jointdata_norm2,minMax))

jointdata_new = jointdata_norm
jointdata_new[,colnames(jointdata_new) %in% colnames(norm_jointdata)] = norm_jointdata


