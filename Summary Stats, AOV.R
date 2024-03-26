# Points vs Control 

points_vs_control_riskinfo = jointdata_new %>%
  filter(item_filename == "Control.png" | item_filename == "screencapture-BenefitsRisks Points (1).png") %>%
  filter(aoi_name == "IMportantSafetyInformation_Control" | aoi_name == "Importantsafetyinformation_BenefitsRisksPoints")

frq(points_vs_control_riskinfo$item_filename)
points_vs_control_riskinfo$points_treatment = ifelse(points_vs_control_riskinfo$item_filename == "Control.png", 0, 1)
frq(points_vs_control_riskinfo$points_treatment)

control = points_vs_control_riskinfo %>%
  filter(points_treatment == 0)
mean(control$aoi_fixation_total_count)
sd(control$aoi_fixation_total_count)
mean(control$aoi_fixation_average_total_time_spent_secs)
sd(control$aoi_fixation_average_total_time_spent_secs)
mean(control$TotalRecall_Risks)
sd(control$TotalRecall_Risks)
mean(control$risk_sensitivity)
sd(control$risk_sensitivity)
mean(control$RiskPer_Combined)
sd(control$RiskPer_Combined)
mean(control$easeofuse)
sd(control$easeofuse)
mean(control$sitecred_trust)
sd(control$sitecred_trust)
mean(control$positive_affective_arousal)
sd(control$positive_affective_arousal)
mean(control$Brand_Trust_Attitude)
sd(control$Brand_Trust_Attitude)

points = points_vs_control_riskinfo %>%
  filter(points_treatment == 1)
mean(points$aoi_fixation_total_count)
sd(points$aoi_fixation_total_count)
mean(points$aoi_fixation_average_total_time_spent_secs)
sd(points$aoi_fixation_average_total_time_spent_secs)
mean(points$TotalRecall_Risks)
sd(points$TotalRecall_Risks)
mean(points$risk_sensitivity)
sd(points$risk_sensitivity)
mean(points$RiskPer_Combined)
sd(points$RiskPer_Combined)
mean(points$easeofuse)
sd(points$easeofuse)
mean(points$sitecred_trust)
sd(points$sitecred_trust)
mean(points$positive_affective_arousal)
sd(points$positive_affective_arousal)
mean(points$Brand_Trust_Attitude)
sd(points$Brand_Trust_Attitude)

## ANOVAS

summary(aov(aoi_fixation_total_count ~ points_treatment, data = points_vs_control_riskinfo))
summary(aov(aoi_fixation_total_count ~ points_treatment + age + female + previous_diagnosis, data = points_vs_control_riskinfo))

summary(aov(aoi_fixation_average_total_time_spent_secs ~ points_treatment, data = points_vs_control_riskinfo))
summary(aov(aoi_fixation_average_total_time_spent_secs ~ points_treatment + age + female + previous_diagnosis, data = points_vs_control_riskinfo))

summary(aov(TotalRecall_Risks ~ points_treatment, data = points_vs_control_riskinfo))
summary(aov(TotalRecall_Risks ~ points_treatment + age + female + previous_diagnosis, data = points_vs_control_riskinfo))

summary(aov(risk_sensitivity ~ points_treatment, data = points_vs_control_riskinfo))
summary(aov(risk_sensitivity ~ points_treatment + age + female + previous_diagnosis, data = points_vs_control_riskinfo))

summary(aov(RiskPer_Combined ~ points_treatment, data = points_vs_control_riskinfo))

summary(aov(easeofuse ~ points_treatment, data = points_vs_control_riskinfo))

summary(aov(sitecred_trust ~ points_treatment, data = points_vs_control_riskinfo))

summary(aov(positive_affective_arousal ~ points_treatment, data = points_vs_control_riskinfo))

summary(aov(Brand_Trust_Attitude ~ points_treatment, data = points_vs_control_riskinfo))

summary(aov(riskperceptions ~ points_treatment + age + female + previous_diagnosis, data = points_vs_control_riskinfo))

summary(aov(riskperceptions2 ~ points_treatment, data = points_vs_control_riskinfo))
summary(aov(riskperceptions2 ~ points_treatment + age + female + previous_diagnosis, data = points_vs_control_riskinfo))

summary(aov(risks_outweigh_benefits ~ points_treatment, data = points_vs_control_riskinfo))
summary(aov(risks_outweigh_benefits ~ points_treatment + age + female + previous_diagnosis, data = points_vs_control_riskinfo))


# Box vs Control

box_vs_control_riskinfo = jointdata_new %>%
  filter(item_filename == "Control.png" | item_filename == "static-box treatment2.png") %>%
  filter(aoi_name == "IMportantSafetyInformation_Control" | aoi_name == "Staticboxright_StaticBox")

frq(box_vs_control_riskinfo$item_filename)
box_vs_control_riskinfo$box_treatment = ifelse(box_vs_control_riskinfo$item_filename == "Control.png", 0, 1)
frq(box_vs_control_riskinfo$box_treatment)

points = box_vs_control_riskinfo %>%
  filter(box_treatment == 1)
mean(points$aoi_fixation_total_count)
sd(points$aoi_fixation_total_count)
mean(points$aoi_fixation_average_total_time_spent_secs)
sd(points$aoi_fixation_average_total_time_spent_secs)
mean(points$TotalRecall_Risks)
sd(points$TotalRecall_Risks)
mean(points$risk_sensitivity)
sd(points$risk_sensitivity)
mean(points$RiskPer_Combined)
sd(points$RiskPer_Combined)
mean(points$easeofuse)
sd(points$easeofuse)
mean(points$sitecred_trust)
sd(points$sitecred_trust)
mean(points$positive_affective_arousal)
sd(points$positive_affective_arousal)
mean(points$Brand_Trust_Attitude)
sd(points$Brand_Trust_Attitude)


## ANOVAS

summary(aov(aoi_fixation_total_count ~ box_treatment, data = box_vs_control_riskinfo))
summary(aov(aoi_fixation_total_count ~ box_treatment + age + female + previous_diagnosis, data = box_vs_control_riskinfo))

summary(aov(aoi_fixation_average_total_time_spent_secs ~ box_treatment, data = box_vs_control_riskinfo))
summary(aov(aoi_fixation_average_total_time_spent_secs ~ box_treatment + age + female + previous_diagnosis, data = box_vs_control_riskinfo))

summary(aov(TotalRecall_Risks ~ box_treatment, data = box_vs_control_riskinfo))
summary(aov(TotalRecall_Risks ~ box_treatment + age + female + previous_diagnosis, data = box_vs_control_riskinfo))

summary(aov(risk_sensitivity ~ box_treatment, data = box_vs_control_riskinfo))
summary(aov(risk_sensitivity ~ box_treatment + age + female + previous_diagnosis, data = box_vs_control_riskinfo))

summary(aov(RiskPer_Combined ~ box_treatment, data = box_vs_control_riskinfo))

summary(aov(easeofuse ~ box_treatment, data = box_vs_control_riskinfo))

summary(aov(sitecred_trust ~ box_treatment, data = box_vs_control_riskinfo))

summary(aov(positive_affective_arousal ~ box_treatment, data = box_vs_control_riskinfo))

summary(aov(Brand_Trust_Attitude ~ box_treatment, data = box_vs_control_riskinfo))

# Table vs Control 

table_vs_control_riskinfo = jointdata_new %>%
  filter(item_filename == "Control.png" | item_filename == "Table treatment3.png") %>%
  filter(aoi_name == "IMportantSafetyInformation_Control" | aoi_name == "ImportantSafetyinformationside_Table")

frq(table_vs_control_riskinfo$item_filename)
table_vs_control_riskinfo$table_treatment = ifelse(table_vs_control_riskinfo$item_filename == "Control.png", 0, 1)
frq(table_vs_control_riskinfo$table_treatment)

points = table_vs_control_riskinfo %>%
  filter(table_treatment == 1)
mean(points$aoi_fixation_total_count)
sd(points$aoi_fixation_total_count)
mean(points$aoi_fixation_average_total_time_spent_secs)
sd(points$aoi_fixation_average_total_time_spent_secs)
mean(points$TotalRecall_Risks)
sd(points$TotalRecall_Risks)
mean(points$risk_sensitivity)
sd(points$risk_sensitivity)
mean(points$RiskPer_Combined)
sd(points$RiskPer_Combined)
mean(points$easeofuse)
sd(points$easeofuse)
mean(points$sitecred_trust)
sd(points$sitecred_trust)
mean(points$positive_affective_arousal)
sd(points$positive_affective_arousal)
mean(points$Brand_Trust_Attitude)
sd(points$Brand_Trust_Attitude)

## ANOVAS

summary(aov(aoi_fixation_total_count ~ table_treatment, data = table_vs_control_riskinfo))
summary(aov(aoi_fixation_total_count ~ table_treatment + age + female + previous_diagnosis, data = table_vs_control_riskinfo))

summary(aov(aoi_fixation_average_total_time_spent_secs ~ table_treatment, data = table_vs_control_riskinfo))
summary(aov(aoi_fixation_average_total_time_spent_secs ~ table_treatment + age + female + previous_diagnosis, data = table_vs_control_riskinfo))

summary(aov(TotalRecall_Risks ~ table_treatment, data = table_vs_control_riskinfo))
summary(aov(TotalRecall_Risks ~ table_treatment + age + female + previous_diagnosis, data = table_vs_control_riskinfo))


summary(aov(risk_sensitivity ~ table_treatment, data = table_vs_control_riskinfo))
summary(aov(risk_sensitivity ~ table_treatment + age + female + previous_diagnosis, data = table_vs_control_riskinfo))

summary(aov(RiskPer_Combined ~ table_treatment, data = table_vs_control_riskinfo))

summary(aov(easeofuse ~ table_treatment, data = table_vs_control_riskinfo))

summary(aov(sitecred_trust ~ table_treatment, data = table_vs_control_riskinfo))

summary(aov(positive_affective_arousal ~ table_treatment, data = table_vs_control_riskinfo))

summary(aov(Brand_Trust_Attitude ~ table_treatment, data = table_vs_control_riskinfo))

# Bubble vs Control 

bubble_vs_control_riskinfo = jointdata_new %>%
  filter(item_filename == "Control.png" | item_filename == "Bubble treatment1.png") %>%
  filter(aoi_name == "IMportantSafetyInformation_Control" | aoi_name == "BubbleRiskInfo")

frq(bubble_vs_control_riskinfo$item_filename)
bubble_vs_control_riskinfo$bubble_treatment = ifelse(bubble_vs_control_riskinfo$item_filename == "Control.png", 0, 1)
frq(bubble_vs_control_riskinfo$bubble_treatment)

points = bubble_vs_control_riskinfo %>%
  filter(bubble_treatment == 1)
points = points[-31,]
mean(points$aoi_fixation_total_count)
sd(points$aoi_fixation_total_count)
mean(points$aoi_fixation_average_total_time_spent_secs)
sd(points$aoi_fixation_average_total_time_spent_secs)
mean(points$TotalRecall_Risks)
sd(points$TotalRecall_Risks)
mean(points$risk_sensitivity)
sd(points$risk_sensitivity)
mean(points$RiskPer_Combined)
sd(points$RiskPer_Combined)
mean(points$easeofuse)
sd(points$easeofuse)
mean(points$sitecred_trust)
sd(points$sitecred_trust)
mean(points$positive_affective_arousal)
sd(points$positive_affective_arousal)
mean(points$Brand_Trust_Attitude)
sd(points$Brand_Trust_Attitude)

## ANOVAS

summary(aov(aoi_fixation_total_count ~ bubble_treatment, data = bubble_vs_control_riskinfo))
summary(aov(aoi_fixation_total_count ~ bubble_treatment + age + female + previous_diagnosis, data = bubble_vs_control_riskinfo))

summary(aov(aoi_fixation_average_total_time_spent_secs ~ bubble_treatment, data = bubble_vs_control_riskinfo))
summary(aov(aoi_fixation_average_total_time_spent_secs ~ bubble_treatment + age + female + previous_diagnosis, data = bubble_vs_control_riskinfo))

summary(aov(TotalRecall_Risks ~ bubble_treatment, data = bubble_vs_control_riskinfo))
summary(aov(TotalRecall_Risks ~ bubble_treatment + age + female + previous_diagnosis, data = bubble_vs_control_riskinfo))

summary(aov(risk_sensitivity ~ bubble_treatment, data = bubble_vs_control_riskinfo))
summary(aov(risk_sensitivity ~ bubble_treatment + age + female + previous_diagnosis, data = bubble_vs_control_riskinfo))

summary(aov(RiskPer_Combined ~ bubble_treatment, data = bubble_vs_control_riskinfo))

summary(aov(easeofuse ~ bubble_treatment, data = bubble_vs_control_riskinfo))

summary(aov(sitecred_trust ~ bubble_treatment, data = bubble_vs_control_riskinfo))

summary(aov(positive_affective_arousal ~ bubble_treatment, data = bubble_vs_control_riskinfo))

summary(aov(Brand_Trust_Attitude ~ bubble_treatment, data = bubble_vs_control_riskinfo))



