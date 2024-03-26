# Box vs Control

box_vs_control_riskinfo = jointdata_new %>%
  filter(item_filename == "Control.png" | item_filename == "static-box treatment2.png") %>%
  filter(aoi_name == "IMportantSafetyInformation_Control" | aoi_name == "Staticboxright_StaticBox")

frq(box_vs_control_riskinfo$item_filename)
box_vs_control_riskinfo$box_treatment = ifelse(box_vs_control_riskinfo$item_filename == "Control.png", 0, 1)
frq(box_vs_control_riskinfo$box_treatment)

process(data = box_vs_control_riskinfo, y = "TotalRecall_Risks", x = "box_treatment",
        m = "aoi_fixation_total_count", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "TotalRecall_Risks", x = "box_treatment",
        m = "aoi_fixation_average_total_time_spent_secs", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions", x = "box_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions", x = "box_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions2", x = "box_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)


process(data = box_vs_control_riskinfo, y = "riskperceptions2", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions2", x = "box_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions2", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "box_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "box_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "RiskPer_Combined", x = "box_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "RiskPer_Combined", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "risk_sensitivity", x = "box_treatment",
        m = "aoi_fixation_total_count", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "risk_sensitivity", x = "box_treatment",
        m = "aoi_fixation_average_total_time_spent_secs", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions", x = "box_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions2", x = "box_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "riskperceptions2", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "box_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "RiskPer_Combined", x = "box_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = box_vs_control_riskinfo, y = "RiskPer_Combined", x = "box_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

## Table vs control

table_vs_control_riskinfo = jointdata_new %>%
  filter(item_filename == "Control.png" | item_filename == "Table treatment3.png") %>%
  filter(aoi_name == "IMportantSafetyInformation_Control" | aoi_name == "ImportantSafetyinformationside_Table")

frq(table_vs_control_riskinfo$item_filename)
table_vs_control_riskinfo$table_treatment = ifelse(table_vs_control_riskinfo$item_filename == "Control.png", 0, 1)
frq(table_vs_control_riskinfo$table_treatment)

process(data = table_vs_control_riskinfo, y = "TotalRecall_Risks", x = "table_treatment",
        m = "aoi_fixation_total_count", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "TotalRecall_Risks", x = "table_treatment",
        m = "aoi_fixation_average_total_time_spent_secs", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions", x = "table_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions", x = "table_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions2", x = "table_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)


process(data = table_vs_control_riskinfo, y = "riskperceptions2", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions2", x = "table_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions2", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "table_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "table_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "RiskPer_Combined", x = "table_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "RiskPer_Combined", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "risk_sensitivity", x = "table_treatment",
        m = "aoi_fixation_total_count", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "risk_sensitivity", x = "table_treatment",
        m = "aoi_fixation_average_total_time_spent_secs", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions", x = "table_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions2", x = "table_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "riskperceptions2", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "table_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "RiskPer_Combined", x = "table_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = table_vs_control_riskinfo, y = "RiskPer_Combined", x = "table_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

## Bubble vs Control

bubble_vs_control_riskinfo = jointdata_new %>%
  filter(item_filename == "Control.png" | item_filename == "Bubble treatment1.png") %>%
  filter(aoi_name == "IMportantSafetyInformation_Control" | aoi_name == "BubbleRiskInfo")

frq(bubble_vs_control_riskinfo$item_filename)
bubble_vs_control_riskinfo$bubble_treatment = ifelse(bubble_vs_control_riskinfo$item_filename == "Control.png", 0, 1)
frq(bubble_vs_control_riskinfo$bubble_treatment)

process(data = bubble_vs_control_riskinfo, y = "TotalRecall_Risks", x = "bubble_treatment",
        m = "aoi_fixation_total_count", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "TotalRecall_Risks", x = "bubble_treatment",
        m = "aoi_fixation_average_total_time_spent_secs", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions2", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)


process(data = bubble_vs_control_riskinfo, y = "riskperceptions2", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions2", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions2", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "RiskPer_Combined", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "RiskPer_Combined", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "TotalRecall_Risks"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "risk_sensitivity", x = "bubble_treatment",
        m = "aoi_fixation_total_count", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "risk_sensitivity", x = "bubble_treatment",
        m = "aoi_fixation_average_total_time_spent_secs", model = 4, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions2", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "riskperceptions2", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "risks_outweigh_benefits", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "RiskPer_Combined", x = "bubble_treatment",
        m = c("aoi_fixation_total_count", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)

process(data = bubble_vs_control_riskinfo, y = "RiskPer_Combined", x = "bubble_treatment",
        m = c("aoi_fixation_average_total_time_spent_secs", "risk_sensitivity"), model = 6, 
        effsize = 1, total = 1, stand = 1, 
        cov = c("age", "female", "previous_diagnosis"), boot = 10000,
        modelbt = 1, seed = 654321)


