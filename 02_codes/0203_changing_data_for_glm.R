#-------------------------------------------------------------------
# Project: Scientific Involvement Indicator
# Organization: SFedU Future Skills Research Lab
# Objective: Creating variables for logistic regression
# Author: Valeria Egorova
# Date: 19 Nov 2023
#-------

#downloading data

data_index <- read_dta(file.path(outData,"data_index.dta"))

#vector to scale the variables

var <- c("self_control", "self_efficacy", "educational",
         "communicative", "emotional", "growth", "creativity",
         "position", "achievment", "external", "self_respect", "motivational", 
         "cognitive", "behavioural", "research_potential", "em_knowledge",
         "em_control", "self_mot", "empaty", "em_identification",
         "O", "C", "E", "A", "N", "gm_intelligence", "gm_personality", 
         "externalism",  "commitment", "consistency", "grit", 
         "life_sat",  "AP02", "PI", "sci_env") #"AP01",

#creating data for logisting regression

data_for_glm <- 
  data_index %>%
  replace_with_na(.,
                  replace = list(grade = c(12,13,17,49,9283737828))) %>%
  mutate(sci_env = rowMeans(select(.,c(SC007, SC006)),
                            na.rm = T),
         dataset = factor(dataset),
         sex = factor(sex),
         grade = factor(grade),
         school = case_when(
           grepl("^9", ID) ~ "9",
           grepl("^14", ID) ~ "14",
           grepl("^62", ID) ~ "62",
           grepl("^64", ID) ~ "64",
           grepl("^67", ID) ~ "67",
           grepl("^77", ID) ~ "77",
           grepl("^83", ID) ~ "83",
           grepl("^01", ID) ~ "01",
           grepl("^1", ID) ~ "01",
           grepl("^4", ID) ~ "sunz",
           grepl("^3", ID) ~ "3",
           TRUE ~ as.character(NA)),
         sc_type = as.factor(case_when(school %in% c("9", "14", "62") ~ "lyceum",
                                       school %in% c("3", "sunz") ~ "uni",
                                       school %in% c("64", "67","77", "83", "01") ~ "gen",
                                       T ~ as.character(NA))),
         group = ifelse(index_v2 > mean(index_v2), 1,0)) %>%
  mutate_at(.vars = var, .fun = ~ scale(., center = T, scale = T)) %>%
  mutate(research_potential = as.numeric(research_potential),
         AP02 = as.numeric(AP02),
         PI = as.numeric(PI),
         sci_env = as.numeric(sci_env),
         AP01 = as.factor(AP01),
         ac_ach = as.factor(case_when(AP01 %in% c(1:3) ~ 1, # ТРОЕЧНИКИ
                                      AP01 %in% c(4:5) ~ 2, # ХОРОШИСТЫ
                                      AP01 == 6 ~ 3)), # ОТЛИЧНИКИ
         ses_p = percent_rank(ses_yhat),
         windex5 = case_when(ses_p <= 0.20 ~ 1,
                             ses_p > 0.20 & ses_p <= 0.40 ~ 2,
                             ses_p > 0.4 & ses_p <= 0.60 ~ 3,
                             ses_p >= 0.61 & ses_p <= 0.80 ~ 4,
                             ses_p > 0.8  ~ 5,
                             T ~ as.numeric(NA)),
         ses10 = ntile(ses_yhat, 10),
         ses = case_when(ses10 %in% (1:4) ~ "1. Bottom 40%",
                         ses10 %in% (5:9) ~ "2. Middle 50%",
                         ses10 == 10 ~ "3. Top 10%")) %>%
  write_dta(file.path(outData, "data_for_glm.dta"))

