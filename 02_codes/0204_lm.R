#-------------------------------------------------------------------
# Project: Scientific Involvement Indicator
# Organization: SFedU Future Skills Research Lab
# Objective: lm
# Author:  Valeria Egorova
# Date: 15 July 2024
#-------------------------------------------------------------------
data_for_glm <- read_dta(file.path(outData,"data_for_glm.dta")) %>%
  filter(grade > 1) %>%
  mutate(grade7 = ifelse(grade == 2,1,0),
         grade8 = ifelse(grade == 3,1,0),
         grade9 = ifelse(grade == 4,1,0),
         grade10 = ifelse(grade == 5,1,0),
         grade11 = ifelse(grade == 6,1,0))

# Social factors corrected

mod <- glm(group ~ PI + sci_env  +  as.factor(sex) + as.factor(windex5) +
             + as.factor(sc_type), data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod)

margin <- margins(mod)
margin

vif(mod)

PseudoR2(mod, which = "all")


# Social & psychological

mod2 <- glm(group ~ PI + sci_env  +  as.factor(sex)  + as.factor(windex5) +
            + as.factor(sc_type) + as.factor(ac_ach) + 
              self_control + self_efficacy + 
              growth + gm_intelligence,
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod2)

maBina(mod2)

vif(mod2)

PseudoR2(mod2, which = "all")

####################################

mod3 <- glm(group ~ PI + sci_env  +  as.factor(sex)  
            + as.factor(sc_type) + as.factor(ac_ach) + 
              self_control*as.factor(windex5) + 
              self_efficacy*as.factor(windex5) + 
              growth*as.factor(windex5) + 
              gm_intelligence*as.factor(windex5),
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod3)

maBina(mod3)

vif(mod3)

PseudoR2(mod3, which = "all")



mod <- glm(group ~ PI + sci_env  +  as.factor(sex) + as.factor(ses) +
             + as.factor(sc_type), data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod)

mod2 <- glm(group ~ PI + sci_env  +  as.factor(sex)  + as.factor(ses) +
              + as.factor(sc_type) + as.factor(ac_ach) + 
              self_control + self_efficacy + 
              growth + gm_intelligence,
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod2)


mod3 <- glm(group ~ PI + sci_env  +  as.factor(sex)  
            + as.factor(sc_type) + as.factor(ac_ach) + 
              self_control*as.factor(ses) + 
              self_efficacy*as.factor(ses) + 
              growth*as.factor(ses) + 
              gm_intelligence*as.factor(ses),
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod3)


mod3 <- glm(group ~ PI + sci_env  +  as.factor(sex)  
            + as.factor(sc_type) + as.factor(ac_ach) + 
              self_control*as.factor(windex5) + 
              self_efficacy*as.factor(windex5) + 
              growth*as.factor(windex5) + 
              gm_intelligence*as.factor(windex5),
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod3)

maBina(mod3)

vif(mod3)

PseudoR2(mod3, which = "all")



mod <- lm(index_v2 ~ PI + sci_env  +  as.factor(sex) + as.factor(ses) +
             + as.factor(sc_type), data = data_for_glm)
summary(mod)

vif(mod)

mod2 <- lm(index_v2 ~ PI + sci_env  +  as.factor(sex)  + as.factor(ses) +
              + as.factor(sc_type) + as.factor(ac_ach) + 
              self_control + self_efficacy + 
              growth + gm_intelligence,
            data = data_for_glm)
summary(mod2)

vif(mod2)

mod3 <- lm(index_v2 ~ PI + sci_env  +  as.factor(sex)  
            + as.factor(sc_type) + as.factor(ac_ach) + 
              self_control*as.factor(ses) + 
              self_efficacy*as.factor(ses) + 
              growth*as.factor(ses) + 
              gm_intelligence*as.factor(ses),
            data = data_for_glm)
summary(mod3)
vif(mod3)

















###############################
mod5 <- lm(index_v2 ~ PI + sci_env  +  as.factor(sex) + as.factor(windex5) +
              + as.factor(sc_type), 
           data = data_for_glm)
summary(mod5)

vif(mod5)
#############################

mod6 <- lm(index_v2 ~ PI + sci_env  +  as.factor(sex)  + as.factor(windex5)
            + as.factor(sc_type) + as.factor(ac_ach) + self_control + 
             self_efficacy + growth + gm_intelligence + consistency, 
           data = data_for_glm)
summary(mod6)

vif(mod6)

########################


cf <- data.frame(mod7$coefficients) %>%
write_xlsx(file.path(outData,"cf.xlsx"))
