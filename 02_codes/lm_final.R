data_cor <- 
  data_for_glm %>%
  select(index_v2, self_control, self_efficacy, growth, gm_intelligence, research_potential, motivational,
         behavioural, cognitive)

corr_matrix <- cor(data_cor)
p.mat = cor_pmat(data_cor)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE, p.mat = p.mat, tl.cex = 10, lab_size = 2.5)

lm1 <- lm(index_v2 ~ self_control + self_efficacy + growth + gm_intelligence,
          data = data_for_glm)
summary(lm1)

vif(lm1)

autoplot(lm1)

lm2 <- lm(index_v2 ~ self_control + self_efficacy +  gm_intelligence,
          data = data_for_glm)
summary(lm2)

vif(lm2)

autoplot(lm2)

lm3 <- lm(index_v2 ~ self_control + self_efficacy +  growth,
          data = data_for_glm)
summary(lm3)

vif(lm3)

autoplot(lm3)


mod <- lm(index_v2 ~ PI + sci_env  +  as.factor(sex) + as.factor(ses) +
            + as.factor(sc_type), data = data_for_glm)
summary(mod)

vif(mod)

autoplot(mod3)



mod2 <- lm(index_v2 ~ PI + sci_env  +  as.factor(sex)  + as.factor(ses) +
             + as.factor(sc_type) + as.factor(ac_ach) + 
             self_control + self_efficacy + 
             growth + gm_intelligence,
           data = data_for_glm)
summary(mod2)

vif(mod2)

autoplot(mod3)







mod3 <- lm(index_v2 ~ PI + sci_env  +  as.factor(sex)  
           + as.factor(sc_type) + as.factor(ac_ach) + 
             self_control*as.factor(ses) + 
             self_efficacy*as.factor(ses) + 
             growth*as.factor(ses) + 
             gm_intelligence*as.factor(ses),
           data = data_for_glm)
summary(mod3)
vif(mod3)

autoplot(mod3)









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