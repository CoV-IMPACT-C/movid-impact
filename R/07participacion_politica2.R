# Code for analyzing political participation MOVID-i ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(dplyr, summarytools, sjPlot)

# 2. Load data  -------------------------------------------
movidi <- readRDS(file = "output/data/movidi_partpol.rds")

# 4. Descriptives
movidi %>% 
  select(protesta, disp_protesta, sexo, edad, educ, cambing, lning,
         cronicos, covid_pos, salud_autop, dist_social, no_reunion,
         riesgo, desigualdad, gobierno_bienestar, obediencia) %>% 
  dfSummary(., valid.col = F, graph.col = F)

## Subset listwise dataset
movidi <- movidi %>% 
  select(protesta, disp_protesta, sexo, edad, educ, cambing, lning,
         cronicos, covid_pos, salud_autop, dist_social, no_reunion,
         riesgo, desigualdad, gobierno_bienestar, obediencia,
         cae, cambing) %>% 
  na.omit()

# 5. Models for protest and disposition to protest
## Set reference levels
movidi$cae <- relevel(factor(movidi$cae), ref = "Ocupado")
movidi$cambing <- relevel(factor(movidi$cambing), ref = "Ha subido")
movidi$covid_pos <- relevel(factor(movidi$covid_pos), ref = "No")

preds0 <- paste(c("sexo", "edad",  "educ", "cae", "cambing", "lning"),
                collapse = " + ")
preds1 <- paste(c("sexo", "edad",  "educ", "cae", "cambing", "lning", 
           "cronicos", "covid_pos", "salud_autop", "dist_social", "no_reunion"),
           collapse = " + ")
preds2 <- paste(c("sexo", "edad",  "educ", "cae", "cambing", "lning", 
            "cronicos", "covid_pos", "salud_autop", "dist_social", "no_reunion",
            "riesgo", "desigualdad", "gobierno_bienestar", "obediencia"),
            collapse = " + ")

m_prot0 <- glm(formula(paste("protesta ~", preds0)), 
               data = movidi, family = "binomial")
m_prot1 <- glm(formula(paste("protesta ~", preds1)), 
           data = movidi, family = "binomial")
m_prot2 <- glm(formula(paste("protesta ~", preds2)), 
               data = movidi, family = "binomial")

tab_model(m_prot0, m_prot1, m_prot2)

m_disp0 <- glm(formula(paste("disp_protesta ~", preds0)), 
              data = movidi, family = "binomial")
m_disp1 <- glm(formula(paste("disp_protesta ~", preds1)), 
               data = movidi, family = "binomial")
m_disp2 <- glm(formula(paste("disp_protesta ~", preds2)), 
               data = movidi, family = "binomial")

# Compare models of protest

# Compare models of disposition to protest

# Full models comparison
prdlbl <- c("Intercept", "Female", "Age", "High school ed.", "Professional ed.",
            "Technical ed.", "Unemployed", "Inactive", "Income decrease", 
            "Income stable", "Income (log)", "Chronic disease", "COVID disease", 
            "Health", "Social distancing", "No meetings", "Perceived risks", 
            "Pand. inequality", "Govt. wellbeing", "Compliance")
tab_model(m_prot0, m_disp0,
          m_prot1, m_disp1,
          m_prot2, m_disp2,
          show.ci = F, show.se = T,
          string.est = "OR", string.se = "SE",
          dv.labels = rep(c("Protest", "Disp. protest"), 3),
          pred.labels = prdlbl, auto.label = FALSE,
          file = "output/tables/table1_models.html")

