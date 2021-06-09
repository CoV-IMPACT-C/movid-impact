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
  
# 5. Models for protest and disposition to protest
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





