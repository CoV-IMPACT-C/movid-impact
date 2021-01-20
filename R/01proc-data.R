# Code 1: Proc movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_o <- haven::read_dta("input/data/210119_base_movid_version01.dta")
movid_i <- movid_o

# 3. Recodes -----------------------------------------------------

# Identificacion ----------------------------------------------------------
movid_i$entrevistado <- ifelse(is.na(movid_i$entrevistado), 0, 1)

# Modulo A ----------------------------------------------------------------
# 2.1 Sex -----------------------------------------------------------------
movid_i$sexo <- as.numeric(movid_i$sexo)
movid_i$sexo <- car::recode(movid_i$sexo, c("1='Hombre';2='Mujer'"), as.factor = T,
                            levels = c("Hombre", "Mujer"))
#  2.2 Age -----------------------------------------------------------------
## Edad
str(movid_i$edad) # num

## Edad_3cat
movid_i$edad_3cat <- ifelse(movid_i$edad<40, "18 a 39",
                            ifelse(movid_i$edad<65 & movid_i$edad>39, "40 a 64",
                                   ifelse(movid_i$edad>64, "65 y más", NA)))

movid_i$edad_3cat <- as_factor(movid_i$edad_3cat)


# 2.4 Education ---------------------------------------------------------------
# Education: 3 categories: High school or less, Technical qualification and University degree
table(movid_i$a8a)
movid_i$educ_3cat <- as.numeric(movid_i$a8a)
movid_i$educ_3cat <- car::recode(movid_i$educ_3cat, c("c(1,2,3,4,5,6)='Media o menos';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                                 levels = c("Media o menos", "Profesional", 'Técnica'))

table(movid_i$educ_3cat)

# 2.5 Worker ------------------------------------------------------------------
movid_i$trabaja <- as.numeric(movid_i$g1)
movid_i$trabaja <- car::recode(movid_i$trabaja, c("1='Si'; 2='No'"), as.factor = T)

# Worker (g1 or/type g10)

# 2.6 Health risk -------------------------------------------------------------
# Health risk: arterial hypertension, obesity, diabetes, chronic respiratory diseases (asthma, emphysema or other), cardiovascular diseases, active cancer, chronic kidney disease or immunodeficiencies
## Health risk General
table(movid_i$c1_9)
movid_i <- movid_i %>% mutate(cronicos = case_when(c1_1 == 1 ~ 1,
                                                   c1_2 == 2 ~ 1,
                                                   c1_3 == 3 ~ 1,
                                                   c1_4 == 4 ~ 1,
                                                   c1_5 == 5 ~ 1,
                                                   c1_6_esp == "artritis" ~ 1,
                                                   c1_7 == 7 ~ 1,
                                                   c1_8 == 8 ~ NA_real_,
                                                   c1_9 == 9 ~ NA_real_,
                                                   TRUE ~ 0)) %>%
  mutate(cronicos = if_else(cronicos == 1, "Si", "No"))

table(movid_i$cronicos) ## Artritis
178+339+48+91+161+567+7
# 2.7 Health insurance --------------------------------------------------------
## Prev General
table(movid_i$b2)
movid_i$pr2_prevision <- as.numeric(movid_i$b2)
movid_i$pr2_prevision <- car::recode(movid_i$pr2_prevision, c("1='FONASA';2='ISAPRE';3='Fuerzas Armadas y de Orden';4='Otro';5='Ninguna';c(8,9)=NA"), as.factor = T,
                                     levels = c("FONASA", "ISAPRE", 'Fuerzas Armadas y de Orden', 'Otro', 'Ninguna' ))

table(movid_i$pr2_prevision)

## Prev recodificaciones
movid_i$prev_2categ <- as.factor(ifelse(movid_i$pr2_prevision=="FONASA",0,
                                        ifelse(movid_i$pr2_prevision=="ISAPRE",1,2)))
levels(movid_i$prev_2categ) <- c("FONASA","ISAPRE", "Otro")


movid_i$prev_4categ <- as.factor(ifelse(movid_i$pr2_prevision=="Ninguna",0,
                                        ifelse(movid_i$pr2_prevision=="FONASA",1,
                                               ifelse(movid_i$pr2_prevision=="ISAPRE",2,3))))
levels(movid_i$prev_4categ) <- c("Ninguna","FONASA","ISAPRE", "Otro")


# Modulo F ----------------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(contains("f")), funs(as.numeric(.)))

# F3. Percepcion and legitimacy -----------------------------------------------

# F3_3. Legitimacy (f3.3) ----------------------------------------------------
###Aunque a veces no estemos de acuerdo con las autoridades sanitarias y las medidas que se proponen, es nuestro deber seguir sus indicaciones al pie de la letra.
movid_i$soc1_bienestar <- car::recode(movid_i$f3_2, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                      levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

movid_i$soc2_obedecer <- car::recode(movid_i$f3_3, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                     levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

movid_i$soc3_gob <- car::recode(movid_i$f3_4, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f3_3)
table(movid_i$soc2_obedecer)


# F4. Collective accion ---------------------------------------------------

# F4_3. Communitary help --------------------------------------------------
table(movid_i$f4_3)
movid_i$f4_3 <- as.numeric(movid_i$f4_3)
movid_i$f4_3 <- car::recode(movid_i$f4_3, c("1='Si';2='No'; c(8,9)=NA"), as.factor = T,
                            levels = c("Si", "No"))
table(movid_i$f4_3)
# F5. Fatigue -------------------------------------------------------------
# F5_1. Can protect  (f5_1) --------------------------------------------
### Puedo protegerme completamente del coronavirus si tomo las medidas de protección adecuadas.
table(movid_i$f5_1)
movid_i$f5_1 <- car::recode(movid_i$f5_1, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_1)

# F5_2. Inform  (f5_2) --------------------------------------------
### Me informo constantemente sobre los avances del coronavirus y sus efectos en Chile
movid_i$f5_2 <- car::recode(movid_i$f5_2, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_2)

# F5_3. Care motivation  (f5_3) --------------------------------------------
### A medida que ha avanzado la crisis sanitaria, me siento cada vez más desmotivado para seguir las medidas de protección recomendadas (f5_3)
movid_i$f5_3 <- car::recode(movid_i$f5_3, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_3)

# F5_4. Dificult protect  (f5_4) --------------------------------------------
### Resulta cada vez más difícil seguir las medidas de protección sin que esto afecte de manera negativa mi vida (f5_4)
movid_i$f5_4 <- car::recode(movid_i$f5_4, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_4)

# F5_5. Legal enforcement  (f5.5) --------------------------------------------
### En Chile, si una persona sale sin permiso durante una cuarentena es muy poco probable que sea controlado y multado.
movid_i$f5_5 <- car::recode(movid_i$f5_5, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                   levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_5)

# F5_6. Willigness to protest  (f5_6) --------------------------------------------
### Incluso considerando la situación sanitaria, estoy dispuesto/a a participar en manifestaciones masivas en este momento.
movid_i$f5_6 <- car::recode(movid_i$f5_6, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                            levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f5_6)


# F6. Perived risk (f6) ----------------------------------------------------
### ¿Qué tan peligroso cree que es el coronavirus para usted y sus cercanos?
movid_i$f6 <- car::recode(movid_i$f6, c("1='Nada peligroso';2='Algo peligroso';3='Bastante peligroso';4='Muy peligroso';5='Extremadamente peligroso';c(8,9)=NA"), as.factor = T,
                                 levels = c("Nada peligroso", "Algo peligroso", "Bastante peligroso","Muy peligroso", "Extremadamente peligroso"))

table(movid_i$f6)

## High risk -------------------------------------------------------------

movid_i$high_risk <- ifelse(movid_i$f6 == "Bastante peligroso" | movid_i$f6 == "Muy peligroso"| movid_i$f6 == "Extremadamente peligroso",1,0)
movid_i$low_risk <-  ifelse(movid_i$f6 == "Nada peligroso"     | movid_i$f6 == "Algo peligroso",1,0)

# F7.  Compliance (f7_*)----------------------------------------------------------
### En la última semana, ¿con qué frecuencia ha realizado las siguientes acciones para protegerse del coronavirus? Ud. me debe decir si las ha realizado Casi nunca, A veces, Frecuentemente, Casi siempre o Siempre
movid_i$f7_1 <- car::recode(movid_i$f7_1, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

movid_i$f7_2 <- car::recode(movid_i$f7_2, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

movid_i$f7_3 <- car::recode(movid_i$f7_3, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

movid_i$f7_4 <- car::recode(movid_i$f7_4, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

movid_i$f7_5 <- car::recode(movid_i$f7_5, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

table(movid_i$f7_1)

## High Compliance  ---------------------------------------------------

movid_i <- movid_i %>% mutate(comp = case_when(f7_1 %in%  c('Casi siempre', 'Siempre')~ 1,
                                               f7_2 %in%  c('Casi siempre', 'Siempre')~ 1,
                                               f7_3 %in%   c('Casi siempre', 'Siempre')~ 1,
                                               f7_4 %in%  c('Casi siempre', 'Siempre')~ 1,
                                               f7_5 %in% c('Casi siempre', 'Siempre')~ 1,
                                               TRUE ~ 0))

table(movid_i$comp)

# F8. Perceived social norms(f8) ----------------------------------------------
### Pensando en distintas medidas de cuidado ante el coronavirus (quedarse en casa, usar     mascarilla, mantener distanciamiento social o lavarse las manos). ¿En qué medida diría Ud.     que su círculo cercano (personas que viven con Ud. o su familia cercana) cumple estas     recomendaciones?
movid_i$normas <- car::recode(movid_i$f8, c("1='Completamente';2='En gran medida';3='Bastante';4='Algo';5='Poco';6 ='Nada';c(8,9)=NA"), as.factor = T,
                              levels = c("Completamente", "En gran medida", "Bastante", "Algo", "Poco", "Nada"))

table(movid_i$f8)
table(movid_i$normas)

## Normas
movid_i$cumple_normas <- ifelse(movid_i$normas=="Completamente" | movid_i$normas=="En gran medida",1,0)
movid_i$nocumple_normas <- ifelse(movid_i$normas=="Nada" | movid_i$normas=="Poco" | movid_i$normas=="Algo",1,0)


# G. Module: Employment -----------------------------------------------------------
# 2.8 Lack income due COVID ---------------------------------------------------
# Worker
table(movid_i$g18)
movid_i$g18 <- as.numeric(movid_i$g18)
movid_i$g18 <- car::recode(movid_i$g18, c("1='Ha bajado';2='Se ha mantenido igual';3='Ha subido';c(8,9)=NA"), as.factor = T,
                           levels = c("Ha bajado", "Se ha mantenido igual", "Ha subido"))
table(movid_i$g18)

# worker
table(movid_i$g49)
movid_i$g49 <- as.numeric(movid_i$g49)
movid_i$g49 <- car::recode(movid_i$g49, c("1='Ha bajado';2='Se ha mantenido igual';3='Ha subido';c(8,9)=NA"), as.factor = T,
                                   levels = c("Ha bajado", "Se ha mantenido igual", "Ha subido"))
table(movid_i$g49)

# 4.Merge data ------------------------------------------------------------

# 4.1 Select variables ----------------------------------------------------
movid_i <- movid_i %>% filter(a5 == 1)
movid_o <- movid_o %>% filter(a5 == 1)

# 5. Save  -----------------------------------------------------------------
saveRDS(movid_i, file = "output/data/movid_i.RDS")
saveRDS(movid_o, file = "output/data/movid_o.RDS")
save(movid_i,movid_o, file = "output/data/movid_impact.RData")
