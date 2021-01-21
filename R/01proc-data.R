# Code 1: Proc movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_o <- haven::read_dta("input/data/210119_base_movid_version01.dta")
movid_i <- movid_o

# 3. Recodes -----------------------------------------------------

# Modulo A ----------------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(contains("a"), sexo, edad), funs(as.numeric(.)))

# A_1 Identificacion ----------------------------------------------------------
movid_i$entrevistado <- ifelse(is.na(movid_i$entrevistado), 0, 1)

# A_2 Sexo -----------------------------------------------------------------
movid_i$sexo <- car::recode(movid_i$sexo, c("1='Hombre';2='Mujer'"), as.factor = T,
                            levels = c("Hombre", "Mujer"))
# A_3 Age -----------------------------------------------------------------
## Edad (numerica)

## Edad en tres categorías
movid_i$edad_3cat <- car::recode(movid_i$edad, c("0:39='18 a 39';40:64='40 a 64';65:hi='65 y más'"), as.factor = T,
                            levels = c("18 a 39", "40 a 64", "65 y más"))

table(movid_i$edad_3cat)

## Edad en cuatro categorías
movid_i$edad_cat <- car::recode(movid_i$edad, c("0:29='18 a 29';30:39='30 a 39';40:49='40 a 49';50:59='50 a 59';60:69='60 a 69';70:79='70 a 79';80:hi='80+'"), as.factor = T,
                                 levels = c("18 a 29", "30 a 39","40 a 49","50 a 59","60 a 69","70 a 79", "80+"))

table(movid_i$edad_cat)

## Edad adultos mayores
movid_i$edad_65 <- car::recode(movid_i$edad, c("0:64='No Adulto Mayor';65:hi='Adulto Mayor'"), as.factor = T,
                                levels = c("Adulto Mayor","No Adulto Mayor"))

table(movid_i$edad_65)

# A4 Relacion Jefe Hogar -------------------------------------------------
table(movid_i$a4)
movid_i$a4 <- car::recode(movid_i$a4, c("1='Jefe de Hogar';2='Cónyuge o pareja';3='Hijo(a)';4='Padre o madre';5='Suegro(a)';6='Yerno o nuera';7='Nieto(a)';8='Hermano(a)';9='Cuñado(a)';10='Otro Familiar';11='Otro no familiar';12='S. Doméstico P. Adentro'"), as.factor = T,
                                levels = c('Jefe de Hogar', 'Cónyuge o pareja','Hijo(a)','Padre o madre','Suegro(a)','Yerno o nuera', 'Nieto(a)', 'Hermano(a)', 'Cuñado(a)', 'Otro Familiar', 'Otro no familiar','S. Doméstico P. Adentro'))

table(movid_i$a4)

# A5 Relacion con entrevistado -------------------------------------------
table(movid_i$a5)
movid_i$a5 <- car::recode(movid_i$a5, c("1='Entrevistado(a)';2='Cónyuge o pareja';3='Hijo(a)';4='Padre o madre';5='Suegro(a)';6='Yerno o nuera';7='Nieto(a)';8='Hermano(a)';9='Cuñado(a)';10='Otro Familiar';11='Otro no familiar';12='S. Doméstico P. Adentro'"), as.factor = T,
                          levels = c('Entrevistado(a)', 'Cónyuge o pareja','Hijo(a)','Padre o madre','Suegro(a)','Yerno o nuera', 'Nieto(a)', 'Hermano(a)', 'Cuñado(a)', 'Otro Familiar', 'Otro no familiar','S. Doméstico P. Adentro'))

table(movid_i$a5)

# A6 Estado civil -------------------------------------------
table(movid_i$a6)
movid_i$a6 <- car::recode(movid_i$a6, c("1='Casado(a)';2='Conviviente o pareja';3='Anulado(a)';4='Separado(a)';5='Divorciado(a)';6='Viudo(a)';7='Soltero(a)'"), as.factor = T,
                          levels = c('Casado(a)', 'Conviviente o pareja','Anulado(a)','Separado(a)','Divorciado(a)','Viudo(a)', 'Soltero(a)'))

table(movid_i$a6)

# A8 Education ---------------------------------------------------------------
# A7 Asistir a educacion -------------------------------------------------------
table(movid_i$a7)
movid_i$a7 <- car::recode(movid_i$a7, c("1='Sí';2='No'"), as.factor = T,
                               levels = c("Sí","No"))

table(movid_i$a7)

# A8A Nivel mas alto alcanzado -------------------------------------------

# Nivel Educacion 3 categorias --------------------------------------------------
# 3 categories: High school or less, Technical qualification and University degree
table(movid_i$a8a)
movid_i$educ_3cat <- car::recode(movid_i$a8a, c("c(1,2,3,4,5,6)='Media o menos';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                                 levels = c("Media o menos", "Profesional", 'Técnica'))

table(movid_i$educ_3cat)

# Nivel Educacion 4 categorias --------------------------------------------------
# 4 categories: Primary or less, High school,Technical qualification and University degree
table(movid_i$a8a)
movid_i$educ_4cat <- car::recode(movid_i$a8a, c("c(1,2,3,4)='Básica o menos';c(5,6)='Media';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                                 levels = c("Básica o menos","Media", "Profesional", 'Técnica'))

table(movid_i$educ_4cat)


# Nivel educacional 9 cat -------------------------------------------------------
table(movid_i$a8a)
movid_i$a8a <- car::recode(movid_i$a8a, c("1='Nunca asistió';2='Preescolar';3='Especial (Diferencial)';4='Básica';5='Media Científico-Humanista';6='Media Técnica-Profesional';7='Superior Técnica (en CFT o I. Profesional)';8='Superior Universitaria (Pregrado)';9='Postgrado';99=NA"), as.factor = T,
                          levels = c('Nunca asistió', 'Preescolar','Especial (Diferencial)','Básica','Media Científico-Humanista','Media Técnica-Profesional', 'Superior Técnica (en CFT o I. Profesional)', 'Superior Universitaria (Pregrado)','Postgrado'))

table(movid_i$a8a)

# A8b Nivel mas alto alcanzado Ano -------------------------------------------
table(movid_i$a8b)
movid_i$a8a[movid_i$a8a == 99] <- NA
table(movid_i$a8a)

# A9 Duracion pregrado -------------------------------------------
table(movid_i$a9)
movid_i$a9[movid_i$a9 == 99] <- NA
table(movid_i$a9)


# Modulo B. Otras variables demograficas ----------------------------------
movid_i <- movid_i %>% mutate_at(vars(contains("b")), funs(as.numeric(.)))

# B1 Pueblo originario -------------------------------------------------------
table(movid_i$b1)
movid_i$b1 <- car::recode(movid_i$b1, c("1='Sí';2='No'; c(8,9)=NA"), as.factor = T,
                          levels = c("Sí","No"))
table(movid_i$b1)


# B2 Prevision salud  --------------------------------------------------------

# Prevision general (b2) --------------------------------------------------
table(movid_i$b2)
movid_i$b2 <- car::recode(movid_i$b2, c("1='FONASA';2='ISAPRE';3='Fuerzas Armadas y de Orden';4='Otro';5='Ninguna';c(8,9)=NA"), as.factor = T,
                                     levels = c("FONASA", "ISAPRE", 'Fuerzas Armadas y de Orden', 'Otro', 'Ninguna' ))

table(movid_i$b2)

# Prevision 2 categorias --------------------------------------------------
movid_i$prev_2categ <- car::recode(movid_i$b2, c("1='FONASA';2='ISAPRE';4='Otro';hi=NA"), as.factor = T,
                                   levels = c("FONASA", "ISAPRE", 'Otro'))

table(movid_i$prev_2categ)

# Prevision 4 categorias --------------------------------------------------
movid_i$prev_4categ <- car::recode(movid_i$b2, c("1='FONASA';2='ISAPRE';4='Otro';5='Ninguna';hi=NA"), as.factor = T,
                                   levels = c("FONASA", "ISAPRE", 'Otro', 'Ninguna'))

table(movid_i$prev_4categ)

# B3 Establecimiento última atencion --------------------------------------
table(movid_i$b3)
movid_i <- movid_i %>% mutate(b3_tipo = case_when(b3 %in% c(1:7,15) ~ "Público",
                                                  b3 %in% c(8,9,10,12,16) ~ "Privada",
                                                  b3 %in% c(11,13,14,17) ~ "Otro",
                                                  TRUE ~ NA_character_))

movid_i$b3 <- car::recode(movid_i$b3, c("1='Consultorio general';
                                        2='Posta rural';
                                        3='CRS o CDT';
                                        4='COSAM';
                                        5='SAPU';
                                        6='Posta';
                                        7 ='Hospital público o del SNSS';
                                        8='Consulta o centro médico privado';
                                        9='Clínica u hospital privado';
                                        10='Centro de salud mental privado';
                                        11= 'Establecimiento de las FF.AA. o del Orden';
                                        12='Servicio de urgencia de clínica u hospital privado';
                                        13='Mutual de Seguridad';
                                        14='Servicio médico de alumnos del lugar en que estudia';
                                        15='Telemedicina por consultorio u hospital público';
                                        16='Telemedicina por centro médico o clinica privada';
                                        17 ='Otro';
                                        c(98,99)=NA"), as.factor = T,
                          levels = c('Consultorio general',
                                     'Posta rural',
                                     'CRS o CDT',
                                     'COSAM',
                                     'SAPU',
                                     'Posta',
                                     'Hospital público o del SNSS',
                                     'Consulta o centro médico privado',
                                     'Clínica u hospital privado',
                                     'Centro de salud mental privado',
                                     'Servicio de urgencia de clínica u hospital privado',
                                     'Mutual de Seguridad',
                                     'Servicio médico de alumnos del lugar en que estudia',
                                     'Telemedicina por consultorio u hospital público',
                                     'Telemedicina por centro médico o clinica privada',
                                     'Otro'))

table(movid_i$b3)
table(movid_i$b3_tipo)

# Modulo C. Preexistencias ------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(contains("c"), -c1_6_esp), funs(as.numeric(.)))

# C1_* Enfermedades cronicas ----------------------------------------------
# C1_1 Diabetes -----------------------------------------------------------
movid_i$c1_1 <- ifelse(is.na(movid_i$c1_1), 0, 1)
# C1_2 Hipertension -----------------------------------------------------------
movid_i$c1_2 <- ifelse(is.na(movid_i$c1_2), 0, 1)
# C1_3 Cardiovascular -----------------------------------------------------------
movid_i$c1_3 <- ifelse(is.na(movid_i$c1_3), 0, 1)
# C1_4 Respiratoria -----------------------------------------------------------
movid_i$c1_4 <- ifelse(is.na(movid_i$c1_4), 0, 1)
# C1_5 Mental -----------------------------------------------------------
movid_i$c1_5 <- ifelse(is.na(movid_i$c1_5), 0, 1)
# C1_6 Otra -----------------------------------------------------------
movid_i$c1_6 <- ifelse(is.na(movid_i$c1_6), 0, 1)
# C1_7 Sano -----------------------------------------------------------
movid_i$c1_7 <- ifelse(is.na(movid_i$c1_7), 0, 1)
# C1_9 NA -----------------------------------------------------------
movid_i$c1_8 <- ifelse(is.na(movid_i$c1_8), 0, 1)
movid_i$c1_9 <- ifelse(is.na(movid_i$c1_9), 0, 1)


# Health risk: arterial hypertension, obesity, diabetes, chronic respiratory diseases (asthma, emphysema or other), cardiovascular diseases, active cancer, chronic kidney disease or immunodeficiencies
## Health risk General
table(movid_i$c1_9)
movid_i <- movid_i %>% mutate(cronicos = case_when(c1_1 == 1 ~ 1,
                                                   c1_2 == 1 ~ 1,
                                                   c1_3 == 1 ~ 1,
                                                   c1_4 == 1 ~ 1,
                                                   c1_5 == 1 ~ 1,
                                                   c1_6_esp == "artritis" ~ 1,
                                                   c1_7 == 0 ~ 1,
                                                   c1_8 == 1 ~ NA_real_,
                                                   c1_9 == 1 ~ NA_real_)) %>%
  mutate(cronicos = if_else(cronicos == 1, "Sí", "No"))

movid_i$cronicos <- ifelse(movid_i$c1_1 == 1|
                             movid_i$c1_2 == 1|
                             movid_i$c1_3 == 1|
                             movid_i$c1_4 == 1|
                             movid_i$c1_5 == 1|
                             movid_i$c1_6_esp == "artritis"|
                             movid_i$c1_7 == 0|
                             !is.na(movid_i$c1_8)|
                             !is.na(movid_i$c1_9), 1,0)

table(movid_i$cronicos) ## Artritis
178+339+48+91+161+7

# C2 Sintomas Salud Mental ----------------------------------------------------------------

# C2_1 Nervioso -----------------------------------------------------------
table(movid_i$c2_1)
movid_i$c2_1 <- car::recode(movid_i$c2_1, c("1='Nunca';2='Varios días';3='Más de la mitad de los días';4='Casi todos los días';hi=NA"), as.factor = T,
                                   levels = c('Nunca', 'Varios días', 'Más de la mitad de los días', 'Casi todos los días'))

table(movid_i$c2_1)
# C2_2 Angustia -----------------------------------------------------------
table(movid_i$c2_2)
movid_i$c2_2 <- car::recode(movid_i$c2_2, c("1='Nunca';2='Varios días';3='Más de la mitad de los días';4='Casi todos los días';hi=NA"), as.factor = T,
                            levels = c('Nunca', 'Varios días', 'Más de la mitad de los días', 'Casi todos los días'))
table(movid_i$c2_2)

# C2_3 Deprimido -----------------------------------------------------------
table(movid_i$c2_3)
movid_i$c2_3 <- car::recode(movid_i$c2_3, c("1='Nunca';2='Varios días';3='Más de la mitad de los días';4='Casi todos los días';hi=NA"), as.factor = T,
                            levels = c('Nunca', 'Varios días', 'Más de la mitad de los días', 'Casi todos los días'))
table(movid_i$c2_3)

# C2_4 Desmotivacion -----------------------------------------------------------
table(movid_i$c2_4)
movid_i$c2_4 <- car::recode(movid_i$c2_4, c("1='Nunca';2='Varios días';3='Más de la mitad de los días';4='Casi todos los días';hi=NA"), as.factor = T,
                            levels = c('Nunca', 'Varios días', 'Más de la mitad de los días', 'Casi todos los días'))
table(movid_i$c2_4)


# Modulo D Acceso a Salud COVID-19 ----------------------------------------

# D1 Sospechoso -----------------------------------------------------------

# D2 Sospechoso2 ----------------------------------------------------------

# D3_* Sintomas -----------------------------------------------------------

# D4 Consulta -------------------------------------------------------------

# D5 No consulta ----------------------------------------------------------

# D6 Diagnostico ----------------------------------------------------------

# D7 Licencia ----------------------------------------------------------------------

# D8 Hospitalizado ----------------------------------------------------------------------

# D9 Long COVID -----------------------------------------------------------






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

# 2.5 Worker ------------------------------------------------------------------
movid_i$trabaja <- as.numeric(movid_i$g1)
movid_i$trabaja <- car::recode(movid_i$trabaja, c("1='Si'; 2='No'"), as.factor = T)

# Worker (g1 or/type g10)

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
saveRDS(movid_i, file = "output/data/movid_i_proc.RDS")
#saveRDS(movid_i, file = "output/data/movid_i.RDS")
saveRDS(movid_o, file = "output/data/movid_o.RDS")
save(movid_i,movid_o, file = "output/data/movid_impact.RData")
