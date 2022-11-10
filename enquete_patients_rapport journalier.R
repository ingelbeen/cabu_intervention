#############################################
# CABU-C WP2 Enquêtes de patients           #
# RAPPORT JOURNALIER DE COLLECTE DE DONNEES #
# baseline Oct-Déc 2022                     #
#############################################

#### installer+charger les packages, importer+nettoyer les données ####
# install.packages("pacman")
pacman::p_load(here,readxl,lubridate,haven,dplyr,ggplot2,scales,zoo,reshape2,tidyr,stringr,wesanderson,tidyr, knitr, epitools, naniar, survey)

# importer la base de patients de la première semaine (en ODK) - REMPLCAER ENCORE AVEC CSV POUR 1ERE SEMANE
patient_semaine1 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/cabu_intervention/db/Questionnaire_patient_CABU_RDC_results (1).csv", sep=";")

# importer la base de patients  la plus récente - METTRE A JOUR
patient <- read_excel("db/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2022-11-10-06-41-38.xlsx", 
                       sheet = "Questionnaire patient CABU-RDC")
# importer la base d'antibiotiques  la plus récente - METTRE A JOUR
ab <- read_excel("db/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2022-11-10-06-41-38.xlsx", 
                            sheet = "ab")
# lier les deux
patient_ab <- merge(patient, ab, by.x = "_uuid", by.y = "_submission__uuid") # vérifier quel est l'identifier

# public vs private
table(patient$providertype, useNA = "always")
patient$publicprivate <- "private"
patient$publicprivate[patient$providertype=="healthcentre_publique"] <- "public"

# var agegroups
table(patient$ageyears)
patient$agegroups[patient$ageyears<5] <- "0-4 yr"
patient$agegroups[patient$ageyears>4.999] <- "5-17 yr"
patient$agegroups[patient$ageyears>17.999] <- "18-64 yr"
patient$agegroups[patient$ageyears>64.999] <- "65+ yr"
table(patient$agegroups)

# var agegroup children vs adults
patient$adoadult[patient$ageyears>17.99] <- "adult"
patient$adoadult[patient$ageyears<18] <- "child/adolescent"

# dates reformatted
patient$surveydate <- as.Date(patient$today)
patient$onsetdate <- as.Date(patient$date_onset)
table(patient$date_onset, useNA = "always")

# duration symptoms until consultation
patient$date_onset_num <- as.numeric(patient$date_onset)
patient$duration <- patient$interviewdate_num - patient$date_onset_num

#### analyse journalier ####
# histogramme des enquêtes
interviewdays <- as.numeric(max(patient$surveydate) - min(patient$surveydate))
interviewdays
hist(patient$surveydate, breaks = interviewdays, freq = T)

# distribution de l'âge par grappe
hist(patient$ageyears, breaks = 100)
table(patient$agegroups)
round(prop.table(table(patient$agegroups))*100,1)

# distribution par sexe
table(patient$sex)

# le nombre d'enquêtes par type de fournisseur privé vs. publique
table(patient$choices_cluster,patient$publicprivate)

# nombre d'enquêtes à faire encore, par grappe 
nombreenquetesrestantes <- -table(patient$choices_cluster,patient$publicprivate)+100
nombreenquetesrestantes[nombreenquetesrestantes<0] <- 0
nombreenquetesrestantes

# créer liste des enquêtes par fournisseur
table(patient$choices_cluster,patient$providernr) # vérifier quel n° correspond à quel fournisseur

# prévalence d'usage d'antibiotiques
table(patient$providertype, patient$antibiotic)
round(prop.table(table(patient$providertype, patient$antibiotic),1)*100,1)

# distribution d'antibiotiques
table(patient_ab$abgeneric)
table(patient_ab$abgeneric_other)
