#############################################
# CABU-C WP2 Enquêtes de patients           #
# RAPPORT JOURNALIER DE COLLECTE DE DONNEES #
# baseline Oct-Déc 2022                     #
#############################################

#### installer et charger les packages ####
# install.packages("pacman")
pacman::p_load(here,readxl,lubridate,haven,dplyr,ggplot2,scales,zoo,reshape2,tidyr,stringr,wesanderson,tidyr, knitr, epitools, naniar, survey)

# importer la base de patients  la plus récente - METTRE A JOUR
patients <- read_excel("db/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2022-11-10-06-41-38.xlsx", 
                       sheet = "Questionnaire patient CABU-RDC")
# importer la base d'antibiotiques  la plus récente - METTRE A JOUR
antibiotiques <- read_excel("db/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2022-11-10-06-41-38.xlsx", 
                            sheet = "ab")
# lier les deux
patients_ab <- merge(patients, antibiotiques, by = "") # vérifier quel est l'identifier

# identifier le nombre d'enquêtes par fournisseur


# replace "" with NA
patientkis %>% replace_with_na_all(condition = ~.x == "")
