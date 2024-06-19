################################################################################
# CABU-EICO household surveys and rodent collection, KIMPESE                   #
# Link stool bacterial culture results to household or rodent collection data  #
################################################################################

# Author: Brecht Ingelbeen
# Last update: 19 June 2024 by E. van Kleef

rm(list=ls())
# install and load packages
pacman::p_load(readxl,lubridate,dplyr,ggplot2, janitor, writexl)

### 1 IMPORT & MERGE HUMAN HOUSEHOLD AND STOOL RESULT DATA ####
### ROUND 1 ####
# import lab result datasets
humanR1results <- read_excel("./Data/DRC/Raw/householdsurvey/T1_SELH_BDD_CABU_LAB.xlsx", 
                             sheet = "M0")
humanR1results <- humanR1results %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
humanR1results <- humanR1results %>% filter(!is.na(Identifiant)|!is.na(`Date reception`)|!is.na(`Escherichia coli`)) # remove empty rows

# import household visits - individual data
HHindividualR1 <- read_excel("./Data/DRC/Raw/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx", 
                             sheet = "group_io0xt32")
# remove 'Nom' and 'Prénom' columns
HHindividualR1 <- HHindividualR1 %>% select(-nom_individu, -prenom_individu)
# remove dob
table(HHindividualR1$age, useNA = "always")
table(HHindividualR1$dob[is.na(HHindividualR1$age)], useNA = "always")
HHindividualR1$dob <- as.Date(HHindividualR1$dob, format = "%d/%m/%Y")
HHindividualR1$age <- tolower(HHindividualR1$age) # clean var age
HHindividualR1$age <- gsub(" ", "", HHindividualR1$age) # remove spaces
HHindividualR1$ageyears <- as.numeric(gsub("ans", "", HHindividualR1$age, fixed = TRUE))
HHindividualR1$ageyears[grepl("mois", HHindividualR1$age)==T] <- as.numeric(gsub("mois", "", HHindividualR1$age[grepl("mois", HHindividualR1$age)==T], fixed = TRUE))
HHindividualR1$ageyears[grepl("mois", HHindividualR1$age)] <- HHindividualR1$ageyears[grepl("mois", HHindividualR1$age)] / 12
HHindividualR1$ageyears[is.na(HHindividualR1$age) & !is.na(HHindividualR1$dob)] <- round((as.Date("2023-01-30") - HHindividualR1$dob[is.na(HHindividualR1$age) & !is.na(HHindividualR1$dob)])/365.25, 0)
table(HHindividualR1$ageyears, useNA = "always") # 6 missing ages
HHindividualR1 <- HHindividualR1 %>% select(-dob)
HHindividualR1 <- HHindividualR1 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# import household visits - household data
HHlocationR1 <- read_excel("./Data/DRC/Raw/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx", 
                         sheet = "CABU_enq_comm_2023")
HHlocationR1 <- HHlocationR1 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# merge household visit individual and HH data (including location)
HHR1 <- merge(HHindividualR1, HHlocationR1, by.x = "_submission__id", by.y = "_id", all = T)
HHR1_stoolparticipantsonly <- merge(HHindividualR1, HHlocationR1, by.x = "_submission__id", by.y = "_id", all.x = T)

# merge household visit data with stool lab results
# make a var with a common id (of sample)
table(HHR1$num_echantillon, useNA="always")
HHR1$id <- substr(tolower(gsub("[ -]", "", HHR1$num_echantillon)), 1, 7)
HHR1$id[HHR1$id=="czr1144"] <- "car1144" 
HHR1$id[HHR1$id=="acr1258"] <- "car1258" 
HHR1 <- HHR1 %>%  mutate(id = ifelse(grepl("^\\d{4}$", id), paste0("car", id), id))
table(HHR1$id, useNA="always")

table(humanR1results$Identifiant)
humanR1results$id <- substr(tolower(gsub("[ -]", "", humanR1results$Identifiant)), 1, 8)
humanR1results$id <- substr(tolower(gsub(" ", "", humanR1results$id)), 1, 8)
humanR1results$id <- substr(tolower(gsub("'", "", humanR1results$id)), 1, 8)
table(humanR1results$id, useNA="always")

# reformat date
table(humanR1results$`Date reception`, useNA = "always")
humanR1results$receptiondate <- as.Date(as.numeric(humanR1results$`Date reception`), origin = "1899-12-30")
humanR1results$receptiondate[humanR1results$`Date reception`=="06/042023"] <- as.Date("2023-04-06")
table(humanR1results$receptiondate, useNA = "always")

# merge both dataframes
humanR1merged <- merge(HHR1, humanR1results, by = "id", all = T)
table(duplicated(humanR1merged$id))
str(humanR1merged)
rodentclusters <- c("cellule_mbuka3", "kiandu", "kilueka", "kimaku", "lukengezi_et_poste", "malanga")
humanR1merged$rodent <- ifelse(humanR1merged$grappe %in% rodentclusters, "yes", "no")

# identify observations that are not merged
# identify ids in lab results not matching to kobo entry
unmatchedidinhumanlabresults <- humanR1merged %>% filter(is.na(`_submission__id`)==T) %>% select(id, receptiondate, grappe)
unmatchedidinkobo <- humanR1merged %>% filter(!is.na(num_echantillon)==T & is.na(receptiondate)==T) %>% select(id, today,grappe, rodent,menage_id) # among those with stool sample collected (therefore there is a numechantillon), select those not received (no reception date)
unmatchedidinhumanlabresults # 4 lab results that can't be matched to Kobo
unmatchedidinkobo # 3 Kobo entries with sample number that can't be matched to lab result

table(duplicated(humanR1merged$cs_id_individu))
dups = humanR1merged$cs_id_individu[duplicated(humanR1merged$cs_id_individu)]
dups = dups[!is.na(dups)]

#View(humanR1merged[humanR1merged$cs_id_individu %in% dups,])
dupidhumanlabresults =  humanR1results %>% filter(duplicated(humanR1results$id)) %>% select(id, receptiondate)

# export to csv files
write.csv(unmatchedidinhumanlabresults,"./Data/DRC/Clean/Checks/unmatchedidinhumanlabresultsR1.csv")
write.csv(unmatchedidinkobo, "./Data/DRC/Clean/Checks/unmatchedidinkoboR1.csv")
write.csv(dupidhumanlabresults, "./Data/DRC/Clean/dupidhumanlabresultsR1.csv")

### ROUND 2 ####
# import lab result datasets
humanR2results <- read_excel("Data/DRC/Raw/householdsurvey/T2_SELH_BDD_CABU_LAB_18082023_V2_adj.xlsx")
humanR2results <- humanR2results %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
humanR2results <- humanR2results %>% filter(!is.na(Identifiant)|!is.na(`Date reception`)|!is.na(`Escherichia coli`)) # remove empty rows

table(humanR2results$Identifiant) # a view have multiple samples

# import household visits - individual data
HHindividualR2_old <- read_excel("./Data/DRC/Raw/householdsurvey/CABU_R2_-_all_versions_-_False_-_2024-01-31-10-14-10.xlsx", 
                             sheet = "group_io0xt32")
HHindividualR2 <- read_excel("./Data/DRC/Raw/householdsurvey/CABU_R2_-_all_versions_-_False_-_2024-06-04-19-45-24.xlsx", 
                             sheet = "group_io0xt32")

names(HHindividualR2)
names(HHindividualR2_old)
names(HHindividualR2) = names(HHindividualR2_old)

# remove 'Nom' and 'Prénom' columns
HHindividualR2 <- HHindividualR2 %>% select(-nom_individu, -prenom_individu)
# remove dob
table(HHindividualR2$age, useNA = "always")
table(HHindividualR2$dob[is.na(HHindividualR2$age)], useNA = "always")
HHindividualR2$dob <- as.Date(HHindividualR2$dob, format = "%d/%m/%Y")
HHindividualR2$age <- tolower(HHindividualR2$age) # clean var age
HHindividualR2$age <- gsub(" ", "", HHindividualR2$age) # remove spaces
HHindividualR2$ageyears <- as.numeric(gsub("ans", "", HHindividualR2$age, fixed = TRUE))
HHindividualR2$ageyears[grepl("mois", HHindividualR2$age)==T] <- as.numeric(gsub("mois", "", HHindividualR2$age[grepl("mois", HHindividualR2$age)==T], fixed = TRUE))
HHindividualR2$ageyears[grepl("mois", HHindividualR2$age)] <- HHindividualR2$ageyears[grepl("mois", HHindividualR2$age)] / 12
HHindividualR2$ageyears[is.na(HHindividualR2$age) & !is.na(HHindividualR2$dob)] <- round((as.Date("2023-01-30") - HHindividualR2$dob[is.na(HHindividualR2$age) & !is.na(HHindividualR2$dob)])/365.25, 0)
table(HHindividualR2$ageyears, useNA = "always") # 1 missing age - no one under 2 yo
HHindividualR2 <- HHindividualR2 %>% select(-dob)
HHindividualR2 <- HHindividualR2 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# import household visits - household data (less individuals as no WASH survey conducted)
HHlocationR2 <- read_excel("./Data/DRC/Raw/householdsurvey/CABU_R2_-_all_versions_-_False_-_2024-06-04-19-45-24.xlsx", 
                           sheet = "CABU_R2")
HHlocationR2 <- HHlocationR2 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# merge household visit individual and household data (including location)
HHR2 <- merge(HHindividualR2, HHlocationR2, by.x = "_submission__id", by.y = "_id", all = T)
HHR2_stoolparticipantsonly <- merge(HHindividualR2, HHlocationR2, by.x = "_submission__id", by.y = "_id", all.x = T)

# merge household visit data with stool lab results
# make a var with a common id (of sample)
table(HHR2$num_echantillon, useNA = "always")

# As in R2, ids have an additional component (probably member number, of new household members sampled perhaps?) 
# the previous code for R1 is not applicable.
#HHR2$id[!HHR2$num_echantillon %in% id_longer_than_7] <- substr(tolower(gsub("[ -]", "", HHR2$num_echantillon[!HHR2$num_echantillon %in% id_longer_than_7])), 1, 7)
#HHR2$id[HHR2$num_echantillon %in% id_longer_than_7] = substr(tolower(gsub("[ -]", "", HHR2$num_echantillon[HHR2$num_echantillon %in% id_longer_than_7])), 1, 9)
HHR2$id = tolower(gsub("[ -]", "",HHR2$num_echantillon))
table(HHR2$id, useNA = "always")
HHR2$id[HHR2$id=="car2038xo"] = "car2038x0"
HHR2$id[HHR2$id=="car2043xo"] = "car2043x0"

table(humanR2results$Identifiant, useNA = "always")
humanR2results$id <- tolower(gsub("[ -]", "", humanR2results$Identifiant))

# humanR2results$id <- substr(tolower(gsub("[ -]", "", humanR2results$Identifiant)), 1, 8)
# humanR2results$id <- substr(tolower(gsub(" ", "", humanR2results$id)), 1, 8)
# humanR2results$id <- substr(tolower(gsub("'", "", humanR2results$id)), 1, 8)
humanR2results$id[humanR2results$id=="???"] <- NA
humanR2results$id[humanR2results$id=="car011"] <- "car1011"
humanR2results$id[humanR2results$id=="car014"] <- "car1014"
humanR2results$id[humanR2results$id=="car015"] <- "car1015"
humanR2results$id[humanR2results$id=="car016"] <- "car1016"
humanR2results$id[humanR2results$id=="car017"] <- "car1017"
humanR2results$id[humanR2results$id=="car018"] <- "car1018"
humanR2results$id[humanR2results$id=="car021"] <- "car1021"
humanR2results$id[humanR2results$id=="car2115"] <- "car21115"
humanR2results$id[humanR2results$id=="car2117"] <- "car21117"

table(humanR2results$id, useNA = "always")

# reformat date
table(humanR2results$`Date reception`, useNA = "always")
humanR2results$receptiondate <- as.Date(humanR2results$`Date reception`) 

# merge both dataframes
humanR2merged <- merge(HHR2, humanR2results, by = "id", all = T)

table(duplicated(humanR2merged$cs_id_individu))
dups = humanR2merged$cs_id_individu[duplicated(humanR2merged$cs_id_individu)]
dups = dups[!is.na(dups)]

#View(humanR2merged[humanR2merged$cs_id_individu %in% dups,])
dupidhumanlabresults =  humanR2results %>% filter(duplicated(humanR2results$id)) %>% select(id, receptiondate)

humanR2merged$rodent <- ifelse(humanR2merged$grappe %in% rodentclusters, "yes", "no")

# identify observations that are not merged
unmatchedidinhumanlabresultsR2 <- humanR2merged %>% filter(is.na(`_submission__id`)==T) %>% select(id, receptiondate)
unmatchedidinkoboR2 <- humanR2merged %>% filter(is.na(receptiondate)==T) %>% select(id, today, grappe, rodent, menage_id)
unmatchedidinhumanlabresultsR2 # lab results that can't be matched to Kobo
unmatchedidinkoboR2 # Kobo entries with sample number that can't be matched to lab result

#View(humanR2results %>% filter(id %in% unmatchedidinhumanlabresultsR2$id))

# export to csv files
write.csv(unmatchedidinhumanlabresultsR2, "./Data/DRC/Clean/Checks/unmatchedidinhumanlabresultsR2.csv")
write.csv(unmatchedidinkoboR2, "./Data/DRC/Clean/Checks/unmatchedidinkoboR2.csv")


## ROUND 3 ##
#######################################

# import lab result datasets
humanR3results <- read_excel("Data/DRC/Raw/householdsurvey/T3_SELH_BDD_CABU_LAB_27022024_V2.xlsx")
humanR3results <- humanR3results %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
humanR3results <- humanR3results %>% filter(!is.na(Identifiant)|!is.na(`Date reception`)|!is.na(`Escherichia coli`)) # remove empty rows

table(humanR3results$Identifiant) # a view have multiple samples

# import household visits - individual data
HHindividualR3 <- read_excel("./Data/DRC/Raw/householdsurvey/R3_CABU_ENQ_COM_-_all_versions_-_labels_-_2024-06-04-19-31-46.xlsx", 
                             sheet = "selles_members")

##
# NEED TO ADJUST COLUMN NAMES
####################################
names(HHindividualR3)

HHindividualR3 = HHindividualR3 %>%
  clean_names()

names(HHindividualR3)
names(HHindividualR2)

# remove 'Nom' and 'Prénom' columns
HHindividualR3 <- HHindividualR3 %>% select(-(c(nom, prenom, 
                                              date_de_naissance_ou_age_connu,
                                              date_de_naissance_2,
                                              age_15,
                                              date_du_consentement_donne_16,
                                              heure_de_production_des_selles_defecation_17,
                                              heure_de_collecte_des_echantillons_18,
                                              submission_validation_status,
                                              submission_notes,
                                              submission_tags))) 

names(HHindividualR3)
names(HHindividualR2)

names(HHindividualR3) = names(HHindividualR2[!names(HHindividualR2)=="ageyears"])

# remove dob
table(HHindividualR3$age, useNA = "always")
table(HHindividualR3$dob_age[is.na(HHindividualR3$age)], useNA = "always")
#HHindividualR2$dob <- as.Date(HHindividualR2$dob, format = "%d/%m/%Y")
HHindividualR3$age <- tolower(HHindividualR3$age) # clean var age
HHindividualR3$age <- gsub(" ", "", HHindividualR3$age) # remove spaces
HHindividualR3$ageyears <- as.numeric(gsub("ans", "", HHindividualR3$age, fixed = TRUE))
HHindividualR3$ageyears[grepl("mois", HHindividualR3$age)==T] <- as.numeric(gsub("mois", "", HHindividualR3$age[grepl("mois", HHindividualR3$age)==T], fixed = TRUE))
HHindividualR3$ageyears[grepl("mois", HHindividualR3$age)] <- HHindividualR3$ageyears[grepl("mois", HHindividualR3$age)] / 12
#HHindividualR3$ageyears[is.na(HHindividualR3$age) & !is.na(HHindividualR3$dob_age)] <- round((as.Date("2023-01-30") - HHindividualR3$dob_age[is.na(HHindividualR3$age) & !is.na(HHindividualR3$dob_age)])/365.25, 0)
table(HHindividualR3$ageyears, useNA = "always") # 0 missing age - no one under 2 yo
HHindividualR3 <- HHindividualR3 %>% select(-dob_age)
HHindividualR3 <- HHindividualR3 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# import household visits - household data
HHlocationR3 <- read_excel("./Data/DRC/Raw/householdsurvey/R3_CABU_ENQ_COM_-_all_versions_-_labels_-_2024-06-04-19-31-46.xlsx", 
                           sheet = "R3 CABU_ENQ_COM")
# Read in column names translated to english
cnameseng = read_excel("./Data/DRC/Raw/householdsurvey/hh_colnames_fr_en.xlsx")
names(HHlocationR3)
names(HHlocationR2)

names(HHlocationR3) = names(cnameseng)

HHlocationR3 <- HHlocationR3 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# merge household visit individual and household data (including location)
HHR3 <- merge(HHindividualR3, HHlocationR3, by.x = "_submission__id", by.y = "_id", all = T)
HHR3_stoolparticipantsonly <- merge(HHindividualR3, HHlocationR3, by.x = "_submission__id", by.y = "_id", all.x = T)

# merge household visit data with stool lab results
# make a var with a common id (of sample)
table(HHR3$num_echantillon, useNA = "always")

HHR3$id = tolower(gsub("[ -]", "",HHR3$num_echantillon))
table(HHR3$id, useNA = "always")

table(humanR3results$Identifiant, useNA = "always")
humanR3results$id <- tolower(gsub("[ -]", "", humanR3results$Identifiant))

table(humanR3results$id, useNA = "always")

# reformat date
table(humanR3results$`Date reception`, useNA = "always")
humanR3results$receptiondate <- as.Date(humanR3results$`Date reception`) 

# merge both dataframes
humanR3merged <- merge(HHR3, humanR3results, by = "id", all = T)
humanR3merged$rodent <- ifelse(humanR3merged$grappe %in% rodentclusters, "yes", "no")

# identify observations that are not merged
unmatchedidinhumanlabresultsR3 <- humanR3merged %>% filter(is.na(`_submission__id`)==T) %>% select(id, receptiondate)
unmatchedidinkoboR3 <- humanR3merged %>% filter(!is.na(num_echantillon)==T & is.na(receptiondate)==T) %>% select(id, today, grappe, rodent, menage_id)
unmatchedidinhumanlabresultsR3 # lab results that can't be matched to Kobo
unmatchedidinkoboR3 # Kobo entries with sample number that can't be matched to lab result

#View(humanR2results %>% filter(id %in% unmatchedidinhumanlabresultsR2$id))

# export to csv files
write.csv(unmatchedidinhumanlabresultsR3, "./Data/DRC/Clean/Checks/unmatchedidinhumanlabresultsR3.csv")
write.csv(unmatchedidinkoboR3, "./Data/DRC/Clean/Checks/unmatchedidinkoboR3.csv")

#View(humanR3merged %>% filter(id %in% unmatchedidinhumanlabresultsR3$id))

### LINK ROUNDS ####
# ID individual
table(HHR1_stoolparticipantsonly$cs_id_individu, useNA = "always")
table(HHR2$cs_id_individu, useNA = "always")
table(HHR3_stoolparticipantsonly$cs_id_individu, useNA = "always")

humanR3merged$cs_id_individu[humanR3merged$cs_id_individu=="10\r\nMND0101"] = "10MND0101"
humanR3merged$cs_id_individu[humanR3merged$cs_id_individu=="13NGB0201\r\n"] = "13NGB0201"

# keep only ID and date in simplified data frames
HHR1_IDdate <- humanR1merged %>% filter(!is.na(cs_id_individu)) %>% select(cs_id_individu, id, ageyears, age, sexe, grappe)
#View(HHR1_IDdate[HHR1_IDdate$cs_id_individu%in%HHR1_IDdate$cs_id_individu[duplicated(HHR1_IDdate$cs_id_individu)],])

# So it seems different stool samples are matched to the same HDSS id
HHR2_IDdate <- humanR2merged %>% select(cs_id_individu, id, ageyears, age, sexe, grappe)

HHR3_IDdate <- humanR3merged %>% filter(!is.na(cs_id_individu)) %>% select(cs_id_individu, id, ageyears, age, sexe, grappe)

# merge IDs of both rounds
HHvisits_selles_R1R2 <- merge(HHR1_IDdate, HHR2_IDdate, by = "cs_id_individu", all=T)
HHvisits_selles_R1R2R3 <- merge(HHvisits_selles_R1R2, HHR3_IDdate, by = "cs_id_individu", all = T)

#nam = names(HHvisits_selles_R1R2R3[2:length(HHvisits_selles_R1R2R3)])
#com_nam = paste(nam,collapse=", ")

#HHvisits_selles_R1R2R3 <- HHvisits_selles_R1R2R3 %>% filter(!is.na(com_nam)) # remove empty rows
HHvisits_selles_R1R2R3c = HHvisits_selles_R1R2R3  

# Impute individual data from the other rounds, hence also identify which lab id belongs to which HDSS ID
for(i in 1:length(HHvisits_selles_R1R2R3$cs_id_individu)){
  if(is.na(HHvisits_selles_R1R2R3[i,c(3)])){
    HHvisits_selles_R1R2R3c[i,c(3:6)] =  HHvisits_selles_R1R2R3[i,c(8:11)]
  }
    if(is.na(HHvisits_selles_R1R2R3c[i,c(3)])){
      HHvisits_selles_R1R2R3c[i,c(3:6)] =  HHvisits_selles_R1R2R3[i,c(13:16)]
  }
}

# Keep rows with observations
HHvisits_selles_R1R2R3csel = HHvisits_selles_R1R2R3c %>% select(cs_id_individu, id.x, ageyears.x, age.x,sexe.x, grappe.x,id.y,id)
table(duplicated(HHvisits_selles_R1R2R3csel$cs_id_individu))
dups = HHvisits_selles_R1R2R3csel$cs_id_individu[duplicated(HHvisits_selles_R1R2R3csel$cs_id_individu)]

HHvisits_selles_R1R2R3csel$duplicated = ifelse(HHvisits_selles_R1R2R3csel$cs_id_individu %in%dups, 1,0)
HHvisits_selles_R1R2R3csel = HHvisits_selles_R1R2R3csel %>%
  rename(id.r1 = "id.x",
         id.r2 = "id.y",
         id.r3 = "id",
         ageyears = "ageyears.x",
         age = "age.x",
         sexe = "sexe.x",
         grappe = "grappe.x") %>%
  select(cs_id_individu, id.r1,id.r2,id.r3,ageyears,age,sexe,grappe, duplicated) %>%
  mutate(grappe = tolower(grappe))
table(HHvisits_selles_R1R2R3csel$grappe)


# mark which clusters rodents were collected
rodentclusters <- c("cellule_mbuka3", "kiandu", "kilueka", "kimaku", "lukengezi_et_poste", "malanga")
HHvisits_selles_R1R2R3csel$rodent <- ifelse(HHvisits_selles_R1R2R3csel$grappe %in% rodentclusters, "yes", "no")
table(HHvisits_selles_R1R2R3csel$grappe, HHvisits_selles_R1R2R3csel$rodent)

# All individuals included?
allis = c(unique(humanR1merged$cs_id_individu), unique(humanR2merged$cs_id_individu),unique(humanR3merged$cs_id_individu))
length(unique(allis)) # Should be 447 individuals
table(HHvisits_selles_R1R2R3csel$duplicated) # 484-32 = 452

# Still some cleaning needed in cs_id_individu
unique(allis)
unique(HHvisits_selles_R1R2R3csel$cs_id_individu)

# cleaning
HHvisits_selles_R1R2R3csel$cleaning_needed = ifelse(nchar(HHvisits_selles_R1R2R3csel$cs_id_individu)<9|nchar(HHvisits_selles_R1R2R3csel$cs_id_individu)>10,1,0)
HHvisits_selles_R1R2R3csel$cs_id_individu[HHvisits_selles_R1R2R3csel$cleaning_needed==1]

# export to csv
write.csv(HHvisits_selles_R1R2R3csel, "./Data/DRC/Clean/linked_final/HHvisits_selles_R1R2R3.csv")


# STILL NEED TO CORRECTLY LINK THOSE MISSING
# create a list of IDs for which to check the result (neither RAS or OUI)
missingESBLresultshumanR1 <- humanR1merged %>% filter(is.na(humanR1merged$`Escherichia coli`)) %>% select(id, receptiondate)
missingESBLresultshumanR2 <- humanR2merged %>% filter(is.na(humanR2merged$`Escherichia coli`)) %>% select(id, receptiondate)

# export
write.csv(missingESBLresultshumanR1, "./Data/DRC/Clean/Checks/missingESBLresultshumanR1.csv")
write.csv(missingESBLresultshumanR2, "./Data/DRC/Clean/Checks/missingESBLresultshumanR2.csv")


### 2. EXTRACTION SELECTION HUMAN STOOLS ####
# show for each cluster the number of isolates and ESBL E. coli positives
table(humanR1merged$grappe, humanR1merged$`Escherichia coli`, useNA = "always")
table(humanR2merged$grappe, humanR2merged$`Escherichia coli`, useNA = "always")
table(humanR3merged$grappe, humanR3merged$`Escherichia coli`, useNA = "always")

# mark which clusters rodents were collected
rodentclusters <- c("cellule_mbuka3", "kiandu", "kilueka", "kimaku", "lukengezi_et_poste", "malanga")
humanR1merged$rodent <- ifelse(humanR1merged$grappe %in% rodentclusters, "yes", "no")
table(humanR1merged$grappe, humanR1merged$rodent)
humanR2merged$rodent <- ifelse(humanR2merged$grappe %in% rodentclusters, "yes", "no")
table(humanR2merged$grappe, humanR2merged$rodent)

humanR3merged$grappe <- tolower(humanR3merged$grappe)
humanR3merged$grappe <- gsub(" ", "_",humanR3merged$grappe)

humanR3merged$rodent <- ifelse(humanR3merged$grappe %in% rodentclusters, "yes", "no")
table(humanR3merged$grappe, humanR3merged$rodent)

# number of samples collected and ESBL identified in villages where rodents were collected
table(humanR1merged$rodent, humanR1merged$`Escherichia coli`, useNA = "always") # 46 ESBL E. coli identified in humans in villages where rodents were collected
table(humanR2merged$rodent, humanR2merged$`Escherichia coli`, useNA = "always") # 16 ESBL E. coli identified in humans in villages where rodents were collected
table(humanR3merged$rodent, humanR3merged$`Escherichia coli`, useNA = "always") # 20 ESBL E. coli identified in humans in villages where rodents were collected

dnaR1 = (humanR1merged %>% group_by(grappe) %>% filter(rodent == "yes",`Escherichia coli`== "OUI") %>%
           select(id)) %>%
  mutate(round = "r1")

dnaR2 = (humanR2merged %>% group_by(grappe) %>% filter(rodent == "yes",`Escherichia coli`== "OUI") %>%
           select(id)) %>%
  mutate(round = "r2")

dnaR3 = (humanR3merged %>% group_by(grappe) %>% filter(rodent == "yes",`Escherichia coli`== "OUI") %>%
           select(id)) %>%
  mutate(round = "r3")

dna = rbind(dnaR1,dnaR2,dnaR3)

# current_dna_selection to compare
dna_v1 = read_excel("./Data/DRC/Raw/dnaextractions/dna_extractions_v1.xlsx")
clusters = read_excel("./Data/DRC/Clean/cluster_interv_control.xlsx")
clusters$Grappe = tolower(clusters$Grappe)
clusters = clusters %>%
  rename(grappe = "Grappe")
  
dna_v1$id = tolower(gsub("[ -]", "", dna_v1$id))

dna$missing_in_v1 = ifelse(dna$id %in% dna_v1$id, "no", "yes")
dna_v1$missing_in_v2 = ifelse(dna_v1$id %in% dna$id, "no", "yes")
dna_v1$grappe = tolower(dna_v1$grappe)

# Missing from current selection
table(dna$missing_in_v1)
table(dna_v1$missing_in_v2)
dna = merge(dna_v1, dna, by=c("id", "round", "grappe"), all=T)
dna %>% filter(missing_in_v1=="yes")

write.csv(dna, "./Data/DRC/Raw/dnaextractions/dna_extractions_v2.csv")

##########################
# ADDITIONAL VILLAGE

# COMBINE ALL DATASETS
names(humanR1merged)
hr2 = humanR2merged[,which(names(humanR2merged)%in%names(humanR1merged))]
hr1 = humanR1merged[,which(names(humanR1merged)%in%names(hr2))]
hr3 = humanR3merged[,which(names(humanR3merged)%in%names(hr2))]
hr1 = hr1[,which(names(hr1)%in%names(hr3))]
hr2 = hr2[,which(names(hr2)%in%names(hr3))]

hr1$round = "r1"
hr2$round = "r2"
hr3$round = "r3"
sapply(hr1, function(x) class(x))
sapply(hr2, function(x) class(x))

hr1$`_submission_time` = as.Date(hr1$`_submission_time`) # Date
hr1$`_submission__submission_time` = as.Date(hr1$`_submission__submission_time`)
hr1$`Date reception`= as.Date(as.numeric(hr1$`Date reception`, origin="1899-12-30")) # Date
hr1$receptiondate  = as.Date(hr1$receptiondate) # Date
hr1$today = as.Date(hr1$today)
hr1 = hr1 %>% select(-c(`Heure de Recpt`))

hr2$`_submission_time` = as.Date(hr2$`_submission_time`) # Date
hr2$`_submission__submission_time` = as.Date(hr2$`_submission__submission_time`)
hr2$`Date reception`= as.Date(hr2$`Date reception`) # Date
hr2$receptiondate  = as.Date(hr2$receptiondate) # Date
hr2$today = as.Date(hr2$today)
hr2 = hr2 %>% select(-c(`Heure de Recpt`))

hr3$`_submission_time` = as.Date(hr3$`_submission_time`) # Date
hr3$`_submission__submission_time` = as.Date(hr3$`_submission__submission_time`)
hr3$`Date reception`= as.Date(hr3$`Date reception`) # Date
hr3$receptiondate  = as.Date(hr3$receptiondate) # Date
hr3$today = as.Date(hr3$today)
hr3 = hr3 %>% select(-c(`Heure de Recpt`))

#unique(humanALLmerged$grappe)

humanALLmerged = rbind(hr1,hr2,hr3) %>%
  mutate(grappe = ifelse(grappe =="q3_yanga", "q3(as_yanga_dia_songa)",
                         ifelse(grappe == "q3", "q3(as_kimbanguiste)", grappe)),
         ruralurban = ifelse(grappe%in%c("q2bis", "q2","q3(as_yanga_dia_songa)","q3(as_kimbanguiste)","mont_fleury_a_et_b","cellule_masamuna","lukengezi_et_poste","cellule_mbuka3"),
                              "urban","rural"),
         
         intervention = ifelse(grappe %in% clusters$grappe[clusters$intervention=="intervention"], "intervention", "control"),
         check_kobo_link = receptiondate - today) %>%
  mutate(need_check = ifelse((check_kobo_link >10 | check_kobo_link < -10), 1,0))
table(humanALLmerged$ruralurban)
table(humanALLmerged$grappe,humanALLmerged$intervention)

table(humanALLmerged$need_check) # 99 need their kobo linked checked

# Filter only those positive for e. coli
humanALLmerged_pos = humanALLmerged %>% filter(`Escherichia coli`== "OUI")

# positives per village
pos_c = humanALLmerged_pos %>% group_by(grappe, rodent, ruralurban, intervention) %>%
  summarise(n()) %>% 
  tibble()
#View(pos_c)

# Which villages no unlinked kobo results (i.e. no lab data) 
#kobounlinkedclusters = c(unique(c(unmatchedidinkobo$grappe,unmatchedidinkoboR2$grappe,unmatchedidinkoboR3$grappe)),"kinanga", "kiasungua") # last two added based on lab id in round 3
#kobounlinkedclusters = c("kinanga","kiasungua","viaza","cellule_masamuna", "q3(as_kimbanguiste)") # manually based on where more than 3 are missing
#potentialclusters = unique(humanALLmerged$grappe[!humanALLmerged$grappe %in% kobounlinkedclusters&humanALLmerged$ruralurban =="rural"&humanALLmerged$rodent=="no"])
potentialclusters = unique(humanALLmerged$grappe[!humanALLmerged$grappe %in% rodentclusters & humanALLmerged$ruralurban=="rural"])

to_select = humanALLmerged_pos %>% filter(grappe %in% potentialclusters) %>% group_by(grappe,intervention, ruralurban) %>%
  summarise(n())


# MAKE LIST TAKING NGOMBE1 en VUNDA NSOLE as extra villages
to_select_vil = humanALLmerged_pos %>% filter(grappe %in% c("ngombe1", "vunda_nsole") & `Escherichia coli`== "OUI") %>% select(id, round, grappe)
dna_extra = dna %>% filter(missing_in_v1== "yes") %>% select(id,round,grappe)

dna_extra = rbind(to_select_vil, dna_extra)

write.csv(to_select, "./Data/DRC/Raw/dnaextractions/potential_villages_forfurtherextractions.csv")
write_xlsx(to_select_vil, "./Data/DRC/Raw/dnaextractions/dna_extra.xlsx")


# SEE HOW MANY WITH THREE POSITIVE E. COLIs over time
HHvisits_selles_R1R2R3posr1 = left_join(HHvisits_selles_R1R2R3csel%>%filter(!is.na(id.r1)), humanR1merged%>%select(c(cs_id_individu,`Escherichia coli`))%>%filter(`Escherichia coli`=="OUI"), by="cs_id_individu")
HHvisits_selles_R1R2R3posr1 = HHvisits_selles_R1R2R3posr1 %>% filter(!is.na(`Escherichia coli`))


HHvisits_selles_R1R2R3posr2 = left_join(HHvisits_selles_R1R2R3csel%>%filter(!is.na(id.r2)), humanR2merged%>%select(c(cs_id_individu,`Escherichia coli`))%>%filter(`Escherichia coli`=="OUI"), by="cs_id_individu")
HHvisits_selles_R1R2R3posr2 = HHvisits_selles_R1R2R3posr2 %>% filter(!is.na(`Escherichia coli`))

HHvisits_selles_R1R2R3posr3 = left_join(HHvisits_selles_R1R2R3csel%>%filter(!is.na(id.r3)), humanR3merged%>%select(c(cs_id_individu,`Escherichia coli`))%>%filter(`Escherichia coli`=="OUI"), by="cs_id_individu")
HHvisits_selles_R1R2R3posr3 = HHvisits_selles_R1R2R3posr3 %>% filter(!is.na(`Escherichia coli`))


HHvisits_selles_complete = merge(HHvisits_selles_R1R2R3posr1, HHvisits_selles_R1R2R3posr2, by="cs_id_individu")
HHvisits_selles_complete = merge(HHvisits_selles_complete, HHvisits_selles_R1R2R3posr3, by="cs_id_individu") # Only three individuals have three positive e.colis


### PSEUDONYMIZE AND EXPORT CLEANED DATA ####
# STILL NEED TO DO CLEANING, NOTABLY OF CS_ID_INDIVIDUE VARIABLE which has currently names in there and/or unlikely difference between 'today' and 'receptiondate'

# aggregate age
humanR1merged$agegr10 <- cut(humanR1merged$ageyears, 
                               breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                               labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                               include.lowest = TRUE)
humanR2merged$agegr10 <- cut(humanR2merged$ageyears, 
                             breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                             labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                             include.lowest = TRUE)

humanR3merged$agegr10 <- cut(humanR3merged$ageyears, 
                             breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                             labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                             include.lowest = TRUE)

humanALLmerged$agegr10 <- cut(humanALLmerged$ageyears, 
                             breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                             labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                             include.lowest = TRUE)

# remove all identifying variables
########################################################################



humanR1_pseudo <- humanR1merged %>% select("menage_id","grappe","today","id","cs_id_individu", "agegr10","age", "sexe","date_consentement","date_recuperation_selle","nmbre_personne_menage_001",
                                           "nbre_enf_0_5ans_001","nbre_menage_conc","q1_diarrhee_prevenu","q1_diarrhee_prevenu/garder_le_r_cipient_d_eau_couvert_dans_l",
                                           "q1_diarrhee_prevenu/ne_pas_tremper_les_doigts_dans_le_verre_","q1_diarrhee_prevenu/utiliser_un_ustensile_avec_une_poign_e_p",
                                           "q1_diarrhee_prevenu/couvrir_les_aliments","q1_diarrhee_prevenu/faire_bouillir_l_eau_de_boisson","q1_diarrhee_prevenu/filtrer_l_eau_de_boisson",
                                           "q1_diarrhee_prevenu/autre_pr_ciser_","q1_diarrhee_prevenu/on_ne_peut_pas_l_viter","q1_diarrhee_prevenu/ne_sait_pas",
                                           "q2_source_princ_saison_seche","q3_source_princ_saison_pluv","q4_bidon_stock","q5a_bidon_ferme_rempli",
                                           "q5b_bidon_ferme_vide","q5c_bidon_nettoye","q6_traite_eau","q7_type_inst_sanitaire",
                                           "q8_autr_lieu_defecation","q8_autr_lieu_defecation/toilettes_avec_chasse_d_eau_raccord_e___","q8_autr_lieu_defecation/latrines___fosse_am_lior_e_avec_ventilat",
                                           "q8_autr_lieu_defecation/latrines___fosse_avec_dalle","q8_autr_lieu_defecation/latrine___fosse_sans_dalle","q8_autr_lieu_defecation/d_f_cation_en_plein_air",
                                           "q8_autr_lieu_defecation/autre","q9_toilette_partagee","q10_combien_partag","q11_dernier_nettoyage",
                                           "q12_elimine_selle_enf","q13_vidange_toilette","q14_produit_lavag_main","q15_lave_apr_defec",
                                           "q16_lave_apr_repas","q17_animaux_menage","q18_animaux_interieur","q18_animaux_interieur/1__b_ufs",
                                           "q18_animaux_interieur/2__moutons_ou_ch_vres","q18_animaux_interieur/3__porcs","q18_animaux_interieur/4___nes_chevaux",
                                           "q18_animaux_interieur/5__poules_oies_ou_canards","q18_animaux_interieur/6__autre_sp_cifiez","q18_autre_specifie",
                                           "q19_animaux_dehors","q19_animaux_dehors/1__b_ufs","q19_animaux_dehors/2__moutons_ou_ch_vres","q19_animaux_dehors/3__porcs",
                                           "q19_animaux_dehors/4___nes_chevaux","q19_animaux_dehors/5__poules_oies_ou_canards","q19_animaux_dehors/6__autre_sp_cifiez",
                                           "q20_excrement_animaux","q21_animal_malade","q21_animal_malade/1__faire_venir_un_v_t_rinaire_qui_peut_p",
                                           "q21_animal_malade/2__acheter_un_traitement_sans_consultati","q21_animal_malade/3__vendre_l_animal___un_boucher",
                                           "q21_animal_malade/4__on_l_abat_et_on_consomme_la_viande___","q21_animal_malade/5__quand_l_animal_meurt_on_le_jette_ou_o",
                                           "q21_animal_malade/6__quand_l_animal_meurt_on_consomme_la_v","q1_dispo_medica_menag","q2_nbre_sorte_medica",
                                           "Un_des_membres_de_votre_m_nage","_uuid","_submission_time","Escherichia coli","salmonella","cip_d","cip_interpr",
                                           "merop_d","merop_interpr","cef_d","cef_int","cotrim_d","cotrim_int","ampi_d","ampi_int","amoxiclav_d","amoxiclav_int",
                                           "ertap_d","ertap_int","genta_d","genta_int","pip_d","pip_int","amik_d","amik_int","pefl_d","pefl_int","cefep_d","cefep_int",
                                           "receptiondate")
humanR2_pseudo <- humanR2merged %>% select("menage_id","grappe","today","id","cs_id_individu", "agegr10","age", "sexe","date_consentement","date_recuperation_selle","_uuid","_submission_time","Escherichia coli","salmonella","cip_d","cip_interpr",
                                           "merop_d","merop_interpr","cef_d","cef_int","cotrim_d","cotrim_int","ampi_d","ampi_int","amoxiclav_d","amoxiclav_int",
                                           "ertap_d","ertap_int","genta_d","genta_int","pip_d","pip_int","amik_d","amik_int","pefl_d","pefl_int","cefep_d","cefep_int",
                                           "receptiondate")

humanR3_pseudo <- humanR3merged %>% select("menage_id","grappe","today","id","cs_id_individu", "agegr10","age", "sexe","date_consentement","date_recuperation_selle","nmbre_personne_menage_001",
                                           "nbre_enf_0_5ans_001","nbre_menage_conc","q1_diarrhee_prevenu","q1_diarrhee_prevenu/garder_le_r_cipient_d_eau_couvert_dans_l",
                                           "q1_diarrhee_prevenu/ne_pas_tremper_les_doigts_dans_le_verre_","q1_diarrhee_prevenu/utiliser_un_ustensile_avec_une_poign_e_p",
                                           "q1_diarrhee_prevenu/couvrir_les_aliments","q1_diarrhee_prevenu/faire_bouillir_l_eau_de_boisson","q1_diarrhee_prevenu/filtrer_l_eau_de_boisson",
                                           "q1_diarrhee_prevenu/autre_pr_ciser_","q1_diarrhee_prevenu/on_ne_peut_pas_l_viter","q1_diarrhee_prevenu/ne_sait_pas",
                                           "q2_source_princ_saison_seche","q3_source_princ_saison_pluv","q4_bidon_stock","q5a_bidon_ferme_rempli",
                                           "q5b_bidon_ferme_vide","q5c_bidon_nettoye","q6_traite_eau","q7_type_inst_sanitaire",
                                           "q8_autr_lieu_defecation","q8_autr_lieu_defecation/toilettes_avec_chasse_d_eau_raccord_e___","q8_autr_lieu_defecation/latrines___fosse_am_lior_e_avec_ventilat",
                                           "q8_autr_lieu_defecation/latrines___fosse_avec_dalle","q8_autr_lieu_defecation/latrine___fosse_sans_dalle","q8_autr_lieu_defecation/d_f_cation_en_plein_air",
                                           "q8_autr_lieu_defecation/autre","q9_toilette_partagee","q10_combien_partag","q11_dernier_nettoyage",
                                           "q12_elimine_selle_enf","q13_vidange_toilette","q14_produit_lavag_main","q15_lave_apr_defec",
                                           "q16_lave_apr_repas","q17_animaux_menage","q18_animaux_interieur","q18_animaux_interieur/1__b_ufs",
                                           "q18_animaux_interieur/2__moutons_ou_ch_vres","q18_animaux_interieur/3__porcs","q18_animaux_interieur/4___nes_chevaux",
                                           "q18_animaux_interieur/5__poules_oies_ou_canards","q18_animaux_interieur/6__autre_sp_cifiez","q18_autre_specifie",
                                           "q19_animaux_dehors","q19_animaux_dehors/1__b_ufs","q19_animaux_dehors/2__moutons_ou_ch_vres","q19_animaux_dehors/3__porcs",
                                           "q19_animaux_dehors/4___nes_chevaux","q19_animaux_dehors/5__poules_oies_ou_canards","q19_animaux_dehors/6__autre_sp_cifiez",
                                           "q20_excrement_animaux","q21_animal_malade","q21_animal_malade/1__faire_venir_un_v_t_rinaire_qui_peut_p",
                                           "q21_animal_malade/2__acheter_un_traitement_sans_consultati","q21_animal_malade/3__vendre_l_animal___un_boucher",
                                           "q21_animal_malade/4__on_l_abat_et_on_consomme_la_viande___","q21_animal_malade/5__quand_l_animal_meurt_on_le_jette_ou_o",
                                           "q21_animal_malade/6__quand_l_animal_meurt_on_consomme_la_v","q1_dispo_medica_menag","q2_nbre_sorte_medica",
                                           "Un_des_membres_de_votre_m_nage","_uuid","_submission_time","Escherichia coli","salmonella","cip_d","cip_interpr",
                                           "merop_d","merop_interpr","cef_d","cef_int","cotrim_d","cotrim_int","ampi_d","ampi_int","amoxiclav_d","amoxiclav_int",
                                           "ertap_d","ertap_int","genta_d","genta_int","pip_d","pip_int","amik_d","amik_int","pefl_d","pefl_int","cefep_d","cefep_int",
                                           "receptiondate")

humanALL_pseudo <- humanALLmerged %>% select("menage_id","round","grappe","intervention","today","id","cs_id_individu", "agegr10","age", "sexe","date_consentement","date_recuperation_selle","nmbre_personne_menage_001",
                                           "nbre_enf_0_5ans_001","nbre_menage_conc","q1_diarrhee_prevenu","q1_diarrhee_prevenu/garder_le_r_cipient_d_eau_couvert_dans_l",
                                           "q1_diarrhee_prevenu/ne_pas_tremper_les_doigts_dans_le_verre_","q1_diarrhee_prevenu/utiliser_un_ustensile_avec_une_poign_e_p",
                                           "q1_diarrhee_prevenu/couvrir_les_aliments","q1_diarrhee_prevenu/faire_bouillir_l_eau_de_boisson","q1_diarrhee_prevenu/filtrer_l_eau_de_boisson",
                                           "q1_diarrhee_prevenu/autre_pr_ciser_","q1_diarrhee_prevenu/on_ne_peut_pas_l_viter","q1_diarrhee_prevenu/ne_sait_pas",
                                           "q2_source_princ_saison_seche","q3_source_princ_saison_pluv","q4_bidon_stock","q5a_bidon_ferme_rempli",
                                           "q5b_bidon_ferme_vide","q5c_bidon_nettoye","q6_traite_eau","q7_type_inst_sanitaire",
                                           "q8_autr_lieu_defecation","q8_autr_lieu_defecation/toilettes_avec_chasse_d_eau_raccord_e___","q8_autr_lieu_defecation/latrines___fosse_am_lior_e_avec_ventilat",
                                           "q8_autr_lieu_defecation/latrines___fosse_avec_dalle","q8_autr_lieu_defecation/latrine___fosse_sans_dalle","q8_autr_lieu_defecation/d_f_cation_en_plein_air",
                                           "q8_autr_lieu_defecation/autre","q9_toilette_partagee","q10_combien_partag","q11_dernier_nettoyage",
                                           "q12_elimine_selle_enf","q13_vidange_toilette","q14_produit_lavag_main","q15_lave_apr_defec",
                                           "q16_lave_apr_repas","q17_animaux_menage","q18_animaux_interieur","q18_animaux_interieur/1__b_ufs",
                                           "q18_animaux_interieur/2__moutons_ou_ch_vres","q18_animaux_interieur/3__porcs","q18_animaux_interieur/4___nes_chevaux",
                                           "q18_animaux_interieur/5__poules_oies_ou_canards","q18_animaux_interieur/6__autre_sp_cifiez","q18_autre_specifie",
                                           "q19_animaux_dehors","q19_animaux_dehors/1__b_ufs","q19_animaux_dehors/2__moutons_ou_ch_vres","q19_animaux_dehors/3__porcs",
                                           "q19_animaux_dehors/4___nes_chevaux","q19_animaux_dehors/5__poules_oies_ou_canards","q19_animaux_dehors/6__autre_sp_cifiez",
                                           "q20_excrement_animaux","q21_animal_malade","q21_animal_malade/1__faire_venir_un_v_t_rinaire_qui_peut_p",
                                           "q21_animal_malade/2__acheter_un_traitement_sans_consultati","q21_animal_malade/3__vendre_l_animal___un_boucher",
                                           "q21_animal_malade/4__on_l_abat_et_on_consomme_la_viande___","q21_animal_malade/5__quand_l_animal_meurt_on_le_jette_ou_o",
                                           "q21_animal_malade/6__quand_l_animal_meurt_on_consomme_la_v","q1_dispo_medica_menag","q2_nbre_sorte_medica",
                                           "Un_des_membres_de_votre_m_nage","_uuid","_submission_time","Escherichia coli","salmonella","cip_d","cip_interpr",
                                           "merop_d","merop_interpr","cef_d","cef_int","cotrim_d","cotrim_int","ampi_d","ampi_int","amoxiclav_d","amoxiclav_int",
                                           "ertap_d","ertap_int","genta_d","genta_int","pip_d","pip_int","amik_d","amik_int","pefl_d","pefl_int","cefep_d","cefep_int",
                                           "receptiondate")


# save databases
# write.csv(humanR1_pseudo, "./Data/DRC/Clean/linked_final/humanR1_Kim_pseudo.csv")
# write.csv(humanR2_pseudo, "./Data/DRC/Clean/linked_final/humanR2_Kim_pseudo.csv")
# write.csv(humanR3_pseudo, "./Data/DRC/Clean/linked_final/humanR3_Kim_pseudo.csv")
# write_xlsx(humanALL_pseudo, "./Data/DRC/Clean/linked_final/humanALL_Kim_pseudo.xlsx")


# NOW CLEAN THE WASH VARIABLES IN THE SAME FORMAT AS BF
#################################################################
# CLEAN HOUSEHOLD DATA
#################################################################

## STILL NEED TO CHECK IF LINKAGE WENT WELL AS FOR SOME CS_ID_INDIVIDUELLE THE AGE AND SEX
## IS CHANGING BETWEEN THE ROUNDS

# ALL ROUNDS
################################
humanALL_pseudo = humanALL_pseudo %>% mutate(
  #sexe = factor(sexe, levels=c(1,2), labels=c("Male", "Female")),
  date_consentement = as.Date(date_consentement, format="%d/%m/%Y"),
  date_recuperation_selle = as.Date(date_recuperation_selle,format="%d/%m/%Y"),
  age = tolower(age),
  age = as.numeric(age),
  round = factor(round, levels = c("r1","r2","r3"), labels = c("round_1_arm_1","round_2_arm_1","round_3_arm_1")),
  # agegr10 = cut(age, 
  #               breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
  #               labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
  #               include.lowest = TRUE),
  nmbre_personne_menage_001 = as.numeric(nmbre_personne_menage_001),
  nbre_enf_0_5ans_001 = as.numeric(nbre_enf_0_5ans_001),
  nbre_menage_conc = as.numeric(nbre_menage_conc), 
  #date_enquete = as.Date(date_enquete, format="%Y-%m-%d"),
  date_consentement = as.Date(date_consentement, format="%Y-%m-%d"),
  date_recuperation_selle = as.Date(date_recuperation_selle, format="%Y-%m-%d"))%>% 
  rename(
  n.householdmember = "nmbre_personne_menage_001",
  n.child.0to5 = "nbre_enf_0_5ans_001",
  n.households.concession = "nbre_menage_conc",
  #date.enquete = "today",
  date.consent = "date_consentement",
  date.stool.collection = "date_recuperation_selle",
  redcap_event_name = "round",
  village = "grappe",
  esble = "Escherichia coli",
  salm = "salmonella",
  q1.diar.prev.water.pot.covered = "q1_diarrhee_prevenu/garder_le_r_cipient_d_eau_couvert_dans_l",
  q1.diar.prev.no.finger.in.waterglass =  "q1_diarrhee_prevenu/ne_pas_tremper_les_doigts_dans_le_verre_",
  q1.diar.prev.utensil.to.take.water.from.pot = "q1_diarrhee_prevenu/utiliser_un_ustensile_avec_une_poign_e_p",    
  q1.diar.prev.cover.food = "q1_diarrhee_prevenu/utiliser_un_ustensile_avec_une_poign_e_p",
  q1.diar.prev.boil.water = "q1_diarrhee_prevenu/couvrir_les_aliments",
  q1.diar.prev.filter.water = "q1_diarrhee_prevenu/faire_bouillir_l_eau_de_boisson",
  q1.diar.prev.other = "q1_diarrhee_prevenu/autre_pr_ciser_",
  q1.diar.prev.cant.be.avoided = "q1_diarrhee_prevenu/on_ne_peut_pas_l_viter",
  q1.diar.prev.dont.know = "q1_diarrhee_prevenu/ne_sait_pas",
  q2.main.water.source.dry= "q2_source_princ_saison_seche",
  q3.main.water.source.rainy= "q3_source_princ_saison_pluv",
  q4.cans.storage.water= "q4_bidon_stock", 
  q5a.cans.storage.water.closed.when.filled= "q5a_bidon_ferme_rempli", 
  q5b.cans.storage.water.closed.wen.empty= "q5b_bidon_ferme_vide",          
  q5c.cans.cleaned.before.reuse= "q5c_bidon_nettoye", 
  q6.treatment.water= "q6_traite_eau", 
  q7.principle.defication= "q7_type_inst_sanitaire", 
  q8.other.defecation.flush.toiled.septic  = "q8_autr_lieu_defecation/toilettes_avec_chasse_d_eau_raccord_e___", 
  q8.other.defecation.pit.latrine.ventilation = "q8_autr_lieu_defecation/latrines___fosse_am_lior_e_avec_ventilat",
  q8.other.defecation.pit.latrine.slab = "q8_autr_lieu_defecation/latrines___fosse_avec_dalle",   
  q8.other.defecation.pit.latrine.no.slab = "q8_autr_lieu_defecation/latrine___fosse_sans_dalle",
  q8.other.defecation.open.defecation = "q8_autr_lieu_defecation/d_f_cation_en_plein_air",   
  q8.other.defecation.other = "q8_autr_lieu_defecation/autre",
  #q8.other.defecation.none = "q8_autr_lieu_defecation___7",
  q9.shared.toilet= "q9_toilette_partagee",
  q10.n.shared.toilet= "q10_combien_partag",
  q11.toilet.last.cleaned= "q11_dernier_nettoyage",
  q12.disposal.child.stool= "q12_elimine_selle_enf",
  q13.disposal.latrine.pit= "q13_vidange_toilette",
  q14.handwashing.product= "q14_produit_lavag_main",
  q15.handwashing.defecation= "q15_lave_apr_defec",
  q16.handwashing.meals= "q16_lave_apr_repas",
  q17.animals.around.household= "q17_animaux_menage",
  q18.animal.inside.cow = "q18_animaux_interieur/1__b_ufs",
  q18.animal.inside.sheep.goat = "q18_animaux_interieur/2__moutons_ou_ch_vres",       
  q18.animal.inside.pig = "q18_animaux_interieur/3__porcs",
  q18.animal.inside.donkey.horse = "q18_animaux_interieur/4___nes_chevaux",       
  q18.animal.inside.chicken.goose.duck = "q18_animaux_interieur/5__poules_oies_ou_canards",
  q18.animal.inside.other = "q18_animaux_interieur/6__autre_sp_cifiez",
  q19.animal.outside.cow = "q19_animaux_dehors/1__b_ufs",
  q19.animal.outside.sheep.goat = "q19_animaux_dehors/2__moutons_ou_ch_vres",       
  q19.animal.outside.pig = "q19_animaux_dehors/3__porcs",
  q19.animal.outside.donkey.horse = "q19_animaux_dehors/4___nes_chevaux",       
  q19.animal.outside.chicken.goose.duck = "q19_animaux_dehors/5__poules_oies_ou_canards",
  q19.animal.inside.other = "q19_animaux_dehors/6__autre_sp_cifiez",
  q20.animal.excrement.floor= "q20_excrement_animaux", 
  q21.when.animal.ill.treatment.with.vet = "q21_animal_malade/1__faire_venir_un_v_t_rinaire_qui_peut_p",           
  q21.when.animal.ill.treatment.without.vet= "q21_animal_malade/2__acheter_un_traitement_sans_consultati",  
  q21.when.animal.ill.sell.bucher = "q21_animal_malade/3__vendre_l_animal___un_boucher",  
  q21.when.animal.ill.slaugther.eat.meat.at.home= "q21_animal_malade/4__on_l_abat_et_on_consomme_la_viande___",  
  q21.when.animal.ill.dies.burie.dispose = "q21_animal_malade/5__quand_l_animal_meurt_on_le_jette_ou_o",             
  q21.when_animal.ill.dies.eat.at.home = "q21_animal_malade/6__quand_l_animal_meurt_on_consomme_la_v") %>%
  mutate(
    esble = ifelse(esble == "OUI", 1,0),
    today = as.Date(today,format = "%Y-%m-%d"),
    receptiondate = as.Date(receptiondate,format = "%Y-%m-%d"),
    date.check = receptiondate - date.consent,
    date.check.collect = receptiondate - date.stool.collection,
    date.use = as.character(date.stool.collection),
    date.use = ifelse(is.na(as.character(date.stool.collection)), 
                      as.character(date.consent),as.character(date.stool.collection)), # This replaced the empty dates with consent date as proxy
    date.use = as.Date(date.use, format = "%Y-%m-%d"),
    month = format(date.use,"%m"),
    #date.check.use = today - date.use,
    time = ifelse(redcap_event_name =="round_0_arm_1", 0,
                  ifelse(redcap_event_name == "round_1_arm_1", 1,
                         ifelse(redcap_event_name == "round_2_arm_1", 2,3))),
     rainy = ifelse(month%in%c("06","07","08","09"),"yes", "no"))

#%>% 
  # select(-c(date.check, date.check.collect))

# piam_q1 = as.numeric(piam_q1), # Not asked in DRC
# piam_q2a = as.numeric(piam_q2a),
# piam_q2b = factor(piam_q2b, levels=c(1,2), labels=c("Male", "Female")),
# piam_q6a = as.numeric(piam_q6a),
# piam_q6b = as.numeric(piam_q6b),
# piam_q6c = as.numeric(piam_q6c),
# piam_q6d = as.numeric(piam_q6d),
# piam_q6e = as.numeric(piam_q6e),
# piam_q6f = as.numeric(piam_q6f),
# piam_q6g = as.numeric(piam_q6g),
# piam_q6h = as.numeric(piam_q6h),
# q1_diarrhee_prevenu___1 = factor(q1_diarrhee_prevenu___1, levels=c(0,1), labels=c("No","Yes")),        
# q1_diarrhee_prevenu___2 = factor(q1_diarrhee_prevenu___2, levels=c(0,1), labels=c("No","Yes")),
# q1_diarrhee_prevenu___3 = factor(q1_diarrhee_prevenu___3, levels=c(0,1), labels=c("No","Yes")),        
# q1_diarrhee_prevenu___4 = factor(q1_diarrhee_prevenu___4, levels=c(0,1), labels=c("No","Yes")),
# q1_diarrhee_prevenu___5 = factor(q1_diarrhee_prevenu___5, levels=c(0,1), labels=c("No","Yes")),         
# q1_diarrhee_prevenu___6 = factor(q1_diarrhee_prevenu___6, levels=c(0,1), labels=c("No","Yes")),
# q1_diarrhee_prevenu___7 = factor(q1_diarrhee_prevenu___7, levels=c(0,1), labels=c("No","Yes")),         
# q1_diarrhee_prevenu___8 = factor(q1_diarrhee_prevenu___8, levels=c(0,1), labels=c("No","Yes")),
# q1_diarrhee_prevenu___9 = factor(q1_diarrhee_prevenu___9, levels=c(0,1), labels=c("No","Yes")), 
#   q2_source_princ_saison_seche = factor(q2_source_princ_saison_seche, levels=c(1:9), labels=c("Tap house", 
#                                                                                               "Tap concession",
#                                                                                               "Tap public/fountain", 
#                                                                                               "Borehole",
#                                                                                               "improved well (protected)",
#                                                                                               "unimproved well (unprotected)",
#                                                                                               "rainwater",
#                                                                                               "surface water (ponds, dams,rivers,lakes,pits,irrigation canals)",
#                                                                                               "bagged water")),
#   q3_source_princ_saison_pluv = factor(q3_source_princ_saison_pluv, levels=c(1:10), labels=c("Tap house", 
#                                                                                              "Tap concession",
#                                                                                              "Tap public/fountain", 
#                                                                                              "Borehole",
#                                                                                              "improved well (protected)",
#                                                                                              "unimproved well(unprotected)",
#                                                                                              "rainwater",
#                                                                                              "surface water (ponds, dams,rivers,lakes,pits,irrigation canals)",
#                                                                                              "bagged water", "bottled water")),  
#   q4_bidon_stock = factor(q4_bidon_stock, levels = c(1:3), labels=c("Yes, cans", "Yes, only one large tank", "No")),
#   q5a_bidon_ferme_rempli = factor(q5a_bidon_ferme_rempli, levels=c(1:2), labels=c("Yes", "No")),
#   q5b_bidon_ferme_vide = factor(q5b_bidon_ferme_vide, levels=c(1:2), labels=c("Yes", "No")),           
#   q5c_bidon_nettoye = factor(q5c_bidon_nettoye, levels=c(1,2,3,4,6), labels=c("Yes, with soap", "Yes, with water but no soap","Yes, boiled", "No", "Other")),
#   q6_traite_eau = factor(q6_traite_eau, levels = c(1:7), labels= c("No", "Yes,boiling", "Yes,cholinate/add desinfectant", "Yes, filter with cloth",
#                                                                    "Yes, filter with filter", "Yes, Solar desinfection (in the sun)","Yes, decant")),               
#   q7_type_inst_sanitaire = factor(q7_type_inst_sanitaire, levels = c(3,4,5), labels = c("pit latrine with slab", "pit latrine without slab", "open defecation")),
#   q8_autr_lieu_defecation___1 = factor(q8_autr_lieu_defecation___1, levels = c(0,1), labels = c("No","Yes")),      
#   q8_autr_lieu_defecation___2 = factor(q8_autr_lieu_defecation___2, levels = c(0,1), labels = c("No","Yes")),
#   q8_autr_lieu_defecation___3 = factor(q8_autr_lieu_defecation___3, levels = c(0,1), labels = c("No","Yes")),   
#   q8_autr_lieu_defecation___4 = factor(q8_autr_lieu_defecation___4, levels = c(0,1), labels = c("No","Yes")),
#   q8_autr_lieu_defecation___5 = factor(q8_autr_lieu_defecation___5, levels = c(0,1), labels = c("No","Yes")),   
#   q8_autr_lieu_defecation___6 = factor(q8_autr_lieu_defecation___6, levels = c(0,1), labels = c("No","Yes")),
#   q8_autr_lieu_defecation___7 = factor(q8_autr_lieu_defecation___7, levels = c(0,1), labels = c("No","Yes")),  
#   q9_toilette_partagee = factor(q9_toilette_partagee, levels=c(1:3), labels=c("Yes, other households (non-public)", "Yes, public", "No")),             
#   q10_combien_partag = as.numeric(q10_combien_partag),
#   q11_dernier_nettoyage = factor(q11_dernier_nettoyage, levels=c(1:6), labels=c("<24h", ">24h, but <1week", "1-4weeks", ">1month", "Never", "Don't know")), 
#   q12_elimine_selle_enf = factor(q12_elimine_selle_enf, levels = c(1:9), labels=c("Child used toilet/latrine", "Thrown/rinsed into toilet/latrine",
#                                                                                   "Thrown/rinsed into drainage pit",
#                                                                                   "Trown in garbage", "Buried", "Disposed in open air", 
#                                                                                   "Used as manure", "Other", "NA (no child)")),
#   q13_vidange_toilette = factor(q13_vidange_toilette, levels=c(1,2,4,5,7), labels=c("Has not been drained yet", "Don't know", "Removed by service provider and covered in pit",
#                                                                                     "Removed by service provider (don't know where)", "Emptied by hh in uncover pit/open ground")),
#   q14_produit_lavag_main = factor(q14_produit_lavag_main, levels=c(1:5), labels=c("Yes, soap", "Yes, detergent", "Yes, ash/mud/sand", "No, none available", "No, available but not used")),
#   q15_lave_apr_defec = factor(q15_lave_apr_defec, levels=c(1:4), labels=c("Yes, always", "Yes, often", "No or rarerly", "Not sure")),       
#   q16_lave_apr_repas = factor(q16_lave_apr_repas, levels=c(1:4), labels=c("Yes, always", "Yes, often", "No or rarerly", "Not sure")),
#   q17_animaux_menage = factor(q17_animaux_menage, levels=c(1:4), labels = c("Yes, inside and outside", "Yes, outside next to house","Yes, outside in demarked area", "No")),          
#   q18_animaux_interieur___1 = factor(q18_animaux_interieur___1, levels = c(0,1), labels = c("No", "Yes")),
#   q18_animaux_interieur___2 = factor(q18_animaux_interieur___2, levels = c(0,1), labels = c("No", "Yes")),       
#   q18_animaux_interieur___3 = factor(q18_animaux_interieur___3, levels = c(0,1), labels = c("No", "Yes")),
#   q18_animaux_interieur___4 = factor(q18_animaux_interieur___4, levels = c(0,1), labels = c("No", "Yes")),       
#   q18_animaux_interieur___5 = factor(q18_animaux_interieur___5, levels = c(0,1), labels = c("No", "Yes")),
#   q18_animaux_interieur___6 = factor(q18_animaux_interieur___6, levels = c(0,1), labels = c("No", "Yes")),
#   q19_animaux_dehors___1 = factor(q19_animaux_dehors___1, levels = c(0,1), labels = c("No", "Yes")),        
#   q19_animaux_dehors___2 = factor(q19_animaux_dehors___2, levels = c(0,1), labels = c("No", "Yes")),
#   q19_animaux_dehors___3 = factor(q19_animaux_dehors___3, levels = c(0,1), labels = c("No", "Yes")),        
#   q19_animaux_dehors___4 = factor(q19_animaux_dehors___4, levels = c(0,1), labels = c("No", "Yes")),
#   q19_animaux_dehors___5 = factor(q19_animaux_dehors___5, levels = c(0,1), labels = c("No", "Yes")),         
#   q19_animaux_dehors___6 = factor(q19_animaux_dehors___6, levels = c(0,1), labels = c("No", "Yes")),
#   
#   q20_excrement_animaux = factor(q20_excrement_animaux, levels = c(1,2,3), labels = c("Yes", "No", "Not possible to determine")),  
#   q21_animal_malade___1 = factor(q21_animal_malade___1, levels = c(0,1), labels = c("No", "Yes")),           
#   q21_animal_malade___2 = factor(q21_animal_malade___2, levels = c(0,1), labels = c("No", "Yes")),  
#   q21_animal_malade___3 = factor(q21_animal_malade___3, levels = c(0,1), labels = c("No", "Yes")),  
#   q21_animal_malade___4 = factor(q21_animal_malade___4, levels = c(0,1), labels = c("No", "Yes")),  
#   q21_animal_malade___5 = factor(q21_animal_malade___5, levels = c(0,1), labels = c("No", "Yes")),             
#   q21_animal_malade___6 = factor(q21_animal_malade___6, levels = c(0,1), labels = c("No", "Yes")),
#   eau_assainissement_hygine_complete = factor(eau_assainissement_hygine_complete, levels=c(0,2), labels=c("No", "Yes"))
# ) %>% select(-dob)

names(humanALL_pseudo)

human_stool_pseudo = humanALL_pseudo %>% filter(!is.na(cs_id_individu))


write.csv(humanALL_pseudo, "./Data/DRC/Clean/linked_final/drc_hh_all_r012.csv")
write.csv(human_stool_pseudo, "./Data/DRC/Clean/linked_final/drc_hh_stool_esble_r012.csv")

# Read in BF data to check comparison between variables
#humanALL_BF_pseudo = read.csv("./Data/BF/clean/linked_final/bf_hh_all_r0123.csv")


### 3. IMPORT & MERGE RODENT DATA ####
# import rodent stool bacterial culture results
rodentsR1results <- read_excel("db/rodentcollection/rodent_stool_bacterial_culture_results_Kim.xlsx", 
                               sheet = "M0")
rodentsR2results <- read_excel("db/rodentcollection/rodent_stool_bacterial_culture_results_Kim.xlsx", 
                               sheet = "M6")
# append rodent results
rodentsR1results$round <- "R1"
rodentsR2results$round <- "R2"
rodentsresults <- rbind(rodentsR1results, rodentsR2results)
# remove rows with missing 'Identifiant' and 'Date reception'
rodentsresults <- subset(rodentsresults, !(is.na(Identifiant) & is.na(`Date reception`)))
# reformat date
rodentsresults$receptiondate <- as.Date(rodentsresults$`Date reception`)
# reformat ID
rodentsresults$id <- substr(tolower(gsub("[ -]", "", rodentsresults$Identifiant)), 1, 6)
# check for duplicated values in rodentsresults$id
duplicates <- rodentsresults$id[duplicated(rodentsresults$id)]
# print the duplicated values
if (length(duplicates) > 0) {
  cat("Duplicated values in rodentsresults$id: ", paste(duplicates, collapse = ", "), "\n")
} else {
  cat("No duplicated values in rodentsresults$id\n")
} # kim481 kim923 
# remove duplicates
dups <- which(duplicated(rodentsresults%>%select(id))) 
rodentsresults <- rodentsresults %>% filter(!row.names(rodentsresults) %in% duplicates)

# import rodent characteristics, incl location
rodents_char <- read_excel("db/rodentcollection/20230803_FIELDLIST_KIM_RODENTS1-978.xlsx", 
                           sheet = "Captures")

# reformat ID of rodents_char
rodents_char$id <- substr(tolower(gsub("[ -]", "", rodents_char$ua_id)), 1, 6)

# remove the first 247 captured rodents (no correct bacteriology test done)
rodents_char <- rodents_char %>% filter(as.numeric(gsub("[^0-9]", "", id)) >= 248)

# merge rodent char with results
rodentmerged <- merge(rodents_char, rodentsresults, by = "id", all = T) # one observation to remove (duplicate lab result) once clarified what caused the duplicate

# identify mismatches
rodentmismatches_missingchar <- rodentmerged %>% filter(is.na(ua_id)) # no lab results without matched characteristic data
rodentmismatches_missingresults <- rodentmerged %>% filter(is.na(Identifiant)) %>% select(id, ua_id, day, month, year, Location) # 17 samples without result

# check if all have results
table(rodentsresults$`Escherichia coli`, useNA = "always")
table(rodentmerged$`Escherichia coli`, useNA = "always")

# export
write.csv(rodentmismatches_missingresults, "rodentmismatches_missingresults.csv")
write.csv(rodentmerged, "rodentmerged.csv")



### 4. WASH INDICATORS IN RODENT VILLAGES ####
# add var for whether rodents were collected in that village
HHlocationR1$rodent <- ifelse(HHlocationR1$cluster %in% rodentclusters, "yes", "no")
table(HHlocationR1$cluster, HHlocationR1$rodent)
# check how many households in rodent villages
HHlocation_rodents <- HHlocationR1 %>%
  filter(rodent=="yes") %>%
  group_by(`ID ménage`) %>%
  summarise(n=n())
count(HHlocation_rodents)
