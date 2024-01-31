################################################################################
# CABU-EICO household surveys and rodent collection, KIMPESE                   #
# Link stool bacterial culture results to household or rodent collection data  #
################################################################################

# install and load packages
pacman::p_load(readxl,lubridate,dplyr,ggplot2)

### 1 IMPORT & MERGE HUMAN HOUSEHOLD AND STOOL RESULT DATA ####
### ROUND 1 ####
# import lab result datasets
humanR1results <- read_excel("db/householdsurvey/human_stool_bacterial_culture_results_Kim.xlsx", 
                             sheet = "M0")
humanR1results <- humanR1results %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
humanR1results <- humanR1results %>% filter(!is.na(Identifiant)|!is.na(`Date reception`)|!is.na(`Escherichia coli`)) # remove empty rows

# import household visits - individual data
HHindividualR1 <- read_excel("db/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx", 
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
HHlocationR1 <- read_excel("db/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx", 
                         sheet = "CABU_enq_comm_2023")
HHlocationR1 <- HHlocationR1 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# merge household visit individual and HH data (including location)
HHR1 <- merge(HHindividualR1, HHlocationR1, by.x = "_submission__id", by.y = "_id", all = T)
HHR1_stoolparticipantsonly <- merge(HHindividualR1, HHlocationR1, by.x = "_submission__id", by.y = "_id", all.x = T)

# merge household visit data with stool lab results
# make a var with a common id (of sample)
table(HHR1$num_echantillon)
HHR1$id <- substr(tolower(gsub("[ -]", "", HHR1$num_echantillon)), 1, 7)
HHR1$id[HHR1$id=="czr1144"] <- "car1144" 
HHR1$id[HHR1$id=="acr1258"] <- "car1258" 
HHR1 <- HHR1 %>%  mutate(id = ifelse(grepl("^\\d{4}$", id), paste0("car", id), id))
table(HHR1$id)

table(humanR1results$Identifiant)
humanR1results$id <- substr(tolower(gsub("[ -]", "", humanR1results$Identifiant)), 1, 8)
humanR1results$id <- substr(tolower(gsub(" ", "", humanR1results$id)), 1, 8)
humanR1results$id <- substr(tolower(gsub("'", "", humanR1results$id)), 1, 8)
table(humanR1results$id)

# reformat date
table(humanR1results$`Date reception`, useNA = "always")
humanR1results$receptiondate <- as.Date(as.numeric(humanR1results$`Date reception`), origin = "1899-12-30")
humanR1results$receptiondate[humanR1results$`Date reception`=="06/042023"] <- as.Date("2023-04-06")
table(humanR1results$receptiondate, useNA = "always")

# merge both dataframes
humanR1merged <- merge(HHR1, humanR1results, by = "id", all = T)
str(humanR1merged)
# identify observations that are not merged
# identify ids in lab results not matching to kobo entry
unmatchedidinhumanlabresults <- humanR1merged %>% filter(is.na(`_submission__id`)==T) %>% select(id, receptiondate)
unmatchedidinkobo <- humanR1merged %>% filter(!is.na(num_echantillon)==T & is.na(receptiondate)==T) %>% select(id, today) # among those with stool sample collected (therefore there is a numechantillon), select those not received (no reception date)
unmatchedidinhumanlabresults # 4 lab results that can't be matched to Kobo
unmatchedidinkobo # 3 Kobo entries with sample number that can't be matched to lab result

# export to csv files
write.csv(unmatchedidinhumanlabresultsR1, "unmatchedidinhumanlabresultsR1.csv")
write.csv(unmatchedidinkoboR1, "unmatchedidinkoboR1.csv")

### ROUND 2 ####
# import lab result datasets
humanR2results <- read_excel("db/householdsurvey/human_stool_bacterial_culture_results_Kim.xlsx", 
                             sheet = "M6")
humanR2results <- humanR2results %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
humanR2results <- humanR2results %>% filter(!is.na(Identifiant)|!is.na(`Date reception`)|!is.na(`Escherichia coli`)) # remove empty rows

# import household visits - individual data
HHindividualR2 <- read_excel("db/householdsurvey/CABU_R2_-_all_versions_-_False_-_2024-01-31-10-14-10.xlsx", 
                             sheet = "group_io0xt32")

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

# import household visits - household data
HHlocationR2 <- read_excel("db/householdsurvey/CABU_R2_-_all_versions_-_False_-_2024-01-31-10-14-10.xlsx", 
                           sheet = "CABU_R2")
HHlocationR2 <- HHlocationR2 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# merge household visit individual and household data (including location)
HHR2 <- merge(HHindividualR2, HHlocationR2, by.x = "_submission__id", by.y = "_id", all = T)
HHR2_stoolparticipantsonly <- merge(HHindividualR2, HHlocationR2, by.x = "_submission__id", by.y = "_id", all.x = T)

# merge household visit data with stool lab results
# make a var with a common id (of sample)
table(HHR2$num_echantillon, useNA = "always")
HHR2$id <- substr(tolower(gsub("[ -]", "", HHR2$num_echantillon)), 1, 7)
table(HHR2$id, useNA = "always")

table(humanR2results$Identifiant, useNA = "always")
humanR2results$id <- substr(tolower(gsub("[ -]", "", humanR2results$Identifiant)), 1, 8)
humanR2results$id <- substr(tolower(gsub(" ", "", humanR2results$id)), 1, 8)
humanR2results$id <- substr(tolower(gsub("'", "", humanR2results$id)), 1, 8)
humanR2results$id[humanR2results$id=="???"] <- NA
humanR2results$id[humanR2results$id=="car011"] <- "car1011"
humanR2results$id[humanR2results$id=="car014"] <- "car1014"
humanR2results$id[humanR2results$id=="car015"] <- "car1015"
humanR2results$id[humanR2results$id=="car016"] <- "car1016"
humanR2results$id[humanR2results$id=="car017"] <- "car1017"
humanR2results$id[humanR2results$id=="car018"] <- "car1018"
humanR2results$id[humanR2results$id=="car021"] <- "car1021"
table(humanR2results$id, useNA = "always")

# reformat date
table(humanR2results$`Date reception`, useNA = "always")
humanR2results$receptiondate <- as.Date(humanR2results$`Date reception`) 

# merge both dataframes
humanR2merged <- merge(HHR2, humanR2results, by = "id", all = T)

# identify observations that are not merged
unmatchedidinhumanlabresultsR2 <- humanR2merged %>% filter(is.na(`_submission__id`)==T) %>% select(id, receptiondate)
unmatchedidinkoboR2 <- humanR2merged %>% filter(is.na(receptiondate)==T) %>% select(id, today)
unmatchedidinhumanlabresultsR2 # lab results that can't be matched to Kobo
unmatchedidinkoboR2 # Kobo entries with sample number that can't be matched to lab result

# export to csv files
write.csv(unmatchedidinhumanlabresultsR2, "unmatchedidinhumanlabresultsR2.csv")
write.csv(unmatchedidinkoboR2, "unmatchedidinkoboR2.csv")

### LINK ROUNDS ####
# ID individual
table(HHR1_stoolparticipantsonly$cs_id_individu, useNA = "always")
table(HHR2$cs_id_individu, useNA = "always")

# keep only ID and date in simplified data frames
HHR1_IDdate <- HHR1_stoolparticipantsonly %>% select(cs_id_individu, ageyears, age, sexe, grappe)
HHR2_IDdate <- HHR2 %>% select(cs_id_individu, ageyears, age, sexe, grappe)
# merge IDs of both rounds
HHvisits_selles_R1R2 <- merge(HHR1_IDdate, HHR2_IDdate, by = "cs_id_individu", all = T)
# export to csv
write.csv(HHvisits_selles_R1R2, "HHvisits_selles_R1R2.csv")

# STILL NEED TO CORRECTLY LINK THOSE MISSING
# create a list of IDs for which to check the result (neither RAS or OUI)
missingESBLresultshumanR1 <- humanR1merged %>% filter(is.na(humanR1merged$`Escherichia coli`)) %>% select(id, receptiondate)
missingESBLresultshumanR2 <- humanR2merged %>% filter(is.na(humanR2merged$`Escherichia coli`)) %>% select(id, receptiondate)
# export
write.csv(missingESBLresultshumanR1, "missingESBLresultshumanR1.csv")
write.csv(missingESBLresultshumanR2, "missingESBLresultshumanR2.csv")

### PSEUDONYMIZE AND EXPORT CLEANED DATA ####
# aggregate age
humanR1merged$agegr10 <- cut(humanR1merged$ageyears, 
                               breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                               labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                               include.lowest = TRUE)
humanR2merged$agegr10 <- cut(humanR2merged$ageyears, 
                             breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                             labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                             include.lowest = TRUE)
# remove all identifying variables
humanR1_pseudo <- humanR1merged %>% select("id","cs_id_individu","grappe", "agegr10", "sexe","today","menage_id","nmbre_personne_menage_001",
                                           "nbre_enf_0_5ans_001","nbre_menage_conc","q1_diarrhee_prevenu","q1_diarrhee_prevenu/garder_le_r_cipient_d_eau_couvert_dans_l",
                                           "q1_diarrhee_prevenu/ne_pas_tremper_les_doigts_dans_le_verre_","q1_diarrhee_prevenu/utiliser_un_ustensile_avec_une_poign_e_p",
                                           "q1_diarrhee_prevenu/couvrir_les_aliments","q1_diarrhee_prevenu/faire_bouillir_l_eau_de_boisson","q1_diarrhee_prevenu/filtrer_l_eau_de_boisson",
                                           "q1_diarrhee_prevenu/autre_pr_ciser_","q1_diarrhee_prevenu/on_ne_peut_pas_l_viter","q1_diarrhee_prevenu/ne_sait_pas",
                                           "autr_mesur_prev_diarrhe","q2_source_princ_saison_seche","q3_source_princ_saison_pluv","q4_bidon_stock","q5a_bidon_ferme_rempli",
                                           "q5b_bidon_ferme_vide","q5c_bidon_nettoye","q6_traite_eau","q6_autre_traitmen_eau","q7_type_inst_sanitaire","q7_autr_typ_ins_sanitair",
                                           "q8_autr_lieu_defecation","q8_autr_lieu_defecation/toilettes_avec_chasse_d_eau_raccord_e___","q8_autr_lieu_defecation/latrines___fosse_am_lior_e_avec_ventilat",
                                           "q8_autr_lieu_defecation/latrines___fosse_avec_dalle","q8_autr_lieu_defecation/latrine___fosse_sans_dalle","q8_autr_lieu_defecation/d_f_cation_en_plein_air",
                                           "q8_autr_lieu_defecation/autre","q8_autre_preciser","q9_toilette_partagee","q10_combien_partag","q11_dernier_nettoyage",
                                           "q12_elimine_selle_enf","q12_autre_preciser","q13_vidange_toilette","q14_produit_lavag_main","q15_lave_apr_defec",
                                           "q16_lave_apr_repas","q17_animaux_menage","q18_animaux_interieur","q18_animaux_interieur/1__b_ufs",
                                           "q18_animaux_interieur/2__moutons_ou_ch_vres","q18_animaux_interieur/3__porcs","q18_animaux_interieur/4___nes_chevaux",
                                           "q18_animaux_interieur/5__poules_oies_ou_canards","q18_animaux_interieur/6__autre_sp_cifiez","q18_autre_specifie",
                                           "q19_animaux_dehors","q19_animaux_dehors/1__b_ufs","q19_animaux_dehors/2__moutons_ou_ch_vres","q19_animaux_dehors/3__porcs",
                                           "q19_animaux_dehors/4___nes_chevaux","q19_animaux_dehors/5__poules_oies_ou_canards","q19_animaux_dehors/6__autre_sp_cifiez",
                                           "q19_autre_specifie","q20_excrement_animaux","q21_animal_malade","q21_animal_malade/1__faire_venir_un_v_t_rinaire_qui_peut_p",
                                           "q21_animal_malade/2__acheter_un_traitement_sans_consultati","q21_animal_malade/3__vendre_l_animal___un_boucher",
                                           "q21_animal_malade/4__on_l_abat_et_on_consomme_la_viande___","q21_animal_malade/5__quand_l_animal_meurt_on_le_jette_ou_o",
                                           "q21_animal_malade/6__quand_l_animal_meurt_on_consomme_la_v","q1_dispo_medica_menag","q2_nbre_sorte_medica",
                                           "Un_des_membres_de_votre_m_nage","_uuid","_submission_time","Escherichia coli","salmonella","cip_d","cip_interpr",
                                           "merop_d","merop_interpr","cef_d","cef_int","cotrim_d","cotrim_int","ampi_d","ampi_int","amoxiclav_d","amoxiclav_int",
                                           "ertap_d","ertap_int","genta_d","genta_int","pip_d","pip_int","amik_d","amik_int","pefl_d","pefl_int","cefep_d","cefep_int",
                                           "receptiondate")
humanR2_pseudo <- humanR2merged %>% select("id","cs_id_individu","grappe", "agegr10", "sexe","today","menage_id","_uuid","_submission_time","Escherichia coli","salmonella","cip_d","cip_interpr",
                                           "merop_d","merop_interpr","cef_d","cef_int","cotrim_d","cotrim_int","ampi_d","ampi_int","amoxiclav_d","amoxiclav_int",
                                           "ertap_d","ertap_int","genta_d","genta_int","pip_d","pip_int","amik_d","amik_int","pefl_d","pefl_int","cefep_d","cefep_int",
                                           "receptiondate")
# save databases
write.csv(humanR1_pseudo, "humanR1_Kim_pseudo.csv")
write.csv(humanR2_pseudo, "humanR2_Kim_pseudo.csv")

### 2. IMPORT & MERGE RODENT DATA ####
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

### 3. EXTRACTION SELECTION HUMAN STOOLS ####
# show for each cluster the number of isolates and ESBL E. coli positives
table(humanR1merged$grappe, humanR1merged$`Escherichia coli`, useNA = "always")
table(humanR2merged$grappe, humanR2merged$`Escherichia coli`, useNA = "always")

# mark which clusters rodents were collected
rodentclusters <- c("cellule_mbuka3", "kiandu", "kilueka", "kimaku", "lukengezi_et_poste", "malanga")
humanR1merged$rodent <- ifelse(humanR1merged$grappe %in% rodentclusters, "yes", "no")
table(humanR1merged$grappe, humanR1merged$rodent)
humanR2merged$rodent <- ifelse(humanR2merged$grappe %in% rodentclusters, "yes", "no")
table(humanR2merged$grappe, humanR2merged$rodent)

# number of samples collected and ESBL identified in villages where rodents were collected
table(humanR1merged$rodent, humanR1merged$`Escherichia coli`, useNA = "always") # 48 ESBL E. coli identified in humans in villages where rodents were collected
table(humanR2merged$rodent, humanR2merged$`Escherichia coli`, useNA = "always") # 20 ESBL E. coli identified in humans in villages where rodents were collected

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
