#####################################################
# CABU-EICO household survey                        #
# rate of healthcare seeking, prevalence of         #
# WASH behaviour/conditions                         #
#####################################################

# The household data contains questions from three surveys
# 1) Stool collection survey --> individual-level data (age, sex) of those of whom a stool sample was taken
# 2) WASH survey --> household level data, answered by the household head
# 3) Healthcare utilisation survey --> individual level data
# Here for each household member, the household head is asked about the number of 
# healthcare visits in the last 30 days
# Each of these visits (per provider type) are one line of data
# Therefore there are multiple observations within a household

# install/load packages
pacman::p_load(readxl,lubridate,dplyr,tidyr, survey)

### Kimpese ####
# import BASELINE household visits 
hh_kim_bl <- read_excel("db/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx",
                        sheet = "CABU_enq_comm_2023")
hh_kim_bl <- hh_kim_bl %>% select(-intronote, -deviceid, -coordGPS, -`_coordGPS_latitude`, -`_coordGPS_longitude`, -`_coordGPS_altitude`, -`_coordGPS_altitude`, -`_coordGPS_precision`, -initial_cm, -initial_at)  # remove identifiers
hh_kim_bl <- hh_kim_bl %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# recode cluster names
recode_vector <- c(
  "cellule_mbuka3" = "CELLULE_MBUKA3_AS_Yanga_Dia_Songa",
  "kiasungua" = "KIASUNGUA_AS_Kisaunga",
  "kilueka" = "KILUEKA_AS_Kilueka",
  "mbanza_ndamba" = "MBANZA_NDAMBA_AS_Kilueka",
  "lukengezi_et_poste" = "LUKENGEZI_ET_POSTE_AS_CECO",
  "mont_fleury" = "MONT_FLEURY_AS_Kimbanguiste",
  "mont_fleury_a_et_b" = "MONT_FLEURY_AS_Kimbanguiste",
  "mpete_nkondo" = "MPETE_NKONDO_AS_Kiasunga",
  "q3" = "Q3_AS_Kimbanguiste",
  "sanzikua" = "SANZIKUA_AS_Vunda_Nsole",
  "viaza" = "VIAZA_AS_Viaza",
  "vunda_nsole" = "VUNDA_NSOLE_AS_Vunda_Nsole")
hh_kim_bl <- hh_kim_bl %>% mutate(grappe = recode(grappe, !!!recode_vector))
table(hh_kim_bl$grappe)

# add a variable intervention
hh_kim_bl <- hh_kim_bl %>%
  mutate(intervention = ifelse(grappe %in% c("CELLULE_MBUKA3_AS_Yanga_Dia_Songa", "CELLULE MBUKA3 (AS Yanga Dia Songa)", "KIASUNGUA_AS_Kisaunga", 
                                                      "KIASUNGUA (AS Kisaunga)", "KILUEKA_AS_Kilueka", "KILUEKA (AS Kilueka)", "MBANZA NDAMBA (AS Kilueka)", 
                                                      "LUKENGEZI ET POSTE (AS CECO)", "LUKENGEZI_ET_POSTE_AS_CECO", "MBANZA_NDAMBA_AS_Kilueka", 
                                                      "MONT FLEURY (AS Kimbanguiste)", "MONT_FLEURY_AS_Kimbanguiste", "MPETE NKONDO (AS Kiasunga)", 
                                                      "MPETE_NKONDO_AS_Kiasunga", "Q3 (AS Kimbanguiste)", "Q3_AS_Kimbanguiste", "SANZIKUA_AS_Vunda_Nsole", 
                                                      "VIAZA_AS_Viaza", "VUNDA_NSOLE_AS_Vunda_Nsole"), 
                               "intervention", "control"))
table(hh_kim_bl$grappe, hh_kim_bl$intervention, useNA = "always") # 11 clusters in each group, groups correctly assigned

# dataframe with only the essential hh data
hhstructure_kim_bl <- hh_kim_bl %>% select(`_id`, grappe, intervention, nmbre_personne_menage_001, nbre_enf_0_5ans_001, nbre_menage_conc)
colnames(hhstructure_kim_bl) <- c("id", "cluster", "intervention", "nmembersHH", "nunderfiveHH", "nHHinconcession")
head(hhstructure_kim_bl)

# import sheets with healthcare visits
hcu_kim_bl <- read_excel("db/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx", 
                             sheet = "group_sn8od67")
hcu_kim_bl_part2 <- read_excel("db/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx", 
                         sheet = "group_sn8od67") # second sheet with HCU
hcu_kim_bl <- rbind(hcu_kim_bl, hcu_kim_bl_part2)
hcu_kim_bl <- hcu_kim_bl %>% filter(!is.na(q4_type_fournisseur)) # only those sheets with visit entered
hcu_kim_bl <- hcu_kim_bl %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
# keep only HH ID, providertype and when the visit was
hcu_kim_bl <- hcu_kim_bl %>% select(q3_date_visite, q4_type_fournisseur, `_submission__id`)
colnames(hcu_kim_bl) <- c("when_visit", "providertype", "id")
# remove duplicates
hcu_kim_bl <- hcu_kim_bl %>% distinct(providertype, when_visit, id, .keep_all = TRUE)
# rename providertype values
hcu_kim_bl$providertype <- ifelse(hcu_kim_bl$providertype == "1__a__avec_les_m_dicaments_dont_nous_dis", "selfmedication",
                          ifelse(hcu_kim_bl$providertype == "3__c__chez_les_gu_risseurs_traditionnels", "traditionalhealer",
                                 ifelse(hcu_kim_bl$providertype == "4__d__directement___la_pharmacie_priv_e", "privatepharmacy",
                                        ifelse(hcu_kim_bl$providertype == "5__e__directement_au_d_p_t_priv", "privatepharmacy",
                                               ifelse(hcu_kim_bl$providertype == "6__f__directement_au_d_p_t_publique___l_", "privatepharmacy",
                                                      ifelse(hcu_kim_bl$providertype == "7__g__nous_avons_d_abord_consult__au_cen", "healthcentre",
                                                             hcu_kim_bl$providertype))))))
table(hcu_kim_bl$providertype, useNA = "always")

# merge household structure and hcu visits
hh_hcu_kim_bl <- merge(hhstructure_kim_bl, hcu_kim_bl, by = "id", all = T)

# 3) HCU (healthcare utilisation) Kimpese #
# summarize visits in last 3 months and in last month
hh_hcu_kim_bl_counts_last3mo <- hh_hcu_kim_bl %>%
  group_by(id, intervention, nmembersHH, providertype) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = providertype, values_from = n, names_prefix = "hcu_3mo_") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))
head(hh_hcu_kim_bl_counts_last3mo)
# in last month
hh_hcu_kim_bl_counts_last1mo <- hh_hcu_kim_bl %>%
  filter(when_visit!="entre_un_et_trois_mois_avant_cette_enqu_") %>%
  group_by(id, providertype) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = providertype, values_from = n, names_prefix = "hcu_1mo_") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))
head(hh_hcu_kim_bl_counts_last1mo)

# merge both
hh_hcu_kim_bl_counts <- merge(hh_hcu_kim_bl_counts_last3mo, hh_hcu_kim_bl_counts_last1mo, by = "id", all.x = T)
head(hh_hcu_kim_bl_counts)

# import POST INTERVENTION household visits 
hh_kim_post <- read_excel("db/householdsurvey/R3_CABU_ENQ_COM_-_all_versions_-_False_-_2024-07-03-10-27-04.xlsx", 
                          sheet = "R3 CABU_ENQ_COM")
hh_kim_post <- hh_kim_post %>% select(-intronote, -deviceid, -coordGPS, -`_coordGPS_latitude`, -`_coordGPS_longitude`, -`_coordGPS_altitude`, -`_coordGPS_altitude`, -`_coordGPS_precision`, -initial_cm, -initial_at)  # remove identifiers
hh_kim_post <- hh_kim_post %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# recode cluster names
recode_vector <- c(
  "cellule_mbuka3" = "CELLULE_MBUKA3_AS_Yanga_Dia_Songa",
  "kiasungua" = "KIASUNGUA_AS_Kisaunga",
  "kilueka" = "KILUEKA_AS_Kilueka",
  "mbanza_ndamba" = "MBANZA_NDAMBA_AS_Kilueka",
  "lukengezi_et_poste" = "LUKENGEZI_ET_POSTE_AS_CECO",
  "mont_fleury" = "MONT_FLEURY_AS_Kimbanguiste",
  "mont_fleury_a_et_b" = "MONT_FLEURY_AS_Kimbanguiste",
  "mpete_nkondo" = "MPETE_NKONDO_AS_Kiasunga",
  "q3" = "Q3_AS_Kimbanguiste",
  "sanzikua" = "SANZIKUA_AS_Vunda_Nsole",
  "viaza" = "VIAZA_AS_Viaza",
  "vunda_nsole" = "VUNDA_NSOLE_AS_Vunda_Nsole")
hh_kim_post <- hh_kim_post %>% mutate(grappe = recode(grappe, !!!recode_vector))
table(hh_kim_post$grappe)

# add a variable intervention
hh_kim_post <- hh_kim_post %>%
  mutate(intervention = ifelse(grappe %in% c("CELLULE_MBUKA3_AS_Yanga_Dia_Songa", "CELLULE MBUKA3 (AS Yanga Dia Songa)", "KIASUNGUA_AS_Kisaunga", 
                                             "KIASUNGUA (AS Kisaunga)", "KILUEKA_AS_Kilueka", "KILUEKA (AS Kilueka)", "MBANZA NDAMBA (AS Kilueka)", 
                                             "LUKENGEZI ET POSTE (AS CECO)", "LUKENGEZI_ET_POSTE_AS_CECO", "MBANZA_NDAMBA_AS_Kilueka", 
                                             "MONT FLEURY (AS Kimbanguiste)", "MONT_FLEURY_AS_Kimbanguiste", "MPETE NKONDO (AS Kiasunga)", 
                                             "MPETE_NKONDO_AS_Kiasunga", "Q3 (AS Kimbanguiste)", "Q3_AS_Kimbanguiste", "SANZIKUA_AS_Vunda_Nsole", 
                                             "VIAZA_AS_Viaza", "VUNDA_NSOLE_AS_Vunda_Nsole"), 
                               "intervention", "control"))
table(hh_kim_post$grappe, hh_kim_post$intervention, useNA = "always") # 11 clusters in each group, groups correctly assigned

# dataframe with only the essential hh data
hhstructure_kim_post <- hh_kim_post %>% select(`_id`, grappe, intervention, nmbre_personne_menage_001, nbre_enf_0_5ans_001, nbre_menage_conc)
colnames(hhstructure_kim_post) <- c("id", "cluster", "intervention", "nmembersHH", "nunderfiveHH", "nHHinconcession")
head(hhstructure_kim_post)

# import sheets with healthcare visits
hcu_kim_post <- read_excel("db/householdsurvey/R3_CABU_ENQ_COM_-_all_versions_-_False_-_2024-07-03-10-27-04.xlsx", 
                           sheet = "individu_recherche_soin")
hcu_kim_post <- hcu_kim_post %>% filter(!is.na(q4_type_fournisseur)) # only those sheets with visit entered
hcu_kim_post <- hcu_kim_post %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
# keep only HH ID, providertype and when the visit was
hcu_kim_post <- hcu_kim_post %>% select(q3_date_visite, q4_type_fournisseur, `_submission__id`)
colnames(hcu_kim_post) <- c("when_visit", "providertype", "id")
# remove duplicates
hcu_kim_post <- hcu_kim_post %>% distinct(providertype, when_visit, id, .keep_all = TRUE)
# rename providertype values
hcu_kim_post$providertype <- ifelse(hcu_kim_post$providertype == "1__a__avec_les_m_dicaments_dont_nous_dis", "selfmedication",
                                  ifelse(hcu_kim_post$providertype == "3__c__chez_les_gu_risseurs_traditionnels", "traditionalhealer",
                                         ifelse(hcu_kim_post$providertype == "4__d__directement___la_pharmacie_priv_e", "privatepharmacy",
                                                ifelse(hcu_kim_post$providertype == "5__e__directement_au_d_p_t_priv", "privatepharmacy",
                                                       ifelse(hcu_kim_post$providertype == "6__f__directement_au_d_p_t_publique___l_", "privatepharmacy",
                                                              ifelse(hcu_kim_post$providertype == "7__g__nous_avons_d_abord_consult__au_cen", "healthcentre",
                                                                     hcu_kim_post$providertype))))))
table(hcu_kim_post$providertype, useNA = "always")

# merge household structure and hcu visits
hh_hcu_kim_post <- merge(hhstructure_kim_post, hcu_kim_post, by = "id", all = T)

# summarize visits in last 3 months and in last month
hh_hcu_kim_post_counts_last3mo <- hh_hcu_kim_post %>%
  group_by(id, intervention, nmembersHH, providertype) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = providertype, values_from = n, names_prefix = "hcu_3mo_") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))
head(hh_hcu_kim_post_counts_last3mo)
# in last month
hh_hcu_kim_post_counts_last1mo <- hh_hcu_kim_post %>%
  filter(when_visit!="entre_un_et_trois_mois_avant_cette_enqu_") %>%
  group_by(id, providertype) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = providertype, values_from = n, names_prefix = "hcu_1mo_") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))
head(hh_hcu_kim_post_counts_last1mo)

# merge both
hh_hcu_kim_post_counts <- merge(hh_hcu_kim_post_counts_last3mo, hh_hcu_kim_post_counts_last1mo, by = "id", all.x = T)
head(hh_hcu_kim_post_counts)

# append pre and post
hh_hcu_kim_bl_counts$round <- "bl"
hh_hcu_kim_post_counts$round <- "post"
hh_hcu_kim_post_counts$hcu_1mo_traditionalhealer <- 0 # add column with zero
hh_hcu_kim_counts <- rbind(hh_hcu_kim_bl_counts, hh_hcu_kim_post_counts)

# intervention participation Kimpese
# I cannot find this in the questionnaire

### Nanoro ####
# import raw data from the household visits - healthcare utilisation part
# hh_nan_bl_old <- read_excel("db/householdsurvey/WP4_WASH.xls", sheet = "WP4_WASH")
hh_nan <- read_excel("db/householdsurvey/ALL_Data_CABUBWP4_21_05_2024.xlsx", 
                        sheet = "ALL_Data_CABUBWP4_21_05_2024")
# rename and reformat general and WASH HH variables  
hh_nan = hh_nan %>% mutate(
  dob = as.Date(dob, format = "%Y-%m-%d"),
  sexe = factor(sexe, levels=c(1,2), labels=c("Male", "Female")),
  date_enquete = as.Date(date_enquete, format="%Y-%m-%d"),
  date_consentement = as.Date(date_consentement, format="%Y-%m-%d"),
  date_recuperation_selle = as.Date(date_recuperation_selle, format="%Y-%m-%d"),
  age = tolower(age),
  age = as.numeric(age),
  agegr10 = cut(age, 
                breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                include.lowest = TRUE),
  nmbre_personne_menage = as.numeric(nmbre_personne_menage),
  nbre_enf_0_5ans = as.numeric(nbre_enf_0_5ans),
  nbre_menage_conc = as.numeric(nbre_menage_conc), 
  date_enquete = as.Date(date_enquete, format="%Y-%m-%d"),
  date_consentement = as.Date(date_consentement, format="%Y-%m-%d"),
  date_recuperation_selle = as.Date(date_recuperation_selle, format="%Y-%m-%d"),
  q1_diarrhee_prevenu___1 = factor(q1_diarrhee_prevenu___1, levels=c(0,1), labels=c("No","Yes")),        
  q1_diarrhee_prevenu___2 = factor(q1_diarrhee_prevenu___2, levels=c(0,1), labels=c("No","Yes")),
  q1_diarrhee_prevenu___3 = factor(q1_diarrhee_prevenu___3, levels=c(0,1), labels=c("No","Yes")),        
  q1_diarrhee_prevenu___4 = factor(q1_diarrhee_prevenu___4, levels=c(0,1), labels=c("No","Yes")),
  q1_diarrhee_prevenu___5 = factor(q1_diarrhee_prevenu___5, levels=c(0,1), labels=c("No","Yes")),         
  q1_diarrhee_prevenu___6 = factor(q1_diarrhee_prevenu___6, levels=c(0,1), labels=c("No","Yes")),
  q1_diarrhee_prevenu___7 = factor(q1_diarrhee_prevenu___7, levels=c(0,1), labels=c("No","Yes")),         
  q1_diarrhee_prevenu___8 = factor(q1_diarrhee_prevenu___8, levels=c(0,1), labels=c("No","Yes")),
  q1_diarrhee_prevenu___9 = factor(q1_diarrhee_prevenu___9, levels=c(0,1), labels=c("No","Yes")), 
  autr_mesur_prev_diarrhe = ifelse(autr_mesur_prev_diarrhe %in% c("certain nouriture","Certain nouriture", "CERTAIN NOURITURE",
                                                                  "Certaine nouriture", "CERTAINE NOURITURE","Certains nouriture",
                                                                  "CERTAINS NOURITURE", "Eviter certain nouriture"), "certain_food",
                                   ifelse(autr_mesur_prev_diarrhe %in% c("LAVAGE DES MAINS AVANT ET APRES LE REPAS"), "hand washing", "")),
  q2_source_princ_saison_seche = factor(q2_source_princ_saison_seche, levels=c(1:9), labels=c("Tap house", 
                                                                                              "Tap concession",
                                                                                              "Tap public/fountain", 
                                                                                              "Borehole",
                                                                                              "improved well (protected)",
                                                                                              "unimproved well (unprotected)",
                                                                                              "rainwater",
                                                                                              "surface water (ponds, dams,rivers,lakes,pits,irrigation canals)",
                                                                                              "bagged water")),
  q3_source_princ_saison_pluv = factor(q3_source_princ_saison_pluv, levels=c(1:10), labels=c("Tap house", 
                                                                                             "Tap concession",
                                                                                             "Tap public/fountain", 
                                                                                             "Borehole",
                                                                                             "improved well (protected)",
                                                                                             "unimproved well(unprotected)",
                                                                                             "rainwater",
                                                                                             "surface water (ponds, dams,rivers,lakes,pits,irrigation canals)",
                                                                                             "bagged water", "bottled water")),  
  q4_bidon_stock = factor(q4_bidon_stock, levels = c(1:3), labels=c("Yes, cans", "Yes, only one large tank", "No")),
  q5a_bidon_ferme_rempli = factor(q5a_bidon_ferme_rempli, levels=c(1:2), labels=c("Yes", "No")),
  q5b_bidon_ferme_vide = factor(q5b_bidon_ferme_vide, levels=c(1:2), labels=c("Yes", "No")),           
  q5c_bidon_nettoye = factor(q5c_bidon_nettoye, levels=c(1,2,3,4,6), labels=c("Yes, with soap", "Yes, with water but no soap","Yes, boiled", "No", "Other")),
  q6_traite_eau = factor(q6_traite_eau, levels = c(1:7), labels= c("No", "Yes,boiling", "Yes,cholinate/add desinfectant", "Yes, filter with cloth",
                                                                   "Yes, filter with filter", "Yes, Solar desinfection (in the sun)","Yes, decant")),               
  q7_type_inst_sanitaire = factor(q7_type_inst_sanitaire, levels = c(3,4,5), labels = c("pit latrine with slab", "pit latrine without slab", "open defecation")),
  q8_autr_lieu_defecation___1 = factor(q8_autr_lieu_defecation___1, levels = c(0,1), labels = c("No","Yes")),      
  q8_autr_lieu_defecation___2 = factor(q8_autr_lieu_defecation___2, levels = c(0,1), labels = c("No","Yes")),
  q8_autr_lieu_defecation___3 = factor(q8_autr_lieu_defecation___3, levels = c(0,1), labels = c("No","Yes")),   
  q8_autr_lieu_defecation___4 = factor(q8_autr_lieu_defecation___4, levels = c(0,1), labels = c("No","Yes")),
  q8_autr_lieu_defecation___5 = factor(q8_autr_lieu_defecation___5, levels = c(0,1), labels = c("No","Yes")),   
  q8_autr_lieu_defecation___6 = factor(q8_autr_lieu_defecation___6, levels = c(0,1), labels = c("No","Yes")),
  q8_autr_lieu_defecation___7 = factor(q8_autr_lieu_defecation___7, levels = c(0,1), labels = c("No","Yes")),  
  q9_toilette_partagee = factor(q9_toilette_partagee, levels=c(1:3), labels=c("Yes, other households (non-public)", "Yes, public", "No")),             
  q10_combien_partag = as.numeric(q10_combien_partag),
  q11_dernier_nettoyage = factor(q11_dernier_nettoyage, levels=c(1:6), labels=c("<24h", ">24h, but <1week", "1-4weeks", ">1month", "Never", "Don't know")), 
  q12_elimine_selle_enf = factor(q12_elimine_selle_enf, levels = c(1:9), labels=c("Child used toilet/latrine", "Thrown/rinsed into toilet/latrine",
                                                                                  "Thrown/rinsed into drainage pit",
                                                                                  "Trown in garbage", "Buried", "Disposed in open air", 
                                                                                  "Used as manure", "Other", "NA (no child)")),
  q13_vidange_toilette = factor(q13_vidange_toilette, levels=c(1,2,4,5,7), labels=c("Has not been drained yet", "Don't know", "Removed by service provider and covered in pit",
                                                                                    "Removed by service provider (don't know where)", "Emptied by hh in uncover pit/open ground")),
  q14_produit_lavag_main = factor(q14_produit_lavag_main, levels=c(1:5), labels=c("Yes, soap", "Yes, detergent", "Yes, ash/mud/sand", "No, none available", "No, available but not used")),
  q15_lave_apr_defec = factor(q15_lave_apr_defec, levels=c(1:4), labels=c("Yes, always", "Yes, often", "No or rarerly", "Not sure")),       
  q16_lave_apr_repas = factor(q16_lave_apr_repas, levels=c(1:4), labels=c("Yes, always", "Yes, often", "No or rarerly", "Not sure")),
  q17_animaux_menage = factor(q17_animaux_menage, levels=c(1:4), labels = c("Yes, inside and outside", "Yes, outside next to house","Yes, outside in demarked area", "No")),          
  q18_animaux_interieur___1 = factor(q18_animaux_interieur___1, levels = c(0,1), labels = c("No", "Yes")),
  q18_animaux_interieur___2 = factor(q18_animaux_interieur___2, levels = c(0,1), labels = c("No", "Yes")),       
  q18_animaux_interieur___3 = factor(q18_animaux_interieur___3, levels = c(0,1), labels = c("No", "Yes")),
  q18_animaux_interieur___4 = factor(q18_animaux_interieur___4, levels = c(0,1), labels = c("No", "Yes")),       
  q18_animaux_interieur___5 = factor(q18_animaux_interieur___5, levels = c(0,1), labels = c("No", "Yes")),
  q18_animaux_interieur___6 = factor(q18_animaux_interieur___6, levels = c(0,1), labels = c("No", "Yes")),
  q19_animaux_dehors___1 = factor(q19_animaux_dehors___1, levels = c(0,1), labels = c("No", "Yes")),        
  q19_animaux_dehors___2 = factor(q19_animaux_dehors___2, levels = c(0,1), labels = c("No", "Yes")),
  q19_animaux_dehors___3 = factor(q19_animaux_dehors___3, levels = c(0,1), labels = c("No", "Yes")),        
  q19_animaux_dehors___4 = factor(q19_animaux_dehors___4, levels = c(0,1), labels = c("No", "Yes")),
  q19_animaux_dehors___5 = factor(q19_animaux_dehors___5, levels = c(0,1), labels = c("No", "Yes")),         
  q19_animaux_dehors___6 = factor(q19_animaux_dehors___6, levels = c(0,1), labels = c("No", "Yes")),
  
  q20_excrement_animaux = factor(q20_excrement_animaux, levels = c(1,2,3), labels = c("Yes", "No", "Not possible to determine")),  
  q21_animal_malade___1 = factor(q21_animal_malade___1, levels = c(0,1), labels = c("No", "Yes")),           
  q21_animal_malade___2 = factor(q21_animal_malade___2, levels = c(0,1), labels = c("No", "Yes")),  
  q21_animal_malade___3 = factor(q21_animal_malade___3, levels = c(0,1), labels = c("No", "Yes")),  
  q21_animal_malade___4 = factor(q21_animal_malade___4, levels = c(0,1), labels = c("No", "Yes")),  
  q21_animal_malade___5 = factor(q21_animal_malade___5, levels = c(0,1), labels = c("No", "Yes")),             
  q21_animal_malade___6 = factor(q21_animal_malade___6, levels = c(0,1), labels = c("No", "Yes")),
) %>% select(-dob) %>% rename(
  n.householdmember = "nmbre_personne_menage",
  n.child.0to5 = "nbre_enf_0_5ans",
  n.households.concession = "nbre_menage_conc",
  date.consent = "date_consentement",
  date.stool.collection = "date_recuperation_selle",
  q1.diar.prev.water.pot.covered = "q1_diarrhee_prevenu___1",
  q1.diar.prev.no.finger.in.waterglass = "q1_diarrhee_prevenu___2",
  q1.diar.prev.no.finger.in.waterglass =  "q1_diarrhee_prevenu___2",
  q1.diar.prev.utensil.to.take.water.from.pot = "q1_diarrhee_prevenu___3",    
  q1.diar.prev.cover.food = "q1_diarrhee_prevenu___4",
  q1.diar.prev.boil.water = "q1_diarrhee_prevenu___5",
  q1.diar.prev.filter.water = "q1_diarrhee_prevenu___6",
  q1.diar.prev.other = "q1_diarrhee_prevenu___7",
  q1.diar.prev.cant.be.avoided = "q1_diarrhee_prevenu___8",
  q1.diar.prev.dont.know = "q1_diarrhee_prevenu___9",
  q2.main.water.source.dry= "q2_source_princ_saison_seche",
  q3.main.water.source.rainy= "q3_source_princ_saison_pluv",
  q4.cans.storage.water= "q4_bidon_stock", 
  q5a.cans.storage.water.closed.when.filled= "q5a_bidon_ferme_rempli", 
  q5b.cans.storage.water.closed.wen.empty= "q5b_bidon_ferme_vide",          
  q5c.cans.cleaned.before.reuse= "q5c_bidon_nettoye", 
  q6.treatment.water= "q6_traite_eau", 
  q7.principle.defication= "q7_type_inst_sanitaire", 
  q8.other.defecation.flush.toiled.septic  = "q8_autr_lieu_defecation___1", 
  q8.other.defecation.pit.latrine.ventilation = "q8_autr_lieu_defecation___2",
  q8.other.defecation.pit.latrine.slab = "q8_autr_lieu_defecation___3",   
  q8.other.defecation.pit.latrine.no.slab = "q8_autr_lieu_defecation___4",
  q8.other.defecation.open.defecation = "q8_autr_lieu_defecation___5",   
  q8.other.defecation.other = "q8_autr_lieu_defecation___6",
  q8.other.defecation.none = "q8_autr_lieu_defecation___7",
  q9.shared.toilet= "q9_toilette_partagee",
  q10.n.shared.toilet= "q10_combien_partag",
  q11.toilet.last.cleaned= "q11_dernier_nettoyage",
  q12.disposal.child.stool= "q12_elimine_selle_enf",
  q13.disposal.latrine.pit= "q13_vidange_toilette",
  q14.handwashing.product= "q14_produit_lavag_main",
  q15.handwashing.defecation= "q15_lave_apr_defec",
  q16.handwashing.meals= "q16_lave_apr_repas",
  q17.animals.around.household= "q17_animaux_menage",
  q18.animal.inside.cow = "q18_animaux_interieur___1",
  q18.animal.inside.sheep.goat = "q18_animaux_interieur___2",       
  q18.animal.inside.pig = "q18_animaux_interieur___3",
  q18.animal.inside.donkey.horse = "q18_animaux_interieur___4",       
  q18.animal.inside.chicken.goose.duck = "q18_animaux_interieur___5",
  q18.animal.inside.other = "q18_animaux_interieur___6",
  q19.animal.inside.cow = "q19_animaux_dehors___1",
  q19.animal.inside.sheep.goat = "q19_animaux_dehors___2",       
  q19.animal.inside.pig = "q19_animaux_dehors___3",
  q19.animal.inside.donkey.horse = "q19_animaux_dehors___4",       
  q19.animal.inside.chicken.goose.duck = "q19_animaux_dehors___5",
  q19.animal.inside.other = "q19_animaux_dehors___6",
  q20.animal.excrement.floor= q20_excrement_animaux, 
  q21.when.animal.ill.treatment.with.vet = "q21_animal_malade___1",           
  q21.when.animal.ill.treatment.without.vet= "q21_animal_malade___2",  
  q21.when.animal.ill.bucher = "q21_animal_malade___3",  
  q21.when.animal.ill.eat.meat.at.home= "q21_animal_malade___4",  
  q21.when.animal.ill.burie.dispose = "q21_animal_malade___5",             
  q21.when_animal.ill.autre = "q21_animal_malade___6"
)
table(hh_nan$agegr10[!is.na(hh_nan$dob)], useNA = "always") # none with a dob but no age group given -> delete dob

# clean healthcare utilisation variables
#  provider types
table(hh_nan$q4_type_fournisseur)
hh_nan <- hh_nan %>% mutate(providertype = case_when( # add labels 
  q4_type_fournisseur == 1 ~ "selfmedication",
  q4_type_fournisseur == 2 ~ "informalvendor",
  q4_type_fournisseur == 3 ~ "traditionalhealer",
  q4_type_fournisseur == 4 ~ "privatepharmacy",
  q4_type_fournisseur == 5 ~ "privatepharmacy",
  q4_type_fournisseur == 6 ~ "privatepharmacy", # selfmedication/OTC from the health centre pharmacy, so fits probably best private pharmacy
  q4_type_fournisseur == 7 ~ "healthcentre_publique",
  TRUE ~ as.character(q4_type_fournisseur)))
# recode days between the health care visit and the household visit
table(hh_nan$q3_date_visite)
hh_nan <- hh_nan %>% mutate(dayssinceHCU = case_when( # add labels 
  q3_date_visite == 1 ~ "</=7days",
  q3_date_visite == 2 ~ ">7days & </=30days",
  q3_date_visite == 3 ~ ">30days & </= 90days",
  TRUE ~ as.character(q3_date_visite)))
table(hh_nan$dayssinceHCU, useNA = "always")

# add a variable for village and specify which were intervention and control villages
bf_villages_cabu <- read_excel("db/bf_villages_cabu.xlsx")
names(bf_villages_cabu) = c("village_code", "village","intervention","ajouter")
bf_villages_cabu$intervention[bf_villages_cabu$intervention=="contrôle"] <- "control"

# merge with the HH data, to indicate which are in intervention and which in control villages
hh_nan$village_code = substr(hh_nan$menage_id, start = 1, stop = 2)
table(hh_nan$village_code)
hh_nan = merge(hh_nan, bf_villages_cabu, by="village_code")
table(hh_nan$village, hh_nan$intervention, useNA = "always") # checked, correctly assigned

# harmonize column names with those in other scripts
names(hh_nan) = gsub("_",".",names(hh_nan))

# add a variable round to distinguish pre vs post intervention
hh_nan$round[hh_nan$redcap.event.name=="round_0_arm_1"] <- "pre"
hh_nan$round[hh_nan$redcap.event.name=="round_3_arm_1"] <- "post"

# with redcap_repeat_instrument, the different survey answers can be recognised
# 1) Stool collection survey --> redcap_repeat_instrument == "formulaire_collecte_de_selles"
# 2) WASH survey --> redcap_repeat_instrument == " " and redcap_event_name == "round_0_arm_1"
# 3) Healthcare utilisation survey --> redcap_repeat_instrument == "visite_structure_sanitaire"
# 4) Healthcare utilisation survey medicines --> redcap_repeat_instrument == "mdicament"

# 0) general household info and HCU was recorded
hh_nan_general <- hh_nan %>% filter(is.na(redcap.repeat.instrument) & !is.na(n.householdmember)) %>% # first I also filtered out those with no q4_nbr_fois_recherch_medi but it turns out some do have a visit recorded
  select(menage.id, village.code, village, intervention, n.householdmember, n.child.0to5, n.households.concession) # 806 households (22*36=792 in protocol)
hh_nan_general_HCUrecord <- hh_nan %>% filter(is.na(redcap.repeat.instrument) & !is.na(n.householdmember) & !is.na(q4.nbr.fois.recherch.medi)) %>% # those with no q4_nbr_fois_recherch_medi removed
  select(menage.id, village.code, village, intervention, n.householdmember, n.child.0to5, n.households.concession) 

# exclude households with 15 or more household members - more likely to be errors >> 694 households remaining
hh_nan_general <- hh_nan_general %>%
  filter(n.householdmember<15)
hh_nan_general_HCUrecord <- hh_nan_general_HCUrecord %>%
  filter(n.householdmember<15)

# 2) household WASH database
hh_nan_wash <- hh_nan %>% filter(is.na(redcap.repeat.instrument))
# check for duplicates - none found
table(hh_nan_wash$round)

# 3) HCU (healthcare utilisation)
hh_nan_HCU <- hh_nan %>% filter(redcap.repeat.instrument=="visite_structure_sanitaire") %>%
  select(where(~ any(!is.na(.)))) %>%
  select(-c("redcap.event.name", "redcap.repeat.instrument", "redcap.repeat.instance", "village.code", "village", "ajouter"))
table(hh_nan_HCU$intervention, hh_nan_HCU$round, useNA = "always") # 586 with HCU recorded at baseline, 430 post intervention
table(hh_nan$q4.nbr.fois.recherch.medi[is.na(hh_nan$redcap.repeat.instrument)], useNA = "always")

# summarize visits in last 3 months and in last month
hh_nan_HCU_counts_last3mo <- hh_nan_HCU %>%
  filter(!is.na(providertype)) %>%
  group_by(menage.id, round, providertype) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = c(round, providertype), values_from = n, names_prefix = "hcu_3mo_") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))
head(hh_nan_HCU_counts_last3mo)

hh_nan_HCU_counts_last1mo <- hh_nan_HCU %>%
  filter(!is.na(providertype)) %>%
  filter(dayssinceHCU!=">30days & </= 90days") %>%
  group_by(menage.id, round, providertype) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = c(round, providertype), values_from = n, names_prefix = "hcu_1mo_") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))
head(hh_nan_HCU_counts_last1mo)

# add the general HH info to the WASH questionnaire, making sure there is one line per household
hh_nan_HCUfreq3mo <- merge(hh_nan_general, hh_nan_HCU_counts_last3mo, by = "menage.id", all.x = T)
hh_nan_HCUfreq1mo <- merge(hh_nan_general, hh_nan_HCU_counts_last1mo, by = "menage.id", all.x = T)
# check if different if no response for q4 number of HCU episodes
hh_nan_HCUfreq3mo_HCUrecord <- merge(hh_nan_general_HCUrecord, hh_nan_HCU_counts_last3mo, by = "menage.id", all.x = T)
hh_nan_HCUfreq1mo_HCUrecord <- merge(hh_nan_general_HCUrecord, hh_nan_HCU_counts_last1mo, by = "menage.id", all.x = T)

#### 1. ESTIMATE RATE OF HEALTHCARE UTILISATION ####
# KIMPESE BASELINE
# estimated population with HCU recorded - based on question "Un des membres de votre ménage a t'il cherché des soins chez un pourvoyeurs de soins ou ont été traités avec des médicaments stockés à la maison pendant le trois derniers mois?" in the general HH questionnaire part
personmonths_1mo <- sum(hh_kim_bl$nbre_menage_conc[!is.na(hh_kim_bl$Un_des_membres_de_votre_m_nage)])
personmonths_1mo
# person-months recorded over 3 months
personmonths_3mo <- personmonths_1mo*3
personmonths_3mo

# estimate rates
hh_kim_HCUfreq <- hh_hcu_kim_counts %>%
  filter(round == "bl") %>%
  summarise(
    visits_3mo_privatepharmacy = sum(hcu_3mo_privatepharmacy, na.rm = TRUE),
    visits_3mo_healthcentre = sum(hcu_3mo_healthcentre, na.rm = TRUE),
    visits_3mo_selfmedication = sum(hcu_3mo_selfmedication, na.rm = TRUE),
    visits_3mo_traditionalhealer = sum(hcu_3mo_traditionalhealer, na.rm = TRUE),
    visits_1mo_privatepharmacy = sum(hcu_1mo_privatepharmacy, na.rm = TRUE),
    visits_1mo_healthcentre = sum(hcu_1mo_healthcentre, na.rm = TRUE),
    visits_1mo_selfmedication = sum(hcu_1mo_selfmedication, na.rm = TRUE),
    visits_1mo_traditionalhealer = sum(hcu_1mo_traditionalhealer, na.rm = TRUE)
  ) 
HCUKimpese <- hh_kim_HCUfreq %>%
  select(starts_with("visits")) %>% # Select only columns related to visits
  pivot_longer(
    cols = everything(), # Pivot all visit columns
    names_to = c("time_period", "visit_type"), # Split column names into time period and visit type
    names_pattern = "visits_(\\d+mo)_(.*)", # Regex to extract time period and visit type
    values_to = "number_of_visits" # Column for visit values
  ) %>%
  pivot_wider(
    names_from = time_period, # Spread time periods into separate columns
    values_from = number_of_visits # Populate with visit numbers
  )
colnames(HCUKimpese) <- c("providertype", "visitspast3mo", "visitspast1mo")
# remove the few selfmedication observations since no visits
HCUKimpese <- HCUKimpese %>% filter(providertype != "selfmedication")
# add a row with the total number of visits
HCUKimpese <- HCUKimpese %>% bind_rows(summarise(
      HCUKimpese, providertype = "overall", 
      visitspast3mo = sum(visitspast3mo, na.rm = TRUE),
      visitspast1mo = sum(visitspast1mo, na.rm = TRUE)))
# add rates
HCUKimpese$freq_3mo <- round(HCUKimpese$visitspast3mo*1000/personmonths_3mo,1)
HCUKimpese$ci_lower_3mo <- (HCUKimpese$visitspast3mo*1000/personmonths_3mo) - 1.96 * (sqrt(HCUKimpese$visitspast3mo / personmonths_3mo^2)*1000)
HCUKimpese$ci_upper_3mo <- (HCUKimpese$visitspast3mo*1000/personmonths_3mo) + 1.96 * (sqrt(HCUKimpese$visitspast3mo / personmonths_3mo^2)*1000)
HCUKimpese$freq_1mo <- round(HCUKimpese$visitspast1mo*1000/personmonths_1mo,1)
HCUKimpese$ci_lower_1mo <- (HCUKimpese$visitspast1mo*1000/personmonths_1mo) - 1.96 * (sqrt(HCUKimpese$visitspast1mo / personmonths_1mo^2)*1000)
HCUKimpese$ci_upper_1mo <- (HCUKimpese$visitspast1mo*1000/personmonths_1mo) + 1.96 * (sqrt(HCUKimpese$visitspast1mo / personmonths_1mo^2)*1000)
HCUKimpese
# export
write.table(hcu_kim_rate, "hcu_kim_rate.txt")

# NANORO BASELINE
# HH members with person-months recorded: 3881 (all records)/ 2465 (excluding HH with 15 or more)
population_HCUrecorded <- sum(hh_nan_HCUfreq3mo$n.householdmember[!is.na(hh_nan_HCUfreq3mo$hcu_3mo_pre_healthcentre_publique)])
population_HCUrecorded
# those in households where the number of HCU episodes was recorded: 3610
population_HCUrecorded_nvisits <- sum(hh_nan_HCUfreq3mo_HCUrecord$n.householdmember[!is.na(hh_nan_HCUfreq3mo_HCUrecord$hcu_3mo_pre_healthcentre_publique)])
population_HCUrecorded_nvisits

# person time follow up based on HH members with person-months recorded, using all households with a record of HCU (HCU part of the questionnaire) as denominator
sum(hh_nan_HCUfreq3mo$hcu_3mo_pre_healthcentre_publique[!is.na(hh_nan_HCUfreq3mo$hcu_3mo_pre_healthcentre_publique)])*1000/(population_HCUrecorded*90) 
# person time follow up based on HH members with person-months recorded, using all households with a record of HCU (HCU part of the questionnaire) AND the number of HCU episodes in the general part of the questionnaire
sum(hh_nan_HCUfreq3mo_HCUrecord$hcu_3mo_pre_healthcentre_publique[!is.na(hh_nan_HCUfreq3mo_HCUrecord$hcu_3mo_pre_healthcentre_publique)])*1000/(population_HCUrecorded*90) 
# difference is negligible: 1,05 per 1000 members per day vs. 0.881 per 1000 per day -> continue with the data from the HCU part of the questionnaire 
# comparing to 2021 pilot study (CABU1) ~ 1.46 per 1000 in Nanoro and Nazoanga

# number of healthcare visits in past 3 months recorded in the general questionaire (including self-medication)
table(hh_nan_bl_general$q4_nbr_fois_recherch_medi, useNA = "always") # 473 episodes, 329 households report at least one visit

# rate of HCU based on the general number of episodes
473/539280*1000 # 0.8770954 visits per 1000 inhabitants per day 

# estimate rate of visits to providers in PAST 3 MONTHS
# only households where also in the general questionnaire, the number of healthcare visits was recorded & only baseline
hh_nan_HCUfreq3mo_HCUrecord[is.na(hh_nan_HCUfreq3mo_HCUrecord)] <- 0
summary_hcu_nan_3mo <- hh_nan_HCUfreq3mo_HCUrecord %>% # add a row with the total number of visits
  summarize(n_members = sum(n.householdmember), 
            visitshealthcentre_pre = sum(hcu_3mo_pre_healthcentre_publique), 
            visitspharmacy_pre = sum(hcu_3mo_pre_privatepharmacy),
            visitsinformal_pre = sum(hcu_3mo_pre_informalvendor),
            visitstradhealer_pre = sum(hcu_3mo_pre_traditionalhealer), 
            freqhealthcentre_pre = sum(hcu_3mo_pre_healthcentre_publique)*1000/(sum(n.householdmember)*3), # per 1000 inhabitants per month
            freqpharmacy_pre = sum(hcu_3mo_pre_privatepharmacy)*1000/(sum(n.householdmember)*3), # per 1000 inhab per month           
            freqinformal_pre = sum(hcu_3mo_pre_informalvendor)*1000/(sum(n.householdmember)*3), # per 1000 inhab per month
            freqtradhealer_pre = sum(hcu_3mo_pre_traditionalhealer)*1000/(sum(n.householdmember)*3), # per 1000 inhab per month
            # Confidence intervals
            ci_lower_healthcentre_pre = (sum(hcu_3mo_pre_healthcentre_publique) * 1000 / (sum(n.householdmember)*3)) - 
              (1.96 * sqrt(sum(hcu_3mo_pre_healthcentre_publique) / ((sum(n.householdmember)*3)^2)) * 1000),
            ci_upper_healthcentre_pre = (sum(hcu_3mo_pre_healthcentre_publique) * 1000 / (sum(n.householdmember)*3)) + 
              (1.96 * sqrt(sum(hcu_3mo_pre_healthcentre_publique) / ((sum(n.householdmember)*3)^2)) * 1000),
            ci_lower_pharmacy_pre = (sum(hcu_3mo_pre_privatepharmacy) * 1000 / (sum(n.householdmember)*3)) - 
              (1.96 * sqrt(sum(hcu_3mo_pre_privatepharmacy) / ((sum(n.householdmember)*3)^2)) * 1000),
            ci_upper_pharmacy_pre = (sum(hcu_3mo_pre_privatepharmacy) * 1000 / (sum(n.householdmember)*3)) + 
              (1.96 * sqrt(sum(hcu_3mo_pre_privatepharmacy) / ((sum(n.householdmember)*3)^2)) * 1000),
            ci_lower_informal_pre = (sum(hcu_3mo_pre_informalvendor) * 1000 / (sum(n.householdmember)*3)) - 
              (1.96 * sqrt(sum(hcu_3mo_pre_informalvendor) / ((sum(n.householdmember)*3)^2)) * 1000),
            ci_upper_informal_pre = (sum(hcu_3mo_pre_informalvendor) * 1000 / (sum(n.householdmember)*3)) + 
              (1.96 * sqrt(sum(hcu_3mo_pre_informalvendor) / ((sum(n.householdmember)*3)^2)) * 1000),
            ci_lower_tradhealer_pre = (sum(hcu_3mo_pre_traditionalhealer) * 1000 / (sum(n.householdmember)*3)) - 
              (1.96 * sqrt(sum(hcu_3mo_pre_traditionalhealer) / ((sum(n.householdmember)*3)^2)) * 1000),
            ci_upper_tradhealer_pre = (sum(hcu_3mo_pre_traditionalhealer) * 1000 / (sum(n.householdmember)*3)) + 
              (1.96 * sqrt(sum(hcu_3mo_pre_traditionalhealer) / ((sum(n.householdmember)*3)^2)) * 1000))

# check underreporting 
table(hh_nan_bl_HCU$dayssinceHCU, useNA = "always") # 177 episodes reported in the last month versus 246 in the two months after

# estimate rate of visits to providers in PAST MONTH
hh_nan_HCUfreq1mo_HCUrecord[is.na(hh_nan_HCUfreq1mo_HCUrecord)] <- 0
summary_hcu_nan_1mo <- hh_nan_HCUfreq1mo_HCUrecord %>% # add a row with the total number of visits
  summarize(n_members = sum(n.householdmember), 
            visitshealthcentre_pre = sum(hcu_1mo_pre_healthcentre_publique), 
            visitspharmacy_pre = sum(hcu_1mo_pre_privatepharmacy),
            visitsinformal_pre = sum(hcu_1mo_pre_informalvendor),
            visitstradhealer_pre = sum(hcu_1mo_pre_traditionalhealer), 
            freqhealthcentre_pre = sum(hcu_1mo_pre_healthcentre_publique) * 1000 / sum(n.householdmember),
            freqpharmacy_pre = sum(hcu_1mo_pre_privatepharmacy) * 1000 / sum(n.householdmember),
            freqinformal_pre = sum(hcu_1mo_pre_informalvendor) * 1000 / sum(n.householdmember),
            freqtradhealer_pre = sum(hcu_1mo_pre_traditionalhealer)*1000/(sum(n.householdmember)), # per 1000 inhab per month
            # Confidence intervals
            ci_lower_healthcentre_pre = (sum(hcu_1mo_pre_healthcentre_publique) * 1000 / sum(n.householdmember)) - 
              (1.96 * sqrt(sum(hcu_1mo_pre_healthcentre_publique) / (sum(n.householdmember)^2)) * 1000),
            ci_upper_healthcentre_pre = (sum(hcu_1mo_pre_healthcentre_publique) * 1000 / sum(n.householdmember)) + 
              (1.96 * sqrt(sum(hcu_1mo_pre_healthcentre_publique) / (sum(n.householdmember)^2)) * 1000),
            ci_lower_pharmacy_pre = (sum(hcu_1mo_pre_privatepharmacy) * 1000 / sum(n.householdmember)) - 
              (1.96 * sqrt(sum(hcu_1mo_pre_privatepharmacy) / (sum(n.householdmember)^2)) * 1000),
            ci_upper_pharmacy_pre = (sum(hcu_1mo_pre_privatepharmacy) * 1000 / sum(n.householdmember)) + 
              (1.96 * sqrt(sum(hcu_1mo_pre_privatepharmacy) / (sum(n.householdmember)^2)) * 1000),
            ci_lower_informal_pre = (sum(hcu_1mo_pre_informalvendor) * 1000 / sum(n.householdmember)) - 
              (1.96 * sqrt(sum(hcu_1mo_pre_informalvendor) / (sum(n.householdmember)^2)) * 1000),
            ci_upper_informal_pre = (sum(hcu_1mo_pre_informalvendor) * 1000 / sum(n.householdmember)) + 
              (1.96 * sqrt(sum(hcu_1mo_pre_informalvendor) / (sum(n.householdmember)^2)) * 1000),
            ci_lower_tradhealer_pre = (sum(hcu_1mo_pre_traditionalhealer) * 1000 / sum(n.householdmember)) - 
              (1.96 * sqrt(sum(hcu_1mo_pre_traditionalhealer) / (sum(n.householdmember)^2)) * 1000),
            ci_upper_tradhealer_pre = (sum(hcu_1mo_pre_traditionalhealer) * 1000 / sum(n.householdmember)) + 
              (1.96 * sqrt(sum(hcu_1mo_pre_traditionalhealer) / (sum(n.householdmember)^2)) * 1000))

# merge both tables
summary_hcu_nan_1mo$recall <- "1 mo" # specify duration during which healthcare utilisation was recalled
summary_hcu_nan_3mo$recall <- "3 mo"
HCUNanoro <- rbind(summary_hcu_nan_1mo, summary_hcu_nan_3mo)

# add overall
HCUNanoro$allvisits <- HCUNanoro$visitshealthcentre_pre + HCUNanoro$visitspharmacy_pre + HCUNanoro$visitsinformal_pre + HCUNanoro$visitstradhealer_pre
HCUNanoro$overallfreq <- HCUNanoro$allvisits*1000 / HCUNanoro$n_members
HCUNanoro$overallfreq[HCUNanoro$recall=="3 mo"] <- HCUNanoro$allvisits[HCUNanoro$recall=="3 mo"]*1000 / (HCUNanoro$n_members[HCUNanoro$recall=="3 mo"]*3)
HCUNanoro$ci_lower_overallfreq <- HCUNanoro$overallfreq - 
  (1.96 * sqrt(HCUNanoro$allvisits / (HCUNanoro$n_members^2)) * 1000)
HCUNanoro$ci_upper_overallfreq <- HCUNanoro$overallfreq + 
  (1.96 * sqrt(HCUNanoro$allvisits / (HCUNanoro$n_members^2)) * 1000)
HCUNanoro$ci_lower_overallfreq[HCUNanoro$recall=="3 mo"] <- HCUNanoro$overallfreq[HCUNanoro$recall=="3 mo"] - 
  (1.96 * sqrt(HCUNanoro$allvisits[HCUNanoro$recall=="3 mo"] / (HCUNanoro$n_members[HCUNanoro$recall=="3 mo"]^2)) * 1000)
HCUNanoro$ci_upper_overallfreq[HCUNanoro$recall=="3 mo"] <- HCUNanoro$overallfreq[HCUNanoro$recall=="3 mo"] + 
  (1.96 * sqrt(HCUNanoro$allvisits[HCUNanoro$recall=="3 mo"] / (HCUNanoro$n_members[HCUNanoro$recall=="3 mo"]^2)) * 1000)
write.table(HCUNanoro, "HCUNanoro.txt")

#### 2. ESTIMATE PREVALENCE OF WATER-SANITATION-HYGIENE INDICATORS ####
# create a binary variable for whether a sample was collected and whether ESBL E. coli was identified
hh_kim_bl$stoolcollected[!is.na(hh_kim_bl$receptiondate)] <- 1 # reception date is not NA for any of the samples collected
hh_kim_bl$ESBL_E[hh_kim_bl$"Escherichia.coli"=="OUI"] <- 1

# keep just one observation per household (every menage_id corresponds to one _submission__id (Kobo HH entry))
hh_wash_aggregate_kim_bl <- hh_wash_kim_bl %>% 
  filter(!is.na(menage_id)) %>%
  group_by(grappe, menage_id, X_submission__id, nmbre_personne_menage_001, nbre_enf_0_5ans_001, 
           q1_diarrhee_prevenu, q1_diarrhee_prevenu.garder_le_r_cipient_d_eau_couvert_dans_l, q1_diarrhee_prevenu.ne_pas_tremper_les_doigts_dans_le_verre_, 
           q1_diarrhee_prevenu.utiliser_un_ustensile_avec_une_poign_e_p, q1_diarrhee_prevenu.couvrir_les_aliments, q1_diarrhee_prevenu.faire_bouillir_l_eau_de_boisson, 
           q1_diarrhee_prevenu.filtrer_l_eau_de_boisson, q1_diarrhee_prevenu.autre_pr_ciser_, q1_diarrhee_prevenu.on_ne_peut_pas_l_viter, 
           q1_diarrhee_prevenu.ne_sait_pas, autr_mesur_prev_diarrhe, q2_source_princ_saison_seche, q3_source_princ_saison_pluv, q4_bidon_stock, 
           q5a_bidon_ferme_rempli, q5b_bidon_ferme_vide, q5c_bidon_nettoye, q6_traite_eau, q6_autre_traitmen_eau, q7_type_inst_sanitaire, 
           q7_autr_typ_ins_sanitair, q8_autr_lieu_defecation, q8_autr_lieu_defecation.toilettes_avec_chasse_d_eau_raccord_e___, 
           q8_autr_lieu_defecation.latrines___fosse_am_lior_e_avec_ventilat, q8_autr_lieu_defecation.latrines___fosse_avec_dalle, 
           q8_autr_lieu_defecation.latrine___fosse_sans_dalle, q8_autr_lieu_defecation.d_f_cation_en_plein_air, q8_autr_lieu_defecation.autre, 
           q8_autre_preciser, q9_toilette_partagee, q10_combien_partag, q11_dernier_nettoyage, q12_elimine_selle_enf, q12_autre_preciser, 
           q13_vidange_toilette, q14_produit_lavag_main, q15_lave_apr_defec, q16_lave_apr_repas, q17_animaux_menage, q18_animaux_interieur, 
           q18_animaux_interieur.1__b_ufs, q18_animaux_interieur.2__moutons_ou_ch_vres, q18_animaux_interieur.3__porcs, 
           q18_animaux_interieur.4___nes_chevaux, q18_animaux_interieur.5__poules_oies_ou_canards, q18_animaux_interieur.6__autre_sp_cifiez, 
           q18_autre_specifie, q19_animaux_dehors, q19_animaux_dehors.1__b_ufs, q19_animaux_dehors.2__moutons_ou_ch_vres, 
           q19_animaux_dehors.3__porcs, q19_animaux_dehors.4___nes_chevaux, q19_animaux_dehors.5__poules_oies_ou_canards, 
           q19_animaux_dehors.6__autre_sp_cifiez, q19_autre_specifie, q20_excrement_animaux, q21_animal_malade, 
           q21_animal_malade.1__faire_venir_un_v_t_rinaire_qui_peut_p, q21_animal_malade.2__acheter_un_traitement_sans_consultati, 
           q21_animal_malade.3__vendre_l_animal___un_boucher, q21_animal_malade.4__on_l_abat_et_on_consomme_la_viande___, 
           q21_animal_malade.5__quand_l_animal_meurt_on_le_jette_ou_o, q21_animal_malade.6__quand_l_animal_meurt_on_consomme_la_v, 
           q1_dispo_medica_menag, q2_nbre_sorte_medica) %>%
  summarise(nstoolscollected = sum(stoolcollected), nESBL_E = sum(ESBL_E), pESBL_E = (sum(ESBL_E)/sum(stoolcollected)))

# prevalence ESBL E.coli
totalnstoolscollected <- sum(hh_kim_bl$stoolcollected[!is.na(hh_kim_bl$stoolcollected)])
totalESBL_E <- sum(hh_kim_bl$ESBL_E[!is.na(hh_kim_bl$ESBL_E)])
pctESBL_E <- totalESBL_E/totalnstoolscollected
pctESBL_E

# compare to HH data
str(HHlocationR1)

# Kimpese

#### 3. TOPICS RETAINED FROM THE COMMUNITY-BASED INTERVENTION ####
