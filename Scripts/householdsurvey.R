#####################################################
# CABU-EICO household survey                        #
# rate of healthcare seeking, prevalence of         #
# WASH behaviour/conditions                         #
#####################################################

# install/load packages
pacman::p_load(readxl,lubridate,dplyr,survey)

# Kimpese
# import cleaned data from the baseline household visits - WASH part linked to stool culture results
hh_wash_kim_bl <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/cabu_intervention/humanR1_Kim_pseudo.csv")

# import raw data from the baseline household visits - healthcare utilisation part
HHlocationR1 <- read_excel("db/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx", 
                           sheet = "CABU_enq_comm_2023")
HHlocationR1 <- HHlocationR1 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
HCU_kim_bl <- read_excel("db/householdsurvey/CABU_enq_comm_2023_-_all_versions_-_False_-_2024-01-31-10-12-48.xlsx", 
                         sheet = "group_qh5tc83")
table(HCU_kim_bl$q4_nbr_fois_recherch_medi, useNA = "always")
HHR1 <- merge(HHindividualR1, HHlocationR1, by.x = "_submission__id", by.y = "_id", all = T)

# Nanoro
# import raw data from the baseline household visits - healthcare utilisation part
hh_nan_bl <- read_excel("db/householdsurvey/WP4_WASH.xls", sheet = "WP4_WASH")
# several bits/repeats of the questionnaire in redcap format
# general household structure etc.
hh_nan_bl_general <- hh_nan_bl %>% filter(is.na(redcap_repeat_instrument) & !is.na(nmbre_personne_menage))
# repeat with healthcare utilisation
hh_nan_bl_HCU <- hh_nan_bl %>% filter(redcap_repeat_instrument=="visite_structure_sanitaire")

# number of healthcare visits

# distribution of provider types
table(hh_nan_bl_HCU$q4_type_fournisseur)
hh_HCU_nan_bl <- hh_HCU_nan_bl %>% mutate(providertype = case_when( # add labels 
  q4_type_fournisseur == 1 ~ "selfmedication",
  q4_type_fournisseur == 2 ~ "informalvendor",
  q4_type_fournisseur == 3 ~ "traditionalhealer",
  q4_type_fournisseur == 4 ~ "privatepharmacy",
  q4_type_fournisseur == 5 ~ "privatepharmacy",
  q4_type_fournisseur == 6 ~ "privatepharmacy", # selfmedication/OTC from the health centre pharmacy, so fits probably best private pharmacy
  q4_type_fournisseur == 7 ~ "healthcentre_publique",
  TRUE ~ as.character(q4_type_fournisseur)))
  
# remove visits during which HCU was not recorded or incomplete (variable trois_derniers_mois_complete)
hh_HCU_nan_bl <- hh_HCU_nan_bl %>% filter(trois_derniers_mois_complete == 2)

str(hh_HCU_nan_bl)
table(hh_wash_nan_bl$q4_nbr_fois_recherch_medi, hh_wash_nan_bl$trois_derniers_mois_complete, useNA = "always")
table(hh_wash_nan_bl$trois_derniers_mois_complete, useNA = "always")

#### 1. ESTIMATE RATE OF HEALTHCARE UTILISATION ####
# distribution of providers
table(hh_HCU_nan_bl$q4_type_fournisseur)

# estimate rate of visits to providers

# check underreporting - hasn't been completed!
table(hh_HCU_nan_bl$q3_date_visite, useNA = "always")

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
