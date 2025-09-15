#####################################################
# CABU-EICO patientsurveys                          #
# prevalence of antibiotic use, AWaRe distribution  #
#####################################################

# install/load packages
pacman::p_load(readxl, writexl, lubridate, haven, MASS, tidyr, dplyr, digest, ggplot2, ggthemes, ggalluvial, survey, srvyr, gtsummary, lme4, broom.mixed)

#### 1. IMPORT DATA KIMPESE #### 
# 1.1 Kimpese baseline
# import patient data
patient_kim_bl <- read_excel("db/patientsurvey/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-01-16-12-52-23.xlsx", 
                       sheet = "Questionnaire patient CABU-RDC")
patient_kim_post <- read_excel("db/patientsurvey/R2_Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-04-19-11-39-44.xlsx")
# remove variables that the baseline survey data have but the post data don't
patient_kim_bl <- subset(patient_kim_bl, select = -c(interviewdate, diag_spec))
# make sure both have the same column names
colnames(patient_kim_post) <- colnames(patient_kim_bl) # CHECK WHICH VARS DONT CORRESPOND BETWEEN BOTH DF
# append both: add a variable saying which round, then append
patient_kim_bl$round <- "baseline"
patient_kim_post$round <- "post"
patient_kim <- rbind(patient_kim_bl, patient_kim_post)

# remove surveys from two backup clusters that were dropped following too little population/patients at dispensors - SEE if Wene could be recovered as extra control cluster
# patient_kim <- patient_kim %>% filter(choices_cluster != "KITOBOLA_AS_Kilueka" & choices_cluster != "WENE_AS_Kilueka")

# two observations assigned private clinic in Zamba 1 while there are only private pharmacies in that village
patient_kim$providertype[patient_kim$providertype=="privateclinic"&patient_kim$choices_cluster=="ZAMBA_I_AS_Malanga"] <- "privatepharmacy"
# five observations assigned private clinic in Vunda Nsole, while there is only a public health centre in that village
patient_kim$providertype[patient_kim$providertype=="privateclinic"&patient_kim$choices_cluster=="VUNDA_NSOLE_AS_Vunda_Nsole"] <- "healthcentre_publique"
# six observations assigned private clinic in Nkula, while there is none, but there is a public health centre and a private pharmacy in that village. Since 5 out of 6 had a malaria RDT done, must have been at the health centre
patient_kim$providertype[patient_kim$providertype=="privateclinic"&patient_kim$choices_cluster=="NKULA_AS_Viaza"] <- "healthcentre_publique"
# two observations assigned private clinic in Mpete, while there is none, but there is a public health centre. Both had a malaria RDT done
patient_kim$providertype[patient_kim$providertype=="privateclinic"&patient_kim$choices_cluster=="MPETE_NKONDO_AS_Kiasunga"] <- "healthcentre_publique"
# two observations assigned private clinic in Malanga, while there is none, but there is a public health centre and a pharmacy. One had a malaria RDT done
patient_kim$providertype[patient_kim$providertype=="privateclinic"&patient_kim$choices_cluster=="MALANGA_AS_Malanga"] <- "healthcentre_publique"
# one observations assigned private clinic in Lukengezi, while there is none, but there is a public health centre and a pharmacy. Had several tests done
patient_kim$providertype[patient_kim$providertype=="privateclinic"&patient_kim$choices_cluster=="LUKENGEZI_ET_POSTE_AS_CECO"] <- "healthcentre_publique"
# four observations assigned private pharmacy in Lukengezi, checked, and seem indeed pharmacy visits

# add value to type of dispensor
patient_kim$another_disp <- tolower(patient_kim$another_disp)
patient_kim$dispenserpharmacie[grepl("tudiant", patient_kim$another_disp)] <- "student medicine or nursing"

# complete missing ages based on dob
table(patient_kim$birthyear[is.na(patient_kim$ageyears)], useNA = "always")
patient_kim$ageyears[is.na(patient_kim$ageyears)] <- round(as.numeric(patient_kim$today[is.na(patient_kim$ageyears)]-patient_kim$birthyear[is.na(patient_kim$ageyears)])/365.25, 0)

# remove unnecessary variables
patient_kim <- patient_kim[, c("round", "today", "choices_cluster", "providertype", "providernr", "dispenserpharmacie", "patientnr", "caretaker", "sex", 
                                        "ageyears", "educationlevel", "illness", "illness_spec", "illness_other", "symptoms", "symptoms/fever",
                                        "symptoms/vomiting", "symptoms/diarrhoea", "symptoms/cough", "symptoms/mauxdegorge",
                                        "symptoms/ecoulementnasale", "symptoms/other_respiratory_sign", "symptoms/rash", "symptoms/abdo_pain",
                                        "symptoms/wound", "symptoms/myalgia", "symptoms/headache", "symptoms/nausea", "symptoms/prurite",
                                        "symptoms/nosymptoms", "symptoms/othersymptoms", "symptoms_other", "date_onset", "diag_test",
                                        "diag_test/none", "diag_test/RDT_malaria", "diag_test/microscopy_malaria", "diag_test/sputumtb",
                                        "diag_test/other", "Comment_vous_vous_tes_procure", "diag_test_other", "quel_tait_le_diagnostic_final",
                                        "quel_tait_le_diagnostic_final/pneumonia", "quel_tait_le_diagnostic_final/bronchitis",
                                        "quel_tait_le_diagnostic_final/bronchiolitis", "quel_tait_le_diagnostic_final/malaria",
                                        "quel_tait_le_diagnostic_final/typhoid", "quel_tait_le_diagnostic_final/gastroenteritis",
                                        "quel_tait_le_diagnostic_final/unknown", "quel_tait_le_diagnostic_final/other", "diag_spec_other",
                                         "absprescribed", "prescriptionwheredispensed", "prescriptionwhereother",
                                        "nantibiotics", "matchedprescription", "nomatchreasons", "nomatchreason_other", "firstuse",
                                        "preventive_use", "another_raison", "preventive_use_spec", "antimalarial", "antimalarial_spec",
                                        "confirm_end", "_uuid", "_index")]
# dates reformatted
table(patient_kim$date_onset, useNA = "always")
# duration symptoms until consultation
patient_kim$days_since_onset <- as.numeric(as.Date(patient_kim$today) - as.Date(patient_kim$date_onset)) # REQUIRES SOME CHECKS OF SYMPTOM ONSET DATES

# var agegroups
table(patient_kim$ageyears)
patient_kim$agegroup[patient_kim$ageyears<5] <- "0-4 yr"
patient_kim$agegroup[patient_kim$ageyears>4.999] <- "5-17 yr"
patient_kim$agegroup[patient_kim$ageyears>17.999] <- "18-64 yr"
patient_kim$agegroup[patient_kim$ageyears>64.999] <- "65+ yr"
table(patient_kim$agegroup, useNA = "always")

# assign whether intervention or control cluster
patient_kim <- patient_kim %>%
  mutate(intervention = ifelse(choices_cluster %in% c("CELLULE_MBUKA3_AS_Yanga_Dia_Songa", "CELLULE MBUKA3 (AS Yanga Dia Songa)", "KIASUNGUA_AS_Kisaunga", 
                                                      "KIASUNGUA (AS Kisaunga)", "KILUEKA_AS_Kilueka", "KILUEKA (AS Kilueka)", "MBANZA NDAMBA (AS Kilueka)", 
                                                      "LUKENGEZI ET POSTE (AS CECO)", "LUKENGEZI_ET_POSTE_AS_CECO", "MBANZA_NDAMBA_AS_Kilueka", 
                                                      "MONT FLEURY (AS Kimbanguiste)", "MONT_FLEURY_AS_Kimbanguiste", "MPETE NKONDO (AS Kiasunga)", 
                                                      "MPETE_NKONDO_AS_Kiasunga", "Q3 (AS Kimbanguiste)", "Q3_AS_Kimbanguiste", "SANZIKUA_AS_Vunda_Nsole", 
                                                      "VIAZA_AS_Viaza", "VUNDA_NSOLE_AS_Vunda_Nsole"), 
                               "intervention", "control"))
table(patient_kim$choices_cluster, patient_kim$intervention, useNA = "always")

# remove informal provider visits (too little and only at baseline
table(patient_kim$providertype, patient_kim$round)
# patient_kim$providertype[patient_kim$providertype=="informalstore"] <- "privatepharmacy"

# add a variable with a single clinical presentation that is based on the diagnosis given with the highest likelihood to result in antibiotic treatment
# clinical presentations that are likely to result in antibiotic use are assigned first
patient_kim$clinpres[grepl("pneumoni", patient_kim$quel_tait_le_diagnostic_final, ignore.case = TRUE)] <- "pneumonia"
patient_kim$clinpres[grepl("typho", patient_kim$quel_tait_le_diagnostic_final, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "typhoid"
patient_kim$clinpres[grepl("bronchit", patient_kim$quel_tait_le_diagnostic_final, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "bronchitis"
patient_kim$clinpres[grepl("gastroenterit", patient_kim$quel_tait_le_diagnostic_final, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "gastroenteritis"
patient_kim$clinpres[grepl("bronchiolit", patient_kim$quel_tait_le_diagnostic_final, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "bronchiolitis"
patient_kim$clinpres[grepl("buruli_ulcer", patient_kim$quel_tait_le_diagnostic_final, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("malaria", patient_kim$quel_tait_le_diagnostic_final, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "malaria"
table(patient_kim$clinpres, useNA = "always")
# one not entered as pneumonia under diagnosis, but as illness and symptoms match pneumonia
patient_kim$clinpres[patient_kim$illness_spec=="pneumonia"& is.na(patient_kim$clinpres)] <- "pneumonia" 
# for those where other was entered in the final diagnosis field
patient_kim$diag_spec_other <- tolower(patient_kim$diag_spec_other)
patient_kim$clinpres[grepl("septi|sepsis", patient_kim$diag_spec_other, ignore.case = TRUE)] <- "sepsis"
patient_kim$clinpres[grepl("otite moyenne aigue", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "acute otitis media"
patient_kim$clinpres[grepl("dermatose|dermtose|dermatite|dermatolose|dermo-hypodermique necrosante|dermohypodermite|cellulite|brulure|plaie|abcès|ulcère", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("ulcere|brûlure du cou|écorchures|erysipele|folliculite|furoncles", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("gâle|morsure de chien|morsure de serpent|morsure du serpent|staphylococcie cutanee|orgelet|panaris|phlegmon d'origine dentaire", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("infection digest|diarrhee|dysenterie|entérite", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "gastroenteritis"
patient_kim$clinpres[grepl("dermatose", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("carie dentaire|carrie|infection dentaire|periodontite chronique|periodontite sub aigue|phlegmon d'origine dentaire", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "dental"
patient_kim$clinpres[grepl("grippe|syndrome grippal|sd grippal|syndrome paludo gripal", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "acute respiratory infection"
patient_kim$clinpres[grepl("asthme|crise d'asthme", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "asthma/COPD"
patient_kim$clinpres[grepl("appendic", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "appendicitis"
patient_kim$clinpres[grepl("gastrite|dyspeptiq|colique infantile", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "unexplained gastro-intestinal"
patient_kim$clinpres[grepl("hypotension artérielle et sd dyspeptique|hypotension artérielle, sd syspeptique|sd dyspeptique|sd dyspetique|syndrome dyspeptique|péritonite", 
                           patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "unexplained gastro-intestinal"
patient_kim$clinpres[grepl("parasitos|amibias|verminose", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "parasitic infections"
patient_kim$clinpres[grepl("cystit|urinair|urogénital", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "urinary tract infection"
patient_kim$clinpres[grepl("information urogenitale|iu|iug|prostatite", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "urinary tract infection"
patient_kim$clinpres[grepl("IST|annexite|infection sexuellement transmissibles|syphilis|trichomonas vaginal", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "sexually transmitted infection"
patient_kim$clinpres[grepl("diabete|diabète|daiabete|hyperglycémie", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "diabetes"
patient_kim$clinpres[grepl("conjonctivite|corps étranger dans l'oeil|conjoctivite|orgelet", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "eye condition"
patient_kim$clinpres[grepl("fever", patient_kim$symptoms, ignore.case = TRUE)& grepl("syndrome infectieux|infection|sd infectieux|syndromes infectieux", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "unexplained fever"
patient_kim$clinpres[grepl("syndrome infectieux|infection|rougeole|fièvre éruptives|sd infectieux|syndromes infectieux|encéphalite", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other infections"
patient_kim$clinpres[grepl("rhumastime|lombarthose|rhumatisme|arthrite rumathoide|varice|hémorroïdaire|sd syspeptique|stomatite|lymphome|algie de|anemie|arthrose|céphalees|anaphylactique|brûlures thermique|choc|circoncision mal faite|douleur gestionnaire|dysmenorrhee", 
                           patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("dysménorr|dystrophie ovarienne|endométrite|accouchement|avortement|grossesse|menace d'avortement|contraception|contraceptif|IVG", 
                           patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("reaction allergique|gonarthrite|lombarthrose|lombalgies|trouble hormonal|syndrome vestibulaire|vertigineux|trouble érectile", 
                           patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("kyste ovarien|kyste ovarien gauche|suspicion kyste ovarien gauche|placenta praevia|syndrome adherenciel|syndrome appendiculaire|souffrance foetale aigue|post règle|prophylaxie|dystrophies ovarienne|rpm", 
                           patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious" # rpm is rupture prémature des membrames
patient_kim$clinpres[grepl("abdomen aiguë chirurgicale|kystectomie|appendicect", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "post surgery"
patient_kim$clinpres[grepl("amygdalit|engine|laryngite|angine", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "pharyngitis"
patient_kim$clinpres[grepl("covid-19", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "covid-19"
patient_kim$clinpres[grepl("trauma|fracture|contusion|douleur traumatique", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("hernie inguinale|hernie inguinale bilaterale|hernie inguinale droite|hernie inguinale gauche|pointe herniaire bilatérale", 
                           patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("HTA|hypertens|cardiopathie|valvulopathie", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "cardiovascular condition"
patient_kim$clinpres[grepl("hypotension arterielle|hypotension artérielle", patient_kim$diag_spec_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "cardiovascular condition"

# no diagnosis reported by prescriber, but self-reported or based on symptoms
patient_kim$illness_other <- tolower(patient_kim$illness_other)
patient_kim$clinpres[grepl("septi|sepsis", patient_kim$illness_other, ignore.case = TRUE)] <- "sepsis"
patient_kim$clinpres[grepl("otite moyenne aigue", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "acute otitis media"
patient_kim$clinpres[grepl("otalgie", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres) & grepl("fever", patient_kim$symptoms, ignore.case = T)] <- "acute otitis media"
patient_kim$clinpres[grepl("plaie|dermatose|dermtose|dermatite|dermatolose|dermo-hypodermique necrosante|dermohypodermite|cellulite|brulure|plaie|abcès|ulcère", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("ulcere|brûlure du cou|écorchures|erysipele|folliculite|furoncles|tumefaction|teigne tondante|éruptions cuta|eruptions cuta|brûlures", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("gâle|morsure de chien|morsure de serpent|morsure du serpent|staphylococcie cutanee|orgelet|panaris|phlegmon d'origine dentaire", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("infection digest|diarrhee|dysenterie|entérite", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "gastroenteritis"
patient_kim$clinpres[grepl("dermatose", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("carie dentaire|carrie|infection dentaire|periodontite chronique|periodontite sub aigue|phlegmon d'origine dentaire|douleurs dentaire|douleur dentaire", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "dental"
patient_kim$clinpres[grepl("grippe|syndrome grippal|sd grippal|syndrome paludo gripal|rhume", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "acute respiratory infection"
patient_kim$clinpres[grepl("asthme|crise d'asthme", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "asthma/COPD"
patient_kim$clinpres[patient_kim$illness_spec=="fever" & grepl("diarrhee", patient_kim$symptoms_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "gastroenteritis"
patient_kim$clinpres[patient_kim$illness_spec=="fever" & grepl("diarrho", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "gastroenteritis"
patient_kim$clinpres[grepl("gastrite|dyspeptiq|colique infantile", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "unexplained gastro-intestinal"
patient_kim$clinpres[grepl("hypotension artérielle et sd dyspeptique|hypotension artérielle, sd syspeptique|sd dyspeptique|sd dyspetique|syndrome dyspeptique|péritonite", 
                           patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "unexplained gastro-intestinal"
patient_kim$clinpres[grepl("parasitos|amibias|verminose", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "parasitic infections"
patient_kim$clinpres[grepl("cystit|urinair|urogénital", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "urinary tract infection"
patient_kim$clinpres[grepl("information urogenitale|iu|iug|prostatite", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "urinary tract infection"
patient_kim$clinpres[grepl("IST|annexite|infection sexuellement transmissibles|syphilis|trichomonas vaginal", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "sexually transmitted infection"
patient_kim$clinpres[grepl("diabete|diabète|daiabete|hyperglycémie", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "diabetes"
patient_kim$clinpres[grepl("conjonctivite|corps étranger dans l'oeil|conjoctivite|orgelet|douleur oculaire droit|flou visuel|conjonctivale", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "eye condition"
patient_kim$clinpres[grepl("syndrome infectieux|infection|rougeole|fièvre éruptives|sd infectieux|syndromes infectieux|encéphalite", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other infections"
patient_kim$clinpres[grepl("rhumastime|lombarthose|rhumatisme|arthrite rumathoide|varice|hémorroïdaire|sd syspeptique|stomatite|lymphome|algie de|anemie|arthrose|céphalees|anaphylactique|brûlures thermique|choc|circoncision mal faite|douleur gestionnaire|dysmenorrhee", 
                           patient_kim$illness_other, ignore.case = TRUE) & !grepl("fever", patient_kim$symptoms, ignore.case = T) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("dysménorr|dystrophie ovarienne|endométrite|accouchement|avortement|grossesse|menace d'avortement|contraception|contraceptif|IVG", 
                           patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("reaction allergique|gonarthrite|lombarthrose|lombalgies|trouble hormonal|syndrome vestibulaire|vertigineux|trouble érectile", 
                           patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("kyste ovarien|kyste ovarien gauche|suspicion kyste ovarien gauche|placenta praevia|syndrome adherenciel|syndrome appendiculaire|souffrance foetale aigue|post règle|prophylaxie|dystrophies ovarienne", 
                           patient_kim$illness_other, ignore.case = TRUE) & !grepl("fever", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("abdomen aiguë chirurgicale|kystectomie|appendicect|chirurgie", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "post surgery"
patient_kim$clinpres[grepl("amygdalit|engine|laryngite|angine", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "pharyngitis"
patient_kim$clinpres[grepl("covid-19", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "covid-19"
patient_kim$clinpres[grepl("trauma|fracture|contusion|douleur traumatique", patient_kim$illness_other, ignore.case = TRUE) & !grepl("fever", patient_kim$symptoms, ignore.case = T) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("hernie inguinale|hernie inguinale bilaterale|hernie inguinale droite|hernie inguinale gauche|pointe herniaire bilatérale", 
                           patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("HTA|hypertens|cardiopathie|valvulopathie", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "cardiovascular condition"
patient_kim$clinpres[grepl("hypotension arterielle|hypotension artérielle", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "cardiovascular condition"
patient_kim$clinpres[grepl("blessure|démangeaison|eruption cutanee|éruption cutanée|phlyctene|prurit|prurit généralisé|prurite generalise|tuméfaction douloureuse au niveau du quandrant inférieur de la fesse droite|staphylococcie cutanee", 
                           patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("bourdonnement abdominal|colique abdominale|diarrhée|douleur abdominal|douleur abdominale|douleurs abdominales|epigastralgie|hypogastralgie|inappetence|nausée|odynophagie|satiete precoce", 
                           patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "unexplained gastro-intestinal"
patient_kim$clinpres[grepl("maux de gorge", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "pharyngitis"
patient_kim$clinpres[grepl("selle glaireux malodorant|selles liquides|vomissement|vomissements", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "gastroenteritis"
patient_kim$clinpres[grepl("dysurie|hematurie|hématurie|mictalgie|myctalgie|urine malodorante", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "urinary tract infection"
patient_kim$clinpres[grepl("cervicalgie|arthralgie|douleur au niveau du genoux|douleur des membres inférieurs|douleur membre inférieur bilatérale|douleurs de l'epaule|lombalgie|lombo-hypogastralgie|thoracalgie gauche|torticolis", 
                           patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("écoulement nasale|rhinorhee", patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "acute respiratory infection"
patient_kim$clinpres[grepl("anorexie|asthénie physique|convulsions|courbature|courbatures|frissons|myalgie|myalgies|polyarthralgie", patient_kim$illness_other, ignore.case = TRUE) & !grepl("fever", patient_kim$symptoms, ignore.case = T) & is.na(patient_kim$clinpres)] <- "other non infectious"
patient_kim$clinpres[grepl("perte blanche abondante|prurit vaginal|prurit vulvaire|prurit vulvo-vaginal|dilatation du col uterin|perte de liquide|prurit penien|prurit pénien|prurit penien et inguinal|prurit vaginal|prurit vulvaire|prurit vulvo-vaginal|hémorragies genitale|douleur penienne", 
                           patient_kim$illness_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "sexually transmitted infection"
patient_kim$clinpres[patient_kim$illness_spec=="scabies"& is.na(patient_kim$clinpres)] <- "scabies"
patient_kim$clinpres[patient_kim$illness_spec=="buruli_ulcer"& is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[patient_kim$illness_spec=="swollen_finger"& is.na(patient_kim$clinpres)] <- "skin/soft tissue infection" # both as result of a wound
patient_kim$clinpres[patient_kim$illness_spec=="cough"& is.na(patient_kim$clinpres) & !grepl("rash abdo_pain", patient_kim$symptoms, ignore.case = TRUE) & !grepl("prurit", patient_kim$symptoms_other, ignore.case = TRUE)] <- "acute respiratory infection" # checked one by one on symptoms or diagnostic tests indicating anything else than ARI, filtering those out
patient_kim$clinpres[patient_kim$illness_spec=="skinproblem"& is.na(patient_kim$clinpres) & grepl("rash", patient_kim$symptoms, ignore.case = TRUE) & !grepl("wound", patient_kim$symptoms, ignore.case = TRUE)& !grepl("tumefact|tuméfact|ERUPTIONS CUTANÉE PRURIGINEUSE|pustul|PRURITS GÉNITALES|Prurit pénien", patient_kim$symptoms_other, ignore.case = TRUE)] <- "other infections"
patient_kim$clinpres[patient_kim$illness_spec=="skinproblem"& is.na(patient_kim$clinpres) & grepl("wound", patient_kim$symptoms, ignore.case = TRUE)] <-"skin/soft tissue infection"
patient_kim$clinpres[patient_kim$illness_spec=="skinproblem"& is.na(patient_kim$clinpres) & grepl("tumefact|tuméfact|ERUPTIONS CUTANÉE PRURIGINEUSE|pustul", patient_kim$symptoms_other, ignore.case = TRUE)] <- "skin/soft tissue infection"
patient_kim$clinpres[patient_kim$illness_spec=="skinproblem"& is.na(patient_kim$clinpres) & grepl("PRURITS GÉNITALES|Prurit pénien", patient_kim$symptoms_other, ignore.case = TRUE)] <- "sexually transmitted infection"
patient_kim$clinpres[patient_kim$illness_spec=="fever" & grepl("mictal", patient_kim$symptoms_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "urinary tract infection"
patient_kim$clinpres[patient_kim$illness_spec=="fever" & grepl("wound", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[patient_kim$illness_spec=="fever" & grepl("cough", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "acute respiratory infection"
patient_kim$clinpres[patient_kim$illness_spec=="fever" & grepl("ecoulementnasale", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "acute respiratory infection"
patient_kim$clinpres[patient_kim$illness_spec=="fever" & grepl("mauxdegorge", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "acute respiratory infection"
patient_kim$clinpres[grepl("dentai", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "dental"
patient_kim$clinpres[grepl("dentai", patient_kim$symptoms_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "dental"
patient_kim$clinpres[patient_kim$illness_spec=="nausea_vomiting" & grepl("diarrho", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "gastroenteritis"
patient_kim$clinpres[grepl("tumef", patient_kim$symptoms_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("plai", patient_kim$symptoms_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "skin/soft tissue infection"
patient_kim$clinpres[grepl("diarrh", patient_kim$symptoms_other, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "gastroenteritis"

# fever with or without symptoms that can not be linked to a specific clinical presentation
patient_kim$clinpres[grepl("fever", patient_kim$symptoms, ignore.case = T) & is.na(patient_kim$clinpres)] <- "unexplained fever"
patient_kim$clinpres[patient_kim$illness_spec=="fever" & grepl("rash", patient_kim$symptoms, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "unexplained fever"

# check if still some not assigned and what symptoms these have
table(patient_kim$illness_other[patient_kim$illness=="yes_acute_illness"&is.na(patient_kim$clinpres)], useNA = "always")
symptoms_no_diagn <- patient_kim %>% filter(illness=="yes_acute_illness"&is.na(clinpres)) %>% select(providertype, illness_spec, illness_other, symptoms, symptoms_other, diag_test, diag_test_other, diag_spec_other)
write_xlsx(symptoms_no_diagn, "symptoms_no_diagn.xlsx") # checked, remaining are minor complaints or unspecific symptoms

# only when no other diagnostic entered, also add those labelled as unknown
patient_kim$clinpres[grepl("unknown", patient_kim$quel_tait_le_diagnostic_final, ignore.case = TRUE) & is.na(patient_kim$clinpres)] <- "non-specific symptoms or complaints"

table(patient_kim$clinpres[patient_kim$illness=="yes_acute_illness"], patient_kim$providertype[patient_kim$illness=="yes_acute_illness"], useNA = "always")
clinpresdistribution <- as.data.frame(table(patient_kim$clinpres[patient_kim$illness=="yes_acute_illness"], patient_kim$providertype[patient_kim$illness=="yes_acute_illness"], useNA = "always"))
clinpresdistribution
write_xlsx(clinpresdistribution, "clinpresdistribution.xlsx")

# group clinical presentations in broad categories
patient_kim$clinpres_broad <- patient_kim$clinpres
patient_kim$clinpres_broad[patient_kim$clinpres=="acute otitis media"] <- "acute respiratory infection"
patient_kim$clinpres_broad[patient_kim$clinpres=="bronchiolitis"] <- "acute respiratory infection"
patient_kim$clinpres_broad[patient_kim$clinpres=="bronchitis"] <- "acute respiratory infection"
patient_kim$clinpres_broad[patient_kim$clinpres=="covid-19"] <- "acute respiratory infection"
patient_kim$clinpres_broad[patient_kim$clinpres=="pharyngitis"] <- "acute respiratory infection"
patient_kim$clinpres_broad[patient_kim$clinpres=="cardiovascular condition"] <- "other"
patient_kim$clinpres_broad[patient_kim$clinpres=="asthma/COPD"] <- "other"
patient_kim$clinpres_broad[patient_kim$clinpres=="diabetes"] <- "other"
patient_kim$clinpres_broad[patient_kim$clinpres=="other infections"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
patient_kim$clinpres_broad[patient_kim$clinpres=="scabies"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
patient_kim$clinpres_broad[patient_kim$clinpres=="parasitic infections"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
patient_kim$clinpres_broad[patient_kim$clinpres=="post surgery"] <- "other"
patient_kim$clinpres_broad[patient_kim$clinpres=="other non infectious"] <- "other"
patient_kim$clinpres_broad[patient_kim$clinpres=="eye condition"] <- "other"
patient_kim$clinpres_broad[patient_kim$clinpres=="typhoid"] <- "typhoid or sepsis"
patient_kim$clinpres_broad[patient_kim$clinpres=="sepsis"] <- "typhoid or sepsis"
patient_kim$clinpres_broad[is.na(patient_kim$clinpres)] <- "other"

table(patient_kim$clinpres_broad, patient_kim$intervention, useNA = "always")
table(patient_kim$clinpres[patient_kim$clinpres_broad=="other"])

# import antibiotic data (a loop for individual antibiotics recorded can be repeated for the same patient) 
ab_kim_bl <- read_excel("db/patientsurvey/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-01-16-12-52-23.xlsx", 
                            sheet = "ab")
ab_kim_bl <- subset(ab_kim_bl, select = -abfreq) # bl questionnaire had twice the frequency of intake recorded
ab_kim_post <- read_excel("db/patientsurvey/R2_Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-02-05-14-30-05.xlsx",
                          sheet = "ab")
# append both: add a variable saying which round, then append
ab_kim <- rbind(ab_kim_bl, ab_kim_post)

# clean mode of administration
ab_kim$abroute_other <- tolower(ab_kim$abroute_other)
ab_kim$abroute[grepl("intra musc", ab_kim$abroute_other)==T] <- "im_vial"
ab_kim$abroute[grepl("intramusc", ab_kim$abroute_other)==T] <- "im_vial"
table(ab_kim$abroute, useNA = "always")

# remove non systemic use
ab_kim <- ab_kim %>% filter(abroute!="other")

# clean generic names that were not in the list to select from
ab_kim$abgeneric_other <- tolower(ab_kim$abgeneric_other)
ab_kim$abname <- tolower(ab_kim$abname)
ab_kim$abgeneric[grepl("clavulani", ab_kim$abgeneric_other)==T] <- "amoxicillinclavulanic acid"
ab_kim$abgeneric[grepl("sulbact", ab_kim$abgeneric_other)==T&grepl("amox", ab_kim$abgeneric_other)==T] <- "amoxicillin/sulbactam"
ab_kim$abgeneric[grepl("sulbact", ab_kim$abgeneric_other)==T&grepl("cefotax", ab_kim$abgeneric_other)==T] <- "cefotaxime/sulbactam"
ab_kim$abgeneric[grepl("sulbact", ab_kim$abgeneric_other)==T&grepl("ceftri", ab_kim$abgeneric_other)==T] <- "ceftriaxone/sulbactam"
ab_kim$abgeneric[grepl("tazobact", ab_kim$abgeneric_other)==T&grepl("ceftri", ab_kim$abgeneric_other)==T] <- "ceftriaxone/tazobactam"
ab_kim$abgeneric[grepl("cefurox", ab_kim$abgeneric_other)==T] <- "cefuroxime"
ab_kim$abgeneric[grepl("tazobact", ab_kim$abgeneric_other)==T&grepl("ceftri", ab_kim$abgeneric_other)==T] <- "ceftriaxone/tazobactam"
ab_kim$abgeneric[grepl("az", ab_kim$abgeneric_other)==T&grepl("fluconaz", ab_kim$abgeneric_other)==T&grepl("secnidaz", ab_kim$abgeneric_other)==T] <- "azithromycin/fluconazole/secnidazole"
ab_kim$abgeneric[grepl("trio kit", ab_kim$abgeneric_other)==T] <- "azithromycin/fluconazole/secnidazole"
ab_kim$abgeneric[grepl("cipro", ab_kim$abgeneric_other)==T&grepl("tinidaz", ab_kim$abgeneric_other)==T] <- "ciprofloxacin/tinidazole"
ab_kim$abgeneric[grepl("sulfamet", ab_kim$abgeneric_other)==T&grepl("trimet", ab_kim$abgeneric_other)==T] <- "sulfamethoxazole/trimethoprim"
ab_kim$abgeneric[grepl("sulphamet", ab_kim$abgeneric_other)==T&grepl("trimet", ab_kim$abgeneric_other)==T] <- "sulfamethoxazole/trimethoprim"
ab_kim$abgeneric[ab_kim$abgeneric_other=="cotrin"] <- "sulfamethoxazole/trimethoprim"
ab_kim$abgeneric[grepl("co-trimox", ab_kim$abgeneric_other)==T] <- "sulfamethoxazole/trimethoprim"
ab_kim$abgeneric[grepl("co trimox", ab_kim$abgeneric_other)==T] <- "sulfamethoxazole/trimethoprim"
ab_kim$abgeneric[grepl("cotrim", ab_kim$abgeneric_other)==T] <- "sulfamethoxazole/trimethoprim"
ab_kim$abgeneric[grepl("bactrim", ab_kim$abgeneric_other)==T] <- "sulfamethoxazole/trimethoprim"
ab_kim$abgeneric[grepl("tazobact", ab_kim$abgeneric_other)==T&grepl("moxycill", ab_kim$abgeneric_other)==T] <- "amoxicillin/tazobactam"
ab_kim$abgeneric[grepl("tazobact", ab_kim$abgeneric_other)==T&grepl("piperacill", ab_kim$abgeneric_other)==T] <- "piperacillin/tazobactam"
ab_kim$abgeneric[grepl("peni v", ab_kim$abgeneric_other)==T] <- "phenoxymethylpenicillin"
ab_kim$abgeneric[grepl("pénicilline", ab_kim$abgeneric_other)==T] <- "phenoxymethylpenicillin" # checked. has 250mg dosage, p.o.
ab_kim$abgeneric[grepl("benzyl penicillin", ab_kim$abgeneric_other)==T] <- "benzylpenicillin" 
ab_kim$abgeneric[grepl("penicilline procaine", ab_kim$abgeneric_other)==T] <- "procainebenzylpenicillin"
ab_kim$abgeneric[grepl("phenoxymethyl peni", ab_kim$abgeneric_other)==T] <- "phenoxymethylpenicillin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="chloramphenicol"] <- "chloramphenicol"
ab_kim$abgeneric[ab_kim$abgeneric_other=="ornidazole"] <- "ornidazole"
ab_kim$abgeneric[ab_kim$abgeneric_other=="moxifloxacine"] <- "moxifloxacin"
ab_kim$abgeneric[grepl("metronid", ab_kim$abgeneric_other)==T&grepl("norflox", ab_kim$abgeneric_other)==T] <- "piperacillin/norfloxacin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="doxicillin"] <- "doxycycline"
ab_kim$abgeneric[ab_kim$abgeneric_other=="doxicilline"] <- "doxycycline"
ab_kim$abgeneric[ab_kim$abgeneric_other=="doxycycline"] <- "doxycycline"
ab_kim$abgeneric[ab_kim$abgeneric_other=="ampicilline"] <- "ampicillin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="acide nalidixique"] <- "nalidixic acid"
ab_kim$abgeneric[grepl("sulfathiozol-chloramphenicol", ab_kim$abgeneric_other)==T] <- "sulfathiazole/chloramphenicol"
ab_kim$abgeneric[grepl("ofloxa", ab_kim$abgeneric_other)==T&grepl("ornidazol", ab_kim$abgeneric_other)==T] <- "ofloxacin/ornidazole"
ab_kim$abgeneric[grepl("ofloxa", ab_kim$abgeneric_other)==T&grepl("cefix", ab_kim$abgeneric_other)==T] <- "cefixime/ofloxacin"
ab_kim$abgeneric[grepl("métroni", ab_kim$abgeneric_other)==T&grepl("norfloxac", ab_kim$abgeneric_other)==T] <- "metronidazole/norfloxacin"
ab_kim$abgeneric[grepl("metnor", ab_kim$abgeneric_other)==T] <- "metronidazole/norfloxacin"
ab_kim$abgeneric[grepl("mentor", ab_kim$abgeneric_other)==T] <- "metronidazole/norfloxacin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="flucloxacilline"] <- "flucloxacillin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="doxicill"] <- "doxycycline"
ab_kim$abgeneric[ab_kim$abgeneric_other=="tinidazole"] <- "tinidazole"
ab_kim$abgeneric[ab_kim$abgeneric_other=="kanamycine"] <- "kanamycin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="neomycine"] <- "neomycin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="cefoperazone"] <- "cefoperazone"
ab_kim$abgeneric[ab_kim$abgeneric_other=="neomycine - polymycine"] <- "polymyxin-B/neomycin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="polymyxine-neomycine"] <- "polymyxin-B/neomycin"
ab_kim$abgeneric[ab_kim$abgeneric_other=="clarithromycine"] <- "clarithromycin"
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="fluconazole"] <- NA # antimycotic, no antibiotic as such
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="mébendazole"] <- NA 
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="ketaconazole"] <- NA # antimycotic
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="albendazole"] <- NA 
ab_kim$abgeneric_other[grepl("meyam", ab_kim$abgeneric_other)==T] <- NA # some local tannin formulation
ab_kim$abgeneric_other[grepl("decaris", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("analg", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("bruffen", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("cedocard", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("daflon", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("diclofenac", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("enalapril", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("fefol", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("duphaston", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("gripal", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("ibuprofen", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("glyben", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("insulin", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("indomethacin", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("ipprosec", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("isoniazid", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("ketazol", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("ketocona", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("opard", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("lasix", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("mebendaz", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("meyamyc", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("parac", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("papaverine", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("ranitidine", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("paracetamol", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("ondensetron", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("nph", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("nystatin", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("ation salts", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("paracã©tamol", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("phenobarbital", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("polygel", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("prednisolone", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("promethazine", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="relief"] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="rosuvastatine"] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="solution physiologique gouttes nasalas"] <- NA
ab_kim$abgeneric_other[grepl("spasfon", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("utrogestan", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("tribex", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("vermox", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("ribexfort", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("vitamine", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("vols grip", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("temperine", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("pyrimethamine", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("xylometazoline", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="thiamine"] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="sulfadoxine"] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="anset"] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="topicidal"] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="ubuprofene"] <- NA
ab_kim$abgeneric_other[ab_kim$abgeneric_other=="utrogestan"] <- NA
ab_kim$abgeneric_other[grepl("micrigynon", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("rifampicin", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("nã©omycine", ab_kim$abgeneric_other)==TRUE] <- "neomycin"
ab_kim$abgeneric_other[grepl("mucoril", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("clotrimazol", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric[ab_kim$abgeneric_other=="benzathine penicillin"] <- "benzylpenicillin"
ab_kim$abgeneric_other[grepl("aspirin", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("motilium", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("albendaz", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("natamycine collyre", ab_kim$abgeneric_other)==TRUE] <- NA
ab_kim$abgeneric_other[grepl("a-dem", ab_kim$abgeneric_other)==TRUE] <- NA # neomycin cream
ab_kim$abgeneric_other[grepl("acide chrysophanique", ab_kim$abgeneric_other)==TRUE] <- NA # some cream
ab_kim$abgeneric_other[grepl("aucun", ab_kim$abgeneric_other)==TRUE] <- NA # some cream
ab_kim$abgeneric_other[grepl("c-claire", ab_kim$abgeneric_other)==TRUE] <- NA # some cream
ab_kim$abgeneric[ab_kim$abgeneric_other=="cefpodoxime"] <- "cefpodoxime/proxetil" # checked the packaging photo
ab_kim$abgeneric_other[grepl("chlorure de dequalinium", ab_kim$abgeneric_other)==TRUE] <- NA # vaginal desinfectant
ab_kim$abgeneric[ab_kim$abgeneric_other=="cyclomax"] <- "polymyxin-B/neomycin" # chacked based on packaging, ovules
ab_kim$abgeneric_other[grepl("gogynax", ab_kim$abgeneric_other)==TRUE] <- NA # clotrimazole ovules
ab_kim$abgeneric_other[grepl("polygynax", ab_kim$abgeneric_other)==TRUE] <- NA # clotrimazole ovules
ab_kim$abgeneric_other[grepl("griséofulvine", ab_kim$abgeneric_other)==TRUE] <- NA # antifungal
ab_kim$abgeneric[ab_kim$abgeneric_other=="cyclomax"] <- "polymyxin-B/neomycin" # chacked based on packaging, ovules
ab_kim$abgeneric[ab_kim$abgeneric_other=="noci kit"] <- "clarithromycin/tinidazole" # chacked based on packaging
ab_kim$abgeneric[ab_kim$abname=="noci kit"] <- "clarithromycin/tinidazole" # chacked based on packaging
ab_kim$abgeneric[ab_kim$abgeneric_other=="clarithromycine + tinidazole+ lansoprazole"] <- "clarithromycin/tinidazole" 
ab_kim$abgeneric[ab_kim$abgeneric_other=="sulfadiazine"] <- "sulfadiazine" 

# if abgeneric is other but no other specified, checked based on the antibiotic name abname
ab_kim$abgeneric[ab_kim$abname=="noci kit"] <- "clarithromycin/tinidazole" # chacked based on packaging
ab_kim$abgeneric[ab_kim$abname=="fluque-200"] <- NA # chacked based on packaging, is fluconazole
ab_kim$abgeneric[ab_kim$abname=="griséofulvine"] <- NA # antifungal
ab_kim$abgeneric[ab_kim$abname=="ketazol"] <- NA # antifungal
ab_kim$abgeneric[ab_kim$abname=="mébendazole"] <- NA 
ab_kim$abgeneric[ab_kim$abname=="granyst"] <- NA # antifungal
ab_kim$abgeneric[ab_kim$abname=="meyamicin"] <- NA # tanins
ab_kim$abgeneric[ab_kim$abname=="meyamicine"] <- NA # tanins
ab_kim$abgeneric[ab_kim$abname=="meyamycin"] <- NA # tanins
ab_kim$abgeneric[ab_kim$abname=="tanzol"] <- NA # albendazol

# CHECK IF SOME NOT CLEANED/ GENERICNAME ASSIGNED
table(ab_kim$abgeneric_other[ab_kim$abgeneric=="other"])
table(ab_kim$abname[ab_kim$abgeneric=="other"])

# delete observations that were no antibiotic
ab_kim <- ab_kim %>% filter(!is.na(abgeneric)&abgeneric!="other") # the one remaining 'other' observation has no data at all

# add antibiotic classes and AWaRe groups
aware <- read_excel("WHO-MHP-HPS-EML-2023.04-eng.xlsx", 
                    sheet = "AWaRe classification 2023")
# use the values of the third row as variable names
colnames(aware) <- aware[3, ]
# remove the first two rows and last three columns
aware <- aware[-c(1, 2, 3), 1:4]

# antibiotic combinations that are not recommended
notrecommended <- read_excel("WHO-MHP-HPS-EML-2023.04-eng.xlsx", 
                    sheet = "Not recommended")
colnames(notrecommended) <- notrecommended[3, ]
notrecommended <- notrecommended[-c(1, 2, 3), ]
notrecommended$`ATC code` <- NA
notrecommended$Class <- "not recommended fixed-dose combination"
notrecommended$Category <- "not recommended fixed-dose combination"

# append both lists
aware <- rbind(aware, notrecommended)

# same writing of antibiotics in ab database as in aware classification
aware$Antibiotic <- tolower(aware$Antibiotic)
aware$Antibiotic <- gsub("-", "/", aware$Antibiotic)
aware$Antibiotic <- gsub("clavulanic/acid", "clavulanic acid", aware$Antibiotic)
aware$Antibiotic <- gsub("procaine/benzylpenicillin", "procaine benzylpenicillin", aware$Antibiotic)
aware$Antibiotic <- gsub("polymyxin/b", "polymyxin b", aware$Antibiotic)
aware$Antibiotic <- gsub("tinidazole_oral", "tinidazole", aware$Antibiotic) # both IV and oral are Access
aware$Antibiotic <- gsub("neomycin_oral", "neomycin", aware$Antibiotic) # both IV and oral are Watch
aware$Antibiotic <- gsub("ornidazole_oral", "ornidazole", aware$Antibiotic) # both IV and oral are Access
aware$Antibiotic <- gsub("kanamycin_oral", "kanamycin", aware$Antibiotic) # both IV and oral are Watch
aware$Antibiotic <- gsub("metronidazole_oral", "metronidazole", aware$Antibiotic) # both IV and oral are Access
ab_kim$abgeneric <- gsub("procainebenzylpenicillin", "procaine benzylpenicillin", ab_kim$abgeneric)
ab_kim$abgeneric[ab_kim$abgeneric=="amoxicillinclavulanic acid"] <- "amoxicillin/clavulanic acid"

# merge antibiotic database with antibiotic/aware classification 
ab_kim <- merge(ab_kim, aware, by.x = "abgeneric", by.y = "Antibiotic", all.x = T)

# some antibiotics are not on the EML or list of not recommended antibiotic combinations, so then we add class and aware one by one
ab_kim$Class[ab_kim$abgeneric=="nalidixic acid"] <- "Quinolones"
ab_kim$Category[ab_kim$abgeneric=="nalidixic acid"] <- "not listed"
ab_kim$Class[ab_kim$abgeneric=="amoxicillin/tazobactam"] <- "not listed fixed-dose combination"
ab_kim$Category[ab_kim$abgeneric=="amoxicillin/tazobactam"] <- "not listed fixed-dose combination"
ab_kim$Class[ab_kim$abgeneric=="piperacillin/norfloxacin"] <- "not listed fixed-dose combination"
ab_kim$Category[ab_kim$abgeneric=="piperacillin/norfloxacin"] <- "not listed fixed-dose combination"
ab_kim$Class[ab_kim$abgeneric=="clarithromycin/tinidazole"] <- "not listed fixed-dose combination"
ab_kim$Category[ab_kim$abgeneric=="clarithromycin/tinidazole"] <- "not listed fixed-dose combination"

# CHECKS
table(ab_kim$abgeneric, useNA = "always")
table(aware$Antibiotic)
table(ab_kim$Category, useNA = "always")
table(ab_kim$abgeneric[is.na(ab_kim$Category)]) # those that didn't match with the aware/antibiotic class list and couldn't be added manually

# remove unnecessary variables
ab_kim <- ab_kim[, c("abgeneric", "abroute", "abdose", "abfreq_001", "abunits", "abduration", "abproducer", "abexpiry", "abprix",
                               "_submission__uuid", "Class", "ATC code", "Category",
                               "_index", "_parent_table_name", "_parent_index", "_submission__id")]
# link patient and antibiotic data
kim <- merge(patient_kim, ab_kim, by.x = "_uuid", by.y = "_submission__uuid", all = T) 
table(kim$round, useNA = "always") # if Wene and Kitobola are excluded (pharmacy in Wene stopped activities and Kitobola too little patients), 56 antibiotics will not be merged. Can exclude them then

# if a fixed dose combination contains at least one watch AB, consider it as watch
kim$aware <- kim$Category
table(kim$abgeneric[kim$Category=="not listed fixed-dose combination"])
table(kim$abgeneric[kim$Category=="not recommended fixed-dose combination"])
kim$aware[kim$abgeneric=="nalidixic acid"] <- "Not listed"
kim$aware[kim$abgeneric=="amoxicillin/tazobactam"] <- "Watch"
kim$aware[kim$abgeneric=="clarithromycin/tinidazole"] <- "Watch"
kim$aware[kim$abgeneric=="piperacillin/norfloxacin"] <- "Watch"
kim$aware[kim$abgeneric=="amoxicillin/sulbactam"] <- "Access"
kim$aware[kim$abgeneric=="azithromycin/fluconazole/secnidazole"] <- "Watch"
kim$aware[kim$abgeneric=="cefixime/ofloxacin"] <- "Watch"
kim$aware[kim$abgeneric=="cefotaxime/sulbactam"] <- "Watch"
kim$aware[kim$abgeneric=="ceftriaxone/sulbactam"] <- "Watch"
kim$aware[kim$abgeneric=="ceftriaxone/tazobactam"] <- "Watch"
kim$aware[kim$abgeneric=="ciprofloxacin/tinidazole"] <- "Watch"
kim$aware[kim$abgeneric=="metronidazole/norfloxacin"] <- "Watch"
kim$aware[kim$abgeneric=="ofloxacin/ornidazole"] <- "Watch"
table(kim$aware)

# create a database with one line per patient indicating whether the patient used a Watch AB or not
watchkim <- kim %>%
  filter(!is.na(round)) %>%
  group_by(`_uuid`, intervention, round, choices_cluster, providertype, providernr, agegroup, sex, illness, clinpres_broad) %>%
  summarise(watch = if_else(any(aware == "Watch"), 1, 0),
              antibiotic = if_else(any(!is.na(abgeneric)), 1, 0))
watchkim$watch[is.na(watchkim$watch)] <- 0
watchkim$antibiotic[is.na(watchkim$antibiotic)] <- 0
table(watchkim$watch, useNA = "always")
table(watchkim$antibiotic, useNA = "always")

# remove two pilot clusters (we selected 24 before baseline, to retain 22 which had sufficient patient populations)
table(watchkim$choices_cluster, watchkim$round)

# consider healthcare providers as sampling unit, regardless of cluster villages (since all healthcare provieders in those villages included)
watchkim$cluster <- paste(watchkim$choices_cluster, "-", watchkim$providertype, "-", watchkim$providernr) 
table(watchkim$cluster, useNA = "always")
length(unique(watchkim$cluster)) # 126 providers

# many such clusters have just one record, which is not possible, and probably due to the providernr 
watchkim$cluster_villageprovider <- paste(watchkim$choices_cluster, "-", watchkim$providertype) 
watchkim$cluster_villageprovider <- as.factor(watchkim$cluster_villageprovider)
table(watchkim$cluster_villageprovider, useNA = "always")


# anonymize the provider clusters to prevent identification of providers
watchkim$cluster <- sapply(watchkim$cluster, function(cluster) {
  return(substring(digest(as.character(cluster), algo = "crc32"), 1, 5))})
watchkim$clusterID <- sapply(watchkim$cluster_villageprovider, function(clusterID) {
  return(substring(digest(as.character(clusterID), algo = "crc32"), 1, 5))})

# anonymize the provider clusters to prevent identification of providers and remove identifying info in the overall db
kim$cluster <- paste(kim$choices_cluster, "-", kim$providertype, "-", kim$providernr) # 128 providers
kim$clusterID <- sapply(kim$cluster, function(clusterID) {
  return(substring(digest(as.character(clusterID), algo = "crc32"), 1, 5))})
kim_anon <- kim[,c("round", "intervention", "clusterID", "providertype", "today", "dispenserpharmacie", "patientnr", "caretaker", "sex", 
                                        "ageyears", "educationlevel", "illness", "clinpres_broad","illness_spec", "illness_other", "symptoms", "symptoms/fever",
                                        "symptoms/vomiting", "symptoms/diarrhoea", "symptoms/cough", "symptoms/mauxdegorge",
                                        "symptoms/ecoulementnasale", "symptoms/other_respiratory_sign", "symptoms/rash", "symptoms/abdo_pain",
                                        "symptoms/wound", "symptoms/myalgia", "symptoms/headache", "symptoms/nausea", "symptoms/prurite",
                                        "symptoms/nosymptoms", "symptoms/othersymptoms", "symptoms_other", "date_onset", "diag_test",
                                        "diag_test/none", "diag_test/RDT_malaria", "diag_test/microscopy_malaria", "diag_test/sputumtb",
                                        "diag_test/other", "Comment_vous_vous_tes_procure", "diag_test_other", "quel_tait_le_diagnostic_final",
                                        "quel_tait_le_diagnostic_final/pneumonia", "quel_tait_le_diagnostic_final/bronchitis",
                                        "quel_tait_le_diagnostic_final/bronchiolitis", "quel_tait_le_diagnostic_final/malaria",
                                        "quel_tait_le_diagnostic_final/typhoid", "quel_tait_le_diagnostic_final/gastroenteritis",
                                        "quel_tait_le_diagnostic_final/unknown", "quel_tait_le_diagnostic_final/other", "diag_spec_other",
                                        "absprescribed", "prescriptionwheredispensed", "prescriptionwhereother",
                                        "nantibiotics", "matchedprescription", "nomatchreasons", "nomatchreason_other", "abgeneric", "abroute", 
                                        "abdose", "abfreq_001", "abunits", "abduration", "Class", "aware")]

# export database
write.csv(kim_anon, 'kim_anon.csv')

#### 2. IMPORT DATA NANORO #### 
patient_nan <- read.csv("db/patientsurvey/visit_EXIT_registration__Nanoro__2022__results.csv")
# patient_nan <- read_excel("db/patientsurvey/visit_EXIT_registration__Nanoro__2022__results.xlsx")
str(patient_nan) # every recorded antibiotic is there in a column
# show variable names
colnames(patient_nan)

# wide to long generic antibiotic names
ab_nan <- patient_nan %>%
  select("meta.instanceID", starts_with("achatMedic."), -contains(".bg"))
ab_nan_long <- gather(ab_nan, key = "abgeneric", value = "value", -meta.instanceID)
ab_nan_long <- ab_nan_long %>% filter(value!=0) %>% 
  select(-value) %>%
  mutate(abgeneric = gsub("achatMedic.", "", abgeneric)) %>%
  filter(abgeneric!="autreAntibio")

# clean the 'other' antibiotics manually entered
table(patient_nan$achatMedic.bgautreAntibio.autreNomGeneralAntibio)
patient_nan$achatMedic.bgautreAntibio.autreNomGeneralAntibio <- tolower(patient_nan$achatMedic.bgautreAntibio.autreNomGeneralAntibio)
patient_nan$achatMedic.bgautreAntibio.nomSpecialitAutrAntibio <- tolower(patient_nan$achatMedic.bgautreAntibio.nomSpecialitAutrAntibio)
patient_nan$abgeneric[patient_nan$achatMedic.bgautreAntibio.autreNomGeneralAntibio=="co-trimoxazole"] <- "sulfamethoxazole/trimethoprim"
patient_nan$abgeneric[patient_nan$achatMedic.bgautreAntibio.autreNomGeneralAntibio=="peniciline" & patient_nan$achatMedic.bgautreAntibio.presentationAutrAntibio==1] <- "phenoxymethylpenicillin"
patient_nan$abgeneric[patient_nan$achatMedic.bgautreAntibio.autreNomGeneralAntibio=="pénicilline" & patient_nan$achatMedic.bgautreAntibio.presentationAutrAntibio==1] <- "phenoxymethylpenicillin"
patient_nan$abgeneric[patient_nan$achatMedic.bgautreAntibio.nomSpecialitAutrAntibio=="co-trimoxazole"] <- "sulfamethoxazole/trimethoprim"
patient_nan$abgeneric[patient_nan$achatMedic.bgautreAntibio.nomSpecialitAutrAntibio=="penicilline" & patient_nan$achatMedic.bgautreAntibio.presentationAutrAntibio==1] <- "phenoxymethylpenicillin"
patient_nan$abgeneric[patient_nan$achatMedic.bgautreAntibio.nomSpecialitAutrAntibio=="penicillin" & patient_nan$achatMedic.bgautreAntibio.presentationAutrAntibio==1] <- "phenoxymethylpenicillin"
patient_nan$abgeneric[patient_nan$achatMedic.bgautreAntibio.nomSpecialitAutrAntibio=="trimethoprim"] <- "sulfamethoxazole/trimethoprim"
patient_nan$abgeneric[patient_nan$achatMedic.bgautreAntibio.autreNomGeneralAntibio=="tetra oxyd"] <- "oxytetracycline"
# check those entered for which we haven't assigned an antibiotic name yet
table(patient_nan$achatMedic.bgautreAntibio.autreNomGeneralAntibio[is.na(patient_nan$abgeneric)]) # no antibiotics
table(patient_nan$achatMedic.bgautreAntibio.nomSpecialitAutrAntibio[is.na(patient_nan$abgeneric)]) # no antibiotics
table(patient_nan$abgeneric)
# merge these with the antibiotic database
ab_nan_others <- patient_nan %>% filter(!is.na(abgeneric)) %>% select("meta.instanceID", "abgeneric")
ab_nan_long <- rbind(ab_nan_long, ab_nan_others)

# merge antibiotic database with antibiotic/aware classification 
# rename a few generic names to make sure they can match the AWaRe classification
ab_nan_long$abgeneric[ab_nan_long$abgeneric=="ciprofloxacine"] <- "ciprofloxacin"
ab_nan_long$abgeneric[ab_nan_long$abgeneric=="clavulanicAcid"] <- "amoxicillin/clavulanic acid"
ab_nan_long$abgeneric[ab_nan_long$abgeneric=="procaineBenzylpenicillin"] <- "benzylpenicillin"
# merge to the aware list
ab_nan <- merge(ab_nan_long, aware, by.x = "abgeneric", by.y = "Antibiotic", all.x = T)
# check those which haven't merged
table(ab_nan$abgeneric[is.na(ab_nan$Category)]) # none anymore

# clean and simplify patient data - CHECK STILL. MOST WERE NOT REFORMATTED
# dates to date format
# survey date based on visit.q1_date_entretient 
patient_nan$surveydate <- as.Date(patient_nan$visit.q1_date_entretient)
table(patient_nan$visit.q1_date_entretient)
table(patient_nan$surveydate, useNA = "always")
patient_nan$visit.q1_date_entretient[is.na(patient_nan$surveydate)]
# a previous export required a bit more reformatting:
# patient_nan$visit.q1_date_entretient <- gsub("déc\\.", "Dec", patient_nan$visit.q1_date_entretient)
# patient_nan$visit.q1_date_entretient <- gsub("févr\\.", "Feb", patient_nan$visit.q1_date_entretient)
# patient_nan$visit.q1_date_entretient <- gsub("janv\\.", "Jan", patient_nan$visit.q1_date_entretient)
# patient_nan$visit.q1_date_entretient <- gsub("mars", "Mar", patient_nan$visit.q1_date_entretient)
# patient_nan$visit.q1_date_entretient <- gsub("nov\\.", "Nov", patient_nan$visit.q1_date_entretient)
# # some dates in 2016 -> all entered on 5 Jan 2024
# patient_nan$SubmissionDate[grepl("2016", patient_nan$visit.q1_date_entretient)==T]
# patient_nan$visit.q1_date_entretient[grepl("2016", patient_nan$visit.q1_date_entretient)==T] <- gsub("2016", "2024", patient_nan$visit.q1_date_entretient[grepl("2016", patient_nan$visit.q1_date_entretient)==T])
# # convert to date variable
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 1, 2023"] <- "2023-03-01"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 2, 2023"] <- "2023-03-02"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 3, 2023"] <- "2023-03-03"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 4, 2023"] <- "2023-03-04"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 5, 2023"] <- "2023-03-05"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 6, 2023"] <- "2023-03-06"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 7, 2023"] <- "2023-03-07"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 8, 2023"] <- "2023-03-08"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 9, 2023"] <- "2023-03-09"
# patient_nan$surveydate[patient_nan$visit.q1_date_entretient=="Mar 13, 2023"] <- "2023-03-13"

# check distribuition
hist(patient_nan$surveydate, 
     main = "Histogram of Survey Dates", 
     xlab = "Survey Date",
     col = "lightblue",
     freq = TRUE,       
     breaks = "weeks",
     las = 2)             # Rotate x-axis labels by 90 degrees

# reformat dob
table(patient_nan$village.q4_dob, useNA = "always")
patient_nan$dob <- as.Date(patient_nan$village.q4_dob)
table(patient_nan$dob, useNA = "always")
# previous export required more reformatting
# patient_nan$dob <- as.Date(patient_nan$village.q4_dob, "%Y-%m-%d", format = "%b %d, %Y")
# patient_nan$village.q4_dob <- gsub("déc\\.", "dec", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("févr\\.", "feb", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("janv\\.", "jan", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("mars", "mar", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("nov\\.", "nov", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("juin", "jun", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("mai", "may", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("août", "aug", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("avr\\.", "apr", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("juil\\.", "jul", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("sept\\.", "sep", patient_nan$village.q4_dob)
# patient_nan$village.q4_dob <- gsub("oct\\.", "oct", patient_nan$village.q4_dob)
# table(patient_nan$village.q4_dob[is.na(patient_nan$dob)&patient_nan$village.q4_dob!=""]) # oddly, a few dates couldn't be converted this way, so I checked them and formatted manually
# patient_nan$dob[patient_nan$village.q4_dob=="mar 1, 2023"] <- "2023-03-01"
# patient_nan$dob[patient_nan$village.q4_dob=="mar 14, 2023"] <- "2023-03-14"
# patient_nan$dob[patient_nan$village.q4_dob=="mar 17, 2023"] <- "2023-03-17"
# patient_nan$dob[patient_nan$village.q4_dob=="mar 22, 2020"] <- "2020-03-22"
# patient_nan$dob[patient_nan$village.q4_dob=="mar 27, 2023"] <- "2023-03-27"
# patient_nan$dob[patient_nan$village.q4_dob=="may 18, 2022"] <- "2022-05-18"
# patient_nan$dob[patient_nan$village.q4_dob=="may 20, 2022"] <- "2022-05-20"
# patient_nan$dob[patient_nan$village.q4_dob=="may 21, 2022"] <- "2022-05-21"
# patient_nan$dob[patient_nan$village.q4_dob=="may 28, 2022"] <- "2022-05-28"
# patient_nan$dob[patient_nan$village.q4_dob=="may 29, 2023"] <- "2023-05-29"
# patient_nan$dob[patient_nan$village.q4_dob=="may 31, 2021"] <- "2021-05-31"
# patient_nan$dob[patient_nan$village.q4_dob=="may 4, 2022"] <- "2022-05-04"
# patient_nan$dob[patient_nan$village.q4_dob=="may 9, 2023"] <- "2023-05-09"
# patient_nan$dob[patient_nan$village.q4_dob=="oct 20, 2023"] <- "2023-10-20"
# patient_nan$dob[patient_nan$village.q4_dob=="oct 9, 2020"] <- "2023-10-09"

# clean age
patient_nan$ageyears <- patient_nan$village.age_ans
patient_nan$ageyears[is.na(patient_nan$village.age_ans)] <- round(patient_nan$village.age_mois[is.na(patient_nan$village.age_ans)]/12,0)
patient_nan$ageyears[is.na(patient_nan$ageyears)] <- round((as.numeric(patient_nan$surveydate[is.na(patient_nan$ageyears)]) - as.numeric(patient_nan$dob[is.na(patient_nan$ageyears)]))/365.25,0)
patient_nan$ageyears[patient_nan$village.age_mois==1997] <- 27 # one year entered as the age in months

table(patient_nan$village.q4_dob[is.na(patient_nan$ageyears)], useNA = "always")
table(patient_nan$ageyears, useNA = "always")

# var agegroups
patient_nan$agegroup[patient_nan$ageyears<5] <- "0-4 yr"
patient_nan$agegroup[patient_nan$ageyears>4.999] <- "5-17 yr"
patient_nan$agegroup[patient_nan$ageyears>17.999] <- "18-64 yr"
patient_nan$agegroup[patient_nan$ageyears>64.999] <- "65+ yr"
table(patient_nan$agegroup, useNA = "always")

# recode some more patient variables
patient_nan <- patient_nan %>%
  mutate(sex = ifelse(village.q6_sexe == "F", "female", 
                                  ifelse(village.q6_sexe == "M", "male", village.q6_sexe))) %>%
  mutate(educationlevel = case_when(
    village.q7_niveauEducationPatient == 1 ~ "none",
    village.q7_niveauEducationPatient == 2 ~ "primary",
    village.q7_niveauEducationPatient == 3 ~ "secondary",
    village.q7_niveauEducationPatient == 4 ~ "higher",
    TRUE ~ as.character(village.q7_niveauEducationPatient)  # Default case
  )) %>%
  mutate(educationlevel = factor(educationlevel, levels = c("none", "primary", "secondary", "higher"))) %>%
  mutate(providertype = case_when(
    dispensateur.q9_typeDispensateur == 1 ~ "healthcentre_publique",
    dispensateur.q9_typeDispensateur == 2 ~ "privatepharmacy", # these are dépôts, potentially to analyse separately but smallest groups of providers
    dispensateur.q9_typeDispensateur == 3 ~ "privatepharmacy", # these are official community pharmacies
    dispensateur.q9_typeDispensateur == 4 ~ "informalvendor",
    TRUE ~ as.character(dispensateur.q9_typeDispensateur)  # Default case
  )) %>%
  mutate(illness = case_when(
    consultation.q18_suite_maladie == 1 ~ "yes_acute_illness",
    consultation.q18_suite_maladie == 2 ~ "yes_chronic", # these are dépôts, potentially to analyse separately but smallest groups of providers
    consultation.q18_suite_maladie == 3 ~ "no_animalhealth", # these are official community pharmacies
    consultation.q18_suite_maladie == 4 ~ "no_noillness",
    TRUE ~ as.character(consultation.q18_suite_maladie)  # Default case
  ))

table(patient_nan$sex, useNA = "always")
table(patient_nan$educationlevel, useNA = "always")
table(patient_nan$providertype, useNA = "always")
table(patient_nan$illness, useNA = "always")

# number of each dispensor (only applicable to informal medicine vendors. of other provider types there are max one per cluster)
table(patient_nan$dispensateur.q10_num_vendeur_informel)
patient_nan$providernr <- as.numeric(gsub("\\D", "", patient_nan$dispensateur.q10_num_vendeur_informel))
table(patient_nan$providernr, patient_nan$providertype, useNA = "always")

# show the clusters where no provider number has been entered - need to check those and assign a number
table(patient_nan$village.cluster[is.na(patient_nan$providernr) & patient_nan$providertype=="informalvendor"], useNA = "always")
# first check which clusters have just one medicine vendor
table(patient_nan$village.cluster[(is.na(patient_nan$providernr) | patient_nan$providernr<1) & patient_nan$providertype=="informalvendor"], useNA = "always")
# vector with these villages
clusters_with_just_one_vendor <- c("BAL", "BOU", "KOK", "LAL", "NAN", "NAZ", "PEL", "RAK", "SEG", "SOA", "SOU", "SOW")
# replace in those with a value 1
patient_nan$providernr[is.na(patient_nan$providernr) & patient_nan$providertype=="informalvendor" & patient_nan$village.cluster %in% clusters_with_just_one_vendor] <- 1
# replace the provider number 0 with a provider number 1, if the village has just one vendor
patient_nan$providernr[patient_nan$providernr==0 & patient_nan$village.cluster %in% clusters_with_just_one_vendor] <- 1

# village cluster - both are the same (the open field was in case there would be one that the interviewer doesn't find on the list)
table(patient_nan$village.cluster, useNA = "always")
table(patient_nan$village.code_village, useNA = "always")

# assign whether intervention or control cluster
patient_nan <- patient_nan %>%
  mutate(intervention = ifelse(village.cluster %in% c("BAL", "BOL", "KOU", "PEL", "DAC", "KOK", "NAN", "NAZ", "POE", "SOU", "ZIM"), 
                               "intervention", "control"))
table(patient_nan$village.cluster, patient_nan$intervention, useNA = "always")

# assign the round of the survey (baseline vs. post intervention) based on survey date
patient_nan$round[patient_nan$surveydate<"2023-10-01"] <- "baseline"
patient_nan$round[patient_nan$surveydate>"2023-09-30"] <- "post"
table(patient_nan$round, useNA = "always")
colnames(patient_nan)
# remove all antimalarial variables, to simplify the data
patient_nan <- patient_nan %>% select("meta.instanceID", "providertype", "providernr", "round", "intervention", "ageyears", "agegroup", "sex", "educationlevel", "illness", c(names(patient_nan)[26:45], "surveydate", "village.cluster"))


# add a variable with a single clinical presentation that is based on the diagnosis given with the highest likelihood to result in antibiotic treatment
# clinical presentations that are likely to result in antibiotic use are assigned first
# recode the clinical presentations entered (recorded during 940 surveys, based on consultation.q24_diagnosticSpecif)
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="4"] <- "pharyngitis"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="8"] <- "gastroenteritis" # never entered/reported as diagnosis
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="1"] <- "malaria"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="9"] <- "typhoid or sepsis"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="1 9"] <- "typhoid or sepsis"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="11"] <- "urinary tract infection"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="7"] <- "pneumonia"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="3 7"] <- "pneumonia"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="6"] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="2"] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="10"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="1 12"] <- "peptic ulcer" # is a single case with no symptoms recorded and indeterminate malaria test result
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="12"] <- "peptic ulcer" 
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="1 11"] <- "urinary tract infection"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="1 2"] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="2 1"] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="3"] <- "pharyngitis" 
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="1 4"] <- "pharyngitis" # 'angine' not sure whether to label as RTI or as pharyngitis
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="4"] <- "pharyngitis" # 'angine' not sure whether to label as RTI or as pharyngitis
patient_nan$clinpres[patient_nan$consultation.q25_maladieDiagnostic=="4 2"] <- "pharyngitis" # 'angine' not sure whether to label as RTI or as pharyngitis
# when no diagnosis reported by patient, get it from clinical signs and symptoms reported. malaria only when positive malaria test and no other symptoms that could indicate another (co)infection
patient_nan$clinpres[grepl("15", patient_nan$consultation.q19_signeClinique) & grepl("1 ", patient_nan$consultation.q19_signeClinique) & grepl("11", patient_nan$consultation.q19_signeClinique)==F & is.na(patient_nan$clinpres)] <- "skin/soft tissue infection" # add fever (code 1) but make sure 11 (constipation) isn't selected instead
patient_nan$clinpres[grepl("9", patient_nan$consultation.q19_signeClinique) & is.na(patient_nan$clinpres)] <- "gastroenteritis" # still need to exclude if malaria test is positive
patient_nan$consultation.q19_AutresigneClinique <- tolower(patient_nan$consultation.q19_AutresigneClinique)
patient_nan$clinpres[grepl("dent", patient_nan$consultation.q19_AutresigneClinique) & is.na(patient_nan$clinpres)] <- "dental" 
patient_nan$clinpres[grepl("ulcere", patient_nan$consultation.q19_AutresigneClinique) & is.na(patient_nan$clinpres) ] <- "skin/soft tissue infection" 
patient_nan$clinpres[grepl("furoncle", patient_nan$consultation.q19_AutresigneClinique) & is.na(patient_nan$clinpres) ] <- "skin/soft tissue infection" 
patient_nan$clinpres[grepl("urinaire", patient_nan$consultation.q19_AutresigneClinique) & is.na(patient_nan$clinpres)] <- "urinary tract infection" 
patient_nan$clinpres[grepl("2", patient_nan$consultation.q19_signeClinique) & grepl("12", patient_nan$consultation.q19_signeClinique)==F & is.na(patient_nan$clinpres)] <- "acute respiratory infection" # "rhume" (value 2) but avoidng confusion with value 12
patient_nan$clinpres[grepl("2 ", patient_nan$consultation.q19_signeClinique) & grepl("12", patient_nan$consultation.q19_signeClinique) & grepl("13", patient_nan$consultation.q19_signeClinique)==F & is.na(patient_nan$clinpres)] <- "acute respiratory infection" # "rhume" (value 2) together with 12 (fatigue), avoiding 12 along
patient_nan$clinpres[grepl("3", patient_nan$consultation.q19_signeClinique) & grepl("13", patient_nan$consultation.q19_signeClinique)==F & is.na(patient_nan$clinpres)] <- "acute respiratory infection" # "toux" (value 3) but avoidng confusion with value 13
patient_nan$clinpres[grepl("6", patient_nan$consultation.q19_signeClinique) & is.na(patient_nan$clinpres)] <- "unexplained gastro-intestinal" # "toux" (value 3) but avoidng confusion with value 13
patient_nan$clinpres[grepl("10", patient_nan$consultation.q19_signeClinique) & is.na(patient_nan$clinpres)] <- "unexplained gastro-intestinal" # "toux" (value 3) but avoidng confusion with value 13
patient_nan$clinpres[grepl("11", patient_nan$consultation.q19_signeClinique) & is.na(patient_nan$clinpres)] <- "unexplained gastro-intestinal" # "toux" (value 3) but avoidng confusion with value 13
patient_nan$clinpres[grepl("1 ", patient_nan$consultation.q19_signeClinique) & grepl("11", patient_nan$consultation.q19_signeClinique)==F & patient_nan$consultation.q22_resultaTestDiagnostic!=1 & is.na(patient_nan$clinpres)] <- "unexplained fever" # excluding cases with positive malaria diagnostic test
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1" & patient_nan$consultation.q22_resultaTestDiagnostic!=1 & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1" & patient_nan$consultation.q22_resultaTestDiagnostic==1 & is.na(patient_nan$clinpres)] <- "malaria" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 12" & patient_nan$consultation.q22_resultaTestDiagnostic==1 & is.na(patient_nan$clinpres)] <- "malaria" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 12 13" & patient_nan$consultation.q22_resultaTestDiagnostic==1 & is.na(patient_nan$clinpres)] <- "malaria" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 13" & patient_nan$consultation.q22_resultaTestDiagnostic==1 & is.na(patient_nan$clinpres)] <- "malaria" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 12" & patient_nan$consultation.q22_resultaTestDiagnostic!=1 & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 12 13" & patient_nan$consultation.q22_resultaTestDiagnostic!=1 & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 13" & patient_nan$consultation.q22_resultaTestDiagnostic!=1 & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1" & is.na(patient_nan$consultation.q22_resultaTestDiagnostic) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 13" & is.na(patient_nan$consultation.q22_resultaTestDiagnostic) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 12" & is.na(patient_nan$consultation.q22_resultaTestDiagnostic) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 12 13" & is.na(patient_nan$consultation.q22_resultaTestDiagnostic) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 13 7" & is.na(patient_nan$consultation.q22_resultaTestDiagnostic) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 13 7" & is.na(patient_nan$consultation.q22_resultaTestDiagnostic) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 14" & is.na(patient_nan$consultation.q22_resultaTestDiagnostic) & is.na(patient_nan$clinpres)] <- "skin/soft tissue infection" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 5" & is.na(patient_nan$clinpres)] <- "acute otitis media"  
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="5 1" & is.na(patient_nan$clinpres)] <- "acute otitis media"  
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="5" & is.na(patient_nan$clinpres)] <- "acute otitis media" # no fever, to check! 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="4 5" & is.na(patient_nan$clinpres)] <- "acute otitis media" # no fever, to check! 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="5 4" & is.na(patient_nan$clinpres)] <- "acute otitis media" # no fever, to check! 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 2 7 12 13" & is.na(patient_nan$clinpres)] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 3 13" & is.na(patient_nan$clinpres)] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 3 4 8 13" & is.na(patient_nan$clinpres)] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 3 7 13" & is.na(patient_nan$clinpres)] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 3 7 8 13" & is.na(patient_nan$clinpres)] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="2 13 12" & is.na(patient_nan$clinpres)] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="3 7 12 13" & is.na(patient_nan$clinpres)] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="8 13 3" & is.na(patient_nan$clinpres)] <- "acute respiratory infection"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 4" & is.na(patient_nan$clinpres)] <- "pharyngitis"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="4" & is.na(patient_nan$clinpres)] <- "pharyngitis"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="4 1" & is.na(patient_nan$clinpres)] <- "pharyngitis"
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="4 12" & is.na(patient_nan$clinpres)] <- "pharyngitis"
patient_nan$clinpres[grepl("1 7", patient_nan$consultation.q19_signeClinique) & patient_nan$consultation.q22_resultaTestDiagnostic==1 & is.na(patient_nan$clinpres)] <- "malaria" 
patient_nan$clinpres[grepl("1 7", patient_nan$consultation.q19_signeClinique) & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[grepl("1 8", patient_nan$consultation.q19_signeClinique) & patient_nan$consultation.q22_resultaTestDiagnostic==1 & is.na(patient_nan$clinpres)] <- "malaria" 
patient_nan$clinpres[grepl("1 8", patient_nan$consultation.q19_signeClinique) & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[grepl("oeil", patient_nan$consultation.q19_AutresigneClinique) & is.na(patient_nan$clinpres)] <- "eye condition" 
patient_nan$clinpres[grepl("l’œil", patient_nan$consultation.q19_AutresigneClinique) & is.na(patient_nan$clinpres)] <- "eye condition" 
patient_nan$clinpres[grepl("15", patient_nan$consultation.q19_signeClinique) & is.na(patient_nan$clinpres)] <- "wound" 
patient_nan$clinpres[grepl("blessure", patient_nan$consultation.q19_AutresigneClinique) & is.na(patient_nan$clinpres)] <- "wound" 
patient_nan$clinpres[grepl("maux de bas ", patient_nan$consultation.q19_AutresigneClinique) & is.na(patient_nan$clinpres)] <- "unexplained gastro-intestinal" # to check
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="1 18" & is.na(patient_nan$consultation.q22_resultaTestDiagnostic) & is.na(patient_nan$clinpres)] <- "skin/soft tissue infection" # the remaining "other" symptoms are non specific
# at this point, no specific signs and symptoms are remaining -> any malaria positive test is labelled malaria in patients with reported symptoms
patient_nan$clinpres[!is.na(patient_nan$consultation.q19_signeClinique) & patient_nan$consultation.q19_signeClinique != "" & patient_nan$consultation.q22_resultaTestDiagnostic==1 & is.na(patient_nan$clinpres)] <- "malaria" 
# fever without pos malaria test result labelled as unexplained fever
patient_nan$clinpres[grepl("1 ", patient_nan$consultation.q19_signeClinique) & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="8 13 1" & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="8 7 1" & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="13 1" & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="14 1" & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="7 1" & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="7 13 1" & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
patient_nan$clinpres[patient_nan$consultation.q19_signeClinique=="8 1" & (patient_nan$consultation.q22_resultaTestDiagnostic!=1 | is.na(patient_nan$consultation.q22_resultaTestDiagnostic)) & is.na(patient_nan$clinpres)] <- "unexplained fever" 
# remaining non-specific symptoms or complaints labelled as such
patient_nan$clinpres[!is.na(patient_nan$consultation.q19_signeClinique) & patient_nan$consultation.q19_signeClinique != "" & is.na(patient_nan$clinpres)] <- "non-specific symptoms or complaints" 
table(patient_nan$clinpres, useNA = "always") #832 patients with no symptoms reported

# check the clinical presentations vs the labeling as acute vs chronic illness and relabel when clearly no chronic illness
table(patient_nan$clinpres, patient_nan$illness)
table(patient_nan$consultation.q25_maladieDiagnostic[!is.na(patient_nan$clinpres)&patient_nan$illness=="yes_chronic"], useNA="always")
patient_nan$illness[patient_nan$consultation.q25_maladieDiagnostic=="1"&!is.na(patient_nan$clinpres)&patient_nan$illness=="yes_chronic"] <- "yes_acute_illness" # malaria as diagnosis > acute
patient_nan$illness[patient_nan$consultation.q25_maladieDiagnostic=="1 12"&!is.na(patient_nan$clinpres)&patient_nan$illness=="yes_chronic"] <- "yes_acute_illness" # malaria and peptic ulcer as diagnosis > acute
patient_nan$illness[patient_nan$consultation.q25_maladieDiagnostic=="10"&!is.na(patient_nan$clinpres)&patient_nan$illness=="yes_chronic"] <- "yes_acute_illness" # dengue as diagnosis > acute
patient_nan$illness[patient_nan$consultation.q25_maladieDiagnostic=="11"&!is.na(patient_nan$clinpres)&patient_nan$illness=="yes_chronic"] <- "yes_acute_illness" # UTI as diagnosis > acute
patient_nan$illness[patient_nan$consultation.q25_maladieDiagnostic=="4"&!is.na(patient_nan$clinpres)&patient_nan$illness=="yes_chronic"] <- "yes_acute_illness" # angine as diagnostic
patient_nan$illness[patient_nan$consultation.q25_maladieDiagnostic=="6"&!is.na(patient_nan$clinpres)&patient_nan$illness=="yes_chronic"] <- "yes_acute_illness" # bronchitis as diagnostic
patient_nan$illness[patient_nan$consultation.q25_maladieDiagnostic=="7"&!is.na(patient_nan$clinpres)&patient_nan$illness=="yes_chronic"] <- "yes_acute_illness" # pneumonia as diagnostic

# group clinical presentations in broad categories
patient_nan$clinpres_broad <- patient_nan$clinpres
patient_nan$clinpres_broad[patient_nan$clinpres=="acute otitis media"] <- "acute respiratory infection"
patient_nan$clinpres_broad[patient_nan$clinpres=="bronchiolitis"] <- "acute respiratory infection"
patient_nan$clinpres_broad[patient_nan$clinpres=="bronchitis"] <- "acute respiratory infection"
patient_nan$clinpres_broad[patient_nan$clinpres=="covid-19"] <- "acute respiratory infection"
patient_nan$clinpres_broad[patient_nan$clinpres=="pharyngitis"] <- "acute respiratory infection"
patient_nan$clinpres_broad[patient_nan$clinpres=="cardiovascular condition"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="asthma/COPD"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="diabetes"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="other infections"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
patient_nan$clinpres_broad[patient_nan$clinpres=="scabies"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
patient_nan$clinpres_broad[patient_nan$clinpres=="parasitic infections"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
patient_nan$clinpres_broad[patient_nan$clinpres=="post surgery"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="other non infectious"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="eye condition"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="wound"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="peptic ulcer"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="appendicitis"] <- "other"
patient_nan$clinpres_broad[patient_nan$clinpres=="typhoid"] <- "typhoid or sepsis"
patient_nan$clinpres_broad[patient_nan$clinpres=="sepsis"] <- "typhoid or sepsis"
patient_nan$clinpres_broad[is.na(patient_nan$clinpres)] <- "other"
table(patient_nan$clinpres_broad)


# link patient and antibiotic data
nan <- merge(patient_nan, ab_nan, by = "meta.instanceID", all = T) 

# anonymize the provider clusters to prevent identification of providers and remove identifying info in the overall db
nan$cluster <- paste(nan$village.cluster, "-", nan$providertype, "-", nan$providernr) # 61 providers
nan$clusterID <- sapply(nan$cluster, function(clusterID) {
  return(substring(digest(as.character(clusterID), algo = "crc32"), 1, 5))})

# export database
write.csv(nan, 'nan.csv')
# still need to remove age to make an anonymized db of nan

# create a new variable watch that also is "Watch" if a fixed-dose combination contains at least one watch AB
nan$aware <- nan$Category
table(nan$aware) # no fixed dose combination here

# create a database with one line per patient indicating whether the patient used a Watch AB or not
watchnan <- nan %>%
  filter(!is.na(round)) %>%
  group_by(meta.instanceID, intervention, round, village.cluster, providertype, providernr, agegroup, sex, illness, clinpres_broad) %>%
  summarise(watch = if_else(any(aware == "Watch"), 1, 0),
            antibiotic = if_else(any(!is.na(abgeneric)), 1, 0))
watchnan$watch[is.na(watchnan$watch)] <- 0
watchnan$antibiotic[is.na(watchnan$antibiotic)] <- 0
table(watchnan$watch, useNA = "always")
table(watchnan$antibiotic, useNA = "always")

# consider healthcare providers as sampling unit, regardless of cluster villages (since all healthcare providers in those villages included)
watchnan$cluster <- paste(watchnan$village.cluster, "-", watchnan$providertype, "-", watchnan$providernr) # 128 providers
watchnan$cluster <- as.factor(watchnan$cluster)
# many such clusters have just one record, which is not possible, and probably due to the providernr 
watchnan$cluster_villageprovider <- paste(watchnan$village.cluster, "-", watchnan$providertype) # 128 providers
watchnan$cluster_villageprovider <- as.factor(watchnan$cluster_villageprovider)

# remove observations at providers that do not exist in the list of providers at baseline: no healthcentre in Balogho, Kalwaka, Boulpon, no private pharmacy in Boulpon
# remove when less than 10 observations from one provider, since due to poststratification weighing: informal provider in Gouloure
watchnan <- watchnan %>%
  filter(!cluster_villageprovider %in% c("BAL - healthcentre_publique", 
                                    "BOU - healthcentre_publique", 
                                    "BOU - privatepharmacy", 
                                    "KAL - healthcentre_publique",
                                    "GOU - informalvendor"))

table(watchnan$cluster_villageprovider)

# anonymize the provider clusters to prevent identification of providers
watchnan$cluster <- sapply(watchnan$cluster, function(cluster) {
  return(substring(digest(as.character(cluster), algo = "crc32"), 1, 5))})
watchnan$clusterID <- sapply(watchnan$cluster_villageprovider, function(clusterID) {
  return(substring(digest(as.character(clusterID), algo = "crc32"), 1, 5))})
table(watchnan$clusterID)

#### 3. COMBINED KIMPESE AND NANORO DATA ####
watchnan <- as.data.frame(watchnan)
watchkim <- as.data.frame(watchkim)

# add name of site
watchnan$site <- "Nanoro"
watchkim$site <- "Kimpese"

# rename original cluster variable (village or neighbourhood)
watchkim <- watchkim %>% rename(village.cluster = choices_cluster)

# remove variables that still could allow identification of dispensors
watchnan <- watchnan %>% select(-providernr, -meta.instanceID)
watchkim <- watchkim %>% select(-providernr, -`_uuid`)

# merge both
watch <- rbind(watchnan, watchkim)

# add population numbers per village/neighbourhood cluster 
watch <- watch %>%  mutate(pop_villagecluster = case_when(
    village.cluster == "BAL" ~ 1716,
    village.cluster == "BOL" ~ 6829,
    village.cluster == "BOU" ~ 3777,
    village.cluster == "CELLULE_MASAMUNA_AS_CBCO" ~ 4822,
    village.cluster == "CELLULE_MBUKA3_AS_Yanga_Dia_Songa" ~ 4850,
    village.cluster == "DAC" ~ 1588,
    village.cluster == "GOU" ~ 2781,
    village.cluster == "KAL" ~ 2202,
    village.cluster == "KIANDU_AS_Viaza" ~ 420,
    village.cluster == "KIASUNGUA_AS_Kisaunga" ~ 2200,
    village.cluster == "KILUEKA_AS_Kilueka" ~ 630,
    village.cluster == "KIMAKU_AS_Viaza" ~ 286,
    village.cluster == "KITOBOLA_AS_Kilueka" ~ 572, # excel file had 5772 but I suspect a typo, considering how small the village is
    village.cluster == "KOK" ~ 1446,
    village.cluster == "KOU" ~ 4270,
    village.cluster == "LAL" ~ 2712,
    village.cluster == "LUKENGEZI_ET_POSTE_AS_CECO" ~ 1300,
    village.cluster == "MALANGA_AS_Malanga" ~ 1740,
    village.cluster == "MBANZA_NDAMBA_AS_Kilueka" ~ 477,
    village.cluster == "MONT_FLEURY_AS_Kimbanguiste" ~ 7820,
    village.cluster == "MPETE_NKONDO_AS_Kiasunga" ~ 127,
    village.cluster == "NAN" ~ 7949,
    village.cluster == "NAZ" ~ 5297,
    village.cluster == "NGOMBE1_AS_Vunda_Nsole" ~ 660,
    village.cluster == "NKULA_AS_Viaza" ~ 340,
    village.cluster == "PEL" ~ 4764,
    village.cluster == "POE" ~ 4752,
    village.cluster == "POI" ~ 2437,
    village.cluster == "Q2_(AS_CBCO)" ~ 5435,
    village.cluster == "Q2_AS_CECO" ~ 2198,
    village.cluster == "Q3_AS_Kimbanguiste" ~ 11459,
    village.cluster == "Q3_AS_Yanga_Dia_Songa" ~ 6846,
    village.cluster == "RAK" ~ 2206,
    village.cluster == "SANZIKUA_AS_Vunda_Nsole" ~ 874,
    village.cluster == "SEG" ~ 3870,
    village.cluster == "SIG" ~ 3010,
    village.cluster == "SOA" ~ 2341,
    village.cluster == "SOU" ~ 5322,
    village.cluster == "SOW" ~ 7626,
    village.cluster == "VIAZA_AS_Viaza" ~ 582,
    village.cluster == "VUNDA_NSOLE_AS_Vunda_Nsole" ~ 686,
    village.cluster == "WENE_AS_Kilueka" ~ 600,
    village.cluster == "ZAMBA_I_AS_Malanga" ~ 620,
    TRUE ~ NA ))

# add monthly healthcare use frequency per 1000 inhabitants, by type of provider
# from 2019 HCU survey, Kisantu & Kimpese combined (b/c Kimpese didn't have peri-urban areas included then)
watch$hcu_cabu1[watch$site=="Kimpese" & watch$providertype=="healthcentre_publique"] <- 25.5
watch$hcu_cabu1[watch$site=="Kimpese" & watch$providertype=="privateclinic"] <- 31.0
watch$hcu_cabu1[watch$site=="Kimpese" & watch$providertype=="privatepharmacy"] <- 17.6
# from 2022-23 hcu_cabu1 part of the CABU-EICO household survey 
watch$hcu_cabu1[watch$site=="Nanoro" & watch$providertype=="healthcentre_publique"] <- 22.20
watch$hcu_cabu1[watch$site=="Nanoro" & watch$providertype=="privateclinic"] <- NA
watch$hcu_cabu1[watch$site=="Nanoro" & watch$providertype=="privatepharmacy"] <- 1.67
watch$hcu_cabu1[watch$site=="Nanoro" & watch$providertype=="informalvendor"] <- 4.34
table(watch$providertype, watch$hcu_cabu1, useNA = "always")

# from 2022-23 CABU-EICO HCU survey - Supplementary table 1 in manuscript
watch$hcu[watch$site=="Kimpese" & watch$providertype=="healthcentre_publique"] <- 62.6 # still need to replace private clinics by public health centres
watch$hcu[watch$site=="Kimpese" & watch$providertype=="privatepharmacy"] <- 26.4
watch$hcu[watch$site=="Nanoro" & watch$providertype=="healthcentre_publique"] <- 26.3
watch$hcu[watch$site=="Nanoro" & watch$providertype=="privatepharmacy"] <- 2.4
watch$hcu[watch$site=="Nanoro" & watch$providertype=="informalvendor"] <- 5.4
table(watch$hcu, useNA = "always")

# group private clinics and public health centres together, since they serve the same purpose and the 2023 Kimpese HCU survey does not distinguish both
watch$providertype_publicprivateclinicscombined <- watch$providertype
watch$providertype_publicprivateclinicscombined[watch$providertype=="privateclinic"] <- "healthcentre"
watch$providertype_publicprivateclinicscombined[watch$providertype=="healthcentre_publique"] <- "healthcentre"

# alternatively, assign the healthcare utilisation rate to private clinics and public health centres based on their relative share of healthcare utilisation in the CABU study in 2019-20
watch$hcu[watch$site=="Kimpese" & watch$providertype=="healthcentre_publique"] <- 62.6*(25.5/(25.5+31.0))
watch$hcu[watch$site=="Kimpese" & watch$providertype=="privateclinic"] <- 62.6*(31.0/(25.5+31.0))

# the number of patients expected from that provider in each village_cluster: HCU * population of the village cluster
watch$pop_patients <- watch$hcu*watch$pop_villagecluster/1000
table(watch$pop_patients, useNA = "always")

# add a variable with the number of surveys of patients with acute illness (those retained in analyses) in the cluster
nsurveyscluster <- watch %>% 
  filter(illness=="yes_acute_illness") %>%
  group_by(clusterID, round) %>%
  summarize(n_surveys_cluster = n())
watch <- merge(watch, nsurveyscluster, by = c("clusterID","round"))

# filter out all clusters with less than 20 surveys
watch <- watch %>% filter(n_surveys_cluster>19)

# reformat some chr vars, replace by factor variables
watch$intervention <- as.factor(watch$intervention)
watch$agegroup <- factor(watch$agegroup, levels = c("0-4 yr", "5-17 yr", "18-64 yr", "65+ yr"))
watch$illness <- factor(watch$illness, levels = c("yes_acute_illness", "yes_chronic", "no_noillness", "no_animalhealth"))
watch$providertype <- factor(watch$providertype, levels = c("healthcentre_publique", "privateclinic", "privatepharmacy", "informalvendor"))
watch$providertype_publicprivateclinicscombined <- factor(watch$providertype_publicprivateclinicscombined, levels = c("healthcentre", "privatepharmacy", "informalvendor"))
watch$clinpres_broad <- factor(watch$clinpres_broad, levels = c("malaria", "acute respiratory infection", "skin/soft tissue infection", "unexplained fever", "unexplained gastro-intestinal",
                                                                                           "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)", "non-specific symptoms or complaints", "typhoid or sepsis", 
                                                                                           "urinary tract infection", "gastroenteritis", "pneumonia", "sexually transmitted infection", "dental", "other"))

# exclude animal use, chronic patients, no illness, to keep just acute illness
watch_acute <- watch %>% filter(illness=="yes_acute_illness")

# add a post stratification weight of each survey
# population by site
pop_by_site <- watch %>%
  group_by(site, village.cluster) %>%
  summarise(pop_cluster = mean(pop_villagecluster)) %>%
  group_by(site) %>%
  summarise(pop_site = sum(pop_cluster))
pop_by_site

# keeping all different provider types
nsurveys_by_providertype_by_site <- watch_acute %>%
  group_by(site, providertype, intervention, round) %>%
  summarise(n_surveys_site = n(), hcu = mean(hcu))
nsurveys_by_providertype_by_site

poststratificationweight <- merge(nsurveys_by_providertype_by_site, pop_by_site, by = "site")
poststratificationweight$poststratweight <- (poststratificationweight$pop_site*poststratificationweight$hcu)/poststratificationweight$n_surveys_site

watch_acute <- merge(watch_acute, poststratificationweight, by = c("site", "providertype", "intervention", "round", "hcu"))

# using the combined providertype, combining private clinics and health centres, using the updated 2023 healthcare utilisation rates
# nsurveys_by_providertype_by_site_combinedpublicprivateclinics <- watch_acute %>%
#   group_by(site, providertype_publicprivateclinicscombined, intervention, round) %>%
#   summarise(n_surveys_combinedpublicprivateclinics = n(), hcu = mean(hcu))
# nsurveys_by_providertype_by_site_combinedpublicprivateclinics
# 
# poststratificationweight <- merge(nsurveys_by_providertype_by_site_combinedpublicprivateclinics, pop_by_site, by = "site")
# poststratificationweight$poststratweight_combinedpublicprivateclinics <- (poststratificationweight$pop_site*poststratificationweight$hcu)/poststratificationweight$n_surveys_combinedpublicprivateclinics
# 
# watch_acute <- merge(watch_acute, poststratificationweight, by = c("site", "pop_site", "providertype_publicprivateclinicscombined", "intervention", "round", "hcu"))

# reorganize data for a Poisson with offset model so there is one line per provider/clusterID, calculate 
# number of patients sampled from each provider, and sampling weight (population size/sample size)
watch_acute_offset <- watch_acute %>% 
  group_by(clusterID, pop_patients, village.cluster, pop_villagecluster, hcu, intervention, round, providertype, site) %>%
  summarise(n_surveys = n(), n_antibiotic = sum(antibiotic), n_watch = sum(watch), pop_patients = mean(pop_patients), weight = mean(pop_patients)/n())
head(watch_acute_offset)
# by clinical presentation
watch_acute_clinpres_offset <- watch_acute %>% 
  group_by(clusterID, pop_patients, village.cluster, pop_villagecluster, hcu, intervention, round, providertype, site, clinpres_broad) %>%
  summarise(n_surveys = n(), n_antibiotic = sum(antibiotic), n_watch = sum(watch), pop_patients = mean(pop_patients), weight = mean(pop_patients)/n())
head(watch_acute_clinpres_offset)

# watch_acute_offset_combinedpublicprivateclinics <- watch_acute %>% 
#   group_by(clusterID, pop_patients, village.cluster, pop_villagecluster, hcu, intervention, round, providertype_publicprivateclinicscombined, site) %>%
#   summarise(n_surveys = n(), n_antibiotic = sum(antibiotic), n_watch = sum(watch), weight = mean(pop_patients)/n())
# head(watch_acute_offset_combinedpublicprivateclinics)

# export dataframe with one observation per patient and the aggregated dataframe by provider
write.csv(watch_acute, "watch_acute.csv")
write.csv(watch_acute_offset, "watch_acute_offset.csv")

#### 4. DESCRIPTION PARTICIPANTS ####
# n surveys
table(watch$round, useNA = "always")
table(watch$round, watch$site, useNA = "always")

# number of providers/clusters where interviews were held
nclusters <- watch %>% group_by(site, cluster) %>% summarise(n()) %>% group_by(site) %>% summarise(n())
nclusters
# by type of provider, overall
nclusters_providertype <- watch %>% group_by(providertype, cluster) %>% summarise(n()) %>% group_by(providertype) %>% summarise(n())
nclusters_providertype
# by type of provider, by intervention/control
nclusters_providertype_intervention <- watch %>% group_by(intervention, providertype, cluster) %>% summarise(n()) %>% group_by(intervention, providertype) %>% summarise(n())
nclusters_providertype_intervention

# table 2 patient characteristics
# illness
illnessdistr <- watch %>%
  group_by(site, intervention) %>%
  count(illness) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(n, percent),
    names_sep = "_")
illnessdistr$percent_Kimpese_control  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Kimpese_control )),1)
illnessdistr$percent_Kimpese_intervention  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Kimpese_intervention )),1)
illnessdistr$percent_Nanoro_control  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Nanoro_control )),1)
illnessdistr$percent_Nanoro_intervention  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Nanoro_intervention )),1)
illnessdistr
# agegr - excluding non acute illness cases
agegroupdistr <- watch %>%
  filter(illness=="yes_acute_illness") %>%
  group_by(site, intervention) %>%
  count(agegroup) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(n, percent),
    names_sep = "_")
agegroupdistr$percent_Kimpese_control  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Kimpese_control )),1)
agegroupdistr$percent_Kimpese_intervention  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Kimpese_intervention )),1)
agegroupdistr$percent_Nanoro_control  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Nanoro_control )),1)
agegroupdistr$percent_Nanoro_intervention  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Nanoro_intervention )),1)
agegroupdistr
# sex - excluding non acute illness cases
sexdistr <- watch %>%
  filter(illness=="yes_acute_illness") %>%
  group_by(site, intervention) %>%
  count(sex) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(n, percent),
    names_sep = "_")
sexdistr$percent_Kimpese_control  <- round(as.numeric(gsub("%", "", sexdistr$percent_Kimpese_control )),1)
sexdistr$percent_Kimpese_intervention  <- round(as.numeric(gsub("%", "", sexdistr$percent_Kimpese_intervention )),1)
sexdistr$percent_Nanoro_control  <- round(as.numeric(gsub("%", "", sexdistr$percent_Nanoro_control )),1)
sexdistr$percent_Nanoro_intervention  <- round(as.numeric(gsub("%", "", sexdistr$percent_Nanoro_intervention )),1)
sexdistr
# clinical presentations
clinpresdistr <- watch %>%
  filter(illness=="yes_acute_illness") %>%
  group_by(site, intervention) %>%
  count(clinpres_broad) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(n, percent),
    names_sep = "_")
clinpresdistr$percent_Kimpese_control  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Kimpese_control )),1)
clinpresdistr$percent_Kimpese_intervention  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Kimpese_intervention )),1)
clinpresdistr$percent_Nanoro_control  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Nanoro_control )),1)
clinpresdistr$percent_Nanoro_intervention  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Nanoro_intervention )),1)
clinpresdistr
# append all these tables
colnames(illnessdistr)[1] <- "characteristic"
colnames(agegroupdistr)[1] <- "characteristic"
colnames(sexdistr)[1] <- "characteristic"
colnames(clinpresdistr)[1] <- "characteristic"
table2 <- rbind(illnessdistr)
table2 <- bind_rows(illnessdistr, agegroupdistr, sexdistr, clinpresdistr)
# reorder columns
table2 <- table2 %>% select("characteristic", "n_Kimpese_control","percent_Kimpese_control","n_Kimpese_intervention",
                            "percent_Kimpese_intervention", "n_Nanoro_control", "percent_Nanoro_control", "n_Nanoro_intervention",                     
                            "percent_Nanoro_intervention" )
# save table
write.table(table2, "table2.txt")
write_xlsx(table2, "table2.xlsx")

#### 5. CRUDE PREVALENCE ANTIBIOTIC USE ####
# 5.1 crude prevalence by provider type, by intervention/control group, by site, and pre- vs. post intervention
# any antibiotic
summary_antibioticcounts <- watch_acute %>%
  group_by(site, intervention, providertype, round) %>%
  summarise(
    count_antibiotic = sum(antibiotic == 1),
    total_count = n(),
    percentage_antibiotic = round((count_antibiotic / total_count) * 100, 1)
  ) %>%
  mutate(combined_counts = paste(count_antibiotic, total_count, sep = "/")) %>%
  select(site, intervention, providertype, round, combined_counts, percentage_antibiotic) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_antibiotic),
    names_sep = "_"
  )
summary_antibioticcounts <- summary_antibioticcounts %>% select(c("providertype","round","combined_counts_Kimpese_control","percentage_antibiotic_Kimpese_control",
                                                        "combined_counts_Kimpese_intervention", "percentage_antibiotic_Kimpese_intervention", "combined_counts_Nanoro_control",
                                                        "percentage_antibiotic_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_antibiotic_Nanoro_intervention" )) 
write_xlsx(summary_antibioticcounts, "summary_antibioticcounts.xlsx")

# watch
summary_watchcounts <- watch_acute %>%
  group_by(site, intervention, providertype, round) %>%
  summarise(
    count_watch_1 = sum(watch == 1),
    total_count = n(),
    percentage_watch_1 = round((count_watch_1 / total_count) * 100, 1)
  ) %>%
  mutate(combined_counts = paste(count_watch_1, total_count, sep = "/")) %>%
  select(site, intervention, providertype, round, combined_counts, percentage_watch_1) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_watch_1),
    names_sep = "_"
  )
summary_watchcounts <- summary_watchcounts %>% select(c("providertype","round","combined_counts_Kimpese_control","percentage_watch_1_Kimpese_control",
                                "combined_counts_Kimpese_intervention", "percentage_watch_1_Kimpese_intervention", "combined_counts_Nanoro_control",
                                "percentage_watch_1_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_watch_1_Nanoro_intervention" )) 
write.table(summary_watchcounts, "summary_watchcounts.txt")
write_xlsx(summary_watchcounts, "summary_watchcounts.xlsx")

# 5.2 crude prevalence by clinical presentation, by intervention/control group, by site, and pre- vs. post intervention
# any antibiotic
clinpres_antibioticcounts <- watch_acute %>%
  group_by(site, intervention, clinpres_broad, round) %>%
  summarise(
    count_antibiotic = sum(antibiotic == 1),
    total_count = n(),
    percentage_antibiotic = round((count_antibiotic / total_count) * 100, 1)
  ) %>%
  mutate(combined_counts = paste(count_antibiotic, total_count, sep = "/")) %>%
  select(site, intervention, clinpres_broad, round, combined_counts, percentage_antibiotic) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_antibiotic),
    names_sep = "_"
  )
clinpres_antibioticcounts <- clinpres_antibioticcounts %>% select(c("clinpres_broad","round","combined_counts_Kimpese_control","percentage_antibiotic_Kimpese_control",
                                                                  "combined_counts_Kimpese_intervention", "percentage_antibiotic_Kimpese_intervention", "combined_counts_Nanoro_control",
                                                                  "percentage_antibiotic_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_antibiotic_Nanoro_intervention" )) 
write_xlsx(clinpres_antibioticcounts, "clinpres_antibioticcounts.xlsx")

# watch
clinpres_watchcounts <- watch_acute %>%
  group_by(site, intervention, clinpres_broad, round) %>%
  summarise(
    count_watch_1 = sum(watch == 1),
    total_count = n(),
    percentage_watch_1 = round((count_watch_1 / total_count) * 100, 1)
  ) %>%
  mutate(combined_counts = paste(count_watch_1, total_count, sep = "/")) %>%
  select(site, intervention, clinpres_broad, round, combined_counts, percentage_watch_1) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_watch_1),
    names_sep = "_"
  )
clinpres_watchcounts <- clinpres_watchcounts %>% select(c("clinpres_broad","round","combined_counts_Kimpese_control","percentage_watch_1_Kimpese_control",
                                                        "combined_counts_Kimpese_intervention", "percentage_watch_1_Kimpese_intervention", "combined_counts_Nanoro_control",
                                                        "percentage_watch_1_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_watch_1_Nanoro_intervention" )) 
write.table(summary_watchcounts, "clinpres_watchcounts.txt")
write_xlsx(summary_watchcounts, "clinpres_watchcounts.xlsx")


#### 6. TWO STAGE CLUSTER ADJUSTED PREVALENCE ESTIMATES ####
# WATCH
# proportion estimates by site, type of provider, intervention/control and pre/post
surveydesign <- svydesign(id = ~clusterID, data = watch_acute, nest = TRUE)
watchclusterprop <- svyby(~watch, by = ~providertype + site + intervention + round, design = surveydesign, FUN = svymean, na.rm = TRUE)
# add 95% CI as percentages
watchclusterprop$lower_ci <- round((watchclusterprop$watch - 1.96*watchclusterprop$se)*100,1)
watchclusterprop$upper_ci <- round((watchclusterprop$watch + 1.96*watchclusterprop$se)*100,1)
# proportion to percentage
watchclusterprop$pct <- round(watchclusterprop$watch*100,1)
watchclusterprop <- as.data.frame(watchclusterprop) 
# reshape to a wide table similar to the one with counts above
watchclusterprop_wide <- watchclusterprop %>% 
  mutate(ci = paste(lower_ci, upper_ci, sep = "-")) %>%
  select(site, intervention, providertype, round, pct, ci) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(pct, ci),
    names_sep = "_"
  )
# reorder rows
watchclusterprop_wide <- watchclusterprop_wide[order(watchclusterprop_wide$providertype), ]
# reorder columns
watchclusterprop_wide <- watchclusterprop_wide %>% select(c("providertype","round","pct_Kimpese_control","ci_Kimpese_control",
                                                            "pct_Kimpese_intervention", "ci_Kimpese_intervention", "pct_Nanoro_control",
                                                            "ci_Nanoro_control", "pct_Nanoro_intervention", "ci_Nanoro_intervention" )) 
# save table
write.table(watchclusterprop_wide, "watchclusterprop_wide.txt")

# OVERALL proportion estimates by intervention/control & round
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = watch_acute, nest = TRUE)
poststratweightedwatchprop <- svyby(~watch, by = ~intervention + round, design = poststratweightedsurveydesign, FUN = svymean, na.rm = TRUE)
# add 95% CI as percentages
poststratweightedwatchprop$lower_ci <- round((poststratweightedwatchprop$watch - 1.96*poststratweightedwatchprop$se)*100,1)
poststratweightedwatchprop$upper_ci <- round((poststratweightedwatchprop$watch + 1.96*poststratweightedwatchprop$se)*100,1)
# proportion to percentage
poststratweightedwatchprop$pct <- round(poststratweightedwatchprop$watch*100,1)
poststratweightedwatchprop <- as.data.frame(poststratweightedwatchprop) 
poststratweightedwatchprop

# OVERALL proportion estimates at baseline
watch_acute_bl <- watch_acute %>% filter(round == "baseline")
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = watch_acute_bl, nest = TRUE)
poststratweightedwatchprop <- svymean(~watch, design = poststratweightedsurveydesign, na.rm = TRUE)
poststratweightedwatchprop <- as.data.frame(poststratweightedwatchprop)
poststratweightedwatchprop
# add 95% CI as percentages
poststratweightedwatchprop$lower_ci <- round((poststratweightedwatchprop$mean - 1.96*poststratweightedwatchprop$watch)*100,1)
poststratweightedwatchprop$upper_ci <- round((poststratweightedwatchprop$mean + 1.96*poststratweightedwatchprop$watch)*100,1)
# proportion to percentage
poststratweightedwatchprop$pct <- round(poststratweightedwatchprop$mean*100,1)
poststratweightedwatchprop

# other method but seems to give approx. the same estimates
# ci_estimates <- svyby(~watch, by = ~providertype + site + intervention, design = surveydesign, FUN = svyciprop, ci = 0.95, method = "likelihood")

# ANY ANTIBIOTIC
# OVERALL proportion estimates by intervention/control & round
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = watch_acute, nest = TRUE)
poststratweightedABprop <- svyby(~antibiotic, by = ~intervention + round, design = poststratweightedsurveydesign, FUN = svymean, na.rm = TRUE)
# add 95% CI as percentages
poststratweightedABprop$lower_ci <- round((poststratweightedABprop$antibiotic - 1.96*poststratweightedABprop$se)*100,1)
poststratweightedABprop$upper_ci <- round((poststratweightedABprop$antibiotic + 1.96*poststratweightedABprop$se)*100,1)
# proportion to percentage
poststratweightedABprop$pct <- round(poststratweightedABprop$antibiotic*100,1)
poststratweightedABprop <- as.data.frame(poststratweightedABprop) 
poststratweightedABprop
# with WEIGHING AND OFFSET - using the offset database as used to estimate PR
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
surveydesign <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
prevalence_estimates <- svyby(~ prevalence,  ~ round + intervention,  # Grouping variables
                              design = surveydesign,
                              svymean,  # Function to calculate means
                              vartype = c("ci")  # Include confidence intervals
)
prevalence_estimates

# BY PROVIDERTYPE, by site, by round (baseline/post) 
surveydesign <- svydesign(id = ~clusterID, data = watch_acute, nest = TRUE)
# proportion estimates
antibioticclusterprop <- svyby(~antibiotic, by = ~providertype + site + intervention + round, design = surveydesign, FUN = svymean, na.rm = TRUE)
# add 95% CI as percentages
antibioticclusterprop$lower_ci <- round((antibioticclusterprop$antibiotic - 1.96*antibioticclusterprop$se)*100,1)
antibioticclusterprop$upper_ci <- round((antibioticclusterprop$antibiotic + 1.96*antibioticclusterprop$se)*100,1)
# proportion to percentage
antibioticclusterprop$pct <- round(antibioticclusterprop$antibiotic*100,1)
# reshape to a wide table similar to the one with counts above
antibioticclusterprop_wide <- antibioticclusterprop %>% 
  mutate(ci = paste(lower_ci, upper_ci, sep = "-")) %>%
  select(site, intervention, providertype, round, pct, ci) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(pct, ci),
    names_sep = "_"
  )
# reorder rows
antibioticclusterprop_wide <- antibioticclusterprop_wide[order(antibioticclusterprop_wide$providertype), ]
# reorder columns
antibioticclusterprop_wide <- antibioticclusterprop_wide %>% select(c("providertype","round","pct_Kimpese_control","ci_Kimpese_control",
                                                            "pct_Kimpese_intervention", "ci_Kimpese_intervention", "pct_Nanoro_control",
                                                            "ci_Nanoro_control", "pct_Nanoro_intervention", "ci_Nanoro_intervention" )) 
antibioticclusterprop_wide
# save table
write.table(antibioticclusterprop_wide, "antibioticclusterprop_wide.txt")

# with WEIGHING for EVERY PROVIDER AND OFFSET - using the same "offset database"
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
surveydesign <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
prevalence_estimates <- svyby(~ prevalence,  ~ providertype + round + intervention,  # Grouping variables
  design = surveydesign,
  svymean,  # Function to calculate means
  vartype = c("ci")  # Include confidence intervals
)
prevalence_estimates

#### 6. PREVALENCE RATIO WATCH/ANY ANTIBIOTIC USE INTERVENTION VS CONTROL ####
# model structure: ABU ~ Time + Intervention + Time*Intervention + confounders?? + clusters/providers(clusterID)
# 5.1 neg binomial regression with offset - model retained for analysis
# 5.1.1 WATCH AB 
# define survey design - including weighting for cluster size (village population * provider-specific healthcare utilisation rate)
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
library(MASS)
nb_model_watch <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + providertype, 
  weights = surveydesign$weights,
  data = watch_acute_offset
)
# what I initially used
# nb_model_watch <- svyglm(n_watch ~ offset(log(pop_patients)) + round * intervention + providertype,
#                               design = surveydesign, 
#                               family = quasipoisson)
# summary(nb_model_watch)

# get the model coefficients
coef <- coef(nb_model_watch)
coeci <- confint(nb_model_watch)
rr <- exp(coef)
rr 
ci_rr <- exp(coeci)
ci_rr

# define survey design - including weighting for cluster size (village population * provider-specific healthcare utilisation rate)
surveydesign_noweighing <- svydesign(id = ~clusterID, data = watch_acute_offset)
nb_model <- svyglm(n_watch ~ offset(log(pop_patients)) + round * intervention, 
                   design = surveydesign_noweighing, 
                   family = quasipoisson)
summary(nb_model)
coef <- coef(nb_model)
coeci <- confint(nb_model)
rr <- exp(coef)
rr 
ci_rr <- exp(coeci)
ci_rr

# 5.1.2 ANY ANTIBIOTIC
# with four types of providers in the model: public health centre, private clinic, over-the-counter at private pharmacy, informal vendor
# define survey design - including weighting for cluster size (village population * provider-specific healthcare utilisation rate)
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
library(MASS)
nb_model_anyAB <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + providertype, 
  weights = surveydesign$weights,
  data = watch_acute_offset
)
# get the model coefficients
coef <- coef(nb_model_anyAB)
coeci <- confint(nb_model_anyAB)
rr <- exp(coef)
rr
ci_rr <- exp(coeci)
ci_rr

# define survey design - without weighting -> every interviewed patient has the same weight
surveydesign_noweighing <- svydesign(id = ~clusterID, data = watch_acute_offset)
# fit model
nb_model_anyAB <- svyglm(n_antibiotic ~ offset(log(pop_patients)) + round * intervention + providertype, 
                         design = surveydesign_noweighing, 
                         family = quasipoisson)
summary(nb_model_anyAB)
# get the model coefficients
coef <- coef(nb_model_anyAB)
coeci <- confint(nb_model_anyAB)
rr <- exp(coef)
rr
ci_rr <- exp(coeci)
ci_rr

# with three types of providers in the model: health centre, over-the-counter at private pharmacy, informal vendor
surveydesign_combinedpublicprivateclinics <- svydesign(id = ~clusterID, data = watch_acute_offset_combinedpublicprivateclinics, 
                          # weights = ~weight # for now no weighing because surveys still ongoing in some clusters, so that the few surveys there get too much weight
)
# fit model
nb_model_anyAB <- svyglm(n_antibiotic ~ offset(log(pop_patients)) + round * intervention + providertype_publicprivateclinicscombined, #+ agegroup?  + site
                         design = surveydesign_combinedpublicprivateclinics, 
                         family = poisson)
summary(nb_model_anyAB)
# get the model coefficients
coef <- coef(nb_model_anyAB)
coeci <- confint(nb_model_anyAB)
rr <- exp(coef)
rr 
ci_rr <- exp(coeci)
ci_rr

# 5.2 log binomial regression using the survey package
# WATCH
surveydesign <- svydesign(id = ~clusterID, data = watch_acute, strata = NULL, weights = NULL) # same as above
design_effect <- deff(surveydesign) # Error in sqrt(design_effect) : non-numeric argument to mathematical function 
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(watch ~ intervention*round + providertype, + site, # + agegroup + sex, 
                                 design = surveydesign, family = binomial(link = "log"))
summary(svy_binregressionmodel)
# get the model coefficients
coef <- coef(svy_binregressionmodel) # gives exactly the same result as above -> no design effect taken into account yet?
coeci <- confint(svy_binregressionmodel)
rr <- round(exp(coef),2)
rr # see that it's an OR actually since it's log regression eventually
ci_rr <- round(exp(coeci),2)
ci_rr

mvatable_watch <- as.data.frame(cbind(rr, ci_rr))
mvatable_watch <- mvatable_watch %>% mutate(ci = paste(`2.5 %`, `97.5 %`, sep = "-")) %>%
  select(rr, ci)
write.table(mvatable_watch, "mvatable_watch.txt")
# adjusted_se <- sqrt(diag(vcov(svy_binregressionmodel))) * sqrt(design_effect)

# ANY ANTIBIOTIC
surveydesign <- svydesign(id = ~clusterID, data = watch_acute, nest = TRUE) # same as above
design_effect <- deff(surveydesign) # Error in sqrt(design_effect) : non-numeric argument to mathematical function 
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(antibiotic ~ intervention*round + providertype + site + agegroup + sex, 
                                 design = surveydesign, family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)
summary(svy_binregressionmodel)
# get the model coefficients
coef <- coef(svy_binregressionmodel) # gives exactly the same result as above -> no design effect taken into account yet?
coeci <- confint(svy_binregressionmodel)
rr <- round(exp(coef),2)
rr # see that it's an OR actually since it's log regression eventually
ci_rr <- round(exp(coeci),2)
ci_rr
mvatable_anyantibiotic <- as.data.frame(cbind(rr, ci_rr))
mvatable_anyantibiotic <- mvatable_anyantibiotic %>% mutate(ci = paste(`2.5 %`, `97.5 %`, sep = "-")) %>%
  select(rr, ci)
write.table(mvatable_anyantibiotic, "mvatable_anyantibiotic.txt")

# 5.3 mixed effects model with random intercept
# WATCH
mixed_model_option1 <- glmer(watch ~ round*intervention + (1| providertype) + (1|site) + (1|clusterID), # I removed site b/c it took too long
                     family = binomial(link = "logit"),
                     data = watch_acute)
summary(mixed_model_option1)
mixed_model_1_results <- tidy(mixed_model_option1, conf.int = TRUE)
mixed_model_1_results$or <- exp(mixed_model_1_results$estimate)
mixed_model_1_results$ci_lower <- exp(mixed_model_1_results$conf.low)
mixed_model_1_results$ci_upper <- exp(mixed_model_1_results$conf.high)
mixed_model_1_results

mixed_model_option2 <- glmer(watch ~ round*intervention + site + clusterID + (1|providertype) ,
                     family = binomial(link = "logit"),
                     data = watch_acute)


#### 7. FIGURE 1A & B. PREVALENCE ANY ANTIBIOTIC & WATCH AND RR OVERALL, BY SITE AND BY TYPE OF PROVIDER ####
# 7.1 ANY ANTIBIOTIC
# overall prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by provider type 
surveydesign_prevalence <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
prevalence_estimates_overall <- svyby(~ prevalence,  ~ round + intervention,  # Grouping variables
                              design = surveydesign_prevalence,
                              svymean,  # Function to calculate means
                              vartype = c("ci")  # Include confidence intervals
)
prevalence_estimates_overall$prevalence <- round(prevalence_estimates_overall$prevalence*100, 1)
prevalence_estimates_overall$ci_l <- round(prevalence_estimates_overall$ci_l*100, 1)
prevalence_estimates_overall$ci_u <- round(prevalence_estimates_overall$ci_u*100, 1)
prevalence_estimates_overall

# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
library(MASS)
nb_model_subgroup <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + providertype, 
  weights = surveydesign$weights,
  data = watch_acute_offset
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
overall_effect <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = rr,
  CI_Lower = rr_ci_lower,
  CI_Upper = rr_ci_upper
)
print(overall_effect, row.names = FALSE)

# provider type-specific prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by provider type 
surveydesign <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
prevalence_estimates_providertype <- svyby(~ prevalence,  ~ providertype + round + intervention,  # Grouping variables
                              design = surveydesign,
                              svymean,  # Function to calculate means
                              vartype = c("ci")  # Include confidence intervals
)
prevalence_estimates_providertype$prevalence <- round(prevalence_estimates_providertype$prevalence*100, 1)
prevalence_estimates_providertype$ci_l <- round(prevalence_estimates_providertype$ci_l*100, 1)
prevalence_estimates_providertype$ci_u <- round(prevalence_estimates_providertype$ci_u*100, 1)
prevalence_estimates_providertype

# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
nb_model_subgroup <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention * providertype, 
  weights = surveydesign$weights,
  data = watch_acute_offset
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = rr,
  CI_Lower = rr_ci_lower,
  CI_Upper = rr_ci_upper
)
print(subgroup_effects, row.names = FALSE)


# # subsetting by type of provider (below for healthcentre) gives the same result
# surveydesign_subsethealthcentre <- svydesign(id = ~clusterID, data = watch_acute_offset[watch_acute_offset$providertype=="healthcentre_publique",], weights = ~weight)
# # fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
# library(MASS)
# nb_model_subsethealthcentre <- glm.nb(
#   n_antibiotic ~ offset(log(pop_patients)) + round * intervention, 
#   weights = surveydesign_subsethealthcentre$weights,
#   data = watch_acute_offset[watch_acute_offset$providertype=="healthcentre_publique",]
# )
# # get the model coefficients
# coef <- coef(nb_model_subsethealthcentre)
# coeci <- confint(nb_model_subsethealthcentre)
# rr <- exp(coef)
# rr
# ci_rr <- exp(coeci)
# ci_rr
# 
# # prevalence estimate 
# surveydesign_subsethealthcentre <- update(surveydesign_subsethealthcentre, prevalence = n_antibiotic / n_surveys)
# prevalence_estimates <- svyby(~ prevalence,  ~ providertype + round + intervention,  # Grouping variables
#                               design = surveydesign_subsethealthcentre,
#                               svymean,  # Function to calculate means
#                               vartype = c("ci")  # Include confidence intervals
# )
# prevalence_estimates

# site-specific prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by site
surveydesign <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
prevalence_estimates_site <- svyby(~ prevalence,  ~ site + round + intervention,  # Grouping variables
                                           design = surveydesign,
                                           svymean,  # Function to calculate means
                                           vartype = c("ci")  # Include confidence intervals
)
prevalence_estimates_site$prevalence <- round(prevalence_estimates_site$prevalence*100, 1)
prevalence_estimates_site$ci_l <- round(prevalence_estimates_site$ci_l*100, 1)
prevalence_estimates_site$ci_u <- round(prevalence_estimates_site$ci_u*100, 1)
prevalence_estimates_site
  
# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
# subset Nanoro
watch_acute_offset_nan <- watch_acute_offset %>% filter(site=="Nanoro")
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_nan, weights = ~weight)
nb_model_subgroup <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset_nan
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = rr,
  CI_Lower = rr_ci_lower,
  CI_Upper = rr_ci_upper
)
print(subgroup_effects, row.names = FALSE)
# subset Kimpese
watch_acute_offset_kim <- watch_acute_offset %>% filter(site=="Kimpese")
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_kim, weights = ~weight)
nb_model_subgroup <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset_kim
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = rr,
  CI_Lower = rr_ci_lower,
  CI_Upper = rr_ci_upper
)
print(subgroup_effects, row.names = FALSE)

# show on a plot
# append prevalence estimates in a single dataframe
# colnames(prevalence_estimates_clinpres)[colnames(prevalence_estimates_clinpres) == "clinpres_broad"] <- "subgroup"
colnames(prevalence_estimates_providertype)[colnames(prevalence_estimates_providertype) == "providertype"] <- "subgroup"
colnames(prevalence_estimates_site)[colnames(prevalence_estimates_site) == "site"] <- "subgroup"
prevalence_estimates_overall$subgroup <- "OVERALL, weighted"
prevalence_estimates <- rbind(prevalence_estimates_overall, prevalence_estimates_providertype)
prevalence_estimates <- rbind(prevalence_estimates, prevalence_estimates_site)
prevalence_estimates$interventionround <- paste(prevalence_estimates$intervention,", ",prevalence_estimates$round)
# ensure subgroup is categorical
# subgroups <- c("overall, weighted","healthcentre_publique","privateclinic","privatepharmacy","informalvendor","malaria", "acute respiratory infection", "skin/soft tissue infection", "unexplained fever", 
#                    "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)", "non-specific symptoms or complaints", "typhoid or sepsis", 
#                    "urinary tract infection", "gastroenteritis", "pneumonia", "sexually transmitted infection", "dental", "other")
# subgroups_reversed <- rev(subgroups)

# remove unnecessary spaces
prevalence_estimates$subgroup <- gsub(" ,  ", ",", prevalence_estimates$subgroup)

# rename provider labels
prevalence_estimates <- prevalence_estimates %>%
  mutate(subgroup = recode(subgroup,
                           "informalvendor" = "informal vendor",
                           "privatepharmacy" = "community pharmacy/store",
                           "privateclinic" = "private clinic",
                           "healthcentre_publique" = "health centre",
                           "Nanoro" = "Nanoro",
                           "Kimpese" = "Kimpese",
                           "OVERALL, weighted" = "OVERALL, weighted"))

# prevalence_estimates$subgroup <- factor(prevalence_estimates$subgroup, levels = subgroups_reversed)
prevalence_estimates$subgroup <- factor(prevalence_estimates$subgroup, levels = c("informal vendor", "community pharmacy/store","private clinic","health centre", "Nanoro","Kimpese","OVERALL, weighted"))

# add a vertical position for each estimate
prevalence_estimates <- prevalence_estimates %>% mutate(position = case_when(
      intervention == "intervention" & round == "baseline" ~ as.numeric(subgroup) + 0.15,
      intervention == "intervention" & round == "post" ~ as.numeric(subgroup) + 0.08,
      intervention == "control" & round == "baseline" ~ as.numeric(subgroup) - 0.08,
      intervention == "control" & round == "post" ~ as.numeric(subgroup) - 0.15
    )
  )

# add a variable with the PR values of for each change
prevalence_estimates$pr <- ""
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="OVERALL, weighted"] <- "PR 0.45 (95%CI 0.24-0.85)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="Kimpese"] <- "PR 0.44 (95%CI 0.18-1.10)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="Nanoro"] <- "PR 0.52 (95%CI 0.21-1.25)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="health centre"] <- "PR 0.42 (95%CI 0.15-1.18)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="private clinic"] <- "PR 0.57 (95%CI 0.05-6.58)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="community pharmacy/store"] <- "PR 1.3 (95%CI 0.26-6.4)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="informal vendor"] <- "PR 1.9 (95%CI 0.39-9.1)"

# make plot
subgroup_prevalence_plot <- ggplot(prevalence_estimates, aes(x = prevalence, y = position)) +
  geom_point(
    aes(color = interventionround), 
    size = 3  ) +  
  geom_errorbarh(
    aes(xmin = ci_l, xmax = ci_u, color = interventionround), 
    height = 0.1 ) +   
  scale_y_continuous(
    breaks = as.numeric(prevalence_estimates$subgroup),
    labels = prevalence_estimates$subgroup,
  ) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    name = "Prevalence of antibiotic use (%)"
  ) +  
  scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  guides(color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  geom_hline(yintercept = c(4.5,6.5), linetype = "solid", color = "darkgrey") +
  labs(
    y = element_blank(), 
    color = "Intervention/control clusters & time"  
  ) +
  geom_text(aes(label = pr),
            hjust = -0.3,       # shift label a bit to the right of the point
            vjust = 3.9,        # vertical alignment
            size = 2.5)
subgroup_prevalence_plot

ggsave("subgroup_prevalence_plot.jpeg", plot = subgroup_prevalence_plot, width = 6, height = 6, dpi = 300)
write_xlsx(prevalence_estimates, "prevalence_estimates.xlsx")

# 7.2 WATCH
# OVERALL prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by provider type 
surveydesign_prevalence <- update(surveydesign, prevalence = n_watch / n_surveys)
watchprevalence_estimates_overall <- svyby(~ prevalence,  ~ round + intervention,  # Grouping variables
                                      design = surveydesign_prevalence,
                                      svymean,  # Function to calculate means
                                      vartype = c("ci")  # Include confidence intervals
)
watchprevalence_estimates_overall$prevalence <- round(watchprevalence_estimates_overall$prevalence*100, 1)
watchprevalence_estimates_overall$ci_l <- round(watchprevalence_estimates_overall$ci_l*100, 1)
watchprevalence_estimates_overall$ci_u <- round(watchprevalence_estimates_overall$ci_u*100, 1)
watchprevalence_estimates_overall

# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
library(MASS)
nb_model_subgroup <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
overall_effect <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = rr,
  CI_Lower = rr_ci_lower,
  CI_Upper = rr_ci_upper
)
print(overall_effect, row.names = FALSE)

# BY SITE
# site-specific prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by site
surveydesign <- update(surveydesign, prevalence = n_watch / n_surveys)
watchprevalence_estimates_site <- svyby(~ prevalence,  ~ site + round + intervention,  # Grouping variables
                                   design = surveydesign,
                                   svymean,  # Function to calculate means
                                   vartype = c("ci")  # Include confidence intervals
)
watchprevalence_estimates_site$prevalence <- round(watchprevalence_estimates_site$prevalence*100, 1)
watchprevalence_estimates_site$ci_l <- round(watchprevalence_estimates_site$ci_l*100, 1)
watchprevalence_estimates_site$ci_u <- round(watchprevalence_estimates_site$ci_u*100, 1)
watchprevalence_estimates_site

# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
# subset Nanoro
watch_acute_offset_nan <- watch_acute_offset %>% filter(site=="Nanoro")
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_nan, weights = ~weight)
nb_model_subgroup <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset_nan
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = rr,
  CI_Lower = rr_ci_lower,
  CI_Upper = rr_ci_upper
)
print(subgroup_effects, row.names = FALSE)

# subset Kimpese
watch_acute_offset_kim <- watch_acute_offset %>% filter(site=="Kimpese")
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_kim, weights = ~weight)
nb_model_subgroup <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset_kim
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = rr,
  CI_Lower = rr_ci_lower,
  CI_Upper = rr_ci_upper
)
print(subgroup_effects, row.names = FALSE)

# BY PROVIDERTYPE prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by provider type 
surveydesign <- update(surveydesign, prevalence = n_watch / n_surveys)
watchprevalence_estimates_providertype <- svyby(~ prevalence,  ~ providertype + round + intervention,  # Grouping variables
                                           design = surveydesign,
                                           svymean,  # Function to calculate means
                                           vartype = c("ci")  # Include confidence intervals
)
watchprevalence_estimates_providertype$prevalence <- round(watchprevalence_estimates_providertype$prevalence*100, 1)
watchprevalence_estimates_providertype$ci_l <- round(watchprevalence_estimates_providertype$ci_l*100, 1)
watchprevalence_estimates_providertype$ci_u <- round(watchprevalence_estimates_providertype$ci_u*100, 1)
watchprevalence_estimates_providertype

# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
nb_model_subgroup <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention * providertype, 
  weights = surveydesign$weights,
  data = watch_acute_offset
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- round(exp(log_coefs),2)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2))
print(subgroup_effects, row.names = FALSE)
# not sure why the values of private clinics don't make sense -> instead subset by each provider type

# subset private clinics
watch_acute_offset_privateclinics <- watch_acute_offset[watch_acute_offset$providertype=="privateclinic",]
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_privateclinics, weights = ~weight)
nb_model_subgroup <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset_privateclinics
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- round(exp(log_coefs),2)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2))
print(subgroup_effects, row.names = FALSE)
# subset health centres
watch_acute_offset_healthcentre <- watch_acute_offset[watch_acute_offset$providertype=="healthcentre_publique",]
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_healthcentre, weights = ~weight)
nb_model_subgroup <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset_healthcentre
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- round(exp(log_coefs),2)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2))
print(subgroup_effects, row.names = FALSE)
# subset pharmacies
watch_acute_offset_pharmacy <- watch_acute_offset[watch_acute_offset$providertype=="privatepharmacy",]
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_pharmacy, weights = ~weight)
nb_model_subgroup <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset_pharmacy
)
coef_summary <- summary(nb_model_subgroup)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- round(exp(log_coefs),2)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2))
print(subgroup_effects, row.names = FALSE)

# show on a plot
# append prevalence estimates in a single dataframe
# colnames(prevalence_estimates_clinpres)[colnames(prevalence_estimates_clinpres) == "clinpres_broad"] <- "subgroup"
colnames(watchprevalence_estimates_providertype)[colnames(watchprevalence_estimates_providertype) == "providertype"] <- "subgroup"
colnames(watchprevalence_estimates_site)[colnames(watchprevalence_estimates_site) == "site"] <- "subgroup"
watchprevalence_estimates_overall$subgroup <- "OVERALL, weighted"
watchprevalence_estimates <- rbind(watchprevalence_estimates_overall, watchprevalence_estimates_providertype)
watchprevalence_estimates <- rbind(watchprevalence_estimates, watchprevalence_estimates_site)
watchprevalence_estimates$interventionround <- paste(watchprevalence_estimates$intervention,", ",watchprevalence_estimates$round)
# ensure subgroup is categorical
# subgroups <- c("overall, weighted","healthcentre_publique","privateclinic","privatepharmacy","informalvendor","malaria", "acute respiratory infection", "skin/soft tissue infection", "unexplained fever", 
#                    "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)", "non-specific symptoms or complaints", "typhoid or sepsis", 
#                    "urinary tract infection", "gastroenteritis", "pneumonia", "sexually transmitted infection", "dental", "other")
# subgroups_reversed <- rev(subgroups)
# 

# rename provider labels
watchprevalence_estimates <- watchprevalence_estimates %>%
  mutate(subgroup = recode(subgroup,
                           "informalvendor" = "informal vendor",
                           "privatepharmacy" = "community pharmacy/store",
                           "privateclinic" = "private clinic",
                           "healthcentre_publique" = "health centre",
                           "Nanoro" = "Nanoro",
                           "Kimpese" = "Kimpese",
                           "OVERALL, weighted" = "OVERALL, weighted"))

watchprevalence_estimates$subgroup <- factor(watchprevalence_estimates$subgroup, levels = c("informal vendor", "community pharmacy/store","private clinic","health centre", "Nanoro","Kimpese","OVERALL, weighted"))

# add a vertical position for each estimate
watchprevalence_estimates <- watchprevalence_estimates %>% mutate(position = case_when(
  intervention == "intervention" & round == "baseline" ~ as.numeric(subgroup) + 0.15,
  intervention == "intervention" & round == "post" ~ as.numeric(subgroup) + 0.08,
  intervention == "control" & round == "baseline" ~ as.numeric(subgroup) - 0.08,
  intervention == "control" & round == "post" ~ as.numeric(subgroup) - 0.15
))

# add a variable with the PR values of for each change
watchprevalence_estimates$pr <- ""
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="OVERALL, weighted"] <- "PR 0.31 (95%CI 0.11-0.91)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="Kimpese"] <- "PR 0.35 (95%CI 0.14-0.92)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="Nanoro"] <- "PR 0.35 (95%CI 0.06-2.07)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="health centre"] <- "PR 0.39 (95%CI 0.06-2.72)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="private clinic"] <- "PR 0.73 (95%CI 0.16-3.4)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="community pharmacy/store"] <- "PR 0.92 (95%CI 0.08-10)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="informal vendor"] <- ""

# make plot
subgroup_watchprevalence_plot <- ggplot(watchprevalence_estimates, aes(x = prevalence, y = position)) +
  geom_point(
    aes(color = interventionround), 
    size = 3  ) +  
  geom_errorbarh(
    aes(xmin = ci_l, xmax = ci_u, color = interventionround), 
    height = 0.1 ) +   # Error bars for confidence intervals
  scale_y_continuous(
    breaks = as.numeric(watchprevalence_estimates$subgroup),
    labels = watchprevalence_estimates$subgroup,
  ) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    name = "Prevalence use of Watch group antibiotics (%)"
  ) +  
  scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  guides(color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  geom_hline(yintercept = c(4.5,6.5), linetype = "solid", color = "darkgrey") + # Gridlines at 1st & 3rd position
  labs(
    y = element_blank(), 
    color = "Intervention/control clusters & time" ) + # Rename legend title 
  geom_text(aes(label = pr),
            hjust = -0.3,       # shift label a bit to the right of the point
            vjust = 3.9,        # vertical alignment
            size = 2.5)
subgroup_watchprevalence_plot
ggsave("subgroup_watchprevalence_plot.jpeg", plot = subgroup_watchprevalence_plot, width = 6, height = 6, dpi = 300)


#### 8. FIGURE 1C. PREVALENCE AND RR BY CLIN PRESENTATION ####
# 8.1 ANY ANTIBIOTIC
# prevalence and prevalence ratio by clinical presentation, using 2 stage cluster sampling, but not the offset (because it would require offsetting against expected patients per clinical presentation per individual provider)
# use instead post stratification weights, considering hcu and total population 
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = watch_acute, nest = TRUE)
prevalence_estimates_clinpres <- svyby(~antibiotic, by = ~intervention + round + clinpres_broad, 
                                 design = poststratweightedsurveydesign, 
                                 FUN = svymean, 
                                 vartype = c("ci"),  # Include confidence intervals
                                 na.rm = TRUE)
prevalence_estimates_clinpres$prevalence <- round(prevalence_estimates_clinpres$antibiotic*100, 1)
prevalence_estimates_clinpres$ci_l <- round(prevalence_estimates_clinpres$ci_l*100, 1)
prevalence_estimates_clinpres$ci_u <- round(prevalence_estimates_clinpres$ci_u*100, 1)
prevalence_estimates_clinpres$ci <- paste(prevalence_estimates_clinpres$ci_l,"-",prevalence_estimates_clinpres$ci_u)
prevalence_estimates_clinpres <- prevalence_estimates_clinpres %>%  select(-ci_l, -ci_u, -antibiotic)
prevalence_estimates_clinpres
write_xlsx(prevalence_estimates_clinpres, "prevalence_estimates_clinpres.xlsx")

# estimate RR
watch_acute$clinpres_broad <- relevel(watch_acute$clinpres_broad, ref = "other")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = watch_acute, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(antibiotic ~ intervention*round*clinpres_broad, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)
summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE) # values change in function of which clinpres set as reference

# subset only malaria
malaria <- watch_acute %>% filter(clinpres_broad=="malaria")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = malaria, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(antibiotic ~ intervention*round+ providertype*intervention, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# subset only ARI
ari <- watch_acute %>% filter(clinpres_broad=="acute respiratory infection")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = ari, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(antibiotic ~ intervention*round + providertype*intervention, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# subset only unexplained fever
fever <- watch_acute %>% filter(clinpres_broad=="unexplained fever")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = fever, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(antibiotic ~ intervention*round + providertype*intervention, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# subset only skin/soft tissue
skin <- watch_acute %>% filter(clinpres_broad=="skin/soft tissue infection")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = skin, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(antibiotic ~ intervention*round + providertype*intervention, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# mixed effects model with random intercept
mixed_model <- glmer(antibiotic ~ round*intervention*clinpres_broad + (1|clusterID) + (1|providertype),
                             family = binomial(link = "logit"),
                             data = watch_acute)
summary(mixed_model)
mixed_model_results <- tidy(mixed_model, conf.int = TRUE)
mixed_model_results$or <- exp(mixed_model_results$estimate)
mixed_model_results$ci_lower <- exp(mixed_model_results$conf.low)
mixed_model_results$ci_upper <- exp(mixed_model_results$conf.high)
mixed_model_results # it gives output, but not sure if correct and how to interpret; malaria OR 0.87 95%CI 0.54-1.39 & ARI 0.91 95%ci 0.54-1.51; putting providertype in model makes hardly any difference

# # with WEIGHING AND OFFSET - using the offset database as used to estimate PR
# surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# surveydesign <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
# prevalence_estimates <- svyby(~ prevalence,  ~ round + intervention,  # Grouping variables
#                               design = surveydesign,
#                               svymean,  # Function to calculate means
#                               vartype = c("ci")  # Include confidence intervals
# )
# prevalence_estimates
# 
# # with offset - giving prevalence estimates that don't make sense
# surveydesign <- svydesign(id = ~clusterID, data = watch_acute_clinpres_offset, weights = ~weight)
# # prevalence estimates by clinical presentation
# surveydesign <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
# prevalence_estimates_clinpres <- svyby(~ prevalence,  ~ clinpres_broad + round + intervention,  # Grouping variables
#                                        design = surveydesign,
#                                        svymean,  # Function to calculate means
#                                        vartype = c("ci")  # Include confidence intervals
# )
# prevalence_estimates_clinpres$prevalence <- round(prevalence_estimates_clinpres$prevalence*100, 1)
# prevalence_estimates_clinpres$ci_l <- round(prevalence_estimates_clinpres$ci_l*100, 1)
# prevalence_estimates_clinpres$ci_u <- round(prevalence_estimates_clinpres$ci_u*100, 1)
# prevalence_estimates_clinpres
# prevalence_estimates_clinpres$ci <- paste(prevalence_estimates_clinpres$ci_l,"-",prevalence_estimates_clinpres$ci_u)
# prevalence_estimates_clinpres <- prevalence_estimates_clinpres %>%  select(-ci_l, -ci_u)
# write_xlsx(prevalence_estimates_clinpres, "prevalence_estimates_clinpres.xlsx")

# negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
watch_acute_clinpres_offset$clinpres_broad <- relevel(watch_acute_clinpres_offset$clinpres_broad, ref = "other")

nb_model_clinpres <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention * clinpres_broad + providertype, 
  weights = surveydesign$weights,
  data = watch_acute_clinpres_offset
)
coef_summary <- summary(nb_model_clinpres)$coefficients
interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# 8.2 WATCH
# prevalence and prevalence ratio by clinical presentation, using 2 stage cluster sampling, but not the offset (because it would require offsetting against expected patients per clinical presentation per individual provider)
# use instead post stratification weights, considering hcu and total population 
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = watch_acute, nest = TRUE)
watchprevalence_estimates_clinpres <- svyby(~watch, by = ~intervention + round + clinpres_broad, 
                                       design = poststratweightedsurveydesign, 
                                       FUN = svymean, 
                                       vartype = c("ci"),  # Include confidence intervals
                                       na.rm = TRUE)
watchprevalence_estimates_clinpres$prevalence <- round(watchprevalence_estimates_clinpres$watch*100, 1)
watchprevalence_estimates_clinpres$ci_l <- round(watchprevalence_estimates_clinpres$ci_l*100, 1)
watchprevalence_estimates_clinpres$ci_u <- round(watchprevalence_estimates_clinpres$ci_u*100, 1)
watchprevalence_estimates_clinpres$ci <- paste(watchprevalence_estimates_clinpres$ci_l,"-",watchprevalence_estimates_clinpres$ci_u)
watchprevalence_estimates_clinpres <- watchprevalence_estimates_clinpres %>%  select(-ci_l, -ci_u, -watch)
watchprevalence_estimates_clinpres
write_xlsx(watchprevalence_estimates_clinpres, "watchprevalence_estimates_clinpres.xlsx")

# estimate RR
watch_acute$clinpres_broad <- relevel(watch_acute$clinpres_broad, ref = "other")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = watch_acute, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(watch ~ intervention*round*clinpres_broad, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)
summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE) # values change in function of which clinpres set as reference

# subset only malaria
malaria <- watch_acute %>% filter(clinpres_broad=="malaria")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = malaria, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(watch ~ intervention*round+ providertype*intervention, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# subset only ARI
ari <- watch_acute %>% filter(clinpres_broad=="acute respiratory infection")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = ari, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(watch ~ intervention*round + providertype*intervention, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# subset only unexplained fever
fever <- watch_acute %>% filter(clinpres_broad=="unexplained fever")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = fever, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(watch ~ intervention*round + providertype*intervention, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# subset only skin/soft tissue
skin <- watch_acute %>% filter(clinpres_broad=="skin/soft tissue infection")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = skin, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(watch ~ intervention*round + providertype*intervention, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

# subset only STI
sti <- watch_acute %>% filter(clinpres_broad=="sexually transmitted infection")
# log binomial regression
poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = sti, nest = TRUE)
# no sampling weights here yet, so unclear what to do
svy_binregressionmodel <- svyglm(watch ~ intervention*round, 
                                 design = poststratweightedsurveydesign, 
                                 family = binomial(link = "logit")) # logit because with log (log binomial, error message asking to provide starting coefficients)

summary(svy_binregressionmodel)
coef_summary <- summary(svy_binregressionmodel)$coefficients
interaction_coefs <- coef_summary[grep("interventionintervention:roundpost", rownames(coef_summary)), , drop = FALSE] # keep only the interaction terms for the effect of the intervention (round=post & intervention = intervention) by type of provider
log_coefs <- interaction_coefs[, 1]  # Coefficients
se_coefs <- interaction_coefs[, 2]  # Standard errors
ci_lower <- log_coefs - 1.96 * se_coefs
ci_upper <- log_coefs + 1.96 * se_coefs
rr <- exp(log_coefs)
rr_ci_lower <- exp(ci_lower)
rr_ci_upper <- exp(ci_upper)
# table of results
subgroup_effects <- data.frame(
  Providertype = rownames(interaction_coefs),
  RiskRatio = round(rr,2),
  CI_Lower = round(rr_ci_lower,2),
  CI_Upper = round(rr_ci_upper,2)
)
print(subgroup_effects, row.names = FALSE)

#### 9. FIGURE 2. EFFECT ON POPULATION-WIDE ABU BY SITE: HCU frequency*ABU prevalence####
# generate a table with prevalence per type of provider per site
# option 1: use weighting for hcu and cluster size 
# any antibiotic
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
surveydesign <- update(surveydesign, prevalence_ab = n_antibiotic / n_surveys)
prevalence_estimates <- svyby(~ prevalence_ab,  ~intervention + round + providertype + site,
                                           design = surveydesign,
                                           svymean,  # Function to calculate means
                                           vartype = c("ci")  # Include confidence intervals
)
prevalence_estimates$prevalence_ab <- prevalence_estimates$prevalence_ab
prevalence_estimates$ci_l_ab <- prevalence_estimates$ci_l
prevalence_estimates$ci_u_ab <- prevalence_estimates$ci_u
prevalence_estimates <- prevalence_estimates %>%  filter(intervention != "control") %>% select(-ci_l, -ci_u, -intervention)
prevalence_estimates
# watch antibiotics
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
surveydesign <- update(surveydesign, prevalence_watch = n_watch / n_surveys)
watchprevalence_estimates <- svyby(~ prevalence_watch,  ~intervention + round + providertype + site,
                              design = surveydesign,
                              svymean,  # Function to calculate means
                              vartype = c("ci")  # Include confidence intervals
)
watchprevalence_estimates$prevalence_watch <-watchprevalence_estimates$prevalence_watch
watchprevalence_estimates$ci_l_watch <- watchprevalence_estimates$ci_l
watchprevalence_estimates$ci_u_watch <- watchprevalence_estimates$ci_u
watchprevalence_estimates <- watchprevalence_estimates %>%  filter(intervention != "control") %>% select(-ci_l, -ci_u, -intervention)
watchprevalence_estimates
# merge any ab and watch
prevalence_estimates <- merge(prevalence_estimates, watchprevalence_estimates, by = c("round", "providertype", "site"))
prevalence_estimates

# add column for access
prevalence_estimates$prevalence_access <- prevalence_estimates$prevalence_ab - prevalence_estimates$prevalence_watch

# option 2: use post stratification weights, considering hcu and total population -> yields similar results, but weighing per cluster, option 1, is more accurate yet slightly less precise
# poststratweightedsurveydesign <- svydesign(id = ~clusterID, weights = ~poststratweight, data = watch_acute, nest = TRUE)
# prevalence_estimates <- svyby(~antibiotic, by = ~intervention + round + providertype + site, 
#                               design = poststratweightedsurveydesign, 
#                               FUN = svymean, 
#                               vartype = c("ci"),  # Include confidence intervals
#                               na.rm = TRUE)
# prevalence_estimates$prevalence <- round(prevalence_estimates$antibiotic*100, 1)
# prevalence_estimates$ci_l <- round(prevalence_estimates$ci_l*100, 1)
# prevalence_estimates$ci_u <- round(prevalence_estimates$ci_u*100, 1)
# prevalence_estimates$ci <- paste(prevalence_estimates$ci_l,"-",prevalence_estimates$ci_u)
# prevalence_estimates <- prevalence_estimates %>%  filter(intervention != "control") %>% select(-ci_l, -ci_u, -antibiotic)
# prevalence_estimates

# import table with provider type-specific healthcare utilisation rates (per 1000 inhabitants per month)
hcu <- read_excel("hcu.xlsx")
hcu <- hcu %>%  mutate(
  freq_1mo = as.numeric(freq_1mo),
  ci_lower_1mo = as.numeric(ci_lower_1mo),
  ci_upper_1mo = as.numeric(ci_upper_1mo))


# merge tables with hcu and those with the prevalence of ABU
AMUrate <- merge(hcu, prevalence_estimates, by = c("providertype", "site"))

# get rid of negative confidence intervals
AMUrate$ci_l_watch[AMUrate$ci_l_watch<0] <- 0

# add standard errors
AMUrate$se_hcu <- (log(AMUrate$ci_upper_1mo) - log(AMUrate$ci_lower_1mo)) / (2 * 1.96)
AMUrate$se_ab <- (log(AMUrate$ci_u_ab) - log(AMUrate$ci_l_ab)) / (2 * 1.96)
AMUrate$se_watch <- (log(AMUrate$ci_u_watch) - log(AMUrate$ci_l_watch)) / (2 * 1.96)

# central estimate any ab
AMUrate$rate_ab <- AMUrate$prevalence_ab * AMUrate$freq_1mo
# ci
AMUrate$se_rate_ab <- sqrt(AMUrate$se_hcu^2 + AMUrate$se_ab^2)
AMUrate$rate_ab_ci_l <- exp(log(AMUrate$rate_ab) - 1.96 * AMUrate$se_rate_ab)
AMUrate$rate_ab_ci_u <- exp(log(AMUrate$rate_ab) + 1.96 * AMUrate$se_rate_ab)
# central estimate watch
AMUrate$rate_watch <- AMUrate$prevalence_watch * AMUrate$freq_1mo
# ci
AMUrate$se_rate_watch <- sqrt(AMUrate$se_hcu^2 + AMUrate$se_watch^2)
AMUrate$rate_watch_ci_l <- exp(log(AMUrate$rate_watch) - 1.96 * AMUrate$se_rate_watch)
AMUrate$rate_watch_ci_u <- exp(log(AMUrate$rate_watch) + 1.96 * AMUrate$se_rate_watch)
# central estimate Access
AMUrate$rate_access <- AMUrate$prevalence_access * AMUrate$freq_1mo

# keep only essential values
AMUrate <- AMUrate %>% select(providertype, site, round, rate_ab, rate_ab_ci_l, rate_ab_ci_u, rate_watch, rate_watch_ci_l, rate_watch_ci_u, rate_access)
AMUrate
# to long format
AMUrate_long <- AMUrate %>%
  pivot_longer(
    cols = c(rate_ab, rate_watch, rate_access), 
    names_to = "antibiotic",        
    values_to = "rate"              
  ) %>%
  pivot_longer(
    cols = c(rate_ab_ci_l, rate_watch_ci_l),  
    names_to = "ci_type_l", 
    values_to = "ci_l"
  ) %>%
  pivot_longer(
    cols = c(rate_ab_ci_u, rate_watch_ci_u),  
    names_to = "ci_type_u", 
    values_to = "ci_u"
  ) %>%
  mutate(
    antibiotic = case_when(
      antibiotic == "rate_ab" ~ "ab",
      antibiotic == "rate_watch" ~ "watch",
      antibiotic == "rate_access" ~ "access"
    )
  ) %>%
  select(-ci_type_l, -ci_type_u)  # Remove unnecessary columns
AMUrate_long <- AMUrate_long %>%
  distinct(providertype, site, round, antibiotic, .keep_all = TRUE)
AMUrate_long

# overall rate of use
summary_AMUrate <- AMUrate %>%
  group_by(site, round) %>%
  summarise(rate_ab = sum(rate_ab), rate_ab_ci_l = sum(rate_ab_ci_l), rate_ab_ci_u = sum(rate_ab_ci_u),
            rate_watch = sum(rate_watch), rate_watch_ci_l = sum(rate_watch_ci_l), rate_watch_ci_u = sum(rate_watch_ci_u))
summary_AMUrate


# make plot -> Fig2
# rename provider type
AMUrate_long <- AMUrate_long %>%  mutate(providertype = recode(providertype, "healthcentre_publique" = "health centre",
                           "informalvendor" = "informal vendor",
                           "privatepharmacy" = "community pharmacy/store",
                           "privateclinic" = "private clinic"))
# transform to factor vars and set order
AMUrate_long$round <- factor(AMUrate_long$round, levels = c("baseline", "post"))
AMUrate_long$antibiotic <- factor(AMUrate_long$antibiotic, levels = c("watch", "access"))
AMUrate_long$providertype <- factor(AMUrate_long$providertype, levels = c("informal vendor", "community pharmacy/store","private clinic", "health centre"))
AMUrate_long$antibiotic <- factor(AMUrate_long$antibiotic,
                                    levels = rev(levels(AMUrate_long$antibiotic)))

# convert dataframe to alluvial format
AMUrate_long <- AMUrate_long %>% filter(antibiotic!="ab") 
# create an ID for the flow between bars
AMUrate_long$flow_id <- interaction(AMUrate_long$providertype, AMUrate_long$antibiotic, drop = TRUE)

head(AMUrate_long)
library(ggalluvial)
is_alluvia_form(AMUrate_long, axes = 1, silent = FALSE) # dataframe structure is ok

rates_sankey <- ggplot(data = AMUrate_long,
       aes(x = round,
           stratum = providertype,
           alluvium = flow_id,
           y = rate,
           color = providertype)) +
  geom_flow(aes(fill = antibiotic), alpha = 0.6, width = 0.2, tension = 0.5) +
  geom_stratum(aes(fill = antibiotic), alpha = 0.9, width = 0.1) +
  scale_fill_manual(name = "AWaRe group", values = c("watch" = "orange", "access" = "darkgreen"), na.translate = FALSE) +
  scale_color_discrete(name = "provider type") +  # adds stratum to legend
  theme_minimal() +
  labs(y = "Antibiotic treatment episodes per 1000 inhabitants per month", x = NULL) +
  facet_wrap(~ site) # , scales = "free_y"
rates_sankey
ggsave("rates_sankey.jpeg", plot = rates_sankey, width = 9, height = 7, dpi = 300)
