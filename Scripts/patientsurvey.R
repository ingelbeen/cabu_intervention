#####################################################
# CABU-EICO patientsurveys                          #
# prevalence of antibiotic use, AWaRe distribution  #
#####################################################

# install/load packages
pacman::p_load(readxl, lubridate, haven, dplyr, tidyr, digest, ggplot2, survey, srvyr, gtsummary, lme4, broom.mixed)

#### 1. IMPORT DATA KIMPESE #### 
# 1.1 Kimpese baseline
# import patient data
patient_kim_bl <- read_excel("db/patientsurvey/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-01-16-12-52-23.xlsx", 
                       sheet = "Questionnaire patient CABU-RDC")
patient_kim_post <- read_excel("db/patientsurvey/R2_Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-02-05-14-30-05.xlsx")
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

# add value to type of dispensor
patient_kim$another_disp <- tolower(patient_kim$another_disp)
patient_kim$dispenserpharmacie[grepl("tudiant", patient_kim$another_disp)] <- "student medicine or nursing"

# complete missing ages based on dob
table(patient_kim$birthyear[is.na(patient_kim$ageyears)], useNA = "always")
patient_kim$ageyears[is.na(patient_kim$ageyears)] <- round(as.numeric(patient_kim$today[is.na(patient_kim$ageyears)]-patient_kim$birthyear[is.na(patient_kim$ageyears)])/365.25, 0)

# remove unnecessary variables
patient_kim <- patient_kim %>% select(c("round", "today", "antibiotic", "choices_cluster", "providertype", "providernr", "dispenserpharmacie", "patientnr", "caretaker", "sex", 
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
                                        "confirm_end", "_uuid", "_index"))
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

# bring back provider type to three categories, too little informal providers included
patient_kim$providertype[patient_kim$providertype=="informalstore"] <- "privatepharmacy"

# import antibiotic data (a loop for individual antibiotics recorded can be repeated for the same patient) 
ab_kim_bl <- read_excel("db/patientsurvey/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-01-16-12-52-23.xlsx", 
                            sheet = "ab")
ab_kim_bl <- ab_kim_bl %>% select(-abfreq) # bl questionnaire had twice the frequency of intake recorded
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
ab_kim <- ab_kim %>% select( c("abgeneric", "abroute", "abdose", "abfreq_001", "abunits", "abduration", "abproducer", "abexpiry", "abprix",
                               "_submission__uuid", "Class", "ATC code", "Category",
                               "_index", "_parent_table_name", "_parent_index", "_submission__id"))
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
  group_by(`_uuid`, intervention, round, choices_cluster, providertype, providernr, agegroup, sex, illness) %>%
  summarise(watch = if_else(any(aware == "Watch"), 1, 0),
              antibiotic = if_else(any(!is.na(abgeneric)), 1, 0))
watchkim$watch[is.na(watchkim$watch)] <- 0
watchkim$antibiotic[is.na(watchkim$antibiotic)] <- 0
table(watchkim$watch, useNA = "always")
table(watchkim$antibiotic, useNA = "always")

# consider healthcare providers as sampling unit, regardless of cluster villages (since all healthcare provieders in those villages included)
watchkim$cluster <- paste(watchkim$choices_cluster, "-", watchkim$providertype, "-", watchkim$providernr) # 128 providers

# anonymize the provider clusters to prevent identification of providers
watchkim$clusterID <- sapply(watchkim$cluster, function(clusterID) {
  return(substring(digest(as.character(clusterID), algo = "crc32"), 1, 5))})

# anonymize the provider clusters to prevent identification of providers and remove identifying info in the overall db
kim$cluster <- paste(kim$choices_cluster, "-", kim$providertype, "-", kim$providernr) # 128 providers
kim$clusterID <- sapply(kim$cluster, function(clusterID) {
  return(substring(digest(as.character(clusterID), algo = "crc32"), 1, 5))})
kim_anon <- kim %>% select(c("round", "intervention", "clusterID", "providertype", "today", "antibiotic","dispenserpharmacie", "patientnr", "caretaker", "sex", 
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
                                        "nantibiotics", "matchedprescription", "nomatchreasons", "nomatchreason_other", "abgeneric", "abroute", 
                                        "abdose", "abfreq_001", "abunits", "abduration", "Class", "aware"))

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
  select("meta.instanceID", starts_with("achatMedic.")) %>%
  select(-contains(".bg"))
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
patient_nan_noab <- patient_nan %>% select("meta.instanceID", "providertype", "providernr", "round", "intervention", "ageyears", "agegroup", "sex", "educationlevel", "illness", c(names(patient_nan)[26:45], "surveydate", "village.cluster"))

# link patient and antibiotic data
nan <- merge(patient_nan_noab, ab_nan, by = "meta.instanceID", all = T) 

# anonymize the provider clusters to prevent identification of providers and remove identifying info in the overall db
nan$cluster <- paste(nan$village.cluster, "-", nan$providertype, "-", nan$providernr) # 61 providers
nan$clusterID <- sapply(nan$cluster, function(clusterID) {
  return(substring(digest(as.character(clusterID), algo = "crc32"), 1, 5))})

# export database
write.csv(nan, 'nan.csv')
# still need to remove variables to make an anonymized db of nan

# create a new variable watch that also is "Watch" if a fixed-dose combination contains at least one watch AB
nan$aware <- nan$Category
table(nan$aware) # no fixed dose combination here

# create a database with one line per patient indicating whether the patient used a Watch AB or not
watchnan <- nan %>%
  filter(!is.na(round)) %>%
  group_by(meta.instanceID, intervention, round, village.cluster, providertype, providernr, agegroup, sex, illness) %>%
  summarise(watch = if_else(any(aware == "Watch"), 1, 0),
            antibiotic = if_else(any(!is.na(abgeneric)), 1, 0))
watchnan$watch[is.na(watchnan$watch)] <- 0
watchnan$antibiotic[is.na(watchnan$antibiotic)] <- 0
table(watchnan$watch, useNA = "always")
table(watchnan$antibiotic, useNA = "always")

# consider healthcare providers as sampling unit, regardless of cluster villages (since all healthcare provieders in those villages included)
watchnan$cluster <- paste(watchnan$village.cluster, "-", watchnan$providertype, "-", watchnan$providernr) # 128 providers
watchnan$cluster <- as.factor(watchnan$cluster)

# anonymize the provider clusters to prevent identification of providers
watchnan$clusterID <- sapply(watchnan$cluster, function(clusterID) {
  return(substring(digest(as.character(clusterID), algo = "crc32"), 1, 5))})

### append Kimpese and Nanoro (main) data ###
watchnan <- as.data.frame(watchnan)
watchkim <- as.data.frame(watchkim)

# add name of site
watchnan$site <- "Nanoro"
watchkim$site <- "Kimpese"

# rename original cluster variable (village or neighbourhood)
watchkim <- watchkim %>% rename(village.cluster = choices_cluster)

# remove variables that still could allow identification of dispensors
watchnan <- watchnan %>% select(-providernr, -cluster, -meta.instanceID)
watchkim <- watchkim %>% select(-providernr, -cluster, -`_uuid`)

# merge both
watch <- rbind(watchnan, watchkim)

# add population numbers per village/neighbourhood cluster STILL COMPLETE NUMBERS HERE!!!
watch <- watch %>%  mutate(pop_villagecluster = case_when(
    village.cluster == "BAL" ~ 1716,
    village.cluster == "BOL" ~ 6829,
    village.cluster == "BOU" ~ 3658,
    village.cluster == "CELLULE_MASAMUNA_AS_CBCO" ~ NA,
    village.cluster == "CELLULE_MBUKA3_AS_Yanga_Dia_Songa" ~ NA,
    village.cluster == "DAC" ~ 1448,
    village.cluster == "GOU" ~ 2840,
    village.cluster == "KAL" ~ 2202,
    village.cluster == "KIANDU_AS_Viaza" ~ 420,
    village.cluster == "KIASUNGUA_AS_Kisaunga" ~ 2200,
    village.cluster == "KILUEKA_AS_Kilueka" ~ 630,
    village.cluster == "KIMAKU_AS_Viaza" ~ NA,
    village.cluster == "KITOBOLA_AS_Kilueka" ~ 5772, # seems a lot, considering how small the village is
    village.cluster == "KOK" ~ 1440,
    village.cluster == "KOU" ~ 4270,
    village.cluster == "LAL" ~ 2712,
    village.cluster == "LUKENGEZI_ET_POSTE_AS_CECO" ~ 1300,
    village.cluster == "MALANGA_AS_Malanga" ~ 1740,
    village.cluster == "MBANZA_NDAMBA_AS_Kilueka" ~ 477,
    village.cluster == "MONT_FLEURY_AS_Kimbanguiste" ~ 541,
    village.cluster == "MPETE_NKONDO_AS_Kiasunga" ~ NA,
    village.cluster == "NAN" ~ 7542,
    village.cluster == "NAZ" ~ 5293,
    village.cluster == "NGOMBE1_AS_Vunda_Nsole" ~ 660,
    village.cluster == "NKULA_AS_Viaza" ~ 340,
    village.cluster == "PEL" ~ NA,
    village.cluster == "POE" ~ 2304,
    village.cluster == "POI" ~ 4580,
    village.cluster == "Q2_(AS_CBCO)" ~ NA,
    village.cluster == "Q2_AS_CECO" ~ 2198,
    village.cluster == "Q3_AS_Kimbanguiste" ~ NA,
    village.cluster == "Q3_AS_Yanga_Dia_Songa" ~ NA,
    village.cluster == "RAK" ~ 2177,
    village.cluster == "SANZIKUA_AS_Vunda_Nsole" ~ 874,
    village.cluster == "SEG" ~ 3942,
    village.cluster == "SIG" ~ 3010,
    village.cluster == "SOA" ~ 2376,
    village.cluster == "SOU" ~ 5334,
    village.cluster == "SOW" ~ 7541,
    village.cluster == "VIAZA_AS_Viaza" ~ 582,
    village.cluster == "VUNDA_NSOLE_AS_Vunda_Nsole" ~ 686,
    village.cluster == "WENE_AS_Kilueka" ~ 600,
    village.cluster == "ZAMBA_I_AS_Malanga" ~ 620,
    TRUE ~ NA ))
table(watch$pop_villagecluster, useNA = "always") 
watch$pop_villagecluster[is.na(watch$pop_villagecluster)] <- 2000 # if pop is missing (NEED TO UPDATE) then replace with 2000

# add monthly healthcare use frequecy per 1000 inhabitants, by type of provider
# from 2019 HCU survey, Kisantu & Kimpese combined (b/c Kimpese didn't have peri-urban areas included then)
watch$hcu[watch$site=="Kimpese" & watch$providertype=="healthcentre_publique"] <- 25.5
watch$hcu[watch$site=="Kimpese" & watch$providertype=="privateclinic"] <- 31.0
watch$hcu[watch$site=="Kimpese" & watch$providertype=="privatepharmacy"] <- 17.6
# from 2022-23 HCU part of the CABU-EICO household survey 
watch$hcu[watch$site=="Nanoro" & watch$providertype=="healthcentre_publique"] <- 22.20
watch$hcu[watch$site=="Nanoro" & watch$providertype=="privateclinic"] <- NA
watch$hcu[watch$site=="Nanoro" & watch$providertype=="privatepharmacy"] <- 1.67
watch$hcu[watch$site=="Nanoro" & watch$providertype=="informalvendor"] <- 4.34
table(watch$hcu, useNA = "always")

# the number of patients expected from that provider in each village_cluster: HCU * population of the village cluster
watch$pop_patients <- watch$hcu*watch$pop_villagecluster
table(watch$pop_patients, useNA = "always")

# exclude animal use, chronic patients, no illness, to keep just acute illness
watch_acute <- watch %>% filter(illness=="yes_acute_illness")

# add a post stratification weight of each survey
pop_by_site <- watch %>%
  group_by(site, village.cluster) %>%
  summarise(pop_cluster = mean(pop_villagecluster)) %>%
  group_by(site) %>%
  summarise(pop_site = sum(pop_cluster))
pop_by_site

nsurveys_by_providertype_by_site <- watch_acute %>%
  group_by(site, providertype, intervention, round) %>%
  summarise(n_surveys = n(), hcu = mean(hcu))
nsurveys_by_providertype_by_site

poststratificationweight <- merge(nsurveys_by_providertype_by_site, pop_by_site, by = "site")
poststratificationweight$poststratweight <- (poststratificationweight$pop_site*poststratificationweight$hcu)/poststratificationweight$n_surveys

watch_acute <- merge(watch_acute, poststratificationweight, by = c("site", "providertype", "intervention", "round"))

# reorganize data for a Poisson with offset model so there is one line per provider/clusterID, calculate 
# number of patients sampled from each provider, and sampling weight (population size/sample size)
watch_acute_offset <- watch_acute %>% 
  group_by(clusterID, pop_patients, village.cluster, pop_villagecluster, hcu, intervention, round, providertype, site) %>%
  summarise(n_surveys = n(), n_antibiotic = sum(antibiotic), n_watch = sum(watch), weight = mean(pop_patients)/n())
head(watch_acute_offset)
# export dataframe with one observation per patient and the aggregated dataframe by provider
write.csv(watch_acute, "watch_acute.csv")
write.csv(watch_acute_offset, "watch_acute_offset.csv")

# reformat some chr vars, replace by factor variables
watch_acute$intervention <- as.factor(watch_acute$intervention)
watch_acute$agegroup <- factor(watch_acute$agegroup, levels = c("0-4 yr", "5-17 yr", "18-64 yr", "65+ yr"))
watch_acute$illness <- factor(watch_acute$illness, levels = c("yes_acute_illness", "yes_chronic", "no_noillness", "no_animalhealth"))
watch_acute$providertype <- factor(watch_acute$providertype, levels = c("healthcentre_publique", "privateclinic", "privatepharmacy", "informalvendor"))

#### 3. DESCRIPTION PARTICIPANTS ####
# n surveys
table(watch$round, watch$site, useNA = "always")

# providers Kimpese
table(watchkim$providertype[watchkim$round=="baseline"], watchkim$intervention[watchkim$round=="baseline"], useNA = "always")
table(watchkim$providertype[watchkim$round=="post"], watchkim$intervention[watchkim$round=="post"], useNA = "always")

# providers Nanoro
table(watchnan$providertype[watchnan$round=="baseline"], watchnan$intervention[watchnan$round=="baseline"], useNA = "always")
table(watchnan$providertype[watchnan$round=="post"], watchnan$intervention[watchnan$round=="post"], useNA = "always")

# clusters and number of clusters
table(watchkim$clusterID, useNA = "always")
nclusterskim <- watchkim %>% group_by(clusterID) %>% summarise(n())
count(nclusterskim)

table(watchnan$clusterID, useNA = "always")
nclustersnan <- watchnan %>% group_by(clusterID) %>% summarise(n())
count(nclustersnan)

# table 1 patient characteristics
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
# append all these tables
colnames(illnessdistr)[1] <- "characteristic"
colnames(agegroupdistr)[1] <- "characteristic"
colnames(sexdistr)[1] <- "characteristic"
table1 <- rbind(illnessdistr)
table1 <- bind_rows(illnessdistr, agegroupdistr, sexdistr)
# reorder columns
table1 <- table1 %>% select("characteristic", "n_Kimpese_control","percent_Kimpese_control","n_Kimpese_intervention",
                            "percent_Kimpese_intervention", "n_Nanoro_control", "percent_Nanoro_control", "n_Nanoro_intervention",                     
                            "percent_Nanoro_intervention" )
# save table
write.table(table1, "table1.txt")

#### 4. PREVALENCE WATCH ANTIBIOTIC USE ####
# 4.1 crude prevalence by provider type, by intervention/control group, by site, and pre- vs. post intervention
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
write.table(summary_antibioticcounts, "summary_antibioticcounts.txt")

# 4.2 two-stage cluster sampling-corrected prevalence by group
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
# save table
write.table(antibioticclusterprop_wide, "antibioticclusterprop_wide.txt")

#### 5. PREVALENCE RATIO WATCH/ANY ANTIBIOTIC USE INTERVENTION VS CONTROL ####
# model structure: ABU ~ Time + Intervention + Time*Intervention + confounders?? + clusters/providers(clusterID)
# for now (survey in Nanoro less than 50% includions, in Kimpese nearly complete) exclude Nanoro
watch_acute_offset_kim <- watch_acute_offset %>% filter(site=="Kimpese")

# 5.1 neg binomial regression with offset - model retained for analysis
# 5.1.1 WATCH AB
# define survey design
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, 
                          # weights = ~weight # for now no weighing because surveys still ongoing in some clusters, so that the few surveys there get too much weight
)
# fit model
nb_model <- svyglm(n_watch ~ offset(log(pop_patients)) + round * intervention + providertype, #+ agegroup?  + site
                              design = surveydesign, 
                              family = quasipoisson)
summary(nb_model)
# get the model coefficients
coef <- coef(nb_model)
coeci <- confint(nb_model)
rr <- exp(coef)
rr # see that it's an OR actually since it's log regression eventually
ci_rr <- exp(coeci)
ci_rr

# 5.1.2 ANY ANTIBIOTIC
# define survey design
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_kim, 
                          # weights = ~weight # for now no weighing because surveys still ongoing in some clusters, so that the few surveys there get too much weight
)
# fit model
nb_model_anyAB <- svyglm(n_antibiotic ~ offset(log(pop_patients)) + round * intervention + providertype, #+ agegroup?  + site
                   design = surveydesign, 
                   family = poisson)
summary(nb_model_anyAB)
# get the model coefficients
coef <- coef(nb_model_anyAB)
coeci <- confint(nb_model_anyAB)
rr <- exp(coef)
rr # see that it's an OR actually since it's log regression eventually
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
