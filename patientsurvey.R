#####################################################
# CABU-EICO patientsurveys                          #
# prevalence of antibiotic use, AWaRe distribution  #
#####################################################

# install/load packages
pacman::p_load(readxl, lubridate, haven, dplyr, ggplot2, survey, MASS,srvyr, gtsummary)

#### 1. IMPORT DATA #### 
# 1.1 Kimpese baseline
# import patient data
patient_kim_bl <- read_excel("db/patientsurvey/Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-01-16-12-52-23.xlsx", 
                       sheet = "Questionnaire patient CABU-RDC")
patient_kim_post <- read_excel("db/patientsurvey/R2_Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-01-16-13-05-06.xlsx")
# remove variables that the baseline survey data have but the post data don't
patient_kim_bl <- patient_kim_bl %>% select(-interviewdate, -diag_spec)
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

# var agegroup children vs adults
patient_kim$adoadult[patient_kim$ageyears>17.99] <- "adult"
patient_kim$adoadult[patient_kim$ageyears<18] <- "child/adolescent"

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
ab_kim_post <- read_excel("db/patientsurvey/R2_Questionnaire_patient_CABU-RDC_-_all_versions_-_English_-_2024-01-16-13-05-06.xlsx",
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
table(ab_kim$abgeneric)
table(aware$Antibiotic)
table(ab_kim$Category)
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

#### 1. DESCRIPTION PARTICIPANTS ####
# n surveys
table(patient_kim$round, useNA = "always")

# provider
table(patient_kim$providertype[patient_kim$round=="baseline"], patient_kim$intervention[patient_kim$round=="baseline"], useNA = "always")
table(patient_kim$providertype[patient_kim$round=="post"], patient_kim$intervention[patient_kim$round=="post"], useNA = "always")

# cluster
table(patient_kim_bl$choices_cluster, useNA = "always")
# public vs private
table(patient_kim$providertype, useNA = "always")
patient_kim$publicprivate <- "private"
patient_kim$publicprivate[patient_kim$providertype=="healthcentre_publique"] <- "public"

#### 2. PREVALENCE WATCH ANTIBIOTIC USE ####
# 2.1. crude
# crude difference pre and post intervention in control clusters
watchcount_prepost <- table(watchkim$round[watchkim$intervention=="control"], watchkim$watch[watchkim$intervention=="control"], useNA = "always")
watchcount_prepost
watchprev_prepost <- round(prop.table(watchcount_prepost, 1),2)
watchprev_prepost

# crude difference pre and post intervention in intervention clusters only
watchcount_prepost <- table(watchkim$round[watchkim$intervention=="intervention"], watchkim$watch[watchkim$intervention=="intervention"], useNA = "always")
watchcount_prepost
watchprev_prepost <- round(prop.table(watchcount_prepost, 1),2)
watchprev_prepost

# specifically in health centres
watchcount_prepost_healthcentres <- table(watchkim$round[watchkim$intervention=="intervention"&watchkim$providertype=="healthcentre_publique"], watchkim$watch[watchkim$intervention=="intervention"&watchkim$providertype=="healthcentre_publique"], useNA = "always")
watchcount_prepost_healthcentres
round(prop.table(watchcount_prepost_healthcentres, 1),2)

# specifically in private pharmacies
watchcount_prepost_pharmacy <- table(watchkim$round[watchkim$intervention=="intervention"&watchkim$providertype=="privatepharmacy"], watchkim$watch[watchkim$intervention=="intervention"&watchkim$providertype=="privatepharmacy"], useNA = "always")
watchcount_prepost_pharmacy
round(prop.table(watchcount_prepost_pharmacy, 1),2)

# specifically in private clinics
watchcount_prepost_privateclinic <- table(watchkim$round[watchkim$intervention=="intervention"&watchkim$providertype=="privateclinic"], watchkim$watch[watchkim$intervention=="intervention"&watchkim$providertype=="privateclinic"], useNA = "always")
watchcount_prepost_privateclinic
round(prop.table(watchcount_prepost_privateclinic, 1),2)

# prevalence in intervention vs control
watchcount_post <- table(watchkim$intervention[watchkim$round=="post"], watchkim$watch[watchkim$round=="post"], useNA = "always")
watchcount_post
watchprev_post <- round(prop.table(watchcount_post, 1),2)
watchprev_post

# 2.2 two-stage cluster sampling-corrected prevalence by group
# specify 2 stage cluster design, and strata
watchkim$intervention <- as.factor(watchkim$intervention)
watchkim$choices_cluster <- as.factor(watchkim$choices_cluster)
watchkim$agegroup <- as.factor(watchkim$agegroup)
watchkim$providertype <- as.factor(watchkim$providertype)

clusterdesign <- svydesign(
  id = ~ choices_cluster + providertype + providernr,
  data = watchkim)
# Calculate the proportion with 95% confidence intervals for the variable 'Watch'
# currently no weighing is done for the frequency of healthcare seeking by provider/type of provider so that a provider that is visited more frequently also contributes more to the overall prevalence, 
# a weighing variable is ideally still added once it can be estimated from the household survey data
prop_by_provider <- svyby(~watch, ~providertype, clusterdesign, svyciprop, quantiles=0.5,ci=TRUE,vartype="ci")
prop_by_provider

# Model structure: ABU ~ Time + Intervention + Time*Intervention + confounders??
# log binomial regression
binregressionmodel <- glm(watch ~ round + intervention + round*intervention + providertype # + choices_cluster + agegroup + providernr
             family = binomial(link = "logit"), # logit for logistic
             data = watchkim)
summary(binregressionmodel)

# get the model coefficients
coef <- coef(binregressionmodel)
coeci <- confint(binregressionmodel)
rr <- exp(coef)
rr
ci_rr <- exp(coeci)
ci_rr

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

# interviews per provider type
table(patient$providertype)
table(patient$publicprivate)

# prévalence d'usage d'antibiotiques
table(patient$providertype, patient$antibiotic)
round(prop.table(table(patient$providertype, patient$antibiotic),1)*100,1)

# distribution d'antibiotiques
table(patient_ab$abgeneric)
table(patient_ab$abgeneric_other)

# explore effect
tab <- table(patient_kim$antibiotic, patient_kim$round, useNA = "always")
round(prop.table(tab, 2),2)

tab2 <- table(patient_kim$intervention[patient_kim$round=="post"], patient_kim$antibiotic[patient_kim$round=="post"], useNA = "always")
round(prop.table(tab2, 1),2)
