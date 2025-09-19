#####################################################
# CABU-EICO simulated patient visits                #
#####################################################

# install/load packages
pacman::p_load(readxl, lubridate, haven, dplyr, tidyr, digest, ggplot2, survey, srvyr, gtsummary, lme4, broom.mixed, stringr)

#### 1. IMPORT DATA #### 
#### 1.1 Nanoro Diarrhoea ####
# baseline & post intervention, then append both
diarrhoea_nan_bl <- read_excel("mysterypatients/Gastro-entérite_aiguë_non_sanguine_mais_hydrique_-_all_versions_-_labels_-_2023-12-16-08-54-38_ENG.xlsx")
diarrhoea_nan_post<-read_excel("mysterypatients/Gastro-entérite_aiguë_2ENG.xlsx")
diarrhoea_nan_bl$round <- "pre"
diarrhoea_nan_post$round <- "post"
diarrhoea_nan <- rbind(diarrhoea_nan_bl, diarrhoea_nan_post)
diarrhoea_nan$round <- factor(diarrhoea_nan$round, levels = c("pre", "post"))

# one observation post intervention has the wrong village entered
diarrhoea_nan$Village[diarrhoea_nan$Village=="RAKALO" & diarrhoea_nan$Type=="Vendeur informel 1" & diarrhoea_nan$`AbdominalPain?`==1] <- "KOKOLO"

# indicate intervention and control villages
table(diarrhoea_nan$Village, useNA = "always")
intervention_villages <- c("BALOGHO", "BOLOGHO", "KOURIA", "PELLA", "DACISSE", "KOKOLO", "NANORO", "NAZOANGA", "POESSI", "SOUM", "ZIMIDIN")
diarrhoea_nan <- diarrhoea_nan %>%  mutate(intervention = ifelse(Village %in% intervention_villages, "intervention", "control"))
diarrhoea_nan$intervention <- as.factor(diarrhoea_nan$intervention)
table(diarrhoea_nan$Village, diarrhoea_nan$intervention, useNA = "always")

# create groups
diarrhoea_nan$group <- with(diarrhoea_nan, interaction(round, intervention))
table(diarrhoea_nan$group)

# variable for each individual provider
diarrhoea_nan$provider <- paste(diarrhoea_nan$Village,diarrhoea_nan$Type)
table(diarrhoea_nan$provider)

# variable for providertype
diarrhoea_nan$providertype[grepl("CSPS", diarrhoea_nan$Type)==T] <- "health centre"
diarrhoea_nan$providertype[grepl("informel", diarrhoea_nan$Type)==T] <- "informal vendor"
diarrhoea_nan$providertype[grepl("Dépot", diarrhoea_nan$Type)==T] <- "community pharmacy/store"
diarrhoea_nan$providertype[grepl("Pharmacie", diarrhoea_nan$Type)==T] <- "community pharmacy/store"
diarrhoea_nan$providertype <- as.factor(diarrhoea_nan$providertype)
diarrhoea_nan$providertype <- relevel(diarrhoea_nan$providertype, ref = "informal vendor")
table(diarrhoea_nan$Type, diarrhoea_nan$providertype)

# categorical var AB use
table(diarrhoea_nan$`Abgiven/Prescription`, useNA = "always")
diarrhoea_nan$ab[diarrhoea_nan$`Abgiven/Prescription`==-2] <- "yes"
diarrhoea_nan$ab[diarrhoea_nan$`Abgiven/Prescription`!=-2] <- "no"
diarrhoea_nan$ab <- as.factor(diarrhoea_nan$ab)
table(diarrhoea_nan$ab, useNA = "always")

# categorical var Watch ABU
table(diarrhoea_nan$WATCHAB, useNA = "always")
diarrhoea_nan$watch[diarrhoea_nan$WATCHAB==-2] <- "yes"
diarrhoea_nan$watch[diarrhoea_nan$WATCHAB!=-2] <- "no"
diarrhoea_nan$watch <- as.factor(diarrhoea_nan$watch)
table(diarrhoea_nan$watch, useNA = "always")

# numerical variable correct case mgmt 
diarrhoea_nan <- diarrhoea_nan %>%  rowwise() %>%  mutate(score = sum(c_across(c("TypeStool?", "BloodStool?", "MucusStool?", 
                                  "FreqStool?", "Fever?", "AbdominalPain?", 
                                  "VomitNausea?", "QuestionUrination?", 
                                  "CapacityDrink?", "Lethargic?", 
                                  "GeneralHealthCondition?", "RecentUseAB?", 
                                  "OtherFamilyMembers?", "SourceWater?", 
                                  "MealPrep?", "HandHygiene?", 
                                  "WASHNeighborhood?", "HygienePhysicalEnvironment?", 
                                  "VitalSigns", "AbdominalExamination", 
                                  "SkinPinch", "SunkenEyes", "StayHydrated!", 
                                  "AdviseWaterFoodHygiene!", "HowActProlongedDiarrhea", 
                                  "Abgiven/Prescription", "WATCHAB", "ORS", 
                                  "Antimalaria", "NumberIVmedication")), 
                       na.rm = TRUE))

# adapt dataframe to one row per provider
diarrhoea_nan_wide <- diarrhoea_nan %>%  
  select(provider, providertype, round, score, intervention) %>%
  pivot_wider(names_from = round, values_from = score, names_prefix = "score_")

#### 1.2 Nanoro Severe Pneumonia ####
pneumonia_nan_bl <- read_excel("mysterypatients/Pneumonie_aiguë_grave_chez_un_patient_âgé_-_all_versions_-_labels_-_2023-12-16-08-59-59ENG.xlsx")
pneumonia_nan_post<-read_excel("mysterypatients/Pneumonie_aiguë_grave_chez_un_patient_âgé_2ENG.xlsx")
pneumonia_nan_bl$round <- "pre"
pneumonia_nan_post$round <- "post"
pneumonia_nan <- rbind(pneumonia_nan_bl, pneumonia_nan_post)
pneumonia_nan$round <- factor(pneumonia_nan$round, levels = c("pre", "post"))

# one observation post intervention has the wrong village entered - to double check on paper form
pneumonia_nan$Village[pneumonia_nan$Village=="BOLOGHO" & pneumonia_nan$Type=="Vendeur informel 1" & pneumonia_nan$round=="post" & pneumonia_nan$`Sputum?`==0] <- "BALOGHO" 

# indicate intervention and control villages
table(pneumonia_nan$Village, useNA = "always")
intervention_villages <- c("BALOGHO", "BOLOGHO", "KOURIA", "PELLA", "DACISSE", "KOKOLO", "NANORO", "NAZOANGA", "POESSI", "SOUM", "ZIMIDIN")
pneumonia_nan <- pneumonia_nan %>%  mutate(intervention = ifelse(Village %in% intervention_villages, "intervention", "control"))
pneumonia_nan$intervention <- as.factor(pneumonia_nan$intervention)
table(pneumonia_nan$Village, pneumonia_nan$intervention, useNA = "always")

# create groups
pneumonia_nan$group <- with(pneumonia_nan, interaction(round, intervention))
table(pneumonia_nan$group)

# variable for each individual provider
pneumonia_nan$provider <- paste(pneumonia_nan$Village,pneumonia_nan$Type)
table(pneumonia_nan$provider)

# variable for providertype
pneumonia_nan$providertype[grepl("CSPS", pneumonia_nan$Type)==T] <- "health centre"
pneumonia_nan$providertype[grepl("informel", pneumonia_nan$Type)==T] <- "informal vendor"
pneumonia_nan$providertype[grepl("Dépot", pneumonia_nan$Type)==T] <- "community pharmacy/store"
pneumonia_nan$providertype[grepl("Pharmacie", pneumonia_nan$Type)==T] <- "community pharmacy/store"
pneumonia_nan$providertype <- as.factor(pneumonia_nan$providertype)
pneumonia_nan$providertype <- relevel(pneumonia_nan$providertype, ref = "informal vendor")
table(pneumonia_nan$Type, pneumonia_nan$providertype)

# categorical var AB use
table(pneumonia_nan$`Abgiven/prescribed`, useNA = "always")
pneumonia_nan$ab[pneumonia_nan$`Abgiven/prescribed`==-1] <- "yes"
pneumonia_nan$ab[pneumonia_nan$`Abgiven/prescribed`!=-1] <- "no"
pneumonia_nan$ab <- as.factor(pneumonia_nan$ab)
table(pneumonia_nan$ab, useNA = "always")

# Watch AB not applicable in pneumonia scenario - goal is only warning the patient's caretaker and recommending referral to hospital, dispensing Watch AB can or can not be recommended, depending on situation

# categorical variable correct case mgmt (binary)

# numerical variable correct case mgmt 
pneumonia_nan <- pneumonia_nan %>%  rowwise() %>%  mutate(score = sum(c_across(column_names <- c("TypeCough?", "DurationCough?", "CoughWorsened?", 
                                                                                                 "Fever?", "Sputum?", "RR?", "DifficultBreathing?", 
                                                                                                 "ConfusionPresent?", "Lethargic?", "HistroyRespDisease?",
                                                                                                 "ReferralHospitalHC", "SeekAppropriateCare!", "Abgiven/prescribed", 
                                                                                                 "AntimalariaWithoutDiagnose")), 
                                                                      na.rm = TRUE))
# adapt dataframe to one row per provider
pneumonia_nan_wide <- pneumonia_nan %>%  
  select(provider, providertype, round, score, intervention) %>%
  pivot_wider(names_from = round, values_from = score, names_prefix = "score_")

#### 1.3 Nanoro Fever ####
fever_nan_bl <- read_excel("mysterypatients/Fièvre_aiguë_isolée_-_all_versions_-_labels_-_2023-12-16-08-59-27ENG.xlsx")
fever_nan_post<-read_excel("mysterypatients/Fièvre_aiguë_isolée_2ENG.xlsx")
fever_nan_bl$round <- "pre"
fever_nan_post$round <- "post"
fever_nan <- rbind(fever_nan_bl, fever_nan_post)
fever_nan$round <- factor(fever_nan$round, levels = c("pre", "post"))

# three observation which seem to have mistakes in the provider nr. - to double check on paper form
fever_nan$Type[duplicated(fever_nan) & fever_nan$Village=="SOAW" & fever_nan$Type=="Vendeur informel 1" & fever_nan$round=="pre"] <- "Vendeur informel 2"
fever_nan$Type[duplicated(fever_nan) & fever_nan$Village=="RAKALO" & fever_nan$Type=="Vendeur informel 1" & fever_nan$round=="pre"] <- "Vendeur informel 2"
fever_nan$Type[fever_nan$Medicationgiven=="Dyclosa" & fever_nan$Village=="NANORO" & fever_nan$Type=="Vendeur informel 1" & fever_nan$round=="pre"] <- "Vendeur informel 2"
fever_nan$Village[fever_nan$Village=="BOULPON" & fever_nan$Type=="Vendeur informel 2" & fever_nan$round=="post" & fever_nan$`RespiratoryComplaints?`==1] <- "BOLOGHO"

# indicate intervention and control villages
table(fever_nan$Village, useNA = "always")
intervention_villages <- c("BALOGHO", "BOLOGHO", "KOURIA", "PELLA", "DACISSE", "KOKOLO", "NANORO", "NAZOANGA", "POESSI", "SOUM", "ZIMIDIN")
fever_nan <- fever_nan %>%  mutate(intervention = ifelse(Village %in% intervention_villages, "intervention", "control"))
fever_nan$intervention <- as.factor(fever_nan$intervention)
table(fever_nan$Village, fever_nan$intervention, useNA = "always")

# create groups
fever_nan$group <- with(fever_nan, interaction(round, intervention))
table(fever_nan$group)

# variable for each individual provider
fever_nan$provider <- paste(fever_nan$Village,fever_nan$Type)
table(fever_nan$provider)

# variable for providertype
fever_nan$providertype[grepl("CSPS", fever_nan$Type)==T] <- "health centre"
fever_nan$providertype[grepl("informel", fever_nan$Type)==T] <- "informal vendor"
fever_nan$providertype[grepl("Dépot", fever_nan$Type)==T] <- "community pharmacy/store"
fever_nan$providertype[grepl("Pharmacie", fever_nan$Type)==T] <- "community pharmacy/store"
fever_nan$providertype <- as.factor(fever_nan$providertype)
fever_nan$providertype <- relevel(fever_nan$providertype, ref = "informal vendor")
table(fever_nan$Type, fever_nan$providertype)

# categorical var AB use
table(fever_nan$`Abgiven/Prescription`, useNA = "always")
fever_nan$ab[fever_nan$`Abgiven/Prescription`==-1] <- "yes"
fever_nan$ab[fever_nan$`Abgiven/Prescription`!=-1] <- "no"
fever_nan$ab <- as.factor(fever_nan$ab)
table(fever_nan$ab, useNA = "always")

# categorical var Watch ABU
table(fever_nan$WATCH, useNA = "always")
fever_nan$watch[fever_nan$WATCH==-2] <- "yes"
fever_nan$watch[fever_nan$WATCH!=-2] <- "no"
fever_nan$watch <- as.factor(fever_nan$watch)
table(fever_nan$watch, useNA = "always")

# categorical variable correct case mgmt (binary)

# numerical variable correct case mgmt 
fever_nan <- fever_nan %>%  rowwise() %>%  mutate(score = sum(c_across(column_names <- variables <- c("DurationFever?", "AbdominalComplaints?", "RespiratoryComplaints?", 
                                                                                                      "EarPain?", "ThroatPain?", "UrinaryPain?", "SignsMeningitis?", 
                                                                                                      "SkinRash?", "Lethargic?", "Comorbidities?", "FoodDrinks?", "VitalSigns", 
                                                                                                      "ThroatInspection", "StiffNeck", "AbdominalExamination", "BloodPressure", 
                                                                                                      "Auscultation", "InformationImprovement!", "AdviseComeBackWorse!", "FollowUp", 
                                                                                                      "Abgiven/Prescription", "WATCH", "IVTreatment", "Antimalaria")), 
                                                                      na.rm = TRUE))
# adapt dataframe to one row per provider
fever_nan_wide <- fever_nan %>%  
  select(provider, providertype, round, score, intervention) %>%
  pivot_wider(names_from = round, values_from = score, names_prefix = "score_")


#### 1.4 Nanoro UTI ####
# baseline & post intervention, then append both
uti_nan_bl <- read_excel("mysterypatients/Infection_urinaire_aiguë_-_all_versions_-_labels_-_2023-12-16-09-01-00ENG.xlsx")
uti_nan_post<-read_excel("mysterypatients/Infection_urinaire_aiguë_2ENG.xlsx")
uti_nan_bl$round <- "pre"
uti_nan_post$round <- "post"
uti_nan <- rbind(uti_nan_bl, uti_nan_post)
uti_nan$round <- factor(uti_nan$round, levels = c("pre", "post"))

# indicate intervention and control villages
table(uti_nan$Village, useNA = "always")
intervention_villages <- c("BALOGHO", "BOLOGHO", "KOURIA", "PELLA", "DACISSE", "KOKOLO", "NANORO", "NAZOANGA", "POESSI", "SOUM", "ZIMIDIN")
uti_nan <- uti_nan %>%  mutate(intervention = ifelse(Village %in% intervention_villages, "intervention", "control"))
uti_nan$intervention <- as.factor(uti_nan$intervention)
table(uti_nan$Village, uti_nan$intervention, useNA = "always")

# one observation which seem to have mistakes in the Village - I now picked one at random - to double check on paper form!
uti_nan$Village[uti_nan$Duration==30 & uti_nan$Village=="BOLOGHO" & uti_nan$Type=="Vendeur informel 1" & uti_nan$round=="post"] <- "BALOGHO"

# create groups
uti_nan$group <- with(uti_nan, interaction(round, intervention))
table(uti_nan$group)

# variable for each individual provider
uti_nan$provider <- paste(uti_nan$Village,uti_nan$Type)
table(uti_nan$provider)

# variable for providertype
uti_nan$providertype[grepl("CSPS", uti_nan$Type)==T] <- "health centre"
uti_nan$providertype[grepl("informel", uti_nan$Type)==T] <- "informal vendor"
uti_nan$providertype[grepl("Dépot", uti_nan$Type)==T] <- "community pharmacy/store"
uti_nan$providertype[grepl("Pharmacie", uti_nan$Type)==T] <- "community pharmacy/store"
uti_nan$providertype <- as.factor(uti_nan$providertype)
uti_nan$providertype <- relevel(uti_nan$providertype, ref = "informal vendor")
table(uti_nan$Type, uti_nan$providertype)

# categorical var AB use
table(uti_nan$NoAB, useNA = "always") # see this is the inverse of previous clinical presentations
uti_nan$ab <- "no"
uti_nan$ab[uti_nan$NoAB!=1] <- "yes"
uti_nan$ab <- as.factor(uti_nan$ab)
table(uti_nan$ab, useNA = "always")

# categorical var Watch ABU
table(uti_nan$WATCH, useNA = "always")
uti_nan$watch[uti_nan$WATCH<=-2] <- "yes"
uti_nan$watch[uti_nan$WATCH>-2] <- "no"
uti_nan$watch <- as.factor(uti_nan$watch)
table(uti_nan$watch, useNA = "always")

# categorical variable correct case mgmt (binary)

# numerical variable correct case mgmt 
uti_nan <- uti_nan %>%  rowwise() %>%  mutate(score = sum(c_across(column_names <- variables <- var <- c("DurationDysuria?", "FrequencyUrination?", "Haematuria?", "Fever?", "StartFever?", 
                                                                                                         "OtherSymptoms?", "HistoryUTI?", "Comorbidities?", "Thirsty?", "UnprotectedSex?", 
                                                                                                         "Urinalysis?", "TemperatureTaken", "AbdominalPain", 
                                                                                                         "Referral", "AdviseReturnWorsens!", "Instructions!",
                                                                                                         "WATCH", "CorrectAB", "NoAB", "IVtreatment")), 
                                                              na.rm = TRUE))
# adapt dataframe to one row per provider
uti_nan_wide <- uti_nan %>%  
  select(provider, providertype, round, score, intervention) %>%
  pivot_wider(names_from = round, values_from = score, names_prefix = "score_")


#### 1.5 Nanoro Rhinopharyngitis ####
# baseline & post intervention, then append both
rhino_nan_bl <-  read_excel("mysterypatients/Rhinopharyngite_aiguë_chez_un_patient_adulte_-_all_versions_-_labels_-_2023-12-16-09-00-26ENG.xlsx")
rhino_nan_post<-read_excel("mysterypatients/Rhinopharyngite_aiguë_2.xlsx")
rhino_nan_bl$round <- "pre"
rhino_nan_post$round <- "post"
rhino_nan <- rbind(rhino_nan_bl, rhino_nan_post)
rhino_nan$round <- factor(rhino_nan$round, levels = c("pre", "post"))

# indicate intervention and control villages
table(rhino_nan$Village, useNA = "always")
intervention_villages <- c("BALOGHO", "BOLOGHO", "KOURIA", "PELLA", "DACISSE", "KOKOLO", "NANORO", "NAZOANGA", "POESSI", "SOUM", "ZIMIDIN")
rhino_nan <- rhino_nan %>%  mutate(intervention = ifelse(Village %in% intervention_villages, "intervention", "control"))
rhino_nan$intervention <- as.factor(rhino_nan$intervention)
table(rhino_nan$Village, rhino_nan$intervention, useNA = "always")

# one observation which seem to have mistakes in the Village - I now picked one at random - to double check on paper form!
# rhino_nan$Village[rhino_nan$Duration==30 & rhino_nan$Village=="BOLOGHO" & rhino_nan$Type=="Vendeur informel 1" & rhino_nan$round=="post"] <- "BALOGHO"

# create groups
rhino_nan$group <- with(rhino_nan, interaction(round, intervention))
table(rhino_nan$group)

# variable for each individual provider
rhino_nan$provider <- paste(rhino_nan$Village,rhino_nan$Type)
table(rhino_nan$provider)

# variable for providertype
rhino_nan$providertype[grepl("CSPS", rhino_nan$Type)==T] <- "health centre"
rhino_nan$providertype[grepl("informel", rhino_nan$Type)==T] <- "informal vendor"
rhino_nan$providertype[grepl("Dépot", rhino_nan$Type)==T] <- "community pharmacy/store"
rhino_nan$providertype[grepl("Pharmacie", rhino_nan$Type)==T] <- "community pharmacy/store"
rhino_nan$providertype <- as.factor(rhino_nan$providertype)
rhino_nan$providertype <- relevel(rhino_nan$providertype, ref = "informal vendor")
table(rhino_nan$Type, rhino_nan$providertype)

# categorical var AB use
table(rhino_nan$`Abgiven/prescribed`, useNA = "always") # see this is the inverse of previous clinical presentations
rhino_nan$ab <- "no"
rhino_nan$ab[rhino_nan$`Abgiven/prescribed`<0] <- "yes"
rhino_nan$ab <- as.factor(rhino_nan$ab)
table(rhino_nan$ab, useNA = "always")

# categorical var Watch ABU
table(rhino_nan$WATCH, useNA = "always")
rhino_nan$watch[rhino_nan$WATCH<=-2] <- "yes"
rhino_nan$watch[rhino_nan$WATCH>-2] <- "no"
rhino_nan$watch <- as.factor(rhino_nan$watch)
table(rhino_nan$watch, useNA = "always")

# categorical variable correct case mgmt (binary)

# numerical variable correct case mgmt 
rhino_nan <- rhino_nan %>%  rowwise() %>%  mutate(score = sum(c_across(column_names <- variables <- var <- vector <- c("DurationCough?", "TypeCough?", "DurationFever?", "TypeSputum?", 
                                                                                                                       "DifficultyBreathing?", "SoreThroat?", "EarPain?", 
                                                                                                                       "HistoryRespDisease?", "DiagnosisMalaria", "InspectionThroat", 
                                                                                                                       "TempTaken", "PalpationLymphNodes", "HeartRate", "RespiratoryRate", 
                                                                                                                       "Auscultation", "InfoNoNeedAB", "Abgiven/prescribed", "WATCH", 
                                                                                                                       "Antimalaria?", "Ivtreatment")),na.rm = TRUE))
# adapt dataframe to one row per provider
rhino_nan_wide <- rhino_nan %>%  
  select(provider, providertype, round, score, intervention) %>%
  pivot_wider(names_from = round, values_from = score, names_prefix = "score_")

#### 1.6 Nanoro All clinical presentations combined ####
# the long df
diarrhoea_nan$clinpres <- "diarrhoea"
diarrhoea_nan_select <- diarrhoea_nan %>% select(intervention, round, group, ab, watch, provider, providertype, score, clinpres)
pneumonia_nan$clinpres <- "severe pneumonia"
pneumonia_nan_select <- pneumonia_nan %>% select(intervention, round, group, ab, provider, providertype, score, clinpres)
pneumonia_nan_select$watch <- NA
fever_nan$clinpres <- "fever"
fever_nan_select <- fever_nan %>% select(intervention, round, group, ab, watch, provider, providertype, score, clinpres)
uti_nan$clinpres <- "uti"
uti_nan_select <- uti_nan %>% select(intervention, round, group, ab, watch, provider, providertype, score, clinpres)
rhino_nan$clinpres <- "rhinitis/pharyngitis"
rhino_nan_select <- rhino_nan %>% select(intervention, round, group, ab, watch, provider, providertype, score, clinpres)
all_nan <- rbind(diarrhoea_nan_select, pneumonia_nan_select, fever_nan_select, uti_nan_select, rhino_nan_select)

# the wide df
diarrhoea_nan_wide$clinpres <- "diarrhoea"
pneumonia_nan_wide$clinpres <- "severe pneumonia"
fever_nan_wide$clinpres <- "fever"
uti_nan_wide$clinpres <- "uti"
rhino_nan_wide$clinpres <- "rhinitis/pharyngitis"
all_wide_nan <- rbind(diarrhoea_nan_wide, pneumonia_nan_wide, fever_nan_wide, uti_nan_wide, rhino_nan_wide)

# set CSPS as reference type of provider
all_wide_nan$providertype <- relevel(all_wide_nan$providertype, ref = "health centre")

#### 1.7 Kimpese All ####
all_kim_bl <- read_excel("mysterypatients/CABU_enq_Pmystere1_2023_-_all_versions_-_labels_-_2024-11-11-16-00-34.xlsx")
all_kim_post <- read_excel("mysterypatients/R2Pmystere_-_all_versions_-_labels_-_2024-09-21-10-31-22.xlsx")
all_kim_bl$round <- "pre"
all_kim_post$round <- "post"
# append baseline and post intervention data
colnames(all_kim_bl) <- colnames(all_kim_post)
all_kim <- rbind(all_kim_bl, all_kim_post)
all_kim$round <- factor(all_kim$round, levels = c("pre", "post"))

# keep a simple database with only the total score and variables, to merge with the Nanoro data
# "intervention","round"        "group"        "ab"           "watch"        "provider","providertype" "score"        "clinpres"

colnames(all_kim)

# rename clinical presentations
all_kim$clinpres[all_kim$`Choisir :`=="Gastro-entérite aiguë"] <- "diarrhoea"
all_kim$clinpres[all_kim$`Choisir :`=="Pneumonie aiguë grave chez un patient âgé"] <- "severe pneumonia"
all_kim$clinpres[all_kim$`Choisir :`=="Fièvre"] <- "fever"
all_kim$clinpres[all_kim$`Choisir :`=="Infection urinaire aiguë"] <- "uti"
all_kim$clinpres[all_kim$`Choisir :`=="Rhinopharyngite aiguë chez un patient adulte"] <- "rhinitis/pharyngitis"

# recode providertype
all_kim$providertype[grepl("pharmacie priv", all_kim$`Type de structure ou de dispensateur`)]  <- "community pharmacy/store"
all_kim$providertype[grepl("centre de sant", all_kim$`Type de structure ou de dispensateur`)]  <- "health centre"

# delete the single observation at an informal vendor (probably a typo since none recorded)
all_kim <- all_kim %>% filter(providertype!= "autre vendeur de médicaments (pas une pharmacie officielle, vendeurs ambulants)")

table(all_kim$providertype, useNA = "always")      

# rename columns of fever cases - columns 4 to 28 to the names used in the Nan data
colnames(all_kim)[4:30] <- c(
  "DurationFever?", "AbdominalComplaints?", "RespiratoryComplaints?", "EarPain?",
  "ThroatPain?", "UrinaryPain?", "SignsMeningitis?", "SkinRash?", "Lethargic?",
  "Comorbidities?", "FoodDrinks?", "VitalSigns", "ThroatInspection", "StiffNeck",
  "AbdominalExamination", "BloodPressure", "Auscultation", "InformationImprovement!",
  "AdviseComeBackWorse!", "FollowUp", "ab_fever", "NameAB_fever", "WATCH_fever",
  "IVTreatment_fever", "Antimalaria_fever", "NameOtherthanAB", "Antimalaria(NoArtemisinine)")

# rename columns of diarrhoea cases - columns 33 to 64 to the names used in the Nan data
colnames(all_kim)[33:64] <- c(
  "TypeStool?", "BloodStool?", "MucusStool?", "FreqStool?","Fever?", "AbdominalPain?","VomitNausea?", "QuestionUrination?", #33 to 40
  "CapacityDrink?","Lethargic?","GeneralHealthCondition?", "RecentUseAB?", "OtherFamilyMembers?", "SourceWater?",  # 41 to 46
  "MealPrep?", "HandHygiene?",  "WASHNeighborhood?", "HygienePhysicalEnvironment?", "VitalSigns","AbdominalExamination", #47 to 52
  "SkinPinch", "SunkenEyes", "StayHydrated", "StayHydrated_bis","AdviseWaterFoodHygiene!", "HowActProlongedDiarrhea", "ab_diar", #53 to 59
  "NameAB_diar", "WATCH_diar",   "ORS",       "Antimalaria_diar",   "NumberIVmedication" ) #60 to 64
table(all_kim$NameAB_diar, all_kim$ab_diar)

# rename columns of UTI cases - columns 67 to 87 to the names used in the Nan data
colnames(all_kim)[67:87] <- c(
  "DurationDysuria?", "FrequencyUrination?", "Haematuria?", "Fever?",
  "StartFever?", "OtherSymptoms?", "HistoryUTI?", "Comorbidities?", "Thirsty?", "UnprotectedSex?", "Urinalysis?",
  "TemperatureTaken", "AbdominalPain", "Referral", "AdviseReturnWorsens!", "Instructions!", "NameAB_uti", "WATCH_uti",
  "CorrectAB", "NoAB", "IVtreatment_uti")

# rename columns of pneumonia cases - columns 90 to 104 to the names used in the Nan data
colnames(all_kim)[90:104] <- c(
  "TypeCough?", "DurationCough?", "CoughWorsened?", "Fever?", "Sputum?", "RR?", "DifficultBreathing?", "ConfusionPresent?", "Lethargic?", 
  "HistroyRespDisease?", "ReferralHospitalHC", "SeekAppropriateCare!", "ab_pneum", "NameAB_pneum", "AntimalariaWithoutDiagnose")
  
# rename columns of rhino cases - columns 107 to 127 to the names used in the Nan data
colnames(all_kim)[107:127] <- c(
  "DurationCough?", "TypeCough?", "DurationFever?", "TypeSputum?", "DifficultyBreathing?", "SoreThroat?",
    "EarPain?", "HistoryRespDisease?", "DiagnosisMalaria", "InspectionThroat", "TempTaken", "PalpationLymphNodes", "HeartRate", "RespiratoryRate", "Auscultation",
    "InfoNoNeedAB", "ab_rhino", "NameAB_rhino", "WATCH_rhino", "Antimalaria?", "Ivtreatment")
  
# visit duration
all_kim$visitduration <- all_kim$`Durée de la visite (consultation si CSPS, visite totale si dépôt/vendeur) en secondes...31`
all_kim$visitduration[!is.na(all_kim$A34g)] <- all_kim$`Durée de la visite (consultation si CSPS, visite totale si dépôt/vendeur) en secondes...65`[!is.na(all_kim$A34g)] 
all_kim$visitduration[!is.na(all_kim$A34i)] <- all_kim$`Durée de la visite (consultation si CSPS, visite totale si dépôt/vendeur) en secondes...88`[!is.na(all_kim$A34i)] 
all_kim$visitduration[!is.na(all_kim$A34p)] <- all_kim$`Durée de la visite (consultation si CSPS, visite totale si dépôt/vendeur) en secondes...105`[!is.na(all_kim$A34p)] 
all_kim$visitduration[!is.na(all_kim$A34r)] <- all_kim$`Durée de la visite (consultation si CSPS, visite totale si dépôt/vendeur) en secondes...128`[!is.na(all_kim$A34r)] 

# check and clean antibiotics
# create a single var with antibiotic given, from which then to say if a watch AB was given
all_kim$abspec <- tolower(all_kim$NameAB_fever)
all_kim$abspec[is.na(all_kim$abspec)] <- tolower(all_kim$NameAB_diar[is.na(all_kim$abspec)])
all_kim$abspec[is.na(all_kim$abspec)] <- tolower(all_kim$NameAB_uti[is.na(all_kim$abspec)])
all_kim$abspec[is.na(all_kim$abspec)] <- tolower(all_kim$NameAB_pneum[is.na(all_kim$abspec)])
all_kim$abspec[is.na(all_kim$abspec)] <- tolower(all_kim$NameAB_rhino[is.na(all_kim$abspec)])
table(all_kim$abspec, useNA = "always")
# check if any antibiotics were missed
table(all_kim$NameOtherthanAB) # none here

# a variable of whether an antibiotic was given
table(all_kim$ab_fever, useNA = "always")
all_kim$ab <- ifelse(!is.na(all_kim$abspec), 1, 0)
# multiple antibiotics given
all_kim$ab[all_kim$abspec=="tétracycline cellule : 4 gel/jrs, metronidazole comprimé : 2 comp/jr pdt 5 jrs"] <- 2
all_kim$ab[all_kim$abspec=="sulfatrim forte comprimé 480mg: 4 comp par jr pdt 7 jrs per os, amoxycilline gel 500mg: 4 gel par jr pdt 7 jrs per os"] <- 2
all_kim$ab[all_kim$abspec=="ouicef (ceftriaxone + tazobactam) vial : 2 vial par jr pdt 5 jrs ivdl"] <- 2 # two watch antibiotics that are not in a recommended combination - not sure if it should be considered separately since it's a fixed dose comb in DRC
all_kim$ab[all_kim$abspec=="ofloxacin comprimé : 2 comp par jr pdt 6 jr, metronidazole comprimé : 2 comp par jr pdt 5 jrs per os"] <- 2 # one access and a watch AB
all_kim$ab[all_kim$abspec=="metronidazole comprimé 250mg: 4 comp par jr pdt 5 jrs per os, tétracycline gélule 4 gel par jr pdt 5 jrs pdt"] <- 2
all_kim$ab[grepl("metron", all_kim$abspec)==T&grepl("tétra", all_kim$abspec)==T] <- 2
all_kim$ab[all_kim$abspec=="furadantine comprimé, 2 comp par jr pdt 5 jrs per, triokit comprimé"] <- 2 # one access and one watch
all_kim$ab[all_kim$abspec=="furadantine comprimé 100mg: 4 comp par jr pdt 7 jrs per os, triokit comprimé"] <- 2
all_kim$ab[all_kim$abspec=="furadantine comprimé 100mg: 4 comp par jr pdt 5 jrs per os, doxycicline gel 100mg: 2 gel par jr pdt 5 jrs per os"] <- 2
all_kim$ab[all_kim$abspec=="furadantine comprimé : 2 comp par jr pdt 5 jrs per os, cefixime comprimé 200mg: 2 comp par jr pdt 5 jrs per os"] <- 2 # one access, one watch
all_kim$ab[all_kim$abspec=="metronidazole comprimé 250mg: 4 comp par jr pdt 5 jrs per os, tétracycline gélule 4 gel par jr pdt 5 jrs pdt"] <- 2
all_kim$ab[all_kim$abspec=="tétracycline cellule : 4 gel/jrs, metronidazole comprimé : 2 comp/jr pdt 5 jrs"] <- 1 # ont that was wrongly coded (after checking one by one)
# one combination has amoxi/sulbactam as antibiotic femaclin ovule vaginal : 1 ovule par jr pdt 3jr, moxbactam comp 500mg: 2 comp par jr pdt 7 jrs per os

table(all_kim$ab_fever, all_kim$ab, useNA = "always") # correctly assigned
table(all_kim$ab_diar, all_kim$ab, useNA = "always") # five are assigned differently, of which one with an antibiotic specified but ab_diar was 0, and four with no antibiotic specified
table(all_kim$abspec[all_kim$ab==1&all_kim$ab_diar==0])
table(all_kim$ab_pneum, all_kim$ab, useNA = "always") #
table(all_kim$abspec[all_kim$ab==1&all_kim$ab_pneum==0])
table(all_kim$ab_rhino, all_kim$ab, useNA = "always") 


# all_kim$ab[is.na(all_kim$ab)] <- factor(ifelse(all_kim$ab_diar[is.na(all_kim$ab)] == 1, "yes", "no"),
#                      levels = c("no", "yes"))
# all_kim$ab[is.na(all_kim$ab)] <- factor(ifelse(all_kim$ab_pneum[is.na(all_kim$ab)] == 1, "yes", "no"),
#                                         levels = c("no", "yes"))
# all_kim$ab[is.na(all_kim$ab)] <- factor(ifelse(all_kim$ab_rhino[is.na(all_kim$ab)] == 1, "yes", "no"),
#                                         levels = c("no", "yes"))
# all_kim$ab[is.na(all_kim$ab)] <- factor(ifelse(!is.na(all_kim$NameAB_uti[is.na(all_kim$ab)])== T, "yes", "no"),
#                                             levels = c("no", "yes"))
table(all_kim$ab, useNA = "always")

# watch
table(all_kim$WATCH_fever) # none entered but there are watch antibiotics when entering which antibiotic was given
# identify all watch antibiotics
watchspec <- all_kim$abspec[all_kim$clinpres!="severe pneumonia"]
watch_strings <- c("azithro", "zithro", "triokit", "azy", "clarythro", "clarithro", # triokit contains azithro
                    "cefixime", "ceftriaxone", "cipro", "cifin", "norflox", # sudrox is cefadroxil (1st gen ceph, access)
                    "oflox", "aurabact", "levoflox") #aurabact is brandname of levofloxacin
# create new var watch
all_kim$watchnum <- 0
all_kim$watchnum[grepl(paste(watch_strings, collapse="|"), all_kim$abspec, ignore.case = TRUE)==T] <- 2
all_kim$watchnum[grepl("ouicef", all_kim$abspec)==T] <- 4 # ceftriaxone + tazobactam
table(all_kim$watchnum)
table(all_kim$abspec[all_kim$watchnum>1])

# error in value
all_kim$Ivtreatment[all_kim$Ivtreatment==1200] <- 0 # typo, no treatment specified, so likely none

# total scores
all_kim$totalscore <- as.numeric(all_kim$A34f)
all_kim$totalscore[!is.na(all_kim$A34g)] <- as.numeric(all_kim$A34g[!is.na(all_kim$A34g)]) 
all_kim$totalscore[!is.na(all_kim$A34i)] <- as.numeric(all_kim$A34i[!is.na(all_kim$A34i)]) 
all_kim$totalscore[!is.na(all_kim$A34p)] <- as.numeric(all_kim$A34p[!is.na(all_kim$A34p)]) 
all_kim$totalscore[!is.na(all_kim$A34r)] <- as.numeric(all_kim$A34r[!is.na(all_kim$A34r)]) 
table(all_kim$clinpres, all_kim$totalscore, useNA = "always")
all_kim$totalscore[all_kim$`HistroyRespDisease?`==4] <- 1

# recalculate scores
# check individual values
summary(all_kim)
table(all_kim$IVTreatment_fever)
# correct if wrong values
all_kim$`Lethargic?`[all_kim$`Lethargic?`>1] <- 1
# if missing values in numeric variables, replace with 0s to allow calculating a score
num_cols <- which(sapply(all_kim[ , 4:125], is.numeric)) + 3  # shift index because we sliced 4:122
all_kim[ , num_cols][is.na(all_kim[ , num_cols])] <- 0

# compile score
all_kim$totalscore_checked <- ifelse(all_kim$clinpres == "fever",
  rowSums(all_kim[, 4:23], na.rm = TRUE) - all_kim$ab - all_kim$watchnum - all_kim$Antimalaria_fever - 2*all_kim$`Antimalaria(NoArtemisinine)`, NA)
table(all_kim$totalscore_checked[all_kim$clinpres == "fever"], all_kim$totalscore[all_kim$clinpres == "fever"])

all_kim$totalscore_checked <- ifelse(all_kim$clinpres == "diarrhoea",
  rowSums(all_kim[, 33:58], na.rm = TRUE) + all_kim$`TypeStool?` + all_kim$`BloodStool?` + all_kim$`FreqStool?` + all_kim$`Fever?` +
  all_kim$StayHydrated - 2*all_kim$ab - all_kim$watchnum + all_kim$ORS - all_kim$Antimalaria_diar - all_kim$NumberIVmedication, all_kim$totalscore_checked)
table(all_kim$totalscore_checked[all_kim$clinpres == "diarrhoea"], all_kim$totalscore[all_kim$clinpres == "diarrhoea"])

all_kim$totalscore_checked <- ifelse(all_kim$clinpres == "uti",
                                     rowSums(all_kim[, 67:82], na.rm = TRUE) + all_kim$`DurationDysuria?` - all_kim$watchnum +
                                       all_kim$CorrectAB + all_kim$NoAB - all_kim$IVtreatment_uti, all_kim$totalscore_checked)
table(all_kim$totalscore_checked[all_kim$clinpres == "uti"], all_kim$totalscore[all_kim$clinpres == "uti"])

all_kim$totalscore_checked <- ifelse(all_kim$clinpres == "severe pneumonia",
                                     rowSums(all_kim[, 90:99], na.rm = TRUE) + all_kim$ReferralHospitalHC * 4 + all_kim$`SeekAppropriateCare!` +
                                       - all_kim$ab_pneum - all_kim$AntimalariaWithoutDiagnose, all_kim$totalscore_checked)
table(all_kim$totalscore_checked[all_kim$clinpres == "severe pneumonia"], all_kim$totalscore[all_kim$clinpres == "severe pneumonia"])

all_kim$totalscore_checked <- ifelse(all_kim$clinpres == "rhinitis/pharyngitis",
                                     rowSums(all_kim[, 107:122], na.rm = TRUE) - all_kim$ab - all_kim$watchnum - all_kim$`Antimalaria?` -
                                       all_kim$Ivtreatment, all_kim$totalscore_checked)
table(all_kim$totalscore_checked[all_kim$clinpres == "rhinitis/pharyngitis"], all_kim$totalscore[all_kim$clinpres == "rhinitis/pharyngitis"])
# differences between entered score and rechecked score mostly due to no assigning the antibiotic used as a Watch antibiotic when it should have been
table(all_kim$clinpres[is.na(all_kim$totalscore_checked)], useNA = "always")
# still one with a missing score

# alternative score, not considering antibiotic use
# compile score
all_kim$score2 <- ifelse(all_kim$clinpres == "fever",
                                     rowSums(all_kim[, 4:23], na.rm = TRUE) - all_kim$Antimalaria_fever - 2*all_kim$`Antimalaria(NoArtemisinine)`, NA)

all_kim$score2 <- ifelse(all_kim$clinpres == "diarrhoea",
                                     rowSums(all_kim[, 33:58], na.rm = TRUE) + all_kim$`TypeStool?` + all_kim$`BloodStool?` + all_kim$`FreqStool?` + all_kim$`Fever?` +
                                       all_kim$StayHydrated + all_kim$ORS - all_kim$Antimalaria_diar - all_kim$NumberIVmedication, all_kim$score2)

all_kim$score2 <- ifelse(all_kim$clinpres == "uti",
                                     rowSums(all_kim[, 67:82], na.rm = TRUE) + all_kim$`DurationDysuria?` +
                                       all_kim$CorrectAB + all_kim$NoAB - all_kim$IVtreatment_uti, all_kim$score2)

all_kim$score2 <- ifelse(all_kim$clinpres == "severe pneumonia",
                                     rowSums(all_kim[, 90:99], na.rm = TRUE) + all_kim$ReferralHospitalHC * 4 + all_kim$`SeekAppropriateCare!` +
                                       - all_kim$AntimalariaWithoutDiagnose, all_kim$score2)

all_kim$score2 <- ifelse(all_kim$clinpres == "rhinitis/pharyngitis",
                                     rowSums(all_kim[, 107:122], na.rm = TRUE) - all_kim$`Antimalaria?` -
                                       all_kim$Ivtreatment, all_kim$score2)


# intervention
all_kim$intervention <- "control"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "CELLULE MBUKA3"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "KIASUNGUA"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "KILUEKA"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "MBANZA NDAMBA"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "LUKENGEZI ET POSTE"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "Mont Fleury A et B"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "MPETE NKONDO"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "Q3(AS Kimbanguiste)"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "SANZIKUA"] <- "intervention"
all_kim$intervention[all_kim$`cluster (village ou quartier où se situe le ménage)` == "VUNDA NSOLE"] <- "intervention"
table(all_kim$`cluster (village ou quartier où se situe le ménage)`, all_kim$intervention)

# new variable grouping pre/post and intervention/control
all_kim$group <- with(all_kim, interaction(round, intervention))

# a wide dataframe with pre and post scores for the same provider
# create a var with ID per provider
all_kim$provider <- paste(all_kim$`cluster (village ou quartier où se situe le ménage)`, all_kim$providertype)
table(all_kim$provider)
assign <- all_kim %>% select(round, clinpres, provider, score) # impossible to link pre and post values for the same provider

#### 1.8 Merge Nan and Kim data ####
all_nan$abu <- all_nan$ab
all_kim$abu <- factor(ifelse(all_kim$ab > 0, "yes", "no"),
                       levels = c("no", "yes"))
all_kim$watch <- factor(ifelse(all_kim$watchnum > 0, "yes", "no"),
                        levels = c("no", "yes"))
all_kim$score <- all_kim$totalscore_checked
all_kim$intervention <- factor(all_kim$intervention)
# keep only variables essential for the combined analysis
all_kim_sub <- all_kim %>% select(providertype, clinpres, intervention, round, group, abu, watch, score)
all_nan_sub <- all_nan %>% select(providertype, clinpres, intervention, round, group, abu, watch, score)
str(as.data.frame(all_kim_sub))
str(as.data.frame(all_nan_sub))
# merge the two shortened databases
all_kim_sub$site <- "Kimpese"
all_nan_sub$site <- "Nanoro"
all <- rbind(all_kim_sub, all_nan_sub)

#### 2. ANALYSIS #### 
#### 2.1 Nanoro ####
# DIARRHOEA
# percentages antibiotic use in the INTERVENTION group post vs pre intervention
table(diarrhoea_nan$ab[diarrhoea_nan$intervention=="intervention"], diarrhoea_nan$round[diarrhoea_nan$intervention=="intervention"])

# log regression for change in antibiotic use
logregmodel <- glm(ab~round + intervention + round*intervention, family=binomial, data= diarrhoea_nan)
summary(logregmodel)
exp(coef(logregmodel))
exp(confint(logregmodel))
summary(logregmodel)

# percentages WATCH antibiotic use in the intervention group post vs pre intervention
table(diarrhoea_nan$watch[diarrhoea_nan$intervention=="intervention"], diarrhoea_nan$round[diarrhoea_nan$intervention=="intervention"])
prop.table(table(diarrhoea_nan$watch[diarrhoea_nan$intervention=="intervention"], diarrhoea_nan$round[diarrhoea_nan$intervention=="intervention"]),2)*100

# log regression for change in WATCH antibiotic use
logregmodel <- glm(watch~round + intervention + round*intervention, family=binomial, data= diarrhoea_nan)
summary(logregmodel) 
exp(coef(logregmodel))
exp(confint(logregmodel))

# patient mgmt score
table(diarrhoea_nan$score, useNA = "always")
mean(diarrhoea_nan$score)
max(diarrhoea_nan$score)
min(diarrhoea_nan$score)

# summarize score by group - comparing 4 groups (intervention - control and pre - post)
diarrhoea_nan %>%  group_by(group) %>% summarise(
    count = n(),
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE),
    IQR_score = IQR(score, na.rm = TRUE)
  )

ggplot(diarrhoea_nan, aes(x = group, y = score)) +
  geom_boxplot() +
  labs(title = "Distribution of Score by Group",
       x = "Group",
       y = "Score")

ggplot(diarrhoea_nan, aes(x = score, fill = round)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Score by Group",
       x = "Score",
       y = "Density") +
  facet_wrap(~ intervention)
  
# kruskall wallis test - probably overruled by lin regression. number of providers very small, so probably a simple stat test doesn't tell a lot
kruskal_test_result <- kruskal.test(score ~ intervention, data = diarrhoea_nan)
kruskal_test_result

# linear regression, in which the intervention effect should be the interaction term (round (pst vs. pre) * intervention (intervention vs. control))
linregmodel <- lm(score ~ round + intervention + round*intervention, data= diarrhoea_nan)
summary(linregmodel) 

# summarize score comparing intervention vs. control while adding pre/post scores as a co-variable
# linear regression with baseline scores as covariable
linregmodel_prescore_as_covariable <- lm(score_post ~ score_pre + intervention, data= diarrhoea_nan_wide)
summary(linregmodel_prescore_as_covariable) 

# investigate if interaction by type of provider
linregmodel_prescore_as_covariable_interactiontype <- lm(score_post ~ score_pre + intervention + providertype*intervention, data= diarrhoea_nan_wide)
model_summary <- summary(linregmodel_prescore_as_covariable_interactiontype) 

# without considering baseline
linregmodel_nobaseline_interactiontype <- lm(score_post ~ intervention + providertype*intervention, data= diarrhoea_nan_wide)
summary(linregmodel_nobaseline_interactiontype) 

# calculate the providertype-specific change in score with 95% confidence intervals 
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results

# effect informal vendors (reference group, so no need to add the interaction term)
effect_informal <- linregression_results["interventionintervention", "Coefficients"] 
se_informal <- sqrt(linregression_results["interventionintervention", "StandardErrors"]^2)
ci_lower <- effect_informal - qnorm(1 - 0.05/ 2) * se_informal # 1.959964
ci_upper <- effect_informal + qnorm(1 - 0.05/ 2) * se_informal # 1.959964
ci_informal <- c(ci_lower, ci_upper)
effect_informal
ci_informal

# effect CSPS
effect_csps <- linregression_results["interventionintervention", "Coefficients"] + linregression_results["interventionintervention:providertypeCSPS", "Coefficients"]
se_csps <- sqrt(linregression_results["interventionintervention", "StandardErrors"]^2 + linregression_results["interventionintervention:providertypeCSPS", "StandardErrors"]^2)
ci_lower <- effect_csps - qnorm(1 - 0.05/ 2) * se_csps # 1.959964
ci_upper <- effect_csps + qnorm(1 - 0.05/ 2) * se_csps # 1.959964
ci_csps <- c(ci_lower, ci_upper)
effect_csps
ci_csps

# effect pharmacy
effect_pharma <- linregression_results["interventionintervention", "Coefficients"] + linregression_results["interventionintervention:providertypepharmacy", "Coefficients"]
se_pharma <- sqrt(linregression_results["interventionintervention", "StandardErrors"]^2 + linregression_results["interventionintervention:providertypepharmacy", "StandardErrors"]^2)
ci_lower <- effect_pharma - qnorm(1 - 0.05/ 2) * se_pharma # 1.959964
ci_upper <- effect_pharma + qnorm(1 - 0.05/ 2) * se_pharma # 1.959964
ci_pharma <- c(ci_lower, ci_upper)
effect_pharma
ci_pharma

# PNEUMONIA
# percentages antibiotic use in the intervention group post vs pre intervention
table(pneumonia_nan$ab[pneumonia_nan$intervention=="intervention"], pneumonia_nan$round[pneumonia_nan$intervention=="intervention"])

# log regression for change in antibiotic use
logregmodel_pneu <- glm(ab~round + intervention + round*intervention, family=binomial, data= pneumonia_nan)
summary(logregmodel_pneu)
exp(coef(logregmodel_pneu))
exp(confint(logregmodel_pneu))
summary(logregmodel_pneu)

# FEVER
# percentages antibiotic use in the intervention group post vs pre intervention
# percentages antibiotic use in the intervention group post vs pre intervention
table(fever_nan$ab[fever_nan$intervention=="intervention"], fever_nan$round[fever_nan$intervention=="intervention"])

# log regression for change in antibiotic use
logregmodel_fever <- glm(ab~round + intervention + round*intervention, family=binomial, data=fever_nan)
summary(logregmodel_fever)
exp(coef(logregmodel_fever))
exp(confint(logregmodel_fever))
summary(logregmodel_fever)

# percentages WATCH antibiotic use in the intervention group post vs pre intervention
table(fever_nan$watch[fever_nan$intervention=="intervention"], fever_nan$round[fever_nan$intervention=="intervention"])
prop.table(table(fever_nan$watch[fever_nan$intervention=="intervention"], fever_nan$round[fever_nan$intervention=="intervention"]),2)*100

# log regression for change in WATCH antibiotic use
logregmodel_fever_watch <- glm(watch~round + intervention + round*intervention, family=binomial, data= fever_nan)
summary(logregmodel_fever_watch) 
exp(coef(logregmodel_fever_watch))
exp(confint(logregmodel_fever_watch))

# UTI
# percentages WATCH antibiotic use in the intervention group post vs pre intervention
table(uti_nan$watch[uti_nan$intervention=="intervention"], uti_nan$round[uti_nan$intervention=="intervention"])
prop.table(table(uti_nan$watch[uti_nan$intervention=="intervention"], uti_nan$round[uti_nan$intervention=="intervention"]),2)*100

# log regression for change in WATCH antibiotic use
logregmodel_uti_watch <- glm(watch~round + intervention + round*intervention, family=binomial, data= uti_nan)
summary(logregmodel_uti_watch) 
exp(coef(logregmodel_uti_watch))
exp(confint(logregmodel_uti_watch))

# RHINO
# percentages antibiotic use in the intervention group post vs pre intervention
table(rhino_nan$ab[rhino_nan$intervention=="intervention"], rhino_nan$round[rhino_nan$intervention=="intervention"])

# log regression for change in antibiotic use
logregmodel_rhino <- glm(ab~round + intervention + round*intervention, family=binomial, data= rhino_nan)
summary(logregmodel_rhino)
exp(coef(logregmodel_rhino))
exp(confint(logregmodel_rhino))
summary(logregmodel_rhino)

# percentages WATCH antibiotic use in the intervention group post vs pre intervention
table(rhino_nan$watch[rhino_nan$intervention=="intervention"], rhino_nan$round[rhino_nan$intervention=="intervention"])
prop.table(table(rhino_nan$watch[rhino_nan$intervention=="intervention"], rhino_nan$round[rhino_nan$intervention=="intervention"]),2)*100

# log regression for change in WATCH antibiotic use
logregmodel_rhino_watch <- glm(watch~round + intervention + round*intervention, family=binomial, data= rhino_nan)
summary(logregmodel_rhino_watch) 
exp(coef(logregmodel_rhino_watch))
exp(confint(logregmodel_rhino_watch))

# ALL COMBINED
# descriptive: summarize score by group - comparing 4 groups (intervention - control and pre - post)
all_nan %>%  group_by(group, clinpres) %>% summarise(
  count = n(),
  mean_score = mean(score, na.rm = TRUE),
  sd_score = sd(score, na.rm = TRUE),
  median_score = median(score, na.rm = TRUE),
  IQR_score = IQR(score, na.rm = TRUE)
)

means <- all_nan %>%
  group_by(clinpres, intervention, round) %>%
  summarize(mean_score = mean(score), .groups = 'drop')

ggplot(all_nan, aes(x = score, fill = round)) +
  geom_density(alpha = 0.5) +
  labs(x = "Score",
       y = "Density") +
  facet_wrap(clinpres ~ intervention, nrow = 5, ncol = 2, scales = "free") +
  geom_vline(data = means, aes(xintercept = mean_score, color = round), 
             linewidth = 0.7) +
  scale_color_manual(values = c("pre" = "#F8766D", "post" = "#5f9ea0")) 
ggsave("density_scores_by_clinpres.jpeg", plot = last_plot(), dpi = 300, width = 7, height = 10)

# overall effect: quantify intervention effect on patient management scores, ALL providers taken together
linregmodel_all_together <- lm(score_post ~ intervention + score_pre + providertype + clinpres, data= all_wide_nan) # interaction between intervention and clinical presentation is significant, which makes sense 
model_summary <- summary(linregmodel_all_together) 
model_summary # both baseline scores and interaction between clin pres and intervention stat and clinically significant
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results
# effect as difference in score because of the intervention
effect_overall <- linregression_results["interventionintervention", "Coefficients"] 
se_overall <- sqrt(linregression_results["interventionintervention", "StandardErrors"]^2)
ci_lower <- effect_overall - qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_upper <- effect_overall + qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_overall <- c(ci_lower, ci_upper)
effect_overall
ci_overall


# 2.6.3 provider type-specific effect: quantify intervention effect on patient management scores BY TYPE OF PROVIDER, taking an interaction between the type of provider, and between clinical presentation and intervention and the intervention into account
linregmodel_prescore_as_covariable_interactiontype <- lm(score_post ~ score_pre + intervention + clinpres*intervention + providertype*intervention, data= all_wide_nan)
model_summary <- summary(linregmodel_prescore_as_covariable_interactiontype) 
model_summary # baseline score and interaction with clinical presentation are statistically significant 

# calculate the providertype-specific change in score with 95% confidence intervals 
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results

# effect CSPS (reference group, so no need to add the interaction term)
effect_csps <- linregression_results["interventionintervention", "Coefficients"] 
se_csps <- sqrt(linregression_results["interventionintervention", "StandardErrors"]^2)
ci_lower <- effect_csps - qnorm(1 - 0.05/ 2) * se_csps # 1.959964
ci_upper <- effect_csps + qnorm(1 - 0.05/ 2) * se_csps # 1.959964
ci_csps <- c(ci_lower, ci_upper)
effect_csps
ci_csps

# effect informal vendors
effect_informal <- linregression_results["interventionintervention", "Coefficients"] + linregression_results["interventionintervention:providertypeinformal vendor", "Coefficients"]
se_informal <- sqrt(linregression_results["interventionintervention", "StandardErrors"]^2 + linregression_results["interventionintervention:providertypeinformal vendor", "StandardErrors"]^2)
ci_lower <- effect_informal - qnorm(1 - 0.05/ 2) * se_informal # 1.959964
ci_upper <- effect_informal + qnorm(1 - 0.05/ 2) * se_informal # 1.959964
ci_informal <- c(ci_lower, ci_upper)
effect_informal
ci_informal

# effect pharmacies
effect_pharmacy <- linregression_results["interventionintervention", "Coefficients"] + linregression_results["interventionintervention:providertypepharmacy", "Coefficients"]
se_pharmacy <- sqrt(linregression_results["interventionintervention", "StandardErrors"]^2 + linregression_results["interventionintervention:providertypepharmacy", "StandardErrors"]^2)
ci_lower <- effect_pharmacy - qnorm(1 - 0.05/ 2) * se_pharmacy # 1.959964
ci_upper <- effect_pharmacy + qnorm(1 - 0.05/ 2) * se_pharmacy # 1.959964
ci_pharmacy <- c(ci_lower, ci_upper)
effect_pharmacy
ci_pharmacy

#### 2.2 Kimpese ####
# describe clinical presentations
table(all_kim$clinpres, all_kim$group, useNA = "always")
# antibiotic use prevalence
prop.table(table(all_kim$group, all_kim$ab))
# Watch antibiotic use prevalence
prop.table(table(all_kim$group, all_kim$watchnum))
# scores
table(all_kim$totalscore_checked, useNA = "always")

tapply(all_kim$totalscore_checked, all_kim$group, summary)
all_kim %>%  select(group, clinpres, totalscore_checked) %>% group_by(group, clinpres) %>% summarise(
  count = n(),
  mean_score = mean(totalscore_checked, na.rm = TRUE),
  sd_score = sd(totalscore_checked, na.rm = TRUE),
  median_score = median(totalscore_checked, na.rm = TRUE),
  IQR_score = IQR(totalscore_checked, na.rm = TRUE)
)

means <- all_kim %>% select(intervention, round, clinpres, totalscore_checked) %>% 
  filter(!is.na(totalscore_checked)) %>%
  group_by(clinpres, intervention, round) %>%
  summarize(mean_score = mean(totalscore_checked), .groups = 'drop')
means

ggplot(all_kim_short, aes(x = totalscore_checked, fill = round)) +
  geom_density(alpha = 0.5) +
  labs(title = "Kimpese", 
       x = "Score",
       y = "Density") +
  facet_wrap(clinpres ~ intervention, nrow = 5, ncol = 2, scales = "free") +
  geom_vline(data = means, aes(xintercept = mean_score, color = round), 
             linewidth = 0.7) +
  scale_color_manual(values = c("pre" = "#F8766D", "post" = "#5f9ea0")) 
ggsave("density_scores_by_clinpres_kimpese.jpeg", plot = last_plot(), dpi = 300, width = 7, height = 10)

#### 2.3 Overall ####
# n visits at baseline and post intervention
all %>%
  group_by(round) %>%
  summarise(n())
# per intervention vs control group 
all %>% 
  filter(round == "pre") %>%
  group_by(intervention) %>%
  summarise(n())
# per provider type
all %>% 
  filter(round == "pre") %>%
  filter(clinpres == "diarrhoea") %>%
  group_by(providertype) %>%
  summarise(n())

# overall score at baseline
all %>%
  filter(round=="pre") %>%
  summarize(
    count = n(),
    mean_score = mean(score, na.rm = TRUE),
    min_score = min(score, na.rm = TRUE),
    max_score = max(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE),
    IQR_score = IQR(score, na.rm = TRUE),
    se = sd_score / sqrt(count),  # standard error
    ci_lower = mean_score - qt(0.975, df = count - 1) * se,
    ci_upper = mean_score + qt(0.975, df = count - 1) * se
  )

# by healthcare provider
all %>%
  filter(round=="pre") %>%
  group_by(providertype) %>%
  summarize(
    count = n(),
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE),
    IQR_score = IQR(score, na.rm = TRUE),
    se = sd_score / sqrt(count),  # standard error
    ci_lower = mean_score - qt(0.975, df = count - 1) * se,
    ci_upper = mean_score + qt(0.975, df = count - 1) * se
  )

# by site
all %>%
  filter(round=="pre") %>%
  group_by(providertype, site) %>%
  summarize(
    count = n(),
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE),
    IQR_score = IQR(score, na.rm = TRUE),
    se = sd_score / sqrt(count),  # standard error
    ci_lower = mean_score - qt(0.975, df = count - 1) * se,
    ci_upper = mean_score + qt(0.975, df = count - 1) * se
  )

# check UTI in Nanoro (male simulated patient, while the scenario was supposed to be female)
all %>% filter(clinpres=="uti") %>% group_by(site, providertype, intervention, round) %>%
  summarize(count = n(),
            mean_score = mean(score, na.rm = TRUE),
            sd_score = sd(score, na.rm = TRUE),
            median_score = median(score, na.rm = TRUE),
            IQR_score = IQR(score, na.rm = TRUE))
# delete
all_notutinanoro <- all %>% filter(clinpres!="uti"|site!="Nanoro") 


# total score
scoresummary <- all %>%
  group_by(clinpres, site, providertype, intervention) %>%
  summarize(count = n(),
            mean_score = mean(score, na.rm = TRUE),
            sd_score = sd(score, na.rm = TRUE),
            median_score = median(score, na.rm = TRUE),
            IQR_score = IQR(score, na.rm = TRUE))
scoresummary

# total score
tapply(all$score, all$group, summary)
scoresummary <- all %>%  select(site, providertype, group, clinpres, score) %>% group_by(site, providertype, group, clinpres) %>% summarise(
  count = n(),
  mean_score = mean(score, na.rm = TRUE),
  sd_score = sd(score, na.rm = TRUE),
  median_score = median(score, na.rm = TRUE),
  IQR_score = IQR(score, na.rm = TRUE)
)

# density plot overall scores
means <- all %>% select(site, providertype, intervention, round, clinpres, score) %>% 
  filter(!is.na(score)) %>%
  group_by(site, providertype, clinpres, intervention, round) %>%
  summarize(mean_score = mean(score), .groups = 'drop')
means

ggplot(all, aes(x = score, fill = round)) +
  geom_density(alpha = 0.5) +
  labs(x = "Score",
       y = "Density") +
  facet_grid(clinpres ~ paste(intervention, site, providertype), scales = "free") + # , nrow = 5, ncol = 2
  geom_vline(data = means, aes(xintercept = mean_score, color = round), 
             linewidth = 0.7) +
  scale_color_manual(values = c("pre" = "#F8766D", "post" = "#5f9ea0")) 
ggsave("density_scores_all.jpeg", plot = last_plot(), dpi = 300, width = 7, height = 10)

# density plot overall scores - combining sites and only show pre vs post in intervention group
means_sitescombined_intervention <- all_notutinanoro %>% select(providertype, intervention, round, clinpres, score) %>% 
  filter(!is.na(score)) %>%
  filter(intervention=="intervention") %>%
  group_by(providertype, clinpres, round) %>%
  summarize(mean_score = mean(score), .groups = 'drop')
means_sitescombined_intervention

ggplot(all_notutinanoro, aes(x = score, fill = round)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  # geom_density(alpha = 0.5) +
  labs(x = "patient management score",
       y = "frequency") +
  facet_grid(clinpres ~ providertype, scales = "free_y") + # , nrow = 5, ncol = 2
  geom_vline(data = means_sitescombined_intervention, aes(xintercept = mean_score, color = round), 
             linewidth = 0.7) +
  scale_fill_manual(values = c("pre" = "#FF6666", "post" = "#800000")) +
  scale_color_manual(values = c("pre" = "#FF6666", "post" = "#800000")) + # previous red and blue colours: #F8766D #5f9ea0
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),  strip.text = element_text(size = 11),
          panel.border = element_rect(color = "darkgrey", fill = NA, linewidth = 0.5))
ggsave("density_scores_intervention_sitescombined.jpeg", plot = last_plot(), dpi = 300, width = 12, height = 8)

# overall effect: quantify intervention effect on patient management scores, ALL providers taken together
# set reference values
all$providertype <- relevel(factor(all$providertype), ref = "health centre")
all$round  <- factor(all$round, levels = c("pre", "post"))
all$intervention <- factor(all$intervention, levels = c("control", "intervention"))
# linear regression for difference in difference in score
linregmodel_all_together <- lm(score ~ intervention*round + site + providertype + clinpres, data= all) 
model_summary <- summary(linregmodel_all_together) 
model_summary # both baseline scores and interaction between clin pres and intervention stat and clinically significant
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results
# effect as difference in score because of the intervention - difference in difference is the coefficient for the interaction intervention and round
effect_overall <- linregression_results["interventionintervention:roundpost", "Coefficients"] 
se_overall <- sqrt(linregression_results["interventionintervention:roundpost", "StandardErrors"]^2)
ci_lower <- effect_overall - qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_upper <- effect_overall + qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_overall <- c(ci_lower, ci_upper)
effect_overall
ci_overall

# in HEALTH CENTRES
hc <- all %>% filter(providertype=="health centre")
linregmodel <- lm(score ~ intervention*round + site, data= hc) 
model_summary <- summary(linregmodel) 
model_summary 
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results
# effect as difference in score because of the intervention - difference in difference is the coefficient for the interaction intervention and round
effect_overall <- linregression_results["interventionintervention:roundpost", "Coefficients"] 
se_overall <- sqrt(linregression_results["interventionintervention:roundpost", "StandardErrors"]^2)
ci_lower <- effect_overall - qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_upper <- effect_overall + qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_overall <- c(ci_lower, ci_upper)
effect_overall
ci_overall

# in pharmacy/store
pharm <- all %>% filter(providertype=="community pharmacy/store")
linregmodel <- lm(score ~ intervention*round + site + clinpres, data= pharm) 
model_summary <- summary(linregmodel) 
model_summary 
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results
# effect as difference in score because of the intervention - difference in difference is the coefficient for the interaction intervention and round
effect_overall <- linregression_results["interventionintervention:roundpost", "Coefficients"] 
se_overall <- sqrt(linregression_results["interventionintervention:roundpost", "StandardErrors"]^2)
ci_lower <- effect_overall - qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_upper <- effect_overall + qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_overall <- c(ci_lower, ci_upper)
effect_overall
ci_overall

# at informal vendors
informal <- all %>% filter(providertype=="informal vendor")
linregmodel <- lm(score ~ intervention*round + clinpres, data= informal) 
model_summary <- summary(linregmodel) 
model_summary 
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results
# effect as difference in score because of the intervention - difference in difference is the coefficient for the interaction intervention and round
effect_overall <- linregression_results["interventionintervention:roundpost", "Coefficients"] 
se_overall <- sqrt(linregression_results["interventionintervention:roundpost", "StandardErrors"]^2)
ci_lower <- effect_overall - qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_upper <- effect_overall + qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_overall <- c(ci_lower, ci_upper)
effect_overall
ci_overall

# BY CLINICAL PRESENTATION
# diarrhoea
diar <- all %>% filter(clinpres=="diarrhoea")
linregmodel <- lm(score ~ intervention*round + providertype, data= diar) 
model_summary <- summary(linregmodel) 
model_summary 
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results
# effect as difference in score because of the intervention - difference in difference is the coefficient for the interaction intervention and round
effect_overall <- linregression_results["interventionintervention:roundpost", "Coefficients"] 
se_overall <- sqrt(linregression_results["interventionintervention:roundpost", "StandardErrors"]^2)
ci_lower <- effect_overall - qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_upper <- effect_overall + qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_overall <- c(ci_lower, ci_upper)
effect_overall
ci_overall

# ANTIBIOTIC USE: summarize use of Watch antibiotics by clinical presentation and by intervention - control and pre - post
# not applicable to pneumonia scenario
all_nopneumonia <- all %>% filter(clinpres!="severe pneumonia")
# counts by clin pres
counts <- table(all_nopneumonia$clinpres, all_nopneumonia$watch, useNA = "always")
counts
round(prop.table(counts, 1)*100,2)
# prevalence, all providers together
watchprev <- all_nopneumonia %>%  group_by(intervention, round, clinpres) %>% summarise(
  total = n(),
  count_yes = sum(watch == "yes"),
  prevalence = mean(watch == "yes", na.rm = TRUE),
  sd = sd(watch == "yes")) %>%
  mutate(
    ci_lower = prevalence - 1.96 * sqrt((prevalence * (1 - prevalence)) / total),
    ci_upper = prevalence + 1.96 * sqrt((prevalence * (1 - prevalence)) / total)
  )
watchprev # increase among diarrhoea cases

# plot
ggplot(watchprev, aes(x = prevalence, y = intervention, color = round)) +
  geom_point(size = 2, position = position_dodge(width = 0.4)) + 
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.3, position = position_dodge(width = 0.4)) +
  facet_wrap(~clinpres, ncol = 1, scales = "free_y") + #  separate plots for each clinpres 
  labs(x = "prevalence of Watch antibiotic use",
       color = "pre/post intervention",
       linetype = "Intervention Group") +
  theme(legend.position = "bottom")
ggsave("prevalence_use_watch_antibiotics.jpeg", plot = last_plot(), dpi = 300, width = 4, height = 6)

# prevalence, BY PROVIDER TYPE
watchprev_providertype <- all_nopneumonia %>%  group_by(intervention, round, clinpres, providertype) %>% summarise(
  total = n(),
  count_yes = sum(watch == "yes"),
  prevalence = mean(watch == "yes", na.rm = TRUE),
  sd = sd(watch == "yes")) %>%
  mutate(
    ci_lower = prevalence - 1.96 * sqrt((prevalence * (1 - prevalence)) / total),
    ci_upper = prevalence + 1.96 * sqrt((prevalence * (1 - prevalence)) / total)
  )
watchprev_providertype # increase among diarrhoea cases

# check specifically diarrhoea cases at community pharmacies & informal providers
watchprev_providertype %>% filter(intervention=="intervention" & clinpres == "diarrhoea" & providertype == "community pharmacy/store")
watchprev_providertype %>% filter(intervention=="intervention" & clinpres == "diarrhoea" & providertype == "informal vendor")

# plot
ggplot(watchprev_providertype, aes(x = prevalence, y = intervention, color = round)) +
  geom_point(size = 2, position = position_dodge(width = 0.4)) + 
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.3, position = position_dodge(width = 0.4)) +
  labs(x = "prevalence of Watch antibiotic use",
       color = "pre/post intervention",
       linetype = "Intervention Group") +
  facet_grid(clinpres ~ providertype) + 
  scale_color_manual(values = c("pre" = "#FF6666", "post" = "#800000")) + # previous red and blue colours: #F8766D #5f9ea0
  # scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
        panel.border = element_rect(color = "darkgrey", fill = NA, linewidth = 0.5),
        legend.position = "bottom")
ggsave("simulated_patients_prevalence_use_watch_antibiotics_by_providertype.jpeg", plot = last_plot(), dpi = 300, width = 9, height = 6)

# scores without antibiotic use
means_kim_no_ab_intervention <- all_kim %>% select(providertype, intervention, round, clinpres, score2) %>% 
  filter(!is.na(score2)) %>%
  filter(intervention=="intervention") %>%
  group_by(providertype, clinpres, round) %>%
  summarize(mean_score = mean(score2), .groups = 'drop')
means_kim_no_ab_intervention

# set reference values
all_kim$providertype <- relevel(factor(all_kim$providertype), ref = "health centre")
all_kim$round  <- factor(all_kim$round, levels = c("pre", "post"))
all_kim$intervention <- factor(all_kim$intervention, levels = c("control", "intervention"))
# linear regression for difference in difference in score
linregmodel_allkim <- lm(score ~ intervention*round + providertype + clinpres, data= all_kim) 
model_summary <- summary(linregmodel_allkim) 
model_summary # both baseline scores and interaction between clin pres and intervention stat and clinically significant
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
linregression_results <- data.frame(
  Coefficients = coefficients,
  StandardErrors = std_errors
)
linregression_results
# effect as difference in score because of the intervention - difference in difference is the coefficient for the interaction intervention and round
effect_overall <- linregression_results["interventionintervention:roundpost", "Coefficients"] 
se_overall <- sqrt(linregression_results["interventionintervention:roundpost", "StandardErrors"]^2)
ci_lower <- effect_overall - qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_upper <- effect_overall + qnorm(1 - 0.05/ 2) * se_overall # 1.959964
ci_overall <- c(ci_lower, ci_upper)
effect_overall
ci_overall
