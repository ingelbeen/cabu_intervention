#############################################
# CABU-C WP4 BACTERIOLOGIE                  #
# Link sample results to baseline data      #
#############################################
# last update: 19/10/2023

# install and load packages
pacman::p_load(readxl,lubridate,dplyr,ggplot2)

# 1.1 IMPORT DATA
# import lab result datasets
rodentsR1results <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP4/Kimpese_WP4results.xlsx", 
                               sheet = "rodentsR1")
rodentsR2results <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP4/Kimpese_WP4results.xlsx", 
                               sheet = "rodentsR2")
humanR1results <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP4/Kimpese_WP4results.xlsx", 
                               sheet = "humanR1")
humanR2results <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP4/Kimpese_WP4results.xlsx", 
                               sheet = "humanR2")
humanR2results <- humanR2results %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns
humanR2results <- humanR2results %>% filter(!is.na(Identifiant)|!is.na(`Date reception`)|!is.na(`Escherichia coli`)) # remove empty rows


# import rodent characteristics, incl location
rodents_char <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP3/FIELDLIST_KIM_RODENTS1-612_ITG.xlsx", 
                           sheet = "Captures")
# import human household data
# R1
# individuals with samples collected
HHindividualR1 <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP4/CABU_enq_comm_2023_-_all_versions_-_labels_-_2023-04-25-14-13-13.xlsx", 
                           sheet = "group_io0xt32")
# remove 'Nom' and 'Prénom' columns
# HHindividualR1 <- subset(HHindividualR1, select = -c(Nom, Prénom)) # remove identifiers
HHindividualR1 <- HHindividualR1 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# HH data
HHlocationR1 <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP4/CABU_enq_comm_2023_-_all_versions_-_labels_-_2023-04-25-14-13-13.xlsx", 
                         sheet = "CABU_enq_comm_2023")
HHlocationR1 <- subset(HHlocationR1, select = -c(Nom, Prénom)) # remove identifiers
HHlocationR1 <- HHlocationR1 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# merge ODK individual and HH data (including location)
HHR1 <- merge(HHindividualR1, HHlocationR1, by.x = "_submission__id", by.y = "_id", all.x = T)

# R2
HHindividualR2 <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP4/CABU_R2_-_all_versions_-_labels_-_2023-10-19-09-17-36.xlsx", 
                             sheet = "group_io0xt32")
# remove 'Nom' and 'Prénom' columns
# HHindividualR2 <- subset(HHindividualR2, select = -c(Nom, Prénom)) # remove identifiers
HHindividualR2 <- HHindividualR2 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# HH date
HHlocationR2 <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/AMR BIT/CABU JPIAMR/WP4/CABU_R2_-_all_versions_-_labels_-_2023-10-19-09-17-36.xlsx", 
                           sheet = "CABU_R2", col_types = c("date", 
                                                            "text", "numeric", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "text", "text", "text", 
                                                            "text", "numeric", "numeric", "numeric", 
                                                            "text", "text", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "numeric", "text", "numeric", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "text", "text", "text", 
                                                            "text", "numeric", "text", "numeric", 
                                                            "text", "text", "text", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "text", "text", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "text", "text", "text", "numeric", 
                                                            "text", "numeric", "numeric", "numeric", 
                                                            "text", "text", "text", "numeric", 
                                                            "numeric"))
HHlocationR2 <- HHlocationR2 %>% select_if(function(x) !all(is.na(x))) # remove empty variable columns

# merge ODK individual and HH data (including location)
HHR2 <- merge(HHindividualR2, HHlocationR2, by.x = "_submission__id", by.y = "_id", all.x = T)

# 1.2 MERGE RODENT DATA
# append rodent results
rodentsR1results$round <- "R1"
rodentsR2results$round <- "R2"
rodentsresults <- rbind(rodentsR1results, rodentsR2results)
# remove rows with missing 'Identifiant' and 'Date reception'
rodentsresults <- subset(rodentsresults, !(is.na(Identifiant) & is.na(`Date reception`)))
# reformat date
rodentsresults$receptiondate <- as.Date(rodentsresults$`Date reception`)
# FOR NOW REMOVE OBSERVATIONS SECOND ROUND - REMOVE THIS LINE ONCE THE CHAR OF SECOND ROUND ADDED
rodentsresults <- rodentsresults %>% filter(rodentsresults$receptiondate < "2023-06-01")
# reformat ID
rodentsresults$id <- substr(tolower(gsub("[ -]", "", rodentsresults$Identifiant)), 1, 6)
# check for duplicated values in rodentsresults$id
duplicates <- rodentsresults$id[duplicated(rodentsresults$id)]
# print the duplicated values
if (length(duplicates) > 0) {
  cat("Duplicated values in rodentsresults$id: ", paste(duplicates, collapse = ", "), "\n")
} else {
  cat("No duplicated values in rodentsresults$id\n")
} # kim481 WHY??

# reformat ID of rodents_char
rodents_char$id <- substr(tolower(gsub("[ -]", "", rodents_char$ua_id)), 1, 6)
# remove the first 247 captured rodents (no correct bacteriology test done)
rodents_char <- rodents_char %>% filter(as.numeric(gsub("[^0-9]", "", id)) >= 248)

# merge rodent char with results
rodentmerged <- merge(rodents_char, rodentsresults, by = "id", all = T) # one observation to remove (duplicate lab result) once clarified what caused the duplicate

# 1.3 MERGE HUMAN DATA R1
# make a var with a common id (of sample)
table(HHR1$`Numéro de l'échantillon`)
HHR1$id <- substr(tolower(gsub("[ -]", "", HHR1$`Numéro de l'échantillon`)), 1, 7)
HHR1$id[HHR1$id=="czr1144"] <- "car1144" 
HHR1$id[HHR1$id=="acr1258"] <- "car1258" 
table(HHR1$id)
# Assuming the 'id' column contains character strings
HHR1 <- HHR1 %>%
  mutate(id = ifelse(grepl("^\\d{4}$", id), paste0("car", id), id))


table(humanR1results$Identifiant)
humanR1results$id <- substr(tolower(gsub("[ -]", "", humanR1results$Identifiant)), 1, 8)
humanR1results$id <- substr(tolower(gsub(" ", "", humanR1results$id)), 1, 8)
humanR1results$id <- substr(tolower(gsub("'", "", humanR1results$id)), 1, 8)
table(humanR1results$id)

# reformat date
table(humanR1results$`Date reception`)
humanR1results$receptiondate <- as.Date(humanR1results$`Date reception`) # CORRECT SAMPLE DATE 2023-04-06

# merge both dataframes
humanR1merged <- merge(HHR1, humanR1results, by = "id", all = T)

# identify observations that are not merged
# identify ids in lab results not matching to kobo entry
unmatchedidinhumanlabresults <- humanR1merged %>% filter(is.na(`_submission__id`)==T) %>% select(id, receptiondate)
unmatchedidinkobo <- humanR1merged %>% filter(is.na(receptiondate)==T) %>% select(id, today)
# export to csv files
write.csv(unmatchedidinhumanlabresults, "unmatchedidinhumanlabresults.csv")
write.csv(unmatchedidinkobo, "unmatchedidinkobo.csv")

# 1.4 MERGE HUMAN DATA R2
# make a var with a common id (of sample)
table(HHR2$`Numéro de l'échantillon`)
HHR2$id <- substr(tolower(gsub("[ -]", "", HHR2$`Numéro de l'échantillon`)), 1, 7)
table(HHR2$id)

table(humanR2results$Identifiant, useNA = "always")
humanR2results$id <- substr(tolower(gsub("[ -]", "", humanR2results$Identifiant)), 1, 8)
humanR2results$id <- substr(tolower(gsub(" ", "", humanR2results$id)), 1, 8)
humanR2results$id <- substr(tolower(gsub("'", "", humanR2results$id)), 1, 8)
table(humanR2results$id, useNA = "always")

# reformat date
table(humanR2results$`Date reception`, useNA = "always")
humanR2results$receptiondate <- as.Date(humanR2results$`Date reception`) 

# merge both dataframes
humanR2merged <- merge(HHR2, humanR2results, by = "id", all = T)

# identify observations that are not merged
# identify ids in lab results not matching to kobo entry
unmatchedidinhumanlabresultsR2 <- humanR2merged %>% filter(is.na(`_submission__id`)==T) %>% select(id, receptiondate)
unmatchedidinkoboR2 <- humanR2merged %>% filter(is.na(receptiondate)==T) %>% select(id, today)
# export to csv files
write.csv(unmatchedidinhumanlabresultsR2, "unmatchedidinhumanlabresultsR2.csv")
write.csv(unmatchedidinkoboR2, "unmatchedidinkoboR2.csv")

# 1.5 CHECK WHO HAD STOOL COLLECTED IN R1 BUT NOT R2
# ID individual
table(HHR1$`ID individuel du membre du ménage`, useNA = "always")
table(HHR2$`ID individuel du membre du ménage`, useNA = "always")
# keep only ID and date in simplified data frames
HHR1_IDdate <- HHR1 %>% select(`ID individuel du membre du ménage`, today, Nom, Prénom, `cluster (village ou quartier où se situe le ménage)`)
colnames(HHR1_IDdate) <- c("id_individu", "dateR1", "nomR1", "prénomR1", "clusterR1")
HHR2_IDdate <- HHR2 %>% select(`ID individuel du membre du ménage`, today, Nom, Prénom, `cluster (village ou quartier où se situe le ménage)`)
colnames(HHR2_IDdate) <- c("id_individu", "dateR2", "nomR2", "prénomR2", "clusterR2")
# merge IDs of both rounds
HHvisits_selles_R1R2 <- merge(HHR1_IDdate, HHR2_IDdate, by = "id_individu", all = T)
# export to csv
write.csv(HHvisits_selles_R1R2, "HHvisits_selles_R1R2.csv")
