#####################################################
# DATA CLEANING AND LINKAGE BURKINA FASO LAB data
#####################################################
# This code is cleaning the data 

# 22 January 2024
# Last update: 

rm(list=ls())

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr)

# SET DIRECTORY
DirectoryData <- "./Data/BF/Raw"
DirectoryDataOut <- "./Data/BF/clean"

# Lab data
#car_r0 = read_xlsx(paste0(DirectoryData,"/CABUBPortageAsymptom_DATA_2023-10-17_manualchange_no_password.xlsx"))
car_r0 = read_xlsx(paste0(DirectoryData,"/CABUBPortageAsymptom_DATA_2023-10-17_nopassword.xlsx"))
car_r1 = read_xlsx(paste0(DirectoryData, "/CABUBPortageAsymptom_DATA_R1_R2_2024-01-24_nopassword.xlsx"), sheet=1)
car_r2 = read_xlsx(paste0(DirectoryData, "/CABUBPortageAsymptom_DATA_R1_R2_2024-01-24_nopassword.xlsx"), sheet=2)

# Lab ids vs household ids
hh_lab_ids =  read_xlsx(paste0(DirectoryData,"/Correspondande-Code_Lab-ID_Menage.xlsx"))

# Household data
wash_r0 = read_xls(paste0(DirectoryData, "/WP4_WASH_07_09_2023_nopassword.xls"))

# Villages (that are the clusters) of CABU-EICO
villages = read_xlsx(paste0(DirectoryData, "/bf_villages_cabu.xlsx"))
names(villages) = c("village", "village_name","intervention_text","ajouter")


# Check variables between r0 and r1 and r2 - seems we have received the household variables
# of those with a lab sample, not the lab data
which(names(car_r1) %in% names(car_r0))
which(names(car_r1) %in% names(wash_r0))
length(which(names(car_r1) %in% names(wash_r0)))
which(!names(car_r1) %in% names(wash_r0))
names(car_r1[which(names(car_r1) %in% names(car_r0))])
names(car_r1[which(!names(car_r1) %in% names(wash_r0))])
names(car_r1[which(!names(car_r1) %in% names(wash_r0))])

# Add variables village and household
car_r0$village = substr(car_r0$record_id, start = 1, stop = 2)
#car_r0$household = str_extract(car_r0$record_id_manual_change, "[^-]+")
car_r0$household = str_extract(car_r0$record_id, "[^-]+")
car_r0 = merge(car_r0, villages, by="village")

############################################################
# CLEAN LAB DATA
############################################################

# Clean germe; diameters are CLSI guidelines, 
# Brecht discussed with Jan jacobs, there understood ESBL positive is defined as cetriax/cefo <=22 (including I and R, based on CLSI guidelines,
# See in WP6 folder "word file: Interpretation antibiogramme des isolats PORTAGE asymptomatique_ESBL_E. coliKlebsielle.docx), 
# So I understood to use this, instead of esbltest == 1 
# This is then interpreted as, cetriax/cefo resistant following ESBL selective medium)

# Evk 20 September 2023:
# Would be good to do a final check with Yougbare if this is correct or whether
# we can just use all the observations in the car_r0 file (i.e. esbltest == 0 and esbltest == 1). 

car_r0 = car_r0 %>%
  mutate(germe_c = ifelse(germe %in% c("E.COLI", "E-COLI", "ECOLI", "E.COLI 2", "E.COLI 1", 
                                       "E-COLI 2","E-CLI","E-CLOI"),"e.coli", 
                          ifelse(germe %in% c("SALMONELLA SP","SALMONELLA SPP","SALMONELLA SSP",
                                              "SALMONELLA", "SALMO"),"salmonella",NA)),
         germe_c = ifelse(morphotyp%in%c(1, NA),germe_c, paste0(germe_c, "_", 2)),
         esbl_pos = ifelse(diametr_cetriax_or_cefota <= 22, 1, 0))

# Number of cases positive
table(car_r0$germe_c, car_r0$esbl_pos)

table(car_r0$esbl_pos) # These are the ESBL positive patients based on cetriax_or_cefota, 769
table(car_r0$testesbl) # These are the ESBL positive patients based on esbl_pos, 772
table(car_r0$esbl_pos==1 & car_r0$testesbl==1) # difference of 7 which need resolving

names(car_r0)