#####################################################
# CABU-EICO patientsurveys                          #
# prevalence of antibiotic use, AWaRe distribution  #
#####################################################

# install/load packages
pacman::p_load(readxl, writexl, lubridate, haven, tidyr, dplyr, digest, ggplot2, ggthemes, ggalluvial, survey, srvyr, gtsummary, lme4, broom.mixed, patchwork)

#### 1. IMPORT DATA #### 
watch <- read_excel("public data/watch_pseudo.xlsx")

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

# reorganize data for a negative binomial regression model with offset so there is one line per provider/clusterID, calculate 
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

#### 2. DESCRIPTION PARTICIPANTS ####
# n surveys
table(watch$round, useNA = "always")
table(watch$round, watch$site, useNA = "always")

# n surveys by site, by intervention/control,by round
nsurveys_round_intervention_site <- watch %>% group_by(round, site, intervention) %>% summarise(n())
nsurveys_round_intervention_site

# number of providers/clusters where interviews were held
nclusters <- watch %>% group_by(site, cluster) %>% summarise(n()) %>% group_by(site) %>% summarise(n())
nclusters

# number of providers/clusters by type of provider, overall
nclusters_providertype <- watch %>% group_by(providertype, cluster) %>% summarise(n()) %>% group_by(providertype) %>% summarise(n())
nclusters_providertype

# number of providers/clusters by type of provider, by site
nclusters_site <- watch %>% group_by(site, cluster) %>% summarise(n()) %>% group_by(site) %>% summarise(n())
nclusters_site

# number of providers/clusters by type of provider, by intervention/control
nclusters_providertype_intervention <- watch %>% group_by(intervention, providertype, cluster) %>% summarise(n()) %>% group_by(intervention, providertype) %>% summarise(n())
nclusters_providertype_intervention

# number of providers/clusters by type of provider, by intervention/control, by site
nclusters_providertype_intervention_site <- watch %>% group_by(intervention, providertype, cluster, site) %>% summarise(n()) %>% group_by(site, intervention, providertype) %>% summarise(n())
nclusters_providertype_intervention_site

# population size of intervention clusters
watch %>% filter(intervention=="intervention" & round=="baseline") %>% group_by(village.cluster) %>% summarise(pop=mean(pop_villagecluster)) %>% summarise(total=sum(pop))
watch %>% filter(intervention=="intervention" & round=="baseline" & site=="Kimpese") %>% group_by(village.cluster) %>% summarise(pop=mean(pop_villagecluster)) %>% summarise(total=sum(pop))
watch %>% filter(intervention=="intervention" & round=="baseline" & site=="Nanoro") %>% group_by(village.cluster) %>% summarise(pop=mean(pop_villagecluster)) %>% summarise(total=sum(pop))

# population size of control clusters
watch %>% filter(intervention=="control" & round=="baseline") %>% group_by(village.cluster) %>% summarise(pop=mean(pop_villagecluster)) %>% summarise(total=sum(pop))
watch %>% filter(intervention=="control" & round=="baseline" & site=="Kimpese") %>% group_by(village.cluster) %>% summarise(pop=mean(pop_villagecluster)) %>% summarise(total=sum(pop))
watch %>% filter(intervention=="control" & round=="baseline" & site=="Nanoro") %>% group_by(village.cluster) %>% summarise(pop=mean(pop_villagecluster)) %>% summarise(total=sum(pop))


# table 2 patient characteristics
# illness
illnessdistr <- watch %>%
  group_by(site, intervention, round) %>%
  count(illness) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  pivot_wider(names_from = c(site, intervention, round),
    values_from = c(n, percent),
    names_sep = "_")
illnessdistr$percent_Kimpese_control_baseline  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Kimpese_control_baseline )),1)
illnessdistr$percent_Kimpese_intervention_baseline  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Kimpese_intervention_baseline )),1)
illnessdistr$percent_Nanoro_control_baseline  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Nanoro_control_baseline )),1)
illnessdistr$percent_Nanoro_intervention_baseline  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Nanoro_intervention_baseline )),1)
illnessdistr$percent_Kimpese_control_post  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Kimpese_control_post )),1)
illnessdistr$percent_Kimpese_intervention_post  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Kimpese_intervention_post )),1)
illnessdistr$percent_Nanoro_control_post  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Nanoro_control_post )),1)
illnessdistr$percent_Nanoro_intervention_post  <- round(as.numeric(gsub("%", "", illnessdistr$percent_Nanoro_intervention_post )),1)
illnessdistr
# agegr - excluding non acute illness cases
agegroupdistr <- watch %>%
  filter(illness=="yes_acute_illness") %>%
  group_by(site, intervention, round) %>%
  count(agegroup) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  pivot_wider(
    names_from = c(site, intervention, round),
    values_from = c(n, percent),
    names_sep = "_")
agegroupdistr$percent_Kimpese_control_post  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Kimpese_control_post )),1)
agegroupdistr$percent_Kimpese_intervention_post  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Kimpese_intervention_post )),1)
agegroupdistr$percent_Nanoro_control_post  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Nanoro_control_post )),1)
agegroupdistr$percent_Nanoro_intervention_post  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Nanoro_intervention_post )),1)
agegroupdistr$percent_Kimpese_control_baseline  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Kimpese_control_baseline )),1)
agegroupdistr$percent_Kimpese_intervention_baseline  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Kimpese_intervention_baseline )),1)
agegroupdistr$percent_Nanoro_control_baseline  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Nanoro_control_baseline )),1)
agegroupdistr$percent_Nanoro_intervention_baseline  <- round(as.numeric(gsub("%", "", agegroupdistr$percent_Nanoro_intervention_baseline )),1)
agegroupdistr
# sex - excluding non acute illness cases
sexdistr <- watch %>%
  filter(illness=="yes_acute_illness") %>%
  group_by(site, intervention, round) %>%
  count(sex) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  pivot_wider(
    names_from = c(site, intervention, round),
    values_from = c(n, percent),
    names_sep = "_")
sexdistr$percent_Kimpese_control_baseline  <- round(as.numeric(gsub("%", "", sexdistr$percent_Kimpese_control_baseline )),1)
sexdistr$percent_Kimpese_intervention_baseline  <- round(as.numeric(gsub("%", "", sexdistr$percent_Kimpese_intervention_baseline )),1)
sexdistr$percent_Nanoro_control_baseline  <- round(as.numeric(gsub("%", "", sexdistr$percent_Nanoro_control_baseline )),1)
sexdistr$percent_Nanoro_intervention_baseline  <- round(as.numeric(gsub("%", "", sexdistr$percent_Nanoro_intervention_baseline )),1)
sexdistr$percent_Kimpese_control_post  <- round(as.numeric(gsub("%", "", sexdistr$percent_Kimpese_control_post )),1)
sexdistr$percent_Kimpese_intervention_post  <- round(as.numeric(gsub("%", "", sexdistr$percent_Kimpese_intervention_post )),1)
sexdistr$percent_Nanoro_control_post  <- round(as.numeric(gsub("%", "", sexdistr$percent_Nanoro_control_post )),1)
sexdistr$percent_Nanoro_intervention_post  <- round(as.numeric(gsub("%", "", sexdistr$percent_Nanoro_intervention_post )),1)
sexdistr
# clinical presentations
clinpresdistr <- watch %>%
  filter(illness=="yes_acute_illness") %>%
  group_by(site, intervention, round) %>%
  count(clinpres_broad) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  pivot_wider(
    names_from = c(site, intervention, round),
    values_from = c(n, percent),
    names_sep = "_")
clinpresdistr$percent_Kimpese_control_baseline  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Kimpese_control_baseline )),1)
clinpresdistr$percent_Kimpese_intervention_baseline  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Kimpese_intervention_baseline )),1)
clinpresdistr$percent_Nanoro_control_baseline  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Nanoro_control_baseline )),1)
clinpresdistr$percent_Nanoro_intervention_baseline  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Nanoro_intervention_baseline )),1)
clinpresdistr$percent_Kimpese_control_post  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Kimpese_control_post )),1)
clinpresdistr$percent_Kimpese_intervention_post  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Kimpese_intervention_post )),1)
clinpresdistr$percent_Nanoro_control_post  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Nanoro_control_post )),1)
clinpresdistr$percent_Nanoro_intervention_post  <- round(as.numeric(gsub("%", "", clinpresdistr$percent_Nanoro_intervention_post )),1)
clinpresdistr
# append all these tables
colnames(illnessdistr)[1] <- "characteristic"
colnames(agegroupdistr)[1] <- "characteristic"
colnames(sexdistr)[1] <- "characteristic"
colnames(clinpresdistr)[1] <- "characteristic"
table2 <- rbind(illnessdistr)
table2 <- bind_rows(illnessdistr, agegroupdistr, sexdistr, clinpresdistr)
# reorder columns
table2 <- table2 %>% select("characteristic", "n_Kimpese_control_baseline","percent_Kimpese_control_baseline","n_Kimpese_control_post","percent_Kimpese_control_post",
                              "n_Kimpese_intervention_baseline","percent_Kimpese_intervention_baseline", "n_Kimpese_intervention_post","percent_Kimpese_intervention_post", 
                              "n_Nanoro_control_baseline", "percent_Nanoro_control_baseline", "n_Nanoro_control_post", "percent_Nanoro_control_post",  
                              "n_Nanoro_intervention_baseline", "percent_Nanoro_intervention_baseline","n_Nanoro_intervention_post", "percent_Nanoro_intervention_post" )
# save table
write_xlsx(table2, "table2.xlsx")

#### 3. CRUDE PREVALENCE ANTIBIOTIC USE ####
# 5.1 crude prevalence by provider type, by intervention/control group, by site, and pre- vs. post intervention
# any antibiotic
summary_antibioticcounts <- watch_acute %>%
  group_by(site, intervention, providertype, round) %>%
  summarise(count_antibiotic = sum(antibiotic == 1),
    total_count = n(),
    percentage_antibiotic = round((count_antibiotic / total_count) * 100, 1)) %>%
  mutate(combined_counts = paste(count_antibiotic, total_count, sep = "/")) %>%
  select(site, intervention, providertype, round, combined_counts, percentage_antibiotic) %>%
  pivot_wider(names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_antibiotic),
    names_sep = "_")
summary_antibioticcounts <- summary_antibioticcounts %>% select(c("providertype","round","combined_counts_Kimpese_control","percentage_antibiotic_Kimpese_control",
                                                        "combined_counts_Kimpese_intervention", "percentage_antibiotic_Kimpese_intervention", "combined_counts_Nanoro_control",
                                                        "percentage_antibiotic_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_antibiotic_Nanoro_intervention" )) 
summary_antibioticcounts
write_xlsx(summary_antibioticcounts, "summary_antibioticcounts.xlsx")

# watch
summary_watchcounts <- watch_acute %>%
  group_by(site, intervention, providertype, round) %>%
  summarise(count_watch_1 = sum(watch == 1),
    total_count = n(),
    percentage_watch_1 = round((count_watch_1 / total_count) * 100, 1)) %>%
  mutate(combined_counts = paste(count_watch_1, total_count, sep = "/")) %>%
  select(site, intervention, providertype, round, combined_counts, percentage_watch_1) %>%
  pivot_wider(names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_watch_1),
    names_sep = "_")
summary_watchcounts <- summary_watchcounts %>% select(c("providertype","round","combined_counts_Kimpese_control","percentage_watch_1_Kimpese_control",
                                "combined_counts_Kimpese_intervention", "percentage_watch_1_Kimpese_intervention", "combined_counts_Nanoro_control",
                                "percentage_watch_1_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_watch_1_Nanoro_intervention" )) 
write.table(summary_watchcounts, "summary_watchcounts.txt")
write_xlsx(summary_watchcounts, "summary_watchcounts.xlsx")

# 5.2 crude prevalence by clinical presentation, by intervention/control group, by site, and pre- vs. post intervention
# any antibiotic
clinpres_antibioticcounts <- watch_acute %>%
  group_by(site, intervention, clinpres_broad, round) %>%
  summarise(count_antibiotic = sum(antibiotic == 1),
    total_count = n(),
    percentage_antibiotic = round((count_antibiotic / total_count) * 100, 1)) %>%
  mutate(combined_counts = paste(count_antibiotic, total_count, sep = "/")) %>%
  select(site, intervention, clinpres_broad, round, combined_counts, percentage_antibiotic) %>%
  pivot_wider(names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_antibiotic),
    names_sep = "_")
clinpres_antibioticcounts <- clinpres_antibioticcounts %>% select(c("clinpres_broad","round","combined_counts_Kimpese_control","percentage_antibiotic_Kimpese_control",
                                                                  "combined_counts_Kimpese_intervention", "percentage_antibiotic_Kimpese_intervention", "combined_counts_Nanoro_control",
                                                                  "percentage_antibiotic_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_antibiotic_Nanoro_intervention" )) 
write_xlsx(clinpres_antibioticcounts, "clinpres_antibioticcounts.xlsx")

# watch
clinpres_watchcounts <- watch_acute %>%
  group_by(site, intervention, clinpres_broad, round) %>%
  summarise(count_watch_1 = sum(watch == 1),
    total_count = n(),
    percentage_watch_1 = round((count_watch_1 / total_count) * 100, 1)) %>%
  mutate(combined_counts = paste(count_watch_1, total_count, sep = "/")) %>%
  select(site, intervention, clinpres_broad, round, combined_counts, percentage_watch_1) %>%
  pivot_wider(names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_watch_1),
    names_sep = "_")
clinpres_watchcounts <- clinpres_watchcounts %>% select(c("clinpres_broad","round","combined_counts_Kimpese_control","percentage_watch_1_Kimpese_control",
                                                        "combined_counts_Kimpese_intervention", "percentage_watch_1_Kimpese_intervention", "combined_counts_Nanoro_control",
                                                        "percentage_watch_1_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_watch_1_Nanoro_intervention" )) 
write.table(clinpres_watchcounts, "clinpres_watchcounts.txt")
write_xlsx(clinpres_watchcounts, "clinpres_watchcounts.xlsx")

# 5.3 crude prevalence by clinical presentation, by intervention/control group, both site combined, and pre- vs. post intervention
# any antibiotic
clinpres_antibioticcounts <- watch_acute %>%
  group_by(clinpres_broad, intervention, round) %>%
  summarise(count_antibiotic = sum(antibiotic == 1),
    total_count = n(),
    percentage_antibiotic = round((count_antibiotic / total_count) * 100, 1)) %>%
  mutate(combined_counts = paste(count_antibiotic, total_count, sep = "/")) %>%
  select(clinpres_broad, intervention, round, combined_counts, percentage_antibiotic) %>%
  pivot_wider(names_from = c(intervention, round),
    values_from = c(combined_counts, percentage_antibiotic),
    names_sep = "_")
clinpres_antibioticcounts
write_xlsx(clinpres_antibioticcounts, "clinpres_antibioticcounts.xlsx")

# watch
clinpres_watchcounts <- watch_acute %>%
  group_by(clinpres_broad, intervention, round) %>%
  summarise(count_watch = sum(watch == 1),
    total_count = n(),
    percentage_watch = round((count_watch / total_count) * 100, 1)) %>%
  mutate(combined_counts = paste(count_watch, total_count, sep = "/")) %>%
  select(clinpres_broad, intervention, round, combined_counts, percentage_watch) %>%
  pivot_wider(names_from = c(intervention, round),
    values_from = c(combined_counts, percentage_watch),
    names_sep = "_")
clinpres_watchcounts

#### 4. FIGURE 2. PREVALENCE ANY ANTIBIOTIC & WATCH AND RR OVERALL, BY SITE, BY TYPE OF PROVIDER, AND BY INFECTION ####
# set reference categories
watch_acute_offset$round <- relevel(factor(watch_acute_offset$round), ref = "baseline")
watch_acute_offset$intervention <- relevel(factor(watch_acute_offset$intervention), ref = "control")

# 4.1 ANY ANTIBIOTIC
# overall prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates overall
surveydesign_prevalence <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
prevalence_estimates_overall <- svyby(~ prevalence,  ~ round + intervention,  # Grouping variables
                              design = surveydesign_prevalence,
                              svymean,  # Function to calculate means
                              vartype = c("ci"))  # Include confidence intervals
prevalence_estimates_overall$prevalence <- round(prevalence_estimates_overall$prevalence*100, 1)
prevalence_estimates_overall$ci_l <- round(prevalence_estimates_overall$ci_l*100, 1)
prevalence_estimates_overall$ci_u <- round(prevalence_estimates_overall$ci_u*100, 1)
prevalence_estimates_overall

# fit negative binomial regression model
library(MASS) # only load now to avoid a conflict with dplyr
library(sandwich)
nb_model_watch <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# provider type-specific prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by provider type 
surveydesign <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
prevalence_estimates_providertype <- svyby(~ prevalence,  ~ providertype + round + intervention,  # Grouping variables
                              design = surveydesign,
                              svymean,  # Function to calculate means
                              vartype = c("ci"))  # Include confidence intervals
prevalence_estimates_providertype$prevalence <- round(prevalence_estimates_providertype$prevalence*100, 1)
prevalence_estimates_providertype$ci_l <- round(prevalence_estimates_providertype$ci_l*100, 1)
prevalence_estimates_providertype$ci_u <- round(prevalence_estimates_providertype$ci_u*100, 1)
prevalence_estimates_providertype

# subsetting by type of provider
# subsetting by type of provider
# health centre
nb_model_watch_healthcentres <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset[watch_acute_offset$providertype=="healthcentre_publique",])
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_healthcentres, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# private clinic
nb_model_watch_privateclinic <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset[watch_acute_offset$providertype=="privateclinic",])
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_privateclinic, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# private pharmacy
nb_model_watch_privatepharmacy <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset[watch_acute_offset$providertype=="privatepharmacy",])
  # cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_privatepharmacy, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# informalvendor
nb_model_watch_informalvendor <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset[watch_acute_offset$providertype=="informalvendor",])
  # cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_informalvendor, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# site-specific prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by site
surveydesign <- update(surveydesign, prevalence = n_antibiotic / n_surveys)
prevalence_estimates_site <- svyby(~ prevalence,  ~ site + round + intervention,  # Grouping variables
                                           design = surveydesign,
                                           svymean,  # Function to calculate means
                                           vartype = c("ci")  # Include confidence intervals)
prevalence_estimates_site$prevalence <- round(prevalence_estimates_site$prevalence*100, 1)
prevalence_estimates_site$ci_l <- round(prevalence_estimates_site$ci_l*100, 1)
prevalence_estimates_site$ci_u <- round(prevalence_estimates_site$ci_u*100, 1)
prevalence_estimates_site
  
# fit negative binomial model with robust SE
# subset Nanoro
watch_acute_offset_nan <- watch_acute_offset %>% filter(site=="Nanoro")
nb_model_watch_nanoro <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset_nan)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_nanoro, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# subset Kimpese
watch_acute_offset_kim <- watch_acute_offset %>% filter(site=="Kimpese")
nb_model_watch_kimpese <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset_kim)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_kimpese, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# show on a plot
# append prevalence estimates in a single dataframe
# colnames(prevalence_estimates_clinpres)[colnames(prevalence_estimates_clinpres) == "clinpres_broad"] <- "subgroup"
colnames(prevalence_estimates_providertype)[colnames(prevalence_estimates_providertype) == "providertype"] <- "subgroup"
colnames(prevalence_estimates_site)[colnames(prevalence_estimates_site) == "site"] <- "subgroup"
prevalence_estimates_overall$subgroup <- "OVERALL, weighted"
prevalence_estimates <- rbind(prevalence_estimates_overall, prevalence_estimates_providertype)
prevalence_estimates <- rbind(prevalence_estimates, prevalence_estimates_site)
prevalence_estimates$interventionround <- paste(prevalence_estimates$intervention,", ",prevalence_estimates$round)
prevalence_estimates$interventionround <- gsub(" , ", ", ", prevalence_estimates$interventionround)

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
      intervention == "control" & round == "post" ~ as.numeric(subgroup) - 0.15))

# add a variable with the PR values of for each change
prevalence_estimates$pr <- ""
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="OVERALL, weighted"] <- "PR 0·48 (95%CI 0·28-0·82)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="Kimpese"] <- "0·53 (0·28-0·99)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="Nanoro"] <- "0·40 (0·20-0·79)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="health centre"] <- "0·47 (0·24-0·91)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="private clinic"] <- "0·43 (0·14-1·3)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="community pharmacy/store"] <- "0·78 (0·30-2·0)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="informal vendor"] <- "0·81 (0·49-1·4)"

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
    labels = prevalence_estimates$subgroup,) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    name = "Prevalence of antibiotic use (%)") +  
  scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  guides(color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 10)) +
  geom_hline(yintercept = c(4.5,6.5), linetype = "solid", color = "darkgrey") +
  labs(y = element_blank(), color = "Intervention/control clusters & time") +
  geom_text(aes(label = pr),
            hjust = -0.3,       # shift label a bit to the right of the point
            vjust = 3.9,        # vertical alignment
            size = 2.5)
subgroup_prevalence_plot

ggsave("subgroup_prevalence_plot.jpeg", plot = subgroup_prevalence_plot, width = 6, height = 6, dpi = 300)
write_xlsx(prevalence_estimates, "prevalence_estimates.xlsx")

# make a table with all values, in wide format (table S3)
prevalence_estimates_wide <- prevalence_estimates
prevalence_estimates_wide$position <- NULL
prevalence_estimates_wide$ci <- paste(prevalence_estimates_wide$ci_l, prevalence_estimates_wide$ci_u)
prevalence_estimates_wide <- prevalence_estimates_wide %>% pivot_wider(id_cols = c(subgroup),    
                                                                  names_from = c(intervention, round),    
                                                                  values_from = c(prevalence, ci, pr),    
                                                                  names_glue = "{.value}_{intervention}_{round}")
write_xlsx(prevalence_estimates_wide, "prevalence_estimates_wide.xlsx")
                                   
# 7.2 WATCH
# OVERALL prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by provider type 
surveydesign_prevalence <- update(surveydesign, prevalence = n_watch / n_surveys)
watchprevalence_estimates_overall <- svyby(~ prevalence,  ~ round + intervention,  # Grouping variables
                                      design = surveydesign_prevalence,
                                      svymean,  # Function to calculate means
                                      vartype = c("ci"))  # Include confidence intervals
watchprevalence_estimates_overall$prevalence <- round(watchprevalence_estimates_overall$prevalence*100, 1)
watchprevalence_estimates_overall$ci_l <- round(watchprevalence_estimates_overall$ci_l*100, 1)
watchprevalence_estimates_overall$ci_u <- round(watchprevalence_estimates_overall$ci_u*100, 1) #rounded to 0 decimals upper limits intervention group are 45 and 26
watchprevalence_estimates_overall

# fit negative binomial regression model 
library(MASS) # only load now to avoid a conflict with dplyr
library(sandwich)
library(lmtest)
nb_model_watch <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata, # added strata (4, two in Kimpese, two in Nanoro) in the model because the cRCT had stratified randomisation
  data = watch_acute_offset)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# BY SITE
# site-specific prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by site
surveydesign <- update(surveydesign, prevalence = n_watch / n_surveys)
watchprevalence_estimates_site <- svyby(~ prevalence,  ~ site + round + intervention,  # Grouping variables
                                   design = surveydesign,
                                   svymean,  # Function to calculate means
                                   vartype = c("ci")  # Include confidence intervals)
watchprevalence_estimates_site$prevalence <- round(watchprevalence_estimates_site$prevalence*100, 1)
watchprevalence_estimates_site$ci_l <- round(watchprevalence_estimates_site$ci_l*100, 1)
watchprevalence_estimates_site$ci_u <- round(watchprevalence_estimates_site$ci_u*100, 1)
watchprevalence_estimates_site

# fit negative binomial model
# subset Nanoro
watch_acute_offset_nan <- watch_acute_offset %>% filter(site=="Nanoro")
nb_model_watch_nanoro <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset_nan)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_nanoro, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# subset Kimpese
watch_acute_offset_kim <- watch_acute_offset %>% filter(site=="Kimpese")
nb_model_watch_kimpese <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset_kim)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_kimpese, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# BY PROVIDERTYPE prevalence and prevalence ratio
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset, weights = ~weight)
# prevalence estimates by provider type 
surveydesign <- update(surveydesign, prevalence = n_watch / n_surveys)
watchprevalence_estimates_providertype <- svyby(~ prevalence,  ~ providertype + round + intervention,  # Grouping variables
                                           design = surveydesign,
                                           svymean,  # Function to calculate means
                                           vartype = c("ci")  # Include confidence intervals)
watchprevalence_estimates_providertype$prevalence <- round(watchprevalence_estimates_providertype$prevalence*100, 1)
watchprevalence_estimates_providertype$ci_l <- round(watchprevalence_estimates_providertype$ci_l*100, 1)
watchprevalence_estimates_providertype$ci_u <- round(watchprevalence_estimates_providertype$ci_u*100, 1)
watchprevalence_estimates_providertype

# negative binomial model 
# subset health centres
watch_acute_offset_healthcentre <- watch_acute_offset[watch_acute_offset$providertype=="healthcentre_publique",]
nb_model_watch_healthcentre <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset_healthcentre)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_healthcentre, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# subset private clinics
watch_acute_offset_privateclinics <- watch_acute_offset[watch_acute_offset$providertype=="privateclinic",]
nb_model_watch_clinics <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset_privateclinics)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_clinics, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# subset pharmacies
watch_acute_offset_pharmacy <- watch_acute_offset[watch_acute_offset$providertype=="privatepharmacy",]
nb_model_watch_pharmacy <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset_pharmacy)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_pharmacy, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

# subset informal vendors -> since there is a subgroup with n_watch 0 (post in the control group), add a small continuity correction 
watch_acute_offset_informalvendor <- watch_acute_offset %>% filter(providertype=="informalvendor" & site=="Nanoro")
watch_acute_offset_informalvendor$n_watch_adj <-  watch_acute_offset_informalvendor$n_watch + 0.5
nb_model_watch_informal <- glm.nb(
  n_watch_adj ~ offset(log(pop_patients)) + round * intervention + strata, 
  data = watch_acute_offset_informalvendor)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch_informal, vcov = vcovCL, cluster = ~clusterID)
# PR with 95%CI
coefs <- robust_results
beta <- coefs["roundpost:interventionintervention", "Estimate"]
se <- coefs["roundpost:interventionintervention", "Std. Error"]
RR <- exp(beta)
CI <- exp(beta + c(-1, 1) * 1.96 * se)
cat(sprintf("RR: %.2f (95%% CI: %.2f to %.2f)\n", RR, CI[1], CI[2]))

                                                
# show on a plot
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
  intervention == "control" & round == "post" ~ as.numeric(subgroup) - 0.15))

# add a variable with the PR values of for each change
watchprevalence_estimates$pr <- ""
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="OVERALL, weighted"] <- "PR 0·33 (95%CI 0·14-0·78)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="Kimpese"] <- "0·34 (0·18-0·67)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="Nanoro"] <- "0·27 (0·05-1·34)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="health centre"] <- "0·50 (0·21-1·18)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="private clinic"] <- "0·68(0·28-1·69)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="community pharmacy/store"] <- "0·40 (0·14-1·16)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="informal vendor"] <- "1·1 (0·41-2·8)*"

# make plot
subgroup_watchprevalence_plot <- ggplot(watchprevalence_estimates, aes(x = prevalence, y = position)) +
  geom_point(
    aes(color = interventionround), 
    size = 3  ) +  
  geom_errorbarh(aes(xmin = ci_l, xmax = ci_u, color = interventionround), 
    height = 0.1 ) +   # Error bars for confidence intervals
  scale_y_continuous(breaks = as.numeric(watchprevalence_estimates$subgroup),
    labels = watchprevalence_estimates$subgroup,) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    name = "Prevalence use of Watch group antibiotics (%)" +  
  scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  guides(color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  geom_hline(yintercept = c(4.5,6.5), linetype = "solid", color = "darkgrey") + # Gridlines at 1st & 3rd position
  labs(y = element_blank(), 
    color = "Intervention/control clusters & time" ) + # Rename legend title 
  geom_text(aes(label = pr),
            hjust = -0.3,       # shift label a bit to the right of the point
            vjust = 3.9,        # vertical alignment
            size = 2.5)
subgroup_watchprevalence_plot
ggsave("subgroup_watchprevalence_plot.jpeg", plot = subgroup_watchprevalence_plot, width = 6, height = 6, dpi = 300)

# table with all numeric values (Table S3)
watchprevalence_estimates_wide <- watchprevalence_estimates
watchprevalence_estimates_wide$position <- NULL
watchprevalence_estimates_wide$ci <- paste(watchprevalence_estimates_wide$ci_l, watchprevalence_estimates_wide$ci_u)
watchprevalence_estimates_wide <- watchprevalence_estimates_wide %>% pivot_wider(id_cols = c(subgroup),    
                                                                       names_from = c(intervention, round),    
                                                                       values_from = c(prevalence, ci, pr),    
                                                                       names_glue = "{.value}_{intervention}_{round}")
write_xlsx(watchprevalence_estimates_wide, "watchprevalence_estimates_wide.xlsx")

#### PREVALENCE AND RR BY CLIN PRESENTATION ####
# ANY ANTIBIOTIC
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

# show on a plot
# remove missing clin pres, or clin pres with <5 observations per group
prevalence_estimates_clinpres_plot <- prevalence_estimates_clinpres %>% filter(!is.na(clinpres_broad)&clinpres_broad!="sexually transmitted infection"&clinpres_broad!="dental")
# rename to shorten 
levels(prevalence_estimates_clinpres_plot$clinpres_broad)[levels(prevalence_estimates_clinpres_plot$clinpres_broad) == "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"] <- "non-bacterial infections"
levels(prevalence_estimates_clinpres_plot$clinpres_broad)[levels(prevalence_estimates_clinpres_plot$clinpres_broad) == "non-specific symptoms or complaints"] <- "non-specific symptoms"

# add a var for the combination round, intervention
prevalence_estimates_clinpres_plot$interventionround <- paste(prevalence_estimates_clinpres_plot$intervention,", ",prevalence_estimates_clinpres_plot$round)
prevalence_estimates_clinpres_plot$interventionround <- gsub(" , ", ", ", prevalence_estimates_clinpres_plot$interventionround)
# values of confidence intervals
prevalence_estimates_clinpres_plot <- prevalence_estimates_clinpres_plot %>% separate(ci, into = c("ci_l", "ci_u"), sep = " - ", convert = TRUE, remove = FALSE) # separate ci values
prevalence_estimates_clinpres_plot <- prevalence_estimates_clinpres_plot %>%  mutate( 
  ci_l = pmin(replace_na(ci_l, 0), 100),  ci_u = pmin(replace_na(ci_u, 0), 100)) # if CI limits under 0 or >100, replace with 0 or 100

# prevalence_estimates$subgroup <- factor(prevalence_estimates$subgroup, levels = subgroups_reversed)
prevalence_estimates_clinpres_plot$clinpres_broad <- factor(prevalence_estimates_clinpres_plot$clinpres_broad, 
                                                  levels = c("other", "pneumonia", "gastroenteritis", "urinary tract infection", "typhoid or sepsis", "non-specific symptoms", "unexplained gastro-intestinal", "non-bacterial infections", 
                                                             "unexplained fever", "skin/soft tissue infection", "acute respiratory infection", "malaria"))

# add a vertical position for each estimate
prevalence_estimates_clinpres_plot <- prevalence_estimates_clinpres_plot %>% mutate(position = case_when(
  intervention == "intervention" & round == "baseline" ~ as.numeric(clinpres_broad) + 0.15,
  intervention == "intervention" & round == "post" ~ as.numeric(clinpres_broad) + 0.08,
  intervention == "control" & round == "baseline" ~ as.numeric(clinpres_broad) - 0.08,
  intervention == "control" & round == "post" ~ as.numeric(clinpres_broad) - 0.15))

# add a variable with the PR values of for each change
prevalence_estimates_clinpres_plot$pr <- ""
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="malaria"] <- "0·29 (0·13-0·65)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="acute respiratory infection"] <- "0·7 (0·32-1·51)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="skin/soft tissue infection"] <- "0·96 (0·41-2·25)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="unexplained fever"] <- "0·36 (0·14-0·92)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="non-bacterial infections"] <- "0·41 (0·11-1·57)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="unexplained gastro-intestinal"] <- "0·59 (0·15-2·29)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="non-specific symptoms"] <- "0·95 (0·38-2·39)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="typhoid or sepsis"] <- "0·37 (0·12-1·12)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="urinary tract infection"] <- "0·54 (0·16-1·87)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="gastroenteritis"] <- "0·38 (0·13-1·08)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="pneumonia"] <- "1·57 (0·55-4·53)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="sexually transmitted infection"] <- "0·11 (0·03-0·43)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="dental"] <- "2·93 (0·84-10·14)"

# make plot
clinpres_prevalence_plot <- ggplot(prevalence_estimates_clinpres_plot, aes(x = prevalence, y = position)) +
  geom_point(
    aes(color = interventionround), 
    size = 3  ) +  
  geom_errorbarh(
    aes(xmin = ci_l, xmax = ci_u, color = interventionround), 
    height = 0.1 ) +   
  scale_y_continuous(
    breaks = as.numeric(prevalence_estimates_clinpres_plot$clinpres_broad),
    labels = prevalence_estimates_clinpres_plot$clinpres_broad,) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    name = "Prevalence of antibiotic use (%)") +  
  scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  guides(color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 10)) +
  labs(y = element_blank(), color = "") +
  geom_text(aes(label = pr),
            hjust = 0.7,       # shift label a bit to the right of the point
            vjust = 2.7,        # vertical alignment
            size = 2.5)
clinpres_prevalence_plot

ggsave("clinpres_prevalence_plot.jpeg", plot = clinpres_prevalence_plot, width = 8, height = 6, dpi = 300)

# make a table with crude, adjusted prevalence and RR
# to wide format
prevalence_estimates_clinpres_wide <- prevalence_estimates_clinpres %>%
  pivot_wider(
    id_cols = c(clinpres_broad, round),
    names_from = intervention,
    values_from = c(prevalence, ci),
    names_glue = "{.value}_{intervention}") %>%
  select(clinpres_broad, round, prevalence_control, ci_control, prevalence_intervention, ci_intervention)
prevalence_estimates_clinpres_wide
# to wide format
prevalence_estimates_clinpres_wider <- prevalence_estimates_clinpres %>%
  pivot_wider(
    id_cols = c(clinpres_broad),
    names_from = c(round, intervention),
    values_from = c(prevalence, ci),
    names_glue = "{.value}_{intervention}_{round}") 
prevalence_estimates_clinpres_wider <- prevalence_estimates_clinpres_wider %>% select("clinpres_broad","prevalence_control_baseline","ci_control_baseline", "prevalence_control_post", "ci_control_post",
                                              "prevalence_intervention_baseline", "ci_intervention_baseline", "prevalence_intervention_post","ci_intervention_post")
colnames(prevalence_estimates_clinpres_wider)
# merge with counts
prevalence_estimates_counts_clinpres <- merge(prevalence_estimates_clinpres_wider, clinpres_antibioticcounts, by = "clinpres_broad", all = T)
prevalence_estimates_counts_clinpres <- prevalence_estimates_counts_clinpres %>% select("clinpres_broad","combined_counts_control_baseline","percentage_antibiotic_control_baseline","prevalence_control_baseline","ci_control_baseline", 
                                                                                        "combined_counts_control_post", "percentage_antibiotic_control_post","prevalence_control_post", "ci_control_post",
                                                                                        "combined_counts_intervention_baseline", "percentage_antibiotic_intervention_baseline", "prevalence_intervention_baseline", "ci_intervention_baseline", 
                                                                                        "combined_counts_intervention_post", "percentage_antibiotic_intervention_post", "prevalence_intervention_post","ci_intervention_post")
write_xlsx(prevalence_estimates_counts_clinpres, "prevalence_estimates_counts_clinpres.xlsx")

# subset by infection
# subset by infection
clinpres_categories <- c(
  "malaria",
  "acute respiratory infection",
  "skin/soft tissue infection",
  "unexplained fever",
  "unexplained gastro-intestinal",
  "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)",
  "non-specific symptoms or complaints",
  "typhoid or sepsis",
  "urinary tract infection",
  "gastroenteritis",
  "pneumonia",
  "sexually transmitted infection",
  "dental",
  "other")
# loop for each infection subset
library(MASS)       # For glm.nb
library(sandwich)   # For vcovCL
library(lmtest)     # For coeftest
results_list <- list()
for (clinpres in clinpres_categories) {
  data_subset <- watch_acute_clinpres_offset %>% filter(clinpres_broad == clinpres)
  if (nrow(data_subset) == 0) {
    warning(paste("No data for:", clinpres))
    next}
    tryCatch({
    nb_model <- glm.nb(n_antibiotic ~ offset(log(pop_patients)) + round * intervention + strata,
      data = data_subset)
    robust_results <- coeftest(nb_model, vcov = vcovCL, cluster = ~clusterID)
    interaction_row <- grep("round.*:.*intervention|intervention.*:.*round", rownames(robust_results))
    if (length(interaction_row) == 0) {
      warning(paste("No interaction term found for:", clinpres))
      next
    }
    log_coef <- robust_results[interaction_row, "Estimate"]
    se_coef  <- robust_results[interaction_row, "Std. Error"]
    p_val    <- robust_results[interaction_row, "Pr(>|z|)"]
    rr          <- exp(log_coef)
    rr_ci_lower <- exp(log_coef - 1.96 * se_coef)
    rr_ci_upper <- exp(log_coef + 1.96 * se_coef)
    results_list[[clinpres]] <- data.frame(
      ClinicalPresentation = clinpres,
      RiskRatio = round(rr, 2),
      CI = paste0(round(rr_ci_lower, 2), " - ", round(rr_ci_upper, 2)))
  }, error = function(e) {
    warning(paste("Error fitting model for:", clinpres, "-", e$message))})}
# append all into single table
prevalence_ratios_by_clinpres <- do.call(rbind, results_list)
rownames(prevalence_ratios_by_clinpres) <- NULL
print(prevalence_ratios_by_clinpres)
write_xlsx(prevalence_ratios_by_clinpres, "prevalence_ratios_by_clinpres.xlsx")

# 4.2 WATCH
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

# estimate PR, again using the same loop as above, with the same list of clinpres_categories
results_list <- list()
for (clinpres in clinpres_categories) {
  data_subset <- watch_acute_clinpres_offset %>% filter(clinpres_broad == clinpres)
  if (nrow(data_subset) == 0) {
    warning(paste("No data for:", clinpres))
    next}
  tryCatch({
    nb_model <- glm.nb(n_watch ~ offset(log(pop_patients)) + round * intervention + strata,
                       data = data_subset)
    robust_results <- coeftest(nb_model, vcov = vcovCL, cluster = ~clusterID)
    interaction_row <- grep("round.*:.*intervention|intervention.*:.*round", rownames(robust_results))
    if (length(interaction_row) == 0) {
      warning(paste("No interaction term found for:", clinpres))
      next
    }
    log_coef <- robust_results[interaction_row, "Estimate"]
    se_coef  <- robust_results[interaction_row, "Std. Error"]
    p_val    <- robust_results[interaction_row, "Pr(>|z|)"]
    rr          <- exp(log_coef)
    rr_ci_lower <- exp(log_coef - 1.96 * se_coef)
    rr_ci_upper <- exp(log_coef + 1.96 * se_coef)
    results_list[[clinpres]] <- data.frame(
      ClinicalPresentation = clinpres,
      RiskRatio = round(rr, 2),
      CI = paste0(round(rr_ci_lower, 2), " - ", round(rr_ci_upper, 2)))
  }, error = function(e) {
    warning(paste("Error fitting model for:", clinpres, "-", e$message))})}
# append all into single table
watchprevalence_ratios_by_clinpres <- do.call(rbind, results_list)
rownames(watchprevalence_ratios_by_clinpres) <- NULL
print(watchprevalence_ratios_by_clinpres)
write_xlsx(watchprevalence_ratios_by_clinpres, "watchprevalence_ratios_by_clinpres.xlsx")

# make a table with crude, adjusted prevalence and RR
# to wide format
watchprevalence_estimates_clinpres_wide <- watchprevalence_estimates_clinpres %>%
  pivot_wider(
    id_cols = c(clinpres_broad, round),
    names_from = intervention,
    values_from = c(prevalence, ci),
    names_glue = "{.value}_{intervention}") %>%
  select(clinpres_broad, round, prevalence_control, ci_control, prevalence_intervention, ci_intervention)
watchprevalence_estimates_clinpres_wide
# to wide format
watchprevalence_estimates_clinpres_wider <- watchprevalence_estimates_clinpres %>%
  pivot_wider(
    id_cols = c(clinpres_broad),
    names_from = c(round, intervention),
    values_from = c(prevalence, ci),
    names_glue = "{.value}_{intervention}_{round}"
  ) 
watchprevalence_estimates_clinpres_wider <- watchprevalence_estimates_clinpres_wider %>% select("clinpres_broad","prevalence_control_baseline","ci_control_baseline", "prevalence_control_post", "ci_control_post",
                                                                                      "prevalence_intervention_baseline", "ci_intervention_baseline", "prevalence_intervention_post","ci_intervention_post")
colnames(prevalence_estimates_clinpres_wider)
# merge with counts
watchprevalence_estimates_counts_clinpres <- merge(watchprevalence_estimates_clinpres_wider, clinpres_watchcounts, by = "clinpres_broad", all = T)
watchprevalence_estimates_counts_clinpres <- prevalence_estimates_counts_clinpres %>% select("clinpres_broad","combined_counts_control_baseline","percentage_antibiotic_control_baseline","prevalence_control_baseline","ci_control_baseline", 
                                                                                        "combined_counts_control_post", "percentage_antibiotic_control_post","prevalence_control_post", "ci_control_post",
                                                                                        "combined_counts_intervention_baseline", "percentage_antibiotic_intervention_baseline", "prevalence_intervention_baseline", "ci_intervention_baseline", 
                                                                                        "combined_counts_intervention_post", "percentage_antibiotic_intervention_post", "prevalence_intervention_post","ci_intervention_post")
write_xlsx(watchprevalence_estimates_counts_clinpres, "watchprevalence_estimates_counts_clinpres.xlsx")

#### COMBINED site, provider and clin pres ####
#ANY
# harmonise colnames
colnames(prevalence_estimates) 
colnames(prevalence_estimates_clinpres_plot)
prevalence_estimates_clinpres_merge <- prevalence_estimates_clinpres_plot %>% select(-ci)
names(prevalence_estimates_clinpres_merge)[names(prevalence_estimates_clinpres_merge) == "clinpres_broad"] <- "subgroup"
colnames(prevalence_estimates_clinpres_merge)

# append both
prevalence_estimates_allsubgroups <- rbind(prevalence_estimates, prevalence_estimates_clinpres_merge)
# arrange order of subgroups
prevalence_estimates_allsubgroups$subgroup <- factor(prevalence_estimates_allsubgroups$subgroup, levels = c("other", "pneumonia", "gastroenteritis", "urinary tract infection", "typhoid or sepsis", 
                                                                                  "non-specific symptoms", "unexplained gastro-intestinal", "non-bacterial infections", 
                                                                                  "unexplained fever", "skin/soft tissue infection", "acute respiratory infection", "malaria", "informal vendor", 
                                                                                  "community pharmacy/store","private clinic","health centre", "Nanoro","Kimpese", "OVERALL, weighted"))
# add new vertical positions
prevalence_estimates_allsubgroups <- prevalence_estimates_allsubgroups %>% mutate(position = case_when(intervention == "intervention" & round == "baseline" ~ as.numeric(subgroup) + 0.15,
  intervention == "intervention" & round == "post" ~ as.numeric(subgroup) + 0.08, intervention == "control" & round == "baseline" ~ as.numeric(subgroup) - 0.08,
  intervention == "control" & round == "post" ~ as.numeric(subgroup) - 0.15))
# create spece between subgroups
prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "OVERALL, weighted"] <- prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "OVERALL, weighted"] + 3
prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "Nanoro"] <- prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "Nanoro"] + 2
prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "Kimpese"] <- prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "Kimpese"] + 2
prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "informal vendor"] <- prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "informal vendor"] + 1
prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "private clinic"] <- prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "private clinic"] + 1
prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "health centre"] <- prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "health centre"] + 1
prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "community pharmacy/store"] <- prevalence_estimates_allsubgroups$position[prevalence_estimates_allsubgroups$subgroup == "community pharmacy/store"] + 1

label_positions <- prevalence_estimates_allsubgroups %>%  filter(intervention == "intervention", round == "baseline") %>%  distinct(subgroup, position) %>% mutate(label_position = position - 0.15)  # Remove the 0.15 jitter to get centre position


# make plot
subgroups_plot <- ggplot(prevalence_estimates_allsubgroups, aes(x = prevalence, y = position)) +
  geom_point(
    aes(color = interventionround), 
    size = 3  ) +  
  geom_errorbarh(
    aes(xmin = ci_l, xmax = ci_u, color = interventionround), 
    height = 0.1 ) +   
  scale_y_continuous(breaks = label_positions$label_position,
                     labels = label_positions$subgroup) +
  # breaks = as.numeric(watchprevalence_estimates_allsubgroups$subgroup),
  # labels = watchprevalence_estimates_allsubgroups$subgroup,) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    name = "Prevalence of any antibiotic use (%)") +  
  scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  guides(color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  labs(y = element_blank(), color = "") +
  geom_text(aes(label = pr),
            hjust = 0.2,       # shift label a bit to the right of the point
            vjust = 2.7,        # vertical alignment
            size = 2.5) 
subgroups_plot
ggsave("subgroups_anyantibiotic_plot.jpeg", plot = subgroups_plot, width = 7, height = 9, dpi = 300)
ggsave("subgroups_anyantibiotic_plot.pdf", plot = subgroups_plot, width = 7, height = 9)

# WATCH
# remove missing clin pres, or clin pres with <5 observations per group
watchprevalence_estimates_clinpres_plot <- watchprevalence_estimates_clinpres %>% filter(!is.na(clinpres_broad)&clinpres_broad!="sexually transmitted infection"&clinpres_broad!="dental")
# rename to shorten 
levels(watchprevalence_estimates_clinpres_plot$clinpres_broad)[levels(watchprevalence_estimates_clinpres_plot$clinpres_broad) == "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"] <- "non-bacterial infections"
levels(watchprevalence_estimates_clinpres_plot$clinpres_broad)[levels(watchprevalence_estimates_clinpres_plot$clinpres_broad) == "non-specific symptoms or complaints"] <- "non-specific symptoms"

# add a var for the combination round, intervention
watchprevalence_estimates_clinpres_plot$interventionround <- paste(watchprevalence_estimates_clinpres_plot$intervention,", ",watchprevalence_estimates_clinpres_plot$round)
watchprevalence_estimates_clinpres_plot$interventionround <- gsub(" , ", ", ", watchprevalence_estimates_clinpres_plot$interventionround)
# values of confidence intervals
watchprevalence_estimates_clinpres_plot <- watchprevalence_estimates_clinpres_plot %>% separate(ci, into = c("ci_l", "ci_u"), sep = " - ", convert = TRUE, remove = FALSE) # separate ci values
watchprevalence_estimates_clinpres_plot <- watchprevalence_estimates_clinpres_plot %>%  mutate( 
  ci_l = pmin(replace_na(ci_l, 0), 100),  ci_u = pmin(replace_na(ci_u, 0), 100)) # if CI limits under 0 or >100, replace with 0 or 100

# manually add the RR values for each change
watchprevalence_estimates_clinpres_plot$pr <- ""
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="malaria"] <- "0·54 (0·19–1·55)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"& watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="acute respiratory infection"] <- "0·23 (0·05–1·12)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="skin/soft tissue infection"] <- "0·48 (0·18–1·3)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="unexplained fever"] <- "0·29 (0·1–0·87)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="non-bacterial infections"] <- "1·34 (0·36–4·94)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="unexplained gastro-intestinal"] <- "0·21 (0·02–1·91)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="non-specific symptoms"] <- "1·22 (0·22–6·76)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="typhoid or sepsis"] <- "0·33 (0·09–1·21)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="urinary tract infection"] <- "0·19 (0·04–0·95)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="gastroenteritis"] <- "0·75 (0·19–2·96)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="sexually transmitted infection"] <- "0·13 (0·02–0·81)"

# harmonise colnames
colnames(watchprevalence_estimates) 
colnames(watchprevalence_estimates_clinpres_plot)
watchprevalence_estimates_clinpres_plot_merge <- watchprevalence_estimates_clinpres_plot %>% select(-ci)
watchprevalence_estimates <- watchprevalence_estimates %>% select(-position)
names(watchprevalence_estimates_clinpres_plot_merge)[names(watchprevalence_estimates_clinpres_plot_merge) == "clinpres_broad"] <- "subgroup"
# append both
watchprevalence_estimates_allsubgroups <- rbind(watchprevalence_estimates, watchprevalence_estimates_clinpres_plot_merge)
# remove some unnecessary spaces
watchprevalence_estimates_allsubgroups$interventionround <- gsub(" , ", ", ", watchprevalence_estimates_allsubgroups$interventionround)

# limit error bars at 0 to make sure they are displayed
watchprevalence_estimates_allsubgroups$ci_l[watchprevalence_estimates_allsubgroups$ci_l<0] <- 0

# arrange order of subgroups
watchprevalence_estimates_allsubgroups$subgroup <- factor(watchprevalence_estimates_allsubgroups$subgroup, levels = c("other", "pneumonia", "gastroenteritis", "urinary tract infection", "typhoid or sepsis", 
                                                                                                            "non-specific symptoms", "unexplained gastro-intestinal", "non-bacterial infections", 
                                                                                                            "unexplained fever", "skin/soft tissue infection", "acute respiratory infection", "malaria", "informal vendor", 
                                                                                                            "community pharmacy/store","private clinic","health centre", "Nanoro","Kimpese", "OVERALL, weighted"))
# add new vertical positions
watchprevalence_estimates_allsubgroups <- watchprevalence_estimates_allsubgroups %>% mutate(position = case_when(intervention == "intervention" & round == "baseline" ~ as.numeric(subgroup) + 0.15,
                                                                                                       intervention == "intervention" & round == "post" ~ as.numeric(subgroup) + 0.08, 
                                                                                                       intervention == "control" & round == "baseline" ~ as.numeric(subgroup) - 0.08,
                                                                                                       intervention == "control" & round == "post" ~ as.numeric(subgroup) - 0.15))
# create spece between subgroups
watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "OVERALL, weighted"] <- watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "OVERALL, weighted"] + 3
watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "Nanoro"] <- watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "Nanoro"] + 2
watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "Kimpese"] <- watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "Kimpese"] + 2
watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "informal vendor"] <- watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "informal vendor"] + 1
watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "private clinic"] <- watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "private clinic"] + 1
watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "health centre"] <- watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "health centre"] + 1
watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "community pharmacy/store"] <- watchprevalence_estimates_allsubgroups$position[watchprevalence_estimates_allsubgroups$subgroup == "community pharmacy/store"] + 1

label_positions <- watchprevalence_estimates_allsubgroups %>%  filter(intervention == "intervention", round == "baseline") %>%  distinct(subgroup, position) %>% mutate(label_position = position - 0.15)  # Remove the 0.15 jitter to get centre position

# make plot
subgroups_plot <- ggplot(watchprevalence_estimates_allsubgroups, aes(x = prevalence, y = position)) +
  geom_point(
    aes(color = interventionround), 
    size = 3  ) +  
  geom_errorbarh(
    aes(xmin = ci_l, xmax = ci_u, color = interventionround), 
    height = 0.1 ) +   
  scale_y_continuous(breaks = label_positions$label_position,
                     labels = label_positions$subgroup) +
    # breaks = as.numeric(watchprevalence_estimates_allsubgroups$subgroup),
    # labels = watchprevalence_estimates_allsubgroups$subgroup,) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    name = "Prevalence of Watch-group antibiotic use (%)") +  
  scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  guides(color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  labs(y = element_blank(), color = "") +
  geom_text(aes(label = pr),
            hjust = 0.2,       # shift label a bit to the right of the point
            vjust = 2.7,        # vertical alignment
            size = 2.5) 
subgroups_plot
ggsave("subgroups_watch_plot.jpeg", plot = subgroups_plot, width = 7, height = 9, dpi = 300)
ggsave("subgroups_watch_plot.pdf", plot = subgroups_plot, width = 7, height = 9)

#### 5. FIGURE S2. EFFECT ON POPULATION-WIDE ABU BY SITE: HCU frequency*ABU prevalence####
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
                              vartype = c("ci")  # Include confidence intervals)
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
AMUrate$rate_ab <- round((AMUrate$prevalence_ab * AMUrate$freq_1mo),1)
# ci
AMUrate$se_rate_ab <- sqrt(AMUrate$se_hcu^2 + AMUrate$se_ab^2)
AMUrate$rate_ab_ci_l <- round((exp(log(AMUrate$rate_ab) - 1.96 * AMUrate$se_rate_ab)),1)
AMUrate$rate_ab_ci_u <- round((exp(log(AMUrate$rate_ab) + 1.96 * AMUrate$se_rate_ab)),1)
# central estimate watch
AMUrate$rate_watch <- round((AMUrate$prevalence_watch * AMUrate$freq_1mo),1)
# ci
AMUrate$se_rate_watch <- sqrt(AMUrate$se_hcu^2 + AMUrate$se_watch^2)
AMUrate$se_rate_watch[AMUrate$rate_watch==0] <- 0 # if rate is 0, then se is 0
AMUrate$rate_watch_ci_l <- round((exp(log(AMUrate$rate_watch) - 1.96 * AMUrate$se_rate_watch)),1)
AMUrate$rate_watch_ci_u <- round((exp(log(AMUrate$rate_watch) + 1.96 * AMUrate$se_rate_watch)),1)
# central estimate Access
AMUrate$rate_access <- AMUrate$prevalence_access * AMUrate$freq_1mo

# save in a table for appendix 
AMUrate_table <- AMUrate
AMUrate_table$ci_ab <- paste(AMUrate_table$rate_ab_ci_l,"-",AMUrate_table$rate_ab_ci_u)
AMUrate_table$ci_watch <- paste(AMUrate_table$rate_watch_ci_l,"-",AMUrate_table$rate_watch_ci_u)
AMUrate_table <- AMUrate_table %>% select(site, providertype, round, rate_ab, ci_ab, rate_watch, ci_watch)
write_xlsx(AMUrate_table, "TableS5_amu_rate.xlsx")

# to long format
AMUrate_long <- AMUrate %>%
  select(providertype, site, round, rate_ab, rate_ab_ci_l, rate_ab_ci_u, rate_watch, rate_watch_ci_l, rate_watch_ci_u, rate_access) %>%
  pivot_longer(cols = c(rate_ab, rate_watch, rate_access), names_to = "antibiotic", values_to = "rate") %>%
  pivot_longer(cols = c(rate_ab_ci_l, rate_watch_ci_l),  names_to = "ci_type_l", values_to = "ci_l") %>%
  pivot_longer(cols = c(rate_ab_ci_u, rate_watch_ci_u),  names_to = "ci_type_u", values_to = "ci_u") %>%
  mutate(antibiotic = case_when(
      antibiotic == "rate_ab" ~ "ab",
      antibiotic == "rate_watch" ~ "watch",
      antibiotic == "rate_access" ~ "access")) %>%
  select(-ci_type_l, -ci_type_u)  # Remove unnecessary columns
AMUrate_long <- AMUrate_long %>%
  distinct(providertype, site, round, antibiotic, .keep_all = TRUE)
AMUrate_long

# overall rate of use
summary_AMUrate <- AMUrate %>%
  group_by(site, round) %>%
  summarise(rate_ab = sum(rate_ab), rate_watch = sum(rate_watch), se_ab = sqrt(sum(se_rate_ab^2)), rate_watch = sum(rate_watch), se_watch = sqrt(sum(se_rate_watch^2)))
summary_AMUrate$ci_l_ab <- summary_AMUrate$rate_ab - 1.96 * summary_AMUrate$se_ab
summary_AMUrate$ci_u_ab <- summary_AMUrate$rate_ab + 1.96 * summary_AMUrate$se_ab
summary_AMUrate$ci_l_watch <- summary_AMUrate$rate_watch - 1.96 * summary_AMUrate$se_watch
summary_AMUrate$ci_u_watch <- summary_AMUrate$rate_watch + 1.96 * summary_AMUrate$se_watch
summary_AMUrate
# initially I used a simple sum, but was wrong: rate_ab_ci_l = sum(rate_ab_ci_l), rate_ab_ci_u = sum(rate_ab_ci_u),


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
  geom_stratum(aes(fill = antibiotic), alpha = 0.9, width = 0.15) +
  scale_fill_manual(name = "AWaRe group", values = c("watch" = "orange", "access" = "darkgreen"), na.translate = FALSE) +
  scale_color_discrete(name = "provider type") +  # adds stratum to legend
  theme_minimal() +
  labs(y = "Antibiotic treatment episodes per 1000 inhabitants per month", x = NULL) +
  facet_wrap(~ site) +# , scales = "free_y"
  theme(
    panel.spacing = unit(0, "lines"),
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.major.x = element_blank(),  # remove vertical gridlines
    panel.grid.minor.x = element_blank()   # remove minor vertical gridlines
  )
rates_sankey
ggsave("rates_sankey.jpeg", plot = rates_sankey, width = 6, height = 5, dpi = 300)

#### 6. FIGURE 3. CHANGE IN AWARE PRE, POST INTERVENTION AND IF RECOMMENDATIONS FOLLOWED ####
# I need 1) actual aware distr at baseline, 2) actual aware distri post intervention, and 3) recommended aware distr assuming aware guidance followed for each clinpres
table(watch$clinpres)
# create a single var, which could be used for a distribution of AWaRe groups
watch_acute$aware[watch_acute$antibiotic==0] <- "no antibiotic"
watch_acute$aware[watch_acute$antibiotic==1] <- "access"
watch_acute$aware[watch_acute$watch==1] <- "watch"
# regroup clinical presentations for which there is no guidance
watch_acute$clinpres[watch_acute$clinpres=="cardiovascular condition"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="asthma/COPD"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="diabetes"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="other infections"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
watch_acute$clinpres[watch_acute$clinpres=="scabies"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
watch_acute$clinpres[watch_acute$clinpres=="parasitic infections"] <- "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)"
watch_acute$clinpres[watch_acute$clinpres=="post surgery"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="other non infectious"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="eye condition"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="wound"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="peptic ulcer"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="appendicitis"] <- "other"
watch_acute$clinpres[watch_acute$clinpres=="typhoid"] <- "typhoid or sepsis"
watch_acute$clinpres[watch_acute$clinpres=="sepsis"] <- "typhoid or sepsis"
watch_acute$clinpres[watch_acute$clinpres=="covid-19"] <- "acute respiratory infection"

# add the recommended aware group for each
watch_acute$recommended <- "no antibiotic"
watch_acute$recommended[watch_acute$clinpres=="other"] <- "no antibiotic"
watch_acute$recommended[watch_acute$clinpres %in% c("gastroenteritis", "unexplained gastro-intestinal", "non-bacterial infectious (viral outbreak, scabies, worms, amoebae)", 
                                                    "non-specific symptoms or complaints", "acute respiratory infection", "bronchiolitis", "bronchitis", "malaria",
                                                    "unexplained fever", "dental", "pharyngitis", "acute otitis media")] <- "no antibiotic"
watch_acute$recommended[watch_acute$clinpres=="dental"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="pharyngitis"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="acute otitis media"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="pharyngitis"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="acute respiratory infection"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="typhoid or sepsis"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="typhoid or sepsis"&watch_acute$watch==1] <- "watch"
watch_acute$recommended[watch_acute$clinpres=="sexually transmitted infection"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="sexually transmitted infection"&watch_acute$watch==1] <- "watch"
watch_acute$recommended[watch_acute$clinpres=="pneumonia"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="urinary tract infection"&watch_acute$antibiotic==1] <- "access"
watch_acute$recommended[watch_acute$clinpres=="skin/soft tissue infection"&watch_acute$antibiotic==1] <- "access"

# filter only health centres and private clinics
watch_acute_interventionHC <- watch_acute %>% filter(!is.na(clinpres)&intervention=="intervention"&(providertype=="healthcentre_publique"|providertype=="privateclinic")) 

# define colours 
aware_colors <- c(
  "access" = "darkgreen",
  "watch" = "orange",
  "no antibiotic" = "lightgrey")

# 1) BASELINE summary by aware group
baseline_awaredistr <- watch_acute_interventionHC %>% filter(round=="baseline") %>% group_by(aware) %>% summarise(n=n())
baseline_awaredistr$aware <- factor(baseline_awaredistr$aware, levels = c( "no antibiotic", "access", "watch"))
baseline_awaredistr
# simple plot
gap_fraction <- 0.02  # set gap size as a proportion of total counts: 2% of total height
# summarise counts and calculate cumulative positions
baseline_bar_data <- baseline_awaredistr %>%
  group_by(aware) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(aware)) %>%  # order for stacking
  mutate(
    total = sum(n),
    gap = total * gap_fraction,
    ymin = cumsum(lag(n + gap, default = 0)),
    ymax = ymin + n
  )

baseline_bar <- ggplot(baseline_bar_data, aes(xmin = 0.5, xmax = 1.5, ymin = ymin, ymax = ymax, fill = aware)) +
  geom_rect(width = 0.20, color = "white", size = 0.5) +
  geom_text(aes(x=1, y = (ymin + ymax)/2, label = aware),  # place label in middle of each segment
            colour = "black", size = 4, fontface = "bold") +
  scale_fill_manual(values = aware_colors) +
  labs(x = "Baseline", y = "Frequency") +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 11),
    legend.position = "none")
baseline_bar
ggsave("baseline_bar.jpeg", plot = baseline_bar, width = 1.7, height = 6, dpi = 500)

# sankey plot
baseline_awaredistr <- watch_acute_interventionHC %>% filter(round=="baseline") %>% group_by(aware, recommended) %>% summarise(n=n())
baseline_awaredistr$aware <- factor(baseline_awaredistr$aware, levels = c( "no antibiotic", "access", "watch"))
baseline_awaredistr$recommended <- factor(baseline_awaredistr$recommended, levels =  c( "no antibiotic", "access", "watch"))
baseline_awaredistr
# relabel data to generate the sankey using geom_parallel
bl_aware_ggforce <- baseline_awaredistr  %>%
  gather_set_data(1:2) %>%
  arrange(x, aware,desc(recommended))
# # calculate percentages
# label_df <- bl_aware_ggforce %>%
#   group_by(x) %>%
#   mutate(
#     total = sum(n),
#     pct = n / total * 100,
#     pct_label = paste0(round(pct), "%"),
#     ypos = cumsum(n) - n/2)   # midpoint of each bar
# generate the plot using geom_parallel_sets
parallel_sets_bl_plot <- ggplot(bl_aware_ggforce, aes(x = x, id = id, split = y, value = n)) +
  geom_parallel_sets(aes(fill = aware), axis.width = 0.2, alpha = 0.3,
                     n = 100, strength = 0.5) +
  geom_parallel_sets_axes(  # AXIS BARS COLORED BY X and Y VALUES
    aes(fill = x),         # map X value to fill
    axis.width = 0.25,
    color = "gray80",
    size = 0.15) +
  geom_parallel_sets_axes(
    aes(fill = y),         # map Y value to fill
    axis.width = 0.25,
    color = "gray80",
    size = 0.15) +
  geom_parallel_sets_labels(
    colour = "black", size = 4, angle = 0, fontface = "bold") +
  scale_fill_manual(values = aware_colors, name = NULL) +
  theme_minimal() +
  scale_x_discrete(labels = c("aware" = "Recorded use", "recommended" = "Recommended use")) +
  labs(x = "Baseline", y = "Frequency") +       # remove y-axis label
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11), axis.ticks.x = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  # geom_text( data = label_df, aes(x = x, y = ypos, label = pct_label), color = "black", size = 4, fontface = "bold") 
parallel_sets_bl_plot
ggsave("parallel_sets_bl_plot.jpeg", plot = parallel_sets_bl_plot, width = 8, height = 6, dpi = 800)

# 2) POST INTERVENTION summary by aware group & IDEAL
post_awaredistr <- watch_acute_interventionHC %>% filter(round=="post") %>% group_by(aware, recommended) %>% summarise(n=n())
post_awaredistr

# reorder values
post_awaredistr$aware <- factor(post_awaredistr$aware, levels = c( "no antibiotic", "access", "watch"))
post_awaredistr$recommended <- factor(post_awaredistr$recommended, levels =  c( "no antibiotic", "access", "watch"))

# relabel data to generate the sankey using geom_parallel
aware_ggforce <- post_awaredistr  %>%
  gather_set_data(1:2) %>%
  arrange(x, aware,desc(recommended))

# generate the plot using geom_parallel_sets
parallel_sets_plot <- ggplot(aware_ggforce, aes(x = x, id = id, split = y, value = n)) +
  geom_parallel_sets(aes(fill = aware), axis.width = 0.2, alpha = 0.3,
                     n = 100, strength = 0.5) +
  geom_parallel_sets_axes(aes(fill = x), axis.width = 0.25, color = "gray80", size = 0.15) +
  geom_parallel_sets_axes(aes(fill = y), axis.width = 0.25, color = "gray80", size = 0.15) +
  geom_parallel_sets_labels(colour = "black", size = 4, angle = 0, fontface = "bold") +
  scale_fill_manual(values = aware_colors, name = NULL) +
  theme_minimal() +
  scale_x_discrete(labels = c("aware" = "Recorded use", "recommended" = "Recommended use")) +
  labs(x = "Post intervention", y = NULL) +       # remove y-axis label
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# theme(axis.text.x = element_text(size = 11))   # ⬅ larger x-axis labels
parallel_sets_plot
ggsave("parallel_sets_plot.jpeg", plot = parallel_sets_plot, width = 8, height = 6, dpi = 800)

# combine baseline simple and post intervention sankey
baseline_bar + parallel_sets_plot + plot_layout(widths = c(0.8, 6))

# combine baseline and post sankey
parallel_sets_bl_plot <- parallel_sets_bl_plot + theme(plot.margin = unit(c(-5, -5, -5, -5), "pt"))
parallel_sets_plot <- parallel_sets_plot + theme(plot.margin = unit(c(-5, -5, -5, -50), "pt"))
Fig2bis <- parallel_sets_bl_plot + parallel_sets_plot + plot_layout(widths = c(6, 6))
ggsave("Fig2_bis.jpeg", plot = Fig2bis, width = 10, height = 5, dpi = 800)

# some percentages for the text 
# antibiotic use in health centres in intervention clusters at baseline
n_amu_bl <- sum(baseline_awaredistr$n[baseline_awaredistr$aware!="no antibiotic"])
n_amu_bl_averted <- sum(baseline_awaredistr$n[baseline_awaredistr$aware!="no antibiotic"&baseline_awaredistr$recommended=="no antibiotic"])
n_amu_bl
n_amu_bl_averted
n_amu_bl_averted/n_amu_bl
# antibiotic use in health centres in intervention clusters post intervention
n_amu_post <- sum(post_awaredistr$n[post_awaredistr$aware!="no antibiotic"])
n_amu_post_averted <- sum(post_awaredistr$n[post_awaredistr$aware!="no antibiotic"&post_awaredistr$recommended=="no antibiotic"])
n_amu_post
n_amu_post_averted
n_amu_post_averted/n_amu_post
# watch use in health centres in intervention clusters at baseline
baseline_awaredistr
n_watch <- sum(baseline_awaredistr$n[baseline_awaredistr$aware=="watch"])
n_watch_averted <- sum(baseline_awaredistr$n[baseline_awaredistr$aware=="watch"&baseline_awaredistr$recommended!="watch"])
n_watch
n_watch/n_amu_bl # pct watch
n_watch_averted
n_watch_averted/n_watch
# watch use in health centres in intervention clusters post intervention
n_watch <- sum(post_awaredistr$n[post_awaredistr$aware=="watch"])
n_watch_averted <- sum(post_awaredistr$n[post_awaredistr$aware=="watch"&post_awaredistr$recommended!="watch"])
n_watch
n_watch/n_amu_post # pct watch
n_watch_averted
n_watch_averted/n_watch

#### 7. OBSERVED INTRACLASS CORRELATION COEFFICIENT ####
# the power calculation was based on an ICC of 0.01 and for four estimates: one for each of medicine stores and healthcentres in each of both sites
# so we calculate four observed ICC at baseline, twice: for the data used to estimate the prevalence and that with offset for the prevalence ratios
# packages needed
pacman::p_load(dplyr, lme4, ICC, survey)

# calculate weight
watch_acute$weight <- watch_acute$pop_patients/watch_acute$n_surveys_cluster # weight for each cluster

# filter baseline individual data for observed ICC and create subsets per strum that we estimated power for
watch_baseline_kim_medstores <- watch_acute %>%  filter(round == "baseline" & site == "Kimpese" & providertype == "privatepharmacy")  # filter baseline individual data
watch_baseline_kim_healthcentres <- watch_acute %>%  filter(round == "baseline" & site == "Kimpese" & providertype == "healthcentre_publique")  # filter baseline individual data
watch_baseline_nan_medstores <- watch_acute %>%  filter(round == "baseline" & site == "Nanoro" & (providertype == "informalvendor"|providertype == "privatepharmacy"))  # filter baseline individual data
watch_baseline_nan_healthcentres <- watch_acute %>%  filter(round == "baseline" & site == "Nanoro" & providertype == "healthcentre_publique")  # filter baseline individual data

# MED STORES KIMPESE
# crude, non weighted ICC based on a logistic mixed model
null_model_watch_baseline_kim_medstores <- glmer(watch ~ 1 + (1|clusterID), data = watch_baseline_kim_medstores, family = binomial(link = "logit"))
variance_between_baseline <- as.numeric(VarCorr(null_model_watch_baseline_kim_medstores)$clusterID)
variance_within_baseline <- pi^2 / 3
icc_baseline_raw <- variance_between_baseline / (variance_between_baseline + variance_within_baseline)
icc_baseline_raw
# ANOVA approach with CI
icc_baseline_anova <- ICCest(clusterID, watch, data = null_model_watch_baseline_kim_medstores, alpha = 0.05)
round(icc_baseline_anova$ICC, 4)
round(icc_baseline_anova$LowerCI, 4) #95%CI
round(icc_baseline_anova$UpperCI, 4)

# survey-weighted
# cluster-level weighted proportions at baseline
cluster_stats_baseline <- watch_baseline_kim_medstores %>%
  group_by(clusterID) %>%
  summarise(cluster_prop_weighted = weighted.mean(watch, weight, na.rm = TRUE),
            cluster_n = n(),
            cluster_weight_sum = sum(weight, na.rm = TRUE))
overall_prop_baseline <- weighted.mean(watch_baseline_kim_medstores$watch, watch_baseline_kim_medstores$weight, na.rm = TRUE)
# between-cluster variance (weighted, baseline)
between_var_baseline <- sum(cluster_stats_baseline$cluster_weight_sum * (cluster_stats_baseline$cluster_prop_weighted - overall_prop_baseline)^2) / sum(cluster_stats_baseline$cluster_weight_sum)
# within-cluster variance (weighted, baseline)
watch_baseline_with_cluster <- watch_baseline_kim_medstores %>%
  left_join(cluster_stats_baseline %>% select(clusterID, cluster_prop_weighted), 
            by = "clusterID")

within_var_baseline <- with(watch_baseline_with_cluster, sum(weight * (watch - cluster_prop_weighted)^2) / sum(weight))

total_var_baseline <- between_var_baseline + within_var_baseline
icc_baseline_weighted <- between_var_baseline / total_var_baseline
round(icc_baseline_weighted, 4) # ICC
round(overall_prop_baseline, 4) # weighted Watch prevalence at med stores in Kimpese

# MEDICINE STORES NANORO survey-weighted
# cluster-level weighted proportions at baseline
cluster_stats_baseline <- watch_baseline_nan_medstores %>%
  group_by(clusterID) %>%
  summarise(cluster_prop_weighted = weighted.mean(watch, weight, na.rm = TRUE),
            cluster_n = n(),
            cluster_weight_sum = sum(weight, na.rm = TRUE))
overall_prop_baseline <- weighted.mean(watch_baseline_nan_medstores$watch, watch_baseline_nan_medstores$weight, na.rm = TRUE)
# between-cluster variance (weighted, baseline)
between_var_baseline <- sum(cluster_stats_baseline$cluster_weight_sum * (cluster_stats_baseline$cluster_prop_weighted - overall_prop_baseline)^2) / sum(cluster_stats_baseline$cluster_weight_sum)
# within-cluster variance (weighted, baseline)
watch_baseline_with_cluster <- watch_baseline_kim_medstores %>%
  left_join(cluster_stats_baseline %>% select(clusterID, cluster_prop_weighted), 
            by = "clusterID")

within_var_baseline <- with(watch_baseline_with_cluster, sum(weight * (watch - cluster_prop_weighted)^2) / sum(weight))

total_var_baseline <- between_var_baseline + within_var_baseline
icc_baseline_weighted <- between_var_baseline / total_var_baseline
round(icc_baseline_weighted, 4) # ICC
round(overall_prop_baseline, 4) # weighted Watch prevalence at med stores in Nanoro

# HEALTH CENTRES KIMPESE survey-weighted
# cluster-level weighted proportions at baseline
cluster_stats_baseline <- watch_baseline_kim_healthcentres %>%
  group_by(clusterID) %>%
  summarise(cluster_prop_weighted = weighted.mean(watch, weight, na.rm = TRUE),
            cluster_n = n(),
            cluster_weight_sum = sum(weight, na.rm = TRUE))
overall_prop_baseline <- weighted.mean(watch_baseline_kim_healthcentres$watch, watch_baseline_kim_healthcentres$weight, na.rm = TRUE)
# between-cluster variance (weighted, baseline)
between_var_baseline <- sum(cluster_stats_baseline$cluster_weight_sum * (cluster_stats_baseline$cluster_prop_weighted - overall_prop_baseline)^2) / sum(cluster_stats_baseline$cluster_weight_sum)
# within-cluster variance (weighted, baseline)
watch_baseline_with_cluster <- watch_baseline_kim_healthcentres %>%
  left_join(cluster_stats_baseline %>% select(clusterID, cluster_prop_weighted), 
            by = "clusterID")

within_var_baseline <- with(watch_baseline_with_cluster, sum(weight * (watch - cluster_prop_weighted)^2) / sum(weight))

total_var_baseline <- between_var_baseline + within_var_baseline
icc_baseline_weighted <- between_var_baseline / total_var_baseline
round(icc_baseline_weighted, 4) # ICC
round(overall_prop_baseline, 4) # weighted Watch prevalence at med stores in Kimpese

# HEALTH CENTRES NANORO survey-weighted
# cluster-level weighted proportions at baseline
cluster_stats_baseline <- watch_baseline_nan_healthcentres %>%
  group_by(clusterID) %>%
  summarise(cluster_prop_weighted = weighted.mean(watch, weight, na.rm = TRUE),
            cluster_n = n(),
            cluster_weight_sum = sum(weight, na.rm = TRUE))
overall_prop_baseline <- weighted.mean(watch_baseline_nan_healthcentres$watch, watch_baseline_nan_healthcentres$weight, na.rm = TRUE)
# between-cluster variance (weighted, baseline)
between_var_baseline <- sum(cluster_stats_baseline$cluster_weight_sum * (cluster_stats_baseline$cluster_prop_weighted - overall_prop_baseline)^2) / sum(cluster_stats_baseline$cluster_weight_sum)
# within-cluster variance (weighted, baseline)
watch_baseline_with_cluster <- watch_baseline_nan_healthcentres %>%
  left_join(cluster_stats_baseline %>% select(clusterID, cluster_prop_weighted), 
            by = "clusterID")

within_var_baseline <- with(watch_baseline_with_cluster, sum(weight * (watch - cluster_prop_weighted)^2) / sum(weight))

total_var_baseline <- between_var_baseline + within_var_baseline
icc_baseline_weighted <- between_var_baseline / total_var_baseline
round(icc_baseline_weighted, 4) # ICC
round(overall_prop_baseline, 4) # weighted Watch prevalence at med stores in Nanoro

#### 8. INTERACTION BETWEEN SUBGROUPS AND STUDY ARMS ####
library(MASS) # only load now to avoid a conflict with dplyr
library(sandwich)
library(lmtest)
# check interaction with "site"
nb_model_watch <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata + providertype + site*round*intervention, # checking interaction between intervention DiD and site
  data = watch_acute_offset)
summary(nb_model_watch)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch, vcov = vcovCL, cluster = ~clusterID)
robust_results[grep("roundpost:interventionintervention:siteNanoro", rownames(robust_results)), "Pr(>|z|)"] # p value

# check interaction with "providertype"
nb_model_watch <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata + providertype*round*intervention, # checking interaction between intervention DiD and site
  data = watch_acute_offset)
summary(nb_model_watch)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch, vcov = vcovCL, cluster = ~clusterID)
robust_results[grep("roundpost:interventionintervention:providertypeprivateclinic", rownames(robust_results)), "Pr(>|z|)"] # p value for private clinics vs ref (healthcentres)
robust_results[grep("roundpost:interventionintervention:providertypeprivatepharmacy", rownames(robust_results)), "Pr(>|z|)"] # p value for private clinics vs ref (healthcentres)
robust_results[grep("roundpost:interventionintervention:providertypeinformalvendor", rownames(robust_results)), "Pr(>|z|)"] # p value for private clinics vs ref (healthcentres)
# Wald test
library(clubSandwich)
V <- vcovCR(nb_model_watch, cluster = watch_acute_offset$clusterID, type = "CR2")
threeway_terms <- grep("roundpost:interventionintervention:providertype", names(coef(nb_model_watch)), value = TRUE)
threeway_terms
Wald_test(nb_model_watch, constraints = threeway_terms, vcov = V)
# checking private clinics vs informal vendors
watch_acute_offset$providertype <- relevel(factor(watch_acute_offset$providertype), ref = "privateclinic")
nb_model_watch <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention + strata + providertype*round*intervention, # checking interaction between intervention DiD and site
  data = watch_acute_offset)
summary(nb_model_watch)
# cluster-robust standard errors
robust_results <- coeftest(nb_model_watch, vcov = vcovCL, cluster = ~clusterID)
robust_results[grep("roundpost:interventionintervention:providertypeinformalvendor", rownames(robust_results)), "Pr(>|z|)"] # p value for informal vendors vs. private clinics
