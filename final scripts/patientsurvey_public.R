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
  pivot_wider(
    names_from = c(site, intervention, round),
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
  summarise(
    count_antibiotic = sum(antibiotic == 1),
    total_count = n(),
    percentage_antibiotic = round((count_antibiotic / total_count) * 100, 1)) %>%
  mutate(combined_counts = paste(count_antibiotic, total_count, sep = "/")) %>%
  select(site, intervention, providertype, round, combined_counts, percentage_antibiotic) %>%
  pivot_wider(
    names_from = c(site, intervention),
    values_from = c(combined_counts, percentage_antibiotic),
    names_sep = "_" )
summary_antibioticcounts <- summary_antibioticcounts %>% select(c("providertype","round","combined_counts_Kimpese_control","percentage_antibiotic_Kimpese_control",
                                                        "combined_counts_Kimpese_intervention", "percentage_antibiotic_Kimpese_intervention", "combined_counts_Nanoro_control",
                                                        "percentage_antibiotic_Nanoro_control", "combined_counts_Nanoro_intervention","percentage_antibiotic_Nanoro_intervention" )) 
summary_antibioticcounts
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
write.table(clinpres_watchcounts, "clinpres_watchcounts.txt")
write_xlsx(clinpres_watchcounts, "clinpres_watchcounts.xlsx")

# 5.3 crude prevalence by clinical presentation, by intervention/control group, both site combined, and pre- vs. post intervention
# any antibiotic
clinpres_antibioticcounts <- watch_acute %>%
  group_by(clinpres_broad, intervention, round) %>%
  summarise(
    count_antibiotic = sum(antibiotic == 1),
    total_count = n(),
    percentage_antibiotic = round((count_antibiotic / total_count) * 100, 1)
  ) %>%
  mutate(combined_counts = paste(count_antibiotic, total_count, sep = "/")) %>%
  select(clinpres_broad, intervention, round, combined_counts, percentage_antibiotic) %>%
  pivot_wider(
    names_from = c(intervention, round),
    values_from = c(combined_counts, percentage_antibiotic),
    names_sep = "_"
  )
clinpres_antibioticcounts
write_xlsx(clinpres_antibioticcounts, "clinpres_antibioticcounts.xlsx")

# watch
clinpres_watchcounts <- watch_acute %>%
  group_by(clinpres_broad, intervention, round) %>%
  summarise(
    count_watch = sum(watch == 1),
    total_count = n(),
    percentage_watch = round((count_watch / total_count) * 100, 1)
  ) %>%
  mutate(combined_counts = paste(count_watch, total_count, sep = "/")) %>%
  select(clinpres_broad, intervention, round, combined_counts, percentage_watch) %>%
  pivot_wider(
    names_from = c(intervention, round),
    values_from = c(combined_counts, percentage_watch),
    names_sep = "_"
  )
clinpres_watchcounts

#### 4. FIGURE 2. PREVALENCE ANY ANTIBIOTIC & WATCH AND RR OVERALL, BY SITE, BY TYPE OF PROVIDER, AND BY INFECTION ####
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
# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
library(MASS) # only load now to avoid a conflict with dplyr
nb_model_subgroup <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset)
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
  CI_Upper = rr_ci_upper)
print(overall_effect, row.names = FALSE)

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

# subsetting by type of provider - gives the same result for health centres but wider 95%CI 
# health centre
surveydesign_subsethealthcentre <- svydesign(id = ~clusterID, data = watch_acute_offset[watch_acute_offset$providertype=="healthcentre_publique",], weights = ~weight)
# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
library(MASS) # only load now to avoid a conflict with dplyr
nb_model_subsethealthcentre <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention,
  weights = surveydesign_subsethealthcentre$weights,
  data = watch_acute_offset[watch_acute_offset$providertype=="healthcentre_publique",])
# get the model coefficients
coef <- coef(nb_model_subsethealthcentre)
coeci <- confint(nb_model_subsethealthcentre)
rr <- exp(coef)
rr
ci_rr <- exp(coeci)
ci_rr
# private clinics
surveydesign_subsetprivateclinic <- svydesign(id = ~clusterID, data = watch_acute_offset[watch_acute_offset$providertype=="privateclinic",], weights = ~weight)
# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
nb_model_subsetprivateclinic <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention,
  weights = surveydesign_subsetprivateclinic$weights,
  data = watch_acute_offset[watch_acute_offset$providertype=="privateclinic",])
# get the model coefficients
coef <- coef(nb_model_subsetprivateclinic)
coeci <- confint(nb_model_subsetprivateclinic)
rr <- exp(coef)
rr
ci_rr <- exp(coeci)
ci_rr
# private pharmacy
surveydesign_subsetprivatepharmacy <- svydesign(id = ~clusterID, data = watch_acute_offset[watch_acute_offset$providertype=="privatepharmacy",], weights = ~weight)
# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
nb_model_subsetprivatepharmacy <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention,
  weights = surveydesign_subsetprivatepharmacy$weights,
  data = watch_acute_offset[watch_acute_offset$providertype=="privatepharmacy",]
)
# get the model coefficients
coef <- coef(nb_model_subsetprivatepharmacy)
coeci <- confint(nb_model_subsetprivatepharmacy)
rr <- exp(coef)
rr
ci_rr <- exp(coeci)
ci_rr
# informalvendor
surveydesign_subsetinformalvendor <- svydesign(id = ~clusterID, data = watch_acute_offset[watch_acute_offset$providertype=="informalvendor",], weights = ~weight)
# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
nb_model_subsetinformalvendor <- glm.nb(
  n_antibiotic ~ offset(log(pop_patients)) + round * intervention,
  weights = surveydesign_subsetinformalvendor$weights,
  data = watch_acute_offset[watch_acute_offset$providertype=="informalvendor",]
)
# get the model coefficients
coef <- coef(nb_model_subsetinformalvendor)
coeci <- confint(nb_model_subsetinformalvendor)
rr <- exp(coef)
rr
ci_rr <- exp(coeci)
ci_rr

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
library(MASS) # only load now to avoid a conflict with dplyr
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
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="OVERALL, weighted"] <- "PR 0·48 (95%CI 0·26-0·88)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="Kimpese"] <- "PR 0·52 (95%CI 0·21-1·3)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="Nanoro"] <- "PR 0·41 (95%CI 0·18-0·90)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="health centre"] <- "PR 0·41 (95%CI 0·14-1·2)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="private clinic"] <- "PR 0·23 (95%CI 0·03-2·4)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="community pharmacy/store"] <- "PR 0·59 (95%CI 0·18-1·9)"
prevalence_estimates$pr[prevalence_estimates$intervention=="intervention"&prevalence_estimates$round=="post"&prevalence_estimates$subgroup=="informal vendor"] <- "PR 0·79 (95%CI 0·30-2·1)"

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

# fit negative binomial model with survey weights (using glm.nb instead of svyglm because the svyglm does not allow for negative binomial regression)
library(MASS) # only load now to avoid a conflict with dplyr
nb_model_subgroup <- glm.nb(
  n_watch ~ offset(log(pop_patients)) + round * intervention, 
  weights = surveydesign$weights,
  data = watch_acute_offset)
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
library(MASS) # only load now to avoid a conflict with dplyr
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
  CI_Upper = rr_ci_upper)
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
# subset health centres
watch_acute_offset_healthcentre <- watch_acute_offset[watch_acute_offset$providertype=="healthcentre_publique",]
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_healthcentre, weights = ~weight)
library(MASS) # only load now to avoid a conflict with dplyr
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

# subset informal vendors -> since there is a subgroup with n_watch 0 (post in the control group), add a small continuity correction 
watch_acute_offset_informalvendor <- watch_acute_offset %>% filter(providertype=="informalvendor" & site=="Nanoro")
surveydesign <- svydesign(id = ~clusterID, data = watch_acute_offset_informalvendor, weights = ~weight)
watch_acute_offset_informalvendor$n_watch_adj <-  watch_acute_offset_informalvendor$n_watch + 0.5
nb_model_subgroup <- glm.nb(
  n_watch_adj ~ offset(log(pop_patients)) + round * intervention,
  weights = surveydesign$weights,
  data = watch_acute_offset_informalvendor)

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
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="OVERALL, weighted"] <- "PR 0·29 (95%CI 0·10-0·82)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="Kimpese"] <- "PR 0·35 95%CI (0·14-0·92)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="Nanoro"] <- "PR 0·30 95%CI (0·06-1·44)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="health centre"] <- "PR 0·40 (95%CI 0·06-2·45)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="private clinic"] <- "PR 0·71, 95%CI (0·15-3·28)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="community pharmacy/store"] <- "PR 0·38 (95%CI 0·10-1·43)"
watchprevalence_estimates$pr[watchprevalence_estimates$intervention=="intervention"&watchprevalence_estimates$round=="post"&watchprevalence_estimates$subgroup=="informal vendor"] <- "PR 1·1 (95%CI 0·24-4·9)*"

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
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="malaria"] <- "PR 0·20 (95%CI 0·06-0·68)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="acute respiratory infection"] <- "PR 0·69 (95%CI 0·28-1·7)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="skin/soft tissue infection"] <- "PR 1·3	(95%CI 0·44-4·0)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="unexplained fever"] <- "PR 0·28 (95%CI 0·09-0·83)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="non-bacterial infections"] <- "PR 0·27 (95%CI 0·07-1·2)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="unexplained gastro-intestinal"] <- "PR 0·34 (95%CI 0·08-1·5)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="non-specific symptoms"] <- "PR 1·2 95%CI 0·44-3·6)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="typhoid or sepsis"] <- "PR 0·49 (95%CI 0·12-2·0)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="urinary tract infection"] <- "PR 0·38 (95%CI 0·08-1·9)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="gastroenteritis"] <- "PR 0·27 (95%CI 0·09-0·77)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="pneumonia"] <- "PR 1·3 (95%CI 0·14-12)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="sexually transmitted infection"] <- "PR 0·14 (95%CI 0·01-2·1)"
prevalence_estimates_clinpres_plot$pr[prevalence_estimates_clinpres_plot$intervention=="intervention"&prevalence_estimates_clinpres_plot$round=="post"&prevalence_estimates_clinpres_plot$clinpres_broad=="dental"] <- "PR 2·6 (95%CI 0·14-47)"

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
    names_glue = "{.value}_{intervention}_{round}"
  ) 
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
library(MASS) # only load now to avoid a conflict with dplyr
results_list <- list()
for (clinpres in clinpres_categories) {
    data_subset <- watch_acute_clinpres_offset %>% 
    filter(clinpres_broad == clinpres)
    if (nrow(data_subset) == 0) {
    warning(paste("No data for:", clinpres))
    next}
    surveydesign <- svydesign(id = ~clusterID, data = data_subset, weights = ~weight)
    tryCatch({
    nb_model <- glm.nb(
      n_antibiotic ~ offset(log(pop_patients)) + round * intervention, 
      weights = surveydesign$weights,
      data = data_subset)
    coef_summary <- summary(nb_model)$coefficients # nb model output
    interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE]
    log_coef <- interaction_coefs[1]
    se_coef <- interaction_coefs[2]
    rr <- exp(log_coef)
    rr_ci_lower <- exp(log_coef - 1.96 * se_coef)
    rr_ci_upper <- exp(log_coef + 1.96 * se_coef)
    results_list[[clinpres]] <- data.frame( # results in a dataframe that can be appended for all infections
      ClinicalPresentation = clinpres,
      RiskRatio = round(rr, 2),
      ci = paste(round(rr_ci_lower, 2),"-",round(rr_ci_upper, 2)),
      p_value = round(interaction_coefs[4], 4))}, error = function(e) {
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


# estimate RR
# loop for each infection subset
results_list <- list()
for (clinpres in clinpres_categories) {
  cat("\n Processing:", clinpres, "\n")
    data_subset <- watch_acute_clinpres_offset %>% 
    filter(clinpres_broad == clinpres)
    if (nrow(data_subset) == 0) {
    cat("  -> No data available\n")
    next}
  cat("  -> N observations:", nrow(data_subset), "\n")
  cat("  -> N watch antibiotics:", sum(data_subset$n_watch, na.rm = TRUE), "\n")
  surveydesign <- svydesign(
    id = ~clusterID, 
    data = data_subset, 
    weights = ~weight)
  tryCatch({
    nb_model <- glm.nb(
      n_watch ~ offset(log(pop_patients)) + round * intervention, 
      weights = surveydesign$weights,
      data = data_subset,
      control = glm.control(maxit = 100, trace = FALSE))  
    # Check convergence
    if (!nb_model$converged) {
      cat("  -> WARNING: Model did not converge\n")}
    coef_summary <- summary(nb_model)$coefficients
    interaction_coefs <- coef_summary[grep("roundpost:intervention", rownames(coef_summary)), , drop = FALSE]
    if (nrow(interaction_coefs) == 0) {
      cat("  -> No interaction term found\n")
      next}
    log_coef <- interaction_coefs[1]
    se_coef <- interaction_coefs[2]
    rr <- exp(log_coef)
    rr_ci_lower <- exp(log_coef - 1.96 * se_coef)
    rr_ci_upper <- exp(log_coef + 1.96 * se_coef)
    results_list[[clinpres]] <- data.frame(
      ClinicalPresentation = clinpres,
      RiskRatio = round(rr, 2),
      ci = paste(round(rr_ci_lower, 2),"-",round(rr_ci_upper, 2)),
      p_value = round(interaction_coefs[4], 4),
      N_obs = nrow(data_subset),
      Converged = nb_model$converged)
    cat("  -> Model fitted successfully\n")}, error = function(e) {cat("  -> ERROR:", e$message, "\n")})}
# append all
watchprevalence_ratios_by_clinpres <- do.call(rbind, results_list)
rownames(watchprevalence_ratios_by_clinpres) <- NULL
print(watchprevalence_ratios_by_clinpres)
# show which models had convergence issues
if (any(!watchprevalence_ratios_by_clinpres$Converged)) {
  cat("\n WARNING: The following models did not converge:\n")
  print(watchprevalence_ratios_by_clinpres[!watchprevalence_ratios_by_clinpres$Converged, c("ClinicalPresentation", "N_obs")])}
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
prevalence_estimates_clinpres_merge <- prevalence_estimates_clinpres_plot %>% select(-ci) %>% select(-antibiotic)
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

# make plot
subgroups_plot <- ggplot(prevalence_estimates_allsubgroups, aes(x = prevalence, y = position)) +
  geom_point(
    aes(color = interventionround), 
    size = 3  ) +  
  geom_errorbarh(
    aes(xmin = ci_l, xmax = ci_u, color = interventionround), 
    height = 0.1 ) +   
  scale_y_continuous(
    breaks = as.numeric(prevalence_estimates_allsubgroups$subgroup),
    labels = prevalence_estimates_allsubgroups$subgroup,) +
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
            size = 2.5) +
  geom_hline(yintercept = c(18.5,16.5,12.5), linetype = "solid", color = "#4D4D4D")
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
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="malaria"] <- "PR 0·35 (95%CI 0·07-1·9)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="acute respiratory infection"] <- "PR 0·20 (95%CI 0·03-1·3)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="skin/soft tissue infection"] <- "PR 0·54	(95%CI 0·12-2·4)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="unexplained fever"] <- "PR 0·19 (95%CI 0·04-0·91)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="non-bacterial infections"] <- "PR 1·2 (95%CI 0·27-5·7)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="unexplained gastro-intestinal"] <- "PR 0·21 (95%CI 0·02-2·2)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="non-specific symptoms"] <- "PR 1·2 (95%CI 0·24-6·2)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="typhoid or sepsis"] <- "PR 0·44 (95%CI 0·08-2·4)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="urinary tract infection"] <- "PR 0·18 (95%CI 0·03-1·05)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="gastroenteritis"] <- "PR 0·66 (95%CI 0·14-3·1)"
watchprevalence_estimates_clinpres_plot$pr[watchprevalence_estimates_clinpres_plot$intervention=="intervention"&watchprevalence_estimates_clinpres_plot$round=="post"&watchprevalence_estimates_clinpres_plot$clinpres_broad=="sexually transmitted infection"] <- "PR 0·16 (95%CI 0·01-5·1)"

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
# make plot
subgroups_plot <- ggplot(watchprevalence_estimates_allsubgroups, aes(x = prevalence, y = position)) +
  geom_point(
    aes(color = interventionround), 
    size = 3  ) +  
  geom_errorbarh(
    aes(xmin = ci_l, xmax = ci_u, color = interventionround), 
    height = 0.1 ) +   
  scale_y_continuous(
    breaks = as.numeric(watchprevalence_estimates_allsubgroups$subgroup),
    labels = watchprevalence_estimates_allsubgroups$subgroup,) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    name = "Prevalence use of Watch antibiotics (%)") +  
  scale_color_manual(values = c("lightgrey", "darkgrey", "#FF6666", "#800000")) +
  guides(color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 10)) +
  labs(y = element_blank(), color = "") +
  geom_text(aes(label = pr),
            hjust = 0.2,       # shift label a bit to the right of the point
            vjust = 2.7,        # vertical alignment
            size = 2.5) +
  geom_hline(yintercept = c(18.5,16.5,12.5), linetype = "solid", color = "#4D4D4D")
subgroups_plot
ggsave("subgroups_watch_plot.jpeg", plot = subgroups_plot, width = 7, height = 9, dpi = 300)


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
