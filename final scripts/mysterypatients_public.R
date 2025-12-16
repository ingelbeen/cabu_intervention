#####################################################
# CABU-EICO simulated patient visits                #
#####################################################

# install/load packages
pacman::p_load(readxl, writexl, lubridate, haven, dplyr, tidyr, digest, ggplot2, survey, srvyr, gtsummary, lme4, broom.mixed, stringr, lmtest, car, sandwich, emmeans, geepack)
# IMPORT DATA
all <- read_excel("mysterypatients.xlsx")
# ANALYSE DATA 
# n visits at baseline and post intervention
all %>%
  group_by(round) %>%
  summarise(n())
# n visits by site, round, and intervention/control
all %>%
  group_by(site, round, intervention) %>%
  summarise(n())
# per intervention vs control group 
all %>% 
  filter(round == "pre") %>%
  group_by(intervention) %>%
  summarise(n())
# per intervention vs control group 
all %>% 
  filter(round == "post") %>%
  group_by(intervention) %>%
  summarise(n())
# per provider type
all %>% 
  filter(round == "pre") %>%
  filter(clinpres == "diarrhoea") %>%
  group_by(providertype) %>%
  summarise(n())
all %>% 
  filter(round == "post") %>%
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
    ci_upper = mean_score + qt(0.975, df = count - 1) * se)

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
    ci_upper = mean_score + qt(0.975, df = count - 1) * se)

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
    ci_upper = mean_score + qt(0.975, df = count - 1) * se)

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
  IQR_score = IQR(score, na.rm = TRUE))

# histogram overall scores
all$providertype[all$providertype=="community pharmacy/store"] <- "community pharmacy"
means <- all %>% select(site, providertype, intervention, round, clinpres, score) %>% 
  filter(!is.na(score)) %>%
  group_by(site, providertype, clinpres, intervention, round) %>%
  summarize(mean_score = mean(score), .groups = 'drop')
means

ggplot(all, aes(x = score, fill = round)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  # geom_density(alpha = 0.5) +
  labs(x = "patient management score",
       y = "frequency") +
  facet_grid(paste(site, providertype, intervention, sep = "\n") ~clinpres) + # , nrow = 5, ncol = 2
  geom_vline(data = means, aes(xintercept = mean_score, color = round), 
             linewidth = 0.7) +
  scale_fill_manual(values = c("pre" = "#FF6666", "post" = "#800000")) +
  scale_color_manual(values = c("pre" = "#FF6666", "post" = "#800000")) + # previous red and blue colours: #F8766D #5f9ea0
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),  strip.text = element_text(size = 10),
        panel.border = element_rect(color = "darkgrey", fill = NA, linewidth = 0.5),   legend.position = "bottom")

ggsave("patient_mgmt_scores_by_site_provider_intervention.jpeg", plot = last_plot(), dpi = 300, width = 11, height = 15)

# histogram overall scores combining sites and only show pre vs post in intervention group
means_sitescombined_intervention <- all %>% select(providertype, intervention, round, clinpres, score) %>% 
  filter(!is.na(score)) %>%
  filter(intervention=="intervention") %>%
  group_by(providertype, clinpres, round) %>%
  summarize(mean_score = mean(score), .groups = 'drop')
means_sitescombined_intervention

ggplot(all, aes(x = score, fill = round)) +
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
ggsave("density_scores_intervention_sitescombined.pdf", plot = last_plot(), width = 12, height = 8, units = "in")

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

# exclude UTI in Nanoro (mistake in scenario)
# set reference values
all_notutinanoro$providertype <- relevel(factor(all_notutinanoro$providertype), ref = "health centre")
all_notutinanoro$round  <- factor(all_notutinanoro$round, levels = c("pre", "post"))
all_notutinanoro$intervention <- factor(all_notutinanoro$intervention, levels = c("control", "intervention"))
# linear regression for difference in difference in score
linregmodel_all_together <- lm(score ~ intervention*round + site + providertype + clinpres, data= all_notutinanoro) 
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
# check residuals for non-linearity and heteroskedasticity
plot(linregmodel, which = 1)  # non-linearity -> flat line ok, a curve suggests non-linearity
# car::crPlots(linregmodel)
lmtest::resettest(linregmodel) # Ramsey reset test for mis-specification (non-linearity, omitted variables)
plot(linregmodel, which = 3) # heteroskedasticity
lmtest::bptest(linregmodel) # Breusch–Pagan test (p < 0.05 → heteroskedasticity)
lmtest::bptest(linregmodel, ~ fitted(linregmodel) + I(fitted(linregmodel)^2)) # nonlinear forms of heteroskedasticity
# check if impossible values in the model (<-5 or >32)
pred <- predict(linregmodel)
which(pred < -5 | pred > 32) # none -> ok
plot(pred, hc$score,
     xlab = "Predicted score",
     ylab = "Observed score")
abline(h = c(-5, 32), col = "red")

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
# check residuals for non-linearity and heteroskedasticity
plot(linregmodel, which = 1)  # non-linearity -> flat line ok, a curve suggests non-linearity
# car::crPlots(linregmodel)
lmtest::resettest(linregmodel) # Ramsey reset test for mis-specification (non-linearity, omitted variables)
plot(linregmodel, which = 3) # heteroskedasticity
lmtest::bptest(linregmodel) # Breusch–Pagan test (p < 0.05 → heteroskedasticity)
lmtest::bptest(linregmodel, ~ fitted(linregmodel) + I(fitted(linregmodel)^2)) # nonlinear forms of heteroskedasticity
# check if impossible values in the model (<-5 or >32)
pred <- predict(linregmodel)
which(pred < -5 | pred > 32) # none -> ok
plot(pred, hc$score,
     xlab = "Predicted score",
     ylab = "Observed score")
abline(h = c(-5, 32), col = "red")

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

# 3-way interaction, by provider type
model_3way <- lm(score ~ intervention * round * providertype + clinpres + site, 
                 data = all)
summary(model_3way)
emm_3way <- emmeans(model_3way, ~ intervention * round | providertype)
did_results <-  contrast(emm_3way, interaction = "pairwise", by = "providertype")
summary(did_results, infer = TRUE)
# check parallel trends
plot_data <- aggregate(score ~ intervention + round + providertype, 
                       data = all, FUN = mean)
ggplot(all, aes(x = round, y = score, 
                      color = interaction(intervention, providertype), 
                      group = interaction(intervention, providertype))) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Check for parallel trends by provider type")
# check colinearity and heteroskedasticity
bptest(model_3way)
vif(model_3way)

# instead of linear regression, use GEE= generalised estimating equations 
# cluster by clinpres + providertype + site
all$cluster_id <- interaction(all$site, all$clinpres, all$providertype, drop = TRUE)
cat("Number of clusters:", length(unique(all$cluster_id)), "\n") # check number of clusters

gee_model <- geeglm(score ~ intervention * round * providertype * clinpres + site,
                    data = all,
                    id = cluster_id,  # combined clustering variable
                    family = gaussian,
                    corstr = "exchangeable")
# DiD scores by type of provider
summary(gee_model)
emm_gee <- emmeans(gee_model, ~ intervention * round | providertype)
contrast(emm_gee, interaction = "pairwise", by = "providertype",  infer = c(TRUE, TRUE))
# DiD scores overall change
emm_gee <- emmeans(gee_model, ~ intervention * round)
contrast(emm_gee, interaction = "pairwise", infer = c(TRUE, TRUE))


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