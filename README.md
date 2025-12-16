# Optimising Community Antibiotic Use and Infection Control With Behavioural Interventions in Burkina Faso and DR Congo (CABU-EICO project)
This repository contains the study protocol, questionnaires, simulated patient scenarios and checklists, intervention guide, pseudonymized data, and analysis scripts. 

## Study registration 
https://clinicaltrials.gov/ct2/show/NCT05378880

## Project summary
We developed and evaluated a behavioural intervention bundle, targeting any community-level healthcare or medicine providers and communities, to optimise antibiotic use and improve hygiene, and hence reduce AMR prevalence and transmission. After a 6-month local co-development phase, the intervention was implemented over 12 months in 22 clusters (villages or neighbourhoods) within health demographic surveillance sites in Nanoro, Burkina Faso and Kimpese, DR Congo. In a cluster RCT, we compared the 22 intervention with 22 control clusters. The primary outcome measure is the change in Watch antibiotic provision from medicine stores (where a formal prescription is not required), assessed via patient exit interviews and simulated client visits. Changes in hygiene practices and AMR pathogen and gene carriage were assessed in repeated household surveys. Using modelling and sequencing of selected human and rodent isolates, the study quantified how changes in antibiotic use and hygiene practices impact AMR transmission.

## Timeline
Data and sample collection: 01/06/2022 to 31/05/2024. 
Patient surveys during the baseline period: Oct 26, 2022 to Mar 13, 2023
Patient surveys post-intervention: Nov 6, 2023 to Apr 3, 2024

## Manuscripts
- Setting a realistic AWaRe target for primary care antibiotic use in LMIC http://dx.doi.org/10.1016/S1473-3099(23)00002-6
- Study protocol http://doi.org/10.1186/s13063-023-07856-2
- Knowledge, perceptions and practices of informal medicine vendors regarding over- the- counter distribution of antibiotics and antibiotic resistance in Nanoro District, Burkina Faso: an exploratory qualitative study https://doi.org/10.1136/bmjopen-2025-105394
- Effect of a community-based behavioural intervention bundle to improve antibiotic use and patient management in Burkina Faso and DR Congo: a cluster randomized controlled trial (preprint):
- Effect of a community-based intervention bundle on household transmission of Extended Spectrum Beta-lactamase-producing E. coli in rural Burkina Faso - a cluster randomised trial (preprint): 

## Data analysis scripts
Script are in R. Analyses can be run on the open anonymized dataframes available on this repository.
1. "/patientsurvey_public.R" analyses patient survey data pre and post intervention, calculating survey weighted antibiotic use estimates and adjusted prevalence ratios 
2. "/mysterypatients_public.R" does the simulated patient data analysis, estimating patient management scores and the difference-in-difference change in score because of the intervention

## Data
1. "simulatedpatients.xlsx" has the data with total scores (variable score) from the simulated patient visits, by infection scenario (clinpres), type of provider (providertype), round (pre, post)
2. "watch.xlsx" has the patient survey data with a single row per patient, indicating whether the patient received at least one antibiotic (variable antibiotic) or at least one Watch antibiotic (variable watch)
3. "patient_ab.xlsx" has thez patient survey data, but now specifying the antibiotics used. If the patient received multiple antibiotics, there are one row for each antibiotic received.
