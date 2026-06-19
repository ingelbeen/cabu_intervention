#######################################################################
# use of SP data - research note                                        #
# frequency of itms and changes in score - pneumonia and diarrhoea    #
#######################################################################

#   install.packages(c("tidyverse", "readxl", "ggnewscale", "cowplot", "ggtext"))
library(tidyverse)
library(readxl)
library(ggnewscale)
library(cowplot)
library(ggtext)


pneumonia <- read_excel("~/public data/infection scenario specific simulated patient data Nanoro/pneumonia_anon.xlsx")
# subset only Nanoro - in Kimpese pneumonia was only tested at pharmacies
pneumonia <- pneumonia %>% filter(site=="nanoro")
# keep only intervention clusters
pneumonia <- pneumonia %>% filter(intervention=="intervention")
# tabulate
table(pneumonia$intervention, pneumonia$round)

#### PNEUMONIA FIG A - FREQUENCY OF ITEMS ON CHECKLIST #####
# recode scores to be binary, so a count/frequency is possible
df <- pneumonia %>%  mutate(
    hx_type_cough     = as.integer(`TypeCough?`           > 0),
    hx_duration_cough = as.integer(`DurationCough?`       > 0),
    hx_worsened       = as.integer(`CoughWorsened?`       > 0),
    hx_fever          = as.integer(`Fever?`               > 0),
    hx_sputum         = as.integer(`Sputum?`              > 0),
    hx_rr             = as.integer(`RR?`                  > 0),
    hx_difficulty     = as.integer(`DifficultBreathing?`  > 0),
    hx_confusion      = as.integer(`ConfusionPresent?`    > 0),
    hx_lethargy       = as.integer(`Lethargic?`           > 0),
    hx_resp_disease   = as.integer(`HistroyRespDisease?`  > 0),
    act_referral      = as.integer(`ReferralHospitalHC`   > 0),
    act_explain       = as.integer(`SeekAppropriateCare!` > 0),
    disp_ab           = as.integer(ab == 1),
    disp_antimal      = as.integer(AntimalariaWithoutDiagnose == -1))

# add full question labels
items <- tribble(
  ~col,               ~label,                                      ~section,
  "hx_type_cough",    "Type of cough: dry or productive",          "Medical history",
  "hx_duration_cough","Duration of cough, acute onset",            "Medical history",
  "hx_worsened",      "Has the cough worsened?",                   "Medical history",
  "hx_fever",         "Fever/hot body?",                           "Medical history",
  "hx_sputum",        "Sputum (mucous/mucopurulent)?",             "Medical history",
  "hx_rr",            "Respiratory rate?",                         "Medical history",
  "hx_difficulty",    "Difficulty breathing/shortness of breath?", "Medical history",
  "hx_confusion",     "Confusion or disorientation?",              "Medical history",
  "hx_lethargy",      "Lethargy/signs of resp. distress?",         "Medical history",
  "hx_resp_disease",  "History of respiratory disease?",           "Medical history",
  "act_referral",     "Referral to hospital or health centre",     "Actions",
  "act_explain",      "Explains need to seek appropriate care",    "Actions",
  "disp_ab",          "Gives antibiotics for home treatment",      "Dispensing",
  "disp_antimal",     "Antimalarial without malaria diagnosis",    "Dispensing")

# y-axis order including domain subtitles
sections_order <- c("Medical history", "Actions", "Dispensing")
section_header_labels <- c(
  "Medical history" = "── MEDICAL HISTORY QUESTIONS ──────────────────",
  "Actions"         = "── ACTIONS ─────────────────────────────────────",
  "Dispensing"      = "── DISPENSING OF MEDICINES ──────────────────────")
y_levels <- c()
for (sec in sections_order) {
  y_levels <- c(y_levels,
                section_header_labels[[sec]],
                items %>% filter(section == sec) %>% pull(label))
}
# Reverse so that top of plot = first element
y_levels_rev <- rev(y_levels)

# provider labels, combining pharmacies and informal vendors in a single column
df <- df %>%  mutate(
    providertype = case_when(
      providertype == "health centre"            ~ "Health centre",
      providertype %in% c("community pharmacy/store",
                          "informal vendor")     ~ "Medicine vendor",
      TRUE ~ providertype),
    providertype = factor(providertype,
      levels = c("Health centre", "Medicine vendor")),
    round = case_when(
      tolower(round) %in% c("pre",  "before") ~ "Pre",
      tolower(round) %in% c("post", "after")  ~ "Post",
      TRUE ~ round),
    round = factor(round, levels = c("Pre", "Post")))

# colours
col_pre  <- c("Health centre"   = "#8FD4BC",
              "Medicine vendor" = "#C9B8E8")
col_post <- c("Health centre"   = "#1D9E75",
              "Medicine vendor" = "#534AB7")

# combined named vector for scale_fill_manual
fill_vals <- c(
  "Health centre_Pre"    = "#8FD4BC",
  "Health centre_Post"   = "#1D9E75",
  "Medicine vendor_Pre"  = "#C9B8E8",
  "Medicine vendor_Post" = "#534AB7")

# summarize data to frequencies
long <- df %>%
  select(providertype, round, all_of(items$col)) %>%
  pivot_longer(c(-providertype, -round),
               names_to = "col", values_to = "checked") %>%
  left_join(items, by = "col")

freq <- long %>%
  group_by(label, section, providertype, round) %>%
  summarise(
    n_checked = sum(checked, na.rm = TRUE),
    n_visits  = n(),
    pct       = n_checked / n_visits * 100,
    .groups   = "drop"
  ) %>%
  mutate(
    fill_key = paste0(providertype, "_", round),
    fill_key = factor(fill_key, levels = names(fill_vals)),
    # Set y positions using the shared factor (items only, no headers here)
    y_pos = factor(label, levels = y_levels_rev)  )

# add header rows for domains
header_df <- tibble(
  y_pos   = factor(section_header_labels[sections_order], levels = y_levels_rev),
  section = sections_order,
  fill_bg = c("#DCE8F5", "#F0E6DC", "#F5DCDC"))

# section background fill for header rows (one row of tiles per section header)
section_item_bg <- items %>%
  mutate(
    y_pos  = factor(label, levels = y_levels_rev),
    sec_bg = case_when(
      section == "Medical history" ~ "#F8FAFD",
      section == "Actions"         ~ "#FDF8F5",
      section == "Dispensing"      ~ "#FDF5F5"))

# counts/denominators for column headers 
ns <- df %>%
  count(providertype, round) %>%
  pivot_wider(names_from = round, values_from = n, values_fill = 0)

col_labels <- ns %>%
  mutate(lbl = paste0(providertype, "\npre n=", Pre, " / post n=", Post)) %>%
  arrange(factor(providertype, levels = c("Health centre", "Medicine vendor"))) %>%
  select(providertype, lbl)

freq <- freq %>%
  left_join(col_labels, by = "providertype") %>%
  mutate(lbl = factor(lbl, levels = col_labels$lbl))

header_df2 <- cross_join(
  header_df,
  col_labels %>% mutate(lbl = factor(lbl, levels = col_labels$lbl)))
# Text labels only in the first (leftmost) provider panel
first_lbl <- col_labels$lbl[1]
header_df2_text <- header_df2 %>% filter(lbl == first_lbl)

# plot
p <- ggplot(freq, aes(x = pct, y = y_pos)) +
  geom_tile(
    data = header_df2,
    aes(x = 50, y = y_pos, fill = fill_bg),
    width = Inf, height = 0.9,
    inherit.aes = FALSE) +# domain header rows: coloured background tile 
  scale_fill_identity(guide = "none") +
  new_scale_fill() +
  geom_text(
    data = header_df2_text,
    aes(x = 1, y = y_pos, label = section),
    hjust = 0, size = 3.2, fontface = "bold",
    colour = "#333333", inherit.aes = FALSE) + # domain header text
  # # -- Alternating item row backgrounds --
  # geom_tile(
  #   data = section_item_bg %>%
  #     cross_join(col_labels %>%
  #                  mutate(lbl = factor(lbl, levels = col_labels$lbl))) %>%
  #     mutate(row_num = as.integer(y_pos),
  #            bg = if_else(row_num %% 2 == 0, "#F7F9FC", "white")),
  #   aes(x = 50, y = y_pos, fill = bg),
  #   width = Inf, height = 0.9,
  #   inherit.aes = FALSE) +
  scale_fill_identity(guide = "none") +
  new_scale_fill() +
  geom_col(
    aes(fill = fill_key),
    width    = 0.75,
    alpha    = 0.90,
    position = position_dodge(width = 0.85)) +  # -- Pre/Post bars dodged --
  scale_fill_manual(
    name   = "Round",
    values = fill_vals,
    breaks = c("Health centre_Pre", "Health centre_Post"),
    labels = c("Pre", "Post"),
    guide  = guide_legend(
      nrow  = 1,
      title.position = "left",
      override.aes   = list(fill = c("#AAAAAA", "#444444"), alpha = 0.9),
      keywidth  = unit(0.6, "cm"),
      keyheight = unit(0.35, "cm"))) +
  geom_text(
    aes(x     = 99,
        label = paste0(round(pct), "%"),
        group = fill_key),
    position = position_dodge(width = 0.85),
    size = 2.5, hjust = 1, colour = "darkgrey", fontface = "plain") +
  # geom_text(
  #   data     = freq %>% filter(pct >= 10),
  #   aes(x     = pmin(pct - 1.5, 97),
  #       label = paste0(round(pct), "%"),
  #       group = fill_key),
  #   position = position_dodge(width = 0.85),
  #   size = 2.5, hjust = 1, colour = "black", fontface = "plain") +  # % labels: inside white (>= 10%), outside black (0 < pct < 10%), outside grey (0%) --
  # geom_text(
  #   data     = freq %>% filter(pct > 0 & pct < 10),
  #   aes(x     = pct + 1.5,
  #       label = paste0(round(pct), "%"),
  #       group = fill_key),
  #   position = position_dodge(width = 0.85),
  #   size = 2.5, hjust = 0, colour = "#333333", fontface = "plain") +
  # geom_text(
  #   data     = freq %>% filter(pct == 0),
  #   aes(x     = pct + 1.5,
  #       label = "0%",
  #       group = fill_key),
  #   position = position_dodge(width = 0.85),
  #   size = 2.5, hjust = 0, colour = "black", fontface = "plain") +
  facet_grid(. ~ lbl) +  # one column per provider
  scale_x_continuous(
    limits   = c(0, 100),
    breaks   = c(0, 25, 50, 75, 100),
    labels   = function(x) paste0(x, "%"),
    expand   = c(0, 1)) +
  scale_y_discrete(
    limits = y_levels_rev,                         # full y axis incl. headers
    labels = function(lbl) {
      ifelse(lbl %in% section_header_labels, "", lbl)}) + # Replace section header labels with empty string (shown via geom_text)
  labs(x = "Percentage of visits in which item was checked",
    y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.y        = element_text(size = 9, colour = "#333333", hjust = 1),
    axis.text.x        = element_text(size = 7.5, colour = "#666666", angle = 45, hjust = 1),
    axis.title.x       = element_text(size = 10, margin = margin(t = 5)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.4),
    strip.text         = element_text(face = "bold", size = 9.5, colour = "white",
                                       lineheight = 1.2),
    strip.background   = element_rect(fill = "#2E6B9E", colour = NA),
    legend.position    = "bottom",
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 10),
    panel.spacing      = unit(0.6, "lines"),
    plot.margin        = margin(10, 10, 10, 5))

ggsave(filename = "pneumonia_item_frequency_plot.png", plot= p, width= 6, height= 6, dpi= 300)

# having pre and post as labels instead of as legens:
# After computing freq, create a two-row-per-item y structure
freq <- freq %>%
  mutate(
    # Combine item label + round into a single y-axis factor level
    # Round label indented so it reads as a sub-row of the item
    y_round = paste0(label, "__", round),          # unique key
    y_label = paste0("  ", round)                  # what shows on axis: "  Pre" / "  Post"
  )

# Rebuild y_levels_rev to interleave section headers, items, and round sub-rows
y_rows_new <- c()
y_axis_labels <- c()   # named: level -> display label

for (sec in sections_order) {
  # Section header pseudo-level
  hdr_key <- paste0("HDR__", sec)
  y_rows_new   <- c(y_rows_new, hdr_key)
  y_axis_labels[hdr_key] <- section_header_labels[[sec]]
  
  for (lbl in items %>% filter(section == sec) %>% pull(label) %>% as.character()) {
    for (rnd in c("Pre", "Post")) {
      key <- paste0(lbl, "__", rnd)
      y_rows_new      <- c(y_rows_new, key)
      # Item label only on the Pre row; Post row gets a blank item label + "Post"
      y_axis_labels[key] <- if (rnd == "Pre")
        paste0(lbl, "  \u2013  Pre")    # "Item name  –  Pre"
      else
        paste0(strrep(" ", nchar(lbl)), "  \u2013  Post")  # indented "  –  Post"
    }
  }
}

y_levels_rev_new <- rev(y_rows_new)

freq <- freq %>%
  mutate(y_pos = factor(y_round, levels = y_levels_rev_new))

# adapted plot
p2 <- ggplot(freq, aes(x = pct, y = y_pos)) +
  geom_tile(
    data = header_df2,
    aes(x = 50, y = y_pos, fill = fill_bg),
    width = Inf, height = 0.9,
    inherit.aes = FALSE) +# domain header rows: coloured background tile 
  scale_fill_identity(guide = "none") +
  new_scale_fill() +
  geom_text(
    data = header_df2_text,
    aes(x = 1, y = y_pos, label = section),
    hjust = 0, size = 3.2, fontface = "bold",
    colour = "#333333", inherit.aes = FALSE) + # domain header text
  # # -- Alternating item row backgrounds --
  # geom_tile(
  #   data = section_item_bg %>%
  #     cross_join(col_labels %>%
  #                  mutate(lbl = factor(lbl, levels = col_labels$lbl))) %>%
  #     mutate(row_num = as.integer(y_pos),
  #            bg = if_else(row_num %% 2 == 0, "#F7F9FC", "white")),
  #   aes(x = 50, y = y_pos, fill = bg),
  #   width = Inf, height = 0.9,
  #   inherit.aes = FALSE) +
  scale_fill_identity(guide = "none") +
  new_scale_fill() +
  scale_fill_manual(
    name   = "Round",
    values = fill_vals,
    breaks = c("Health centre_Pre", "Health centre_Post"),
    labels = c("Pre", "Post"),
    guide  = guide_legend(
      nrow  = 1,
      title.position = "left",
      override.aes   = list(fill = c("#AAAAAA", "#444444"), alpha = 0.9),
      keywidth  = unit(0.6, "cm"),
      keyheight = unit(0.35, "cm"))) +
  geom_text(
    aes(x     = 99,
        label = paste0(round(pct), "%"),
        group = fill_key),
    position = position_dodge(width = 0.85),
    size = 2.5, hjust = 1, colour = "darkgrey", fontface = "plain") +
  # geom_text(
  #   data     = freq %>% filter(pct >= 10),
  #   aes(x     = pmin(pct - 1.5, 97),
  #       label = paste0(round(pct), "%"),
  #       group = fill_key),
  #   position = position_dodge(width = 0.85),
  #   size = 2.5, hjust = 1, colour = "black", fontface = "plain") +  # % labels: inside white (>= 10%), outside black (0 < pct < 10%), outside grey (0%) --
  # geom_text(
  #   data     = freq %>% filter(pct > 0 & pct < 10),
  #   aes(x     = pct + 1.5,
  #       label = paste0(round(pct), "%"),
  #       group = fill_key),
  #   position = position_dodge(width = 0.85),
  #   size = 2.5, hjust = 0, colour = "#333333", fontface = "plain") +
  # geom_text(
  #   data     = freq %>% filter(pct == 0),
  #   aes(x     = pct + 1.5,
  #       label = "0%",
  #       group = fill_key),
  #   position = position_dodge(width = 0.85),
  #   size = 2.5, hjust = 0, colour = "black", fontface = "plain") +
  facet_grid(. ~ lbl) +  # one column per provider
  scale_x_continuous(
    limits   = c(0, 100),
    breaks   = c(0, 25, 50, 75, 100),
    labels   = function(x) paste0(x, "%"),
    expand   = c(0, 1)) +
  scale_y_discrete(
    limits = y_levels_rev_new,
    labels = y_axis_labels) +  
  labs(x = "Percentage of visits in which item was checked",
       y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.y        = element_text(size = 9, colour = "#333333", hjust = 1),
        axis.text.x        = element_text(size = 7.5, colour = "#666666", angle = 45, hjust = 1),
        axis.title.x       = element_text(size = 10, margin = margin(t = 5)),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.4),
        strip.text         = element_text(face = "bold", size = 9.5, colour = "white",
                                          lineheight = 1.2),
        strip.background   = element_rect(fill = "#2E6B9E", colour = NA),
        legend.position    = "none",
        legend.title       = element_text(size = 10, face = "bold"),
        legend.text        = element_text(size = 10),
        panel.spacing      = unit(0.6, "lines"),
        plot.margin        = margin(10, 10, 10, 5)) +
  geom_col(aes(fill = fill_key),  width = 0.75) 
ggsave(filename = "pneumonia_item_frequency_prepostaxislabels.png", plot= p2, width= 8, height= 6, dpi= 300)


#### PNEUMONIA FIG B - SCORE CHANGES #####
df2 <- pneumonia %>%
  mutate(med_history = rowSums(across(c(`TypeCough?`, `DurationCough?`, `CoughWorsened?`,
                                   `Fever?`, `Sputum?`, `RR?`, `DifficultBreathing?`,
                                   `ConfusionPresent?`, `Lethargic?`,
                                   `HistroyRespDisease?`)) > 0),    # Medical history: 1 pt per item asked (max 10)
    actions     = `ReferralHospitalHC` + `SeekAppropriateCare!`,    # Actions: 1 pt per action performed (max 2)
    ab_pen      = if_else(ab == 1, -1L, 0L),    # Dispensing: 0 = nothing given; -1 per inappropriate medicine
    dispensing  = ab_pen + AntimalariaWithoutDiagnose,
    overall     = score)    # Overall score from data
df2 <- df2 %>%  mutate(
    provider = case_when(
      providertype == "health centre"             ~ "Health centre",
      providertype %in% c("community pharmacy/store",
                          "informal vendor")      ~ "Medicine vendor",
      TRUE ~ providertype
    ),
    provider = factor(provider, levels = c("Health centre", "Medicine vendor")),
    round    = case_when(
      tolower(round) %in% c("pre",  "before") ~ "Pre",
      tolower(round) %in% c("post", "after")  ~ "Post",
      TRUE ~ round
    ),
    round = factor(round, levels = c("Pre", "Post")))
long <- df2 %>%
  select(provider, round, med_history, actions, dispensing, overall) %>%
  pivot_longer(c(med_history, actions, dispensing, overall),
               names_to = "domain", values_to = "score") %>%
  mutate(domain = case_when(
      domain == "med_history" ~ "Medical\nhistory",
      domain == "actions"     ~ "Actions",
      domain == "dispensing"  ~ "Dispensing",
      domain == "overall"     ~ "Overall",
      TRUE ~ domain),
    domain = factor(domain,
                    levels = c("Medical\nhistory", "Actions",
                               "Dispensing", "Overall")))

# summarize: mean + 95%CI (using t-based CI)
summary_df <- long %>%
  group_by(provider, round, domain) %>%
  summarise(
    n    = n(),
    mean = mean(score, na.rm = TRUE),
    sd   = sd(score,   na.rm = TRUE),
    se   = sd / sqrt(n),
    lo   = mean - qt(0.975, n - 1) * se,
    hi   = mean + qt(0.975, n - 1) * se,
    .groups = "drop")

# fill key matching frequency plot colours
fill_vals <- c(
  "Health centre_Pre"    = "#8FD4BC",
  "Health centre_Post"   = "#1D9E75",
  "Medicine vendor_Pre"  = "#C9B8E8",
  "Medicine vendor_Post" = "#534AB7")

summary_df <- summary_df %>%
  mutate(fill_key = paste0(provider, "_", round),
    fill_key = factor(fill_key, levels = names(fill_vals)))

# count and denominator for strip labels
ns <- df %>%
  count(provider, round) %>%
  pivot_wider(names_from = round, values_from = n, values_fill = 0)

provider_labels <- ns %>%
  mutate(lbl = paste0(provider, "\n(pre n=", Pre, ", post n=", Post, ")")) %>%
  select(provider, lbl)

summary_df <- summary_df %>%
  left_join(provider_labels, by = "provider") %>%
  mutate(lbl = factor(lbl, levels = provider_labels$lbl))

# max possible score annotations (for reference lines / secondary axis)
domain_max <- tibble(
  domain    = factor(c("Medical\nhistory", "Actions", "Dispensing", "Overall"),
                     levels = levels(summary_df$domain)),
  max_score = c(10, 5, 0, 15),          # dispensing has max 0 (all positive = none given)
  min_score = c(0,  0, -2, -2))

# plot
p3 <- ggplot(summary_df,
            aes(x = domain, y = mean, fill = fill_key,
                group = interaction(fill_key, domain))) +
    geom_hline(yintercept = 0, colour = "#999999", linewidth = 0.5, linetype = "solid") +
    geom_col(
    position = position_dodge(width = 0.7),
    width    = 0.6,
    alpha    = 0.90) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    position = position_dodge(width = 0.7),
    width    = 0.22,
    linewidth = 0.55,
    colour   = "#444444") +
  geom_text(
    aes(y     = if_else(mean >= 0, hi + 0.15, lo - 0.15),
      label = sprintf("%.1f", mean),
      colour = fill_key),
    position  = position_dodge(width = 0.7),
    size      = 3.0,
    fontface  = "bold",
    vjust     = if_else(summary_df$mean >= 0, 0, 1),
    show.legend = FALSE) +
  scale_fill_manual(
    name   = "Round",
    values = fill_vals,
    breaks = c("Health centre_Pre", "Health centre_Post"),
    labels = c("Pre-intervention", "Post-intervention"),
    guide  = guide_legend(
      nrow           = 1,
      title.position = "left",
      override.aes   = list(fill   = c("#AAAAAA", "#444444"),
                            colour = c("#AAAAAA", "#444444"),
                            alpha  = 0.9),
      keywidth  = unit(0.6, "cm"),
      keyheight = unit(0.35, "cm"))) +
  scale_colour_manual(values = fill_vals, guide = "none") +
  facet_wrap(~ lbl, nrow = 1) +
  scale_y_continuous(
    breaks = seq(-2, 10, by = 2),
    expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = NULL,
    y = "Mean score (points)" ) +
  geom_text(
    data = domain_max %>%
      cross_join(provider_labels %>%
                   mutate(lbl = factor(lbl, levels = provider_labels$lbl))),
    aes(x = domain, y = max_score + 0.5,
        label = paste0("max: ", max_score)),
    inherit.aes = FALSE,
    size = 2.4, colour = "#AAAAAA", vjust = 0, fontface = "italic") +  # -- Annotate max possible score per domain --
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x        = element_text(size = 10, colour = "#333333",
                                      lineheight = 1.2),
    axis.text.y        = element_text(size = 9,  colour = "#666666"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 6)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#EEEEEE", linewidth = 0.4),
    strip.text         = element_text(face = "bold", size = 10, colour = "white",
                                      lineheight = 1.2),
    strip.background   = element_rect(fill = "#2E6B9E", colour = NA),
    legend.position    = "top",
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 10),
    plot.margin        = margin(10, 15, 10, 10) )
ggsave(filename = "pneumonia_domain_scores.png", plot= p3, width = 6.5, height = 4.6,dpi = 300)



#### GASTROENTERITIS FIG C - FREQUENCY OF ITEMS ON CHECKLIST ####

diarrhoea <- read_excel("~/public data/infection scenario specific simulated patient data Nanoro/diarrhoea_anon.xlsx")
diarrhoea <- diarrhoea %>% filter(intervention == "intervention")
table(diarrhoea$providertype, diarrhoea$round)

# recode scores to binary
df_g <- diarrhoea %>% mutate(
    hx_type_stool    = as.integer(`TypeStool?`                  > 0),
    hx_blood         = as.integer(`BloodStool?`                 > 0),
    hx_mucus         = as.integer(`MucusStool?`                 > 0),
    hx_freq          = as.integer(`FreqStool?`                  > 0),
    hx_fever         = as.integer(`Fever?`                      > 0),
    hx_abdom_pain    = as.integer(`AbdominalPain?`              > 0),
    hx_vomit         = as.integer(`VomitNausea?`                > 0),
    hx_urination     = as.integer(`QuestionUrination?`          > 0),
    hx_drinking      = as.integer(`CapacityDrink?`              > 0),
    hx_lethargy      = as.integer(`Lethargic?`                  > 0),
    hx_general       = as.integer(`GeneralHealthCondition?`     > 0),
    hx_recent_ab     = as.integer(`RecentUseAB?`                > 0),
    hx_household     = as.integer(`OtherFamilyMembers?`         > 0),
    hx_water_source  = as.integer(`SourceWater?`                > 0),
    hx_meal_prep     = as.integer(`MealPrep?`                   > 0),
    hx_handwash      = as.integer(`HandHygiene?`                > 0),
    hx_wash          = as.integer(`WASHNeighborhood?`           > 0),
    hx_environment   = as.integer(`HygienePhysicalEnvironment?` > 0),
    ex_vitals        = as.integer(VitalSigns                    > 0),
    ex_abdomen       = as.integer(AbdominalExamination          > 0),
    ex_skin_pinch    = as.integer(SkinPinch                     > 0),
    ex_sunken_eyes   = as.integer(SunkenEyes                    > 0),
    act_hydration    = as.integer(`StayHydrated!`               > 0),
    act_hygiene      = as.integer(`AdviseWaterFoodHygiene!`     > 0),
    act_prolonged    = as.integer(HowActProlongedDiarrhea       > 0),
    disp_ab          = as.integer(`Abgiven/Prescription`        != 0),
    disp_ors         = as.integer(ORS                           > 0),
    disp_antimal     = as.integer(Antimalaria                   != 0),
    disp_iv          = as.integer(NumberIVmedication            != 0))

items_g <- tribble(
  ~col,              ~label,                                                ~section,
  "hx_type_stool",   "Type/consistency of stool",                           "Medical history",
  "hx_blood",        "Blood in stool?",                                     "Medical history",
  "hx_mucus",        "Mucus in stool?",                                     "Medical history",
  "hx_freq",         "Frequency of stools",                                 "Medical history",
  "hx_fever",        "Fever/history of fever?",                             "Medical history",
  "hx_abdom_pain",   "Abdominal pain/cramps?",                              "Medical history",
  "hx_vomit",        "Vomiting or nausea?",                                 "Medical history",
  "hx_urination",    "Questions about urination",                           "Medical history",
  "hx_drinking",     "Ability/desire to drink?",                            "Medical history",
  "hx_lethargy",     "Lethargy/loss of consciousness?",                     "Medical history",
  "hx_general",      "General health/immunocompromised/HIV?",               "Medical history",
  "hx_recent_ab",    "Recent use of antibiotics?",                          "Medical history",
  "hx_household",    "Other household members with symptoms?",              "Medical history",
  "hx_water_source", "Source/storage of drinking water?",                   "Medical history",
  "hx_meal_prep",    "Meal preparation?",                                   "Medical history",
  "hx_handwash",     "Hand washing?",                                       "Medical history",
  "hx_wash",         "WASH conditions in neighbourhood?",                   "Medical history",
  "hx_environment",  "Physical environment/hygiene conditions?",            "Medical history",
  "ex_vitals",       "Takes vital signs (BP, pulse or temperature)",        "Clinical examination",
  "ex_abdomen",      "Abdominal examination",                               "Clinical examination",
  "ex_skin_pinch",   "Skin pinch/mucous membranes (dehydration)",          "Clinical examination",
  "ex_sunken_eyes",  "Checks for sunken eyes",                              "Clinical examination",
  "act_hydration",   "Explains need to stay well hydrated",                 "Actions",
  "act_hygiene",     "Explains safe water, food and hygiene practices",     "Actions",
  "act_prolonged",   "Advice on what to do if diarrhoea persists",         "Actions",
  "disp_ab",         "Prescribes/dispenses antibiotics",                    "Dispensing",
  "disp_ors",        "Gives or offers ORS/zinc",                            "Dispensing",
  "disp_antimal",    "Gives antimalarial drug",                             "Dispensing",
  "disp_iv",         "Prescribes/dispenses injectable medication",          "Dispensing")

sections_order_g <- c("Medical history", "Clinical examination", "Actions", "Dispensing")
section_header_labels_g <- c(
  "Medical history"      = "\u2500\u2500 MEDICAL HISTORY \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500",
  "Clinical examination" = "\u2500\u2500 CLINICAL EXAMINATION \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500",
  "Actions"              = "\u2500\u2500 ACTIONS \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500",
  "Dispensing"           = "\u2500\u2500 DISPENSING OF MEDICINES \u2500\u2500\u2500\u2500\u2500\u2500")

y_levels_g <- c()
for (sec in sections_order_g) {
  y_levels_g <- c(y_levels_g,
                  section_header_labels_g[[sec]],
                  items_g %>% filter(section == sec) %>% pull(label))
}
y_levels_rev_g <- rev(y_levels_g)

df_g <- df_g %>% mutate(
    providertype = case_when(
      providertype == "health centre"             ~ "Health centre",
      providertype %in% c("community pharmacy/store",
                          "informal vendor")      ~ "Medicine vendor",
      TRUE ~ providertype),
    providertype = factor(providertype,
                          levels = c("Health centre", "Medicine vendor")),
    round = case_when(
      tolower(round) %in% c("pre",  "before") ~ "Pre",
      tolower(round) %in% c("post", "after")  ~ "Post",
      TRUE ~ round),
    round = factor(round, levels = c("Pre", "Post")))

long_g <- df_g %>%
  select(providertype, round, all_of(items_g$col)) %>%
  pivot_longer(c(-providertype, -round),
               names_to = "col", values_to = "checked") %>%
  left_join(items_g, by = "col")

freq_g <- long_g %>%
  group_by(label, section, providertype, round) %>%
  summarise(
    n_checked = sum(checked, na.rm = TRUE),
    n_visits  = n(),
    pct       = n_checked / n_visits * 100,
    .groups   = "drop") %>%
  mutate(
    fill_key = paste0(providertype, "_", round),
    fill_key = factor(fill_key, levels = names(fill_vals)),
    y_pos    = factor(label, levels = y_levels_rev_g))

header_df_g <- tibble(
  y_pos   = factor(section_header_labels_g[sections_order_g], levels = y_levels_rev_g),
  section = sections_order_g,
  fill_bg = c("#DCE8F5", "#D9EEE8", "#F0E6DC", "#F5DCDC"))

ns_g <- df_g %>%
  count(providertype, round) %>%
  pivot_wider(names_from = round, values_from = n, values_fill = 0)

col_labels_g <- ns_g %>%
  mutate(lbl = paste0(providertype, "\npre n=", Pre, " / post n=", Post)) %>%
  arrange(factor(providertype, levels = c("Health centre", "Medicine vendor"))) %>%
  select(providertype, lbl)

freq_g <- freq_g %>%
  left_join(col_labels_g, by = "providertype") %>%
  mutate(lbl = factor(lbl, levels = col_labels_g$lbl))

header_df2_g <- cross_join(
  header_df_g,
  col_labels_g %>% mutate(lbl = factor(lbl, levels = col_labels_g$lbl)))
header_df2_text_g <- header_df2_g %>% filter(lbl == col_labels_g$lbl[1])

p_g1 <- ggplot(freq_g, aes(x = pct, y = y_pos)) +
  geom_tile(
    data        = header_df2_g,
    aes(x = 50, y = y_pos, fill = fill_bg),
    width = Inf, height = 0.9, inherit.aes = FALSE) +
  scale_fill_identity(guide = "none") +
  new_scale_fill() +
  geom_text(
    data        = header_df2_text_g,
    aes(x = 1, y = y_pos, label = section),
    hjust = 0, size = 3.2, fontface = "bold",
    colour = "#333333", inherit.aes = FALSE) +
  scale_fill_identity(guide = "none") +
  new_scale_fill() +
  geom_col(
    aes(fill = fill_key),
    width    = 0.75,
    alpha    = 0.90,
    position = position_dodge(width = 0.85)) +
  scale_fill_manual(
    name   = "Round",
    values = fill_vals,
    breaks = c("Health centre_Pre", "Health centre_Post"),
    labels = c("Pre", "Post"),
    guide  = guide_legend(
      nrow           = 1,
      title.position = "left",
      override.aes   = list(fill = c("#AAAAAA", "#444444"), alpha = 0.9),
      keywidth  = unit(0.6, "cm"),
      keyheight = unit(0.35, "cm"))) +
  geom_text(
    aes(x     = 99,
        label = paste0(round(pct), "%"),
        group = fill_key),
    position = position_dodge(width = 0.85),
    size = 2.5, hjust = 1, colour = "darkgrey", fontface = "plain") +
  facet_grid(. ~ lbl) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 1)) +
  scale_y_discrete(
    limits = y_levels_rev_g,
    labels = function(lbl) ifelse(lbl %in% section_header_labels_g, "", lbl)) +
  labs(x = "Percentage of visits in which item was checked", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_text(size = 9, colour = "#333333", hjust = 1),
    axis.text.x        = element_text(size = 7.5, colour = "#666666", angle = 45, hjust = 1),
    axis.title.x       = element_text(size = 10, margin = margin(t = 5)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.4),
    strip.text         = element_text(face = "bold", size = 9.5, colour = "white",
                                       lineheight = 1.2),
    strip.background   = element_rect(fill = "#2E6B9E", colour = NA),
    legend.position    = "bottom",
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 10),
    panel.spacing      = unit(0.6, "lines"),
    plot.margin        = margin(10, 10, 10, 5))

ggsave(filename = "gastroenteritis_item_frequency_plot.png",
       plot = p_g1, width = 6, height = 9, dpi = 300)


#### GASTROENTERITIS FIG D - SCORE CHANGES ####

df_g2 <- diarrhoea %>% mutate(
    med_history = rowSums(across(c(
      `TypeStool?`, `BloodStool?`, `MucusStool?`, `FreqStool?`,
      `Fever?`, `AbdominalPain?`, `VomitNausea?`, `QuestionUrination?`,
      `CapacityDrink?`, `Lethargic?`, `GeneralHealthCondition?`,
      `RecentUseAB?`, `OtherFamilyMembers?`, `SourceWater?`,
      `MealPrep?`, `HandHygiene?`, `WASHNeighborhood?`,
      `HygienePhysicalEnvironment?`)) > 0),              # Medical history: max 18
    clin_exam   = VitalSigns + AbdominalExamination + SkinPinch + SunkenEyes,
    actions     = as.integer(`StayHydrated!`           > 0) +
                  as.integer(`AdviseWaterFoodHygiene!`  > 0) +
                  as.integer(HowActProlongedDiarrhea    > 0),
    dispensing  = `Abgiven/Prescription` + Antimalaria + NumberIVmedication + ORS,
    overall     = score)

df_g2 <- df_g2 %>% mutate(
    provider = case_when(
      providertype == "health centre"             ~ "Health centre",
      providertype %in% c("community pharmacy/store",
                          "informal vendor")      ~ "Medicine vendor",
      TRUE ~ providertype),
    provider = factor(provider, levels = c("Health centre", "Medicine vendor")),
    round    = case_when(
      tolower(round) %in% c("pre",  "before") ~ "Pre",
      tolower(round) %in% c("post", "after")  ~ "Post",
      TRUE ~ round),
    round = factor(round, levels = c("Pre", "Post")))

long_g2 <- df_g2 %>%
  select(provider, round, med_history, clin_exam, actions, dispensing, overall) %>%
  pivot_longer(c(med_history, clin_exam, actions, dispensing, overall),
               names_to = "domain", values_to = "score") %>%
  mutate(
    domain = case_when(
      domain == "med_history" ~ "Medical\nhistory",
      domain == "clin_exam"   ~ "Clinical\nexamination",
      domain == "actions"     ~ "Actions",
      domain == "dispensing"  ~ "Dispensing",
      domain == "overall"     ~ "Overall",
      TRUE ~ domain),
    domain = factor(domain,
                    levels = c("Medical\nhistory", "Clinical\nexamination",
                               "Actions", "Dispensing", "Overall")))

summary_g2 <- long_g2 %>%
  group_by(provider, round, domain) %>%
  summarise(
    n    = n(),
    mean = mean(score, na.rm = TRUE),
    sd   = sd(score,   na.rm = TRUE),
    se   = sd / sqrt(n),
    lo   = mean - qt(0.975, n - 1) * se,
    hi   = mean + qt(0.975, n - 1) * se,
    .groups = "drop") %>%
  mutate(
    fill_key = paste0(provider, "_", round),
    fill_key = factor(fill_key, levels = names(fill_vals)))

ns_g2 <- df_g2 %>%
  count(provider, round) %>%
  pivot_wider(names_from = round, values_from = n, values_fill = 0)

provider_labels_g2 <- ns_g2 %>%
  arrange(factor(provider, levels = c("Health centre", "Medicine vendor"))) %>%
  mutate(lbl = paste0(provider, "\n(pre n=", Pre, ", post n=", Post, ")")) %>%
  select(provider, lbl)

summary_g2 <- summary_g2 %>%
  left_join(provider_labels_g2, by = "provider") %>%
  mutate(lbl = factor(lbl, levels = provider_labels_g2$lbl))

domain_max_g2 <- tibble(
  domain    = factor(
    c("Medical\nhistory", "Clinical\nexamination",
      "Actions", "Dispensing", "Overall"),
    levels = levels(summary_g2$domain)),
  max_score = c(22, 4, 5, 1, 32))

p_g2 <- ggplot(summary_g2,
               aes(x = domain, y = mean, fill = fill_key,
                   group = interaction(fill_key, domain))) +
  geom_hline(yintercept = 0, colour = "#999999",
             linewidth = 0.5, linetype = "solid") +
  geom_col(
    position = position_dodge(width = 0.7),
    width    = 0.6,
    alpha    = 0.90) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    position  = position_dodge(width = 0.7),
    width     = 0.22,
    linewidth = 0.55,
    colour    = "#444444") +
  geom_text(
    aes(y      = if_else(mean >= 0, hi + 0.3, lo - 0.3),
        label  = sprintf("%.1f", mean),
        colour = fill_key),
    position    = position_dodge(width = 0.7),
    size        = 3.0,
    fontface    = "bold",
    vjust       = if_else(summary_g2$mean >= 0, 0, 1),
    show.legend = FALSE) +
  scale_fill_manual(
    name   = "Round",
    values = fill_vals,
    breaks = c("Health centre_Pre", "Health centre_Post"),
    labels = c("Pre-intervention", "Post-intervention"),
    guide  = guide_legend(
      nrow           = 1,
      title.position = "left",
      override.aes   = list(fill   = c("#AAAAAA", "#444444"),
                             colour = c("#AAAAAA", "#444444"),
                             alpha  = 0.9),
      keywidth  = unit(0.6, "cm"),
      keyheight = unit(0.35, "cm"))) +
  scale_colour_manual(values = fill_vals, guide = "none") +
  facet_wrap(~ lbl, nrow = 1) +
  scale_y_continuous(
    breaks = seq(-4, 20, by = 4),
    expand = expansion(mult = c(0.08, 0.15))) +
  geom_text(
    data = domain_max_g2 %>%
      cross_join(provider_labels_g2 %>%
                   mutate(lbl = factor(lbl, levels = provider_labels_g2$lbl))),
    aes(x = domain, y = max_score + 0.6, label = paste0("max: ", max_score)),
    inherit.aes = FALSE,
    size = 2.4, colour = "#BBBBBB", vjust = 0, fontface = "italic") +
  labs(x = NULL, y = "Mean score (points)") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x        = element_text(size = 10, colour = "#333333", lineheight = 1.2),
    axis.text.y        = element_text(size = 9,  colour = "#666666"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 6)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#EEEEEE", linewidth = 0.4),
    strip.text         = element_text(face = "bold", size = 10, colour = "white",
                                       lineheight = 1.2),
    strip.background   = element_rect(fill = "#2E6B9E", colour = NA),
    legend.position    = "top",
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 10),
    plot.margin        = margin(10, 15, 10, 10))

ggsave(filename = "gastroenteritis_domain_scores.png",
       plot = p_g2, width = 7.5, height = 4.6, dpi = 300)


#### COMBINED FIGURE: p1 (pneumonia freq) + p3 (pneumonia scores) + p_g1 (gastro freq) + p_g2 (gastro scores)####
combined <- plot_grid(
  plot_grid(p,   p_g1, nrow = 1, labels = c("A", "C"), label_size = 12,
            align = "h", axis = "tb"),
  plot_grid(p3,  p_g2, nrow = 1, labels = c("B", "D"), label_size = 12,
            align = "h", axis = "tb"),
  ncol        = 1,
  rel_heights = c(1.6, 1))   # frequency plots taller than score plots


ggsave(filename = "SP_combined_figure.png",
       plot = combined, width = 14, height = 18, dpi = 300)
