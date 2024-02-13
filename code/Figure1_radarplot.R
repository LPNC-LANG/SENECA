################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2024

##########################################################################################
# Import processed data------------------------------------------------------------------
##########################################################################################

source("_01_Data_Wrangling.R")
source("helper_functions/_NMI&AMI_functions.R")
source("helper_functions/_radarplotting_function.R")

library(ggalluvial)

# Define normalizing function
# min_max_norm <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }

# Define palette for visualization
custom_palette <- c(
  "Auditory" = "#FF99FF",
  "Language" = "#FF6600",
  "CON" = "#9900CC",
  "DMN" = "#FF0033",
  "FPN" = "#FFCC33",
  "SMN" = "#0099CC",
  "DAN" = "#33FF66",
  "Visual 1" = "#CCCCCC",
  "Visual 2" = "#CCCCCC",
  "PMM" = "#CC0033",
  "VMM" = "#CC0033",
  "NaN" = "white",
  "Multi" = "white",
  "Multi/SM" = "white"
)


###############################################################################
# With the resting-state community structure

Net_proportion_RS_our_method <- data_full_per_region %>%
  mutate_at(vars(ends_with("value")), funs(as.numeric(as.character(.)))) %>%
  group_by(Consensus_vector_0.15, `1st_network`) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(Consensus_vector_0.15, desc(freq))

Weighted_RSN_vector_RS_our_method <- data_full_per_region %>%
  mutate_at(vars(ends_with("value")), funs(as.numeric(as.character(.)))) %>%
  # 1 = High certainty, 0 = Low certainty
  mutate(certainty_factor = `1st_value` / 100) %>%
  group_by(Consensus_vector_0.15, `1st_network`) %>%
  # Mean proportion by RSNs when a ROI is assigned with that RSN
  summarise_at(vars(certainty_factor), mean)

Net_proportion_RS_weighted <- merge(Net_proportion_RS_our_method,
                                    Weighted_RSN_vector_RS_our_method,
                                    by = c("Consensus_vector_0.15", "1st_network")
) %>%
  group_by(Consensus_vector_0.15) %>%
  mutate(n_adjusted = n * certainty_factor) %>%
  mutate(adjusted_freq = n_adjusted / sum(n_adjusted)) %>% 
  mutate(total_adjustment = abs(1 - (adjusted_freq / freq))) %>% 
  arrange(Consensus_vector_0.15, desc(adjusted_freq))


mean_adjustment <- Net_proportion_RS_weighted %>% group_by(Consensus_vector_0.15) %>% 
  slice_max(freq, n = 3) %>% 
  arrange(Consensus_vector_0.15, desc(adjusted_freq))

mean(mean_adjustment$total_adjustment)
# Mean adjustment of 12%


# ggplot(Net_proportion_RS, aes(
#   # the group argument allows to stack according to the increasing values instead of the labels
#   x = forcats::fct_rev(Consensus_vector_0.15), y = freq, group = factor(freq), fill = CAB_NP_assign
# )) +
#   geom_bar(stat = "identity", position = position_stack(), width = 0.5) +
#   geom_text(
#     data = Net_proportion_RS %>% filter(freq > 0.12),
#     aes(
#       x = Consensus_vector_0.15, y = freq,
#       label = scales::percent(freq, accuracy = .1)
#     ),
#     position = position_stack(vjust = .25)
#   ) +
#   geom_text(
#     data = Net_proportion_RS %>% filter(freq > 0.12),
#     aes(
#       x = Consensus_vector_0.15, y = freq,
#       label = CAB_NP_assign
#     ),
#     position = position_stack(vjust = .7)
#   ) +
#   guides(fill = guide_legend(title = "CAB-NP Networks")) +
#   # y implements how certain, on average, LANG nets correspond to each network
#   labs(x = "Nets from resting-state data", y = "Proportion using the InLang method for network assignement") +
#   coord_flip() +
#   scale_fill_manual(values = custom_palette) +
#   theme_pubr(legend = "none")

composition <- ggplot(Net_proportion_RS_weighted, aes(
  # the group argument allows to stack according to the increasing values instead of the labels
  x = forcats::fct_rev(Consensus_vector_0.15), y = adjusted_freq, group = factor(adjusted_freq), fill = `1st_network`
)) +
  geom_bar(stat = "identity", position = position_stack(), width = 0.5) +
  geom_text(
    data = Net_proportion_RS_weighted %>% filter(adjusted_freq > 0.12),
    aes(
      x = Consensus_vector_0.15, y = adjusted_freq,
      label = scales::percent(adjusted_freq, accuracy = .1)
    ),
    position = position_stack(vjust = .25)
  ) +
  geom_text(
    data = Net_proportion_RS_weighted %>% filter(adjusted_freq > 0.12),
    aes(
      x = Consensus_vector_0.15, y = adjusted_freq,
      label = `1st_network`
    ),
    position = position_stack(vjust = .7)
  ) +
  guides(fill = guide_legend(title = "CAB-NP Networks")) +
  # y implements how certain, on average, LANG nets correspond to each network
  labs(x = "Nets from resting-state data", y = "Proportion using our method for network assignment\n Weighted by the certainty of the overlap") +
  coord_flip() +
  scale_fill_manual(values = custom_palette) +
  theme_pubr(legend = "none")


composition + theme_pubr(base_size = 18, base_family = "sans",
                legend = "right")


# Radar plot of RSN per Community
Net_proportion_RS_weighted$`1st_network` <- factor(Net_proportion_RS_weighted$`1st_network`,
                                                   levels = c(
                                                     "DMN", "Language", "CON",
                                                     "FPN", "DAN",
                                                     "Visual 1", "Visual 2",
                                                     "SMN", "PMM",
                                                     "VMM"
                                                   ))

Radar_RSN_community <- Net_proportion_RS_weighted %>%
  subset(adjusted_freq >= .20) %>% 
  filter(`1st_network` != "VMM") %>%
  dplyr::select(`1st_network`, adjusted_freq) %>%
  spread(`1st_network`, adjusted_freq) %>%
  remove_rownames() %>%
  column_to_rownames(var = "Consensus_vector_0.15") %>%
  mutate_at(vars(everything()), funs(. * 100))


radarplotting_overlap(Radar_RSN_community, 80, 0, 1, 1,
                      alpha = 1, label_size = 3,
                      title = NULL,
                      palette = c("#D7301F", "#FC8D59", "#08519C", "#FDCC8A")
)


################################################################################
# With the InLang community structure

# # Random threshold to keep 131 ROIs
# data_InLang <- data_full_per_region
# # %>% subset(threshold == "0.15")
# 
# # With InLang method for network assignment
# Net_proportion_InLang <- data_InLang %>%
#   # Remove non-assigned LANG regions - remaining 117 ROIs
#   # filter(!grepl("NaN", CAB_NP_assign)) %>%
#   group_by(LANG_Net_assign, CAB_NP_assign) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n)) %>%
#   arrange(LANG_Net_assign, desc(freq))
# 
# # With ImCalc method
# Net_proportion_ImCalc <- data_InLang %>%
#   mutate_at(vars(ends_with("value")), funs(as.numeric(as.character(.)))) %>%
#   group_by(LANG_Net_assign, `1st_network`) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n)) %>%
#   arrange(LANG_Net_assign, desc(n))
# 
# Weighted_RSN_vector <- data_InLang %>%
#   mutate_at(vars(ends_with("value")), funs(as.numeric(as.character(.)))) %>%
#   # 1 = High certainty, 0 = Low certainty
#   mutate(certainty_factor = `1st_value` / 100) %>%
#   group_by(LANG_Net_assign, `1st_network`) %>%
#   # Mean proportion by RSNs when a ROI is assigned with that RSN
#   summarise_at(vars(certainty_factor), mean)
# 
# Net_proportion_ImCalc_weighted <- merge(Net_proportion_ImCalc,
#   Weighted_RSN_vector,
#   by = c("LANG_Net_assign", "1st_network")
# ) %>%
#   group_by(LANG_Net_assign) %>%
#   # Normalize within each LANG Net
#   # mutate(norm_LANG = min_max_norm(certainty_factor)) %>%
#   # Compute both adjustments
#   mutate(n_adjusted = n * certainty_factor) %>%
#   mutate(adjusted_freq = n_adjusted / sum(n_adjusted)) %>%
#   #
#   arrange(LANG_Net_assign, desc(adjusted_freq))
# 
# # Network assignment on the 131 LANG ROIs according to our ImCalc method
# p <- ggplot(Net_proportion_InLang, aes(
#   # the group argument allows to stack according to the increasing values instead of the labels
#   x = forcats::fct_rev(LANG_Net_assign), y = freq, group = factor(freq), fill = CAB_NP_assign
# )) +
#   geom_bar(stat = "identity", position = position_stack(), width = 0.5) +
#   geom_text(
#     data = Net_proportion_InLang %>% filter(freq > 0.12),
#     aes(
#       x = LANG_Net_assign, y = freq,
#       label = scales::percent(freq, accuracy = .1)
#     ),
#     position = position_stack(vjust = .25)
#   ) +
#   geom_text(
#     data = Net_proportion_InLang %>% filter(freq > 0.12),
#     aes(
#       x = LANG_Net_assign, y = freq,
#       label = CAB_NP_assign
#     ),
#     position = position_stack(vjust = .7)
#   ) +
#   guides(fill = guide_legend(title = "CAB-NP Networks")) +
#   # y implements how certain, on average, LANG nets correspond to each network
#   labs(x = "LANG Nets", y = "Proportion using InLang method for network assignment") +
#   coord_flip() +
#   scale_fill_manual(values = custom_palette) +
#   theme_pubr(legend = "none")
# 
# p2 <- ggplot(Net_proportion_ImCalc_weighted, aes(
#   # the group argument allows to stack according to the increasing values instead of the labels
#   x = forcats::fct_rev(LANG_Net_assign), y = adjusted_freq, group = factor(adjusted_freq), fill = `1st_network`
# )) +S
#   geom_bar(stat = "identity", position = position_stack(), width = 0.5) +
#   geom_text(
#     data = Net_proportion_ImCalc_weighted %>% filter(adjusted_freq > 0.12),
#     aes(
#       x = LANG_Net_assign, y = adjusted_freq,
#       label = scales::percent(adjusted_freq, accuracy = .1)
#     ),
#     position = position_stack(vjust = .25)
#   ) +
#   geom_text(
#     data = Net_proportion_ImCalc_weighted %>% filter(adjusted_freq > 0.12),
#     aes(
#       x = LANG_Net_assign, y = adjusted_freq,
#       label = `1st_network`
#     ),
#     position = position_stack(vjust = .7)
#   ) +
#   guides(fill = guide_legend(title = "CAB-NP Networks")) +
#   # y implements how certain, on average, LANG nets correspond to each network
#   labs(x = "LANG Nets", y = "Proportion using our method for network assignment\n Weighted by the certainty of the overlap") +
#   coord_flip() +
#   scale_fill_manual(values = custom_palette) +
#   theme_pubr(legend = "none")
# 
# # Compare outputs
# gridExtra::grid.arrange(p, p2)
# 
# # Radar plot of RSN per LANG Nets
# Radar_RSN_LANG_Nets <- Net_proportion_InLang %>%
#   dplyr::select(LANG_Net_assign, CAB_NP_assign, freq) %>%
#   spread(CAB_NP_assign, freq) %>%
#   remove_rownames() %>%
#   column_to_rownames(var = "LANG_Net_assign") %>%
#   mutate_at(vars(everything()), funs(. * 100))
# 
# 
# radarplotting_overlap(Radar_RSN_LANG_Nets, 100, 0, 1, 1,
#   alpha = 0.4, label_size = 1,
#   title = "Composition of each LANG Net (InLang)",
#   palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FF99FF")
# )
# 
# legend(
#   x = "bottomleft", legend = rownames(Radar_RSN_LANG_Nets), horiz = TRUE,
#   bty = "n", pch = 20, col = c("#00AFBB", "#E7B800", "#FC4E07", "#FF99FF"),
#   text.col = "black", cex = 1, pt.cex = 2
# )
# 
# Radar_RSN_LANG_Nets <- Net_proportion_ImCalc_weighted %>%
#   dplyr::select(LANG_Net_assign, `1st_network`, adjusted_freq) %>%
#   spread(`1st_network`, adjusted_freq) %>%
#   remove_rownames() %>%
#   column_to_rownames(var = "LANG_Net_assign") %>%
#   mutate_at(vars(everything()), funs(. * 100))
# 
# 
# radarplotting_overlap(Radar_RSN_LANG_Nets, 100, 0, 1, 1,
#   alpha = 0.4, label_size = 1,
#   title = "Composition of each LANG Net (Network_overlap_calc)",
#   palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FF99FF")
# )
# legend(
#   x = "bottomleft", legend = rownames(Radar_RSN_LANG_Nets), horiz = TRUE,
#   bty = "n", pch = 20, col = c("#00AFBB", "#E7B800", "#FC4E07", "#FF99FF"),
#   text.col = "black", cex = 1, pt.cex = 2
# )
# 
