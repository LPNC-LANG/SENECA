################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

################################################################################
library(ggalluvial)
##########################################################################################
# Import processed data------------------------------------------------------------------
source("_01_Data_Wrangling.R")
source("_NMI&AMI_functions.R")
source("_radarplotting_function.R")

# Define palette for visualization
custom_palette <- c(
  "Auditory" = "#FF99FF",
  "Language" = "#FF6600",
  "CON" = "#9900CC",
  "DMN" = "#FF0033",
  "FPN" = "#FFCC33",
  "SMN" = "#0099CC",
  "DAN" = "#33FF66",
  "Visual_1" = "#CCCCCC",
  "Visual_2" = "#CCCCCC",
  "PMM" = "#CC0033",
  "VMM" = "#CC0033",
  "NaN" = "white",
  "Multi" = "white",
  "Multi/SM" = "white"
)

################################################################################
# State reconfiguration between task & rs-fMRI across the 131 LANG ROIs
################################################################################

# Keeping a 131 row-dataframe
data_131 <- data_full_per_region %>% subset(threshold == "0.15")

# NMI & AMI
NMI_func(factor(data_131$task_irmf_LANG), factor(data_131$Consensus_vector_0.15))
AMI_func(factor(data_131$task_irmf_LANG), factor(data_131$Consensus_vector_0.15))

# Contingency table
addmargins(table(data_131$Consensus_vector_0.15, data_131$task_irmf_LANG))

# Create alluvial diagram between the two community structures

data_alluvial_community <- data_131 %>%
  dplyr::select(task_irmf_LANG, Consensus_vector_0.15, Region) %>%
  mutate(task_irmf_LANG = ifelse(task_irmf_LANG == "1", "Encoding-Decoding",
    ifelse(task_irmf_LANG == "2", "Control-Executive",
      ifelse(task_irmf_LANG == "3", "Abstract-Conceptual",
        "Sensorimotor"
      )
    )
  )) %>%
  mutate(Consensus_vector_0.15 = ifelse(Consensus_vector_0.15 == "1", "RS-NET 1 (DMN, FPN, Language)",
    ifelse(Consensus_vector_0.15 == "3", "RS-NET 3 (CON, FPN, Language)",
      ifelse(Consensus_vector_0.15 == "2", "RS-NET 2 (SMN)",
        ifelse(Consensus_vector_0.15 == "4", "RS-NET 4 (FPN)",
          "RS-NET 5 (VMM)"
        )
      )
    )
  )) %>%
  plyr::rename(c("Consensus_vector_0.15" = "RS-Nets")) %>%
  plyr::rename(c("task_irmf_LANG" = "LANG Nets")) %>%
  pivot_longer(
    cols = c("LANG Nets", "RS-Nets"),
    names_to = "Community_structure",
    values_to = "Communities"
  )

display_percentage <- data_alluvial_community %>%
  group_by(Community_structure, Communities) %>%
  summarize(s = n()) %>%
  group_by(Community_structure) %>%
  mutate(s = scales::percent(s / sum(s), accuracy = 0.1)) %>%
  arrange(Community_structure, desc(Communities)) %>%
  .$s

display_communities <- data_alluvial_community %>%
  group_by(Community_structure, Communities) %>%
  summarize(s = n()) %>%
  arrange(Community_structure, desc(Communities)) %>%
  .$Communities


alluvial_community <- ggplot(
  data_alluvial_community,
  aes(x = Community_structure, stratum = Communities, alluvium = Region, fill = Communities)
) +
  geom_flow(alpha = .7, curve_type = "arctangent", width = .2, na.rm = TRUE) +
  geom_stratum(alpha = .8) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_text(stat = "stratum", label = display_percentage, nudge_y = -5) +
  geom_text(stat = "stratum", label = display_communities) +
  scale_fill_brewer(palette = "Oranges", direction = 1) +
  theme_pubr(base_size = 18, 
             legend = "none") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank()
        )

alluvial_community

