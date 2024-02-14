################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105
# 2024


##########################################################################################
# Import processed data------------------------------------------------------------------
source("./code/_01_Data_Wrangling.R")
source("helper_functions/_NMI&AMI_functions.R")
source("helper_functions/_radarplotting_function.R")

library(readxl)
library(ggalluvial)
library(ggpubr)

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
rs_irmf_LANG <- (data_full_per_region %>% subset(threshold == "0.15"))$`Consensus_vector_0.15`

task_irmf_LANG <- read_excel("LANG_atlas_RSN_overlap/raw_data.xlsx",
  sheet = 1
)$task_irmf_LANG

# NMI & AMI
NMI_func(factor(task_irmf_LANG), factor(rs_irmf_LANG))
AMI_func(factor(task_irmf_LANG), factor(rs_irmf_LANG))

# Contingency table
addmargins(table(task_irmf_LANG, rs_irmf_LANG))



# Create dataframe for visualization
Region <- (data_full_per_region %>% subset(threshold == "0.15"))$Region
data_131 <- cbind(rs_irmf_LANG, task_irmf_LANG, Region) %>% as.data.frame()

# Create alluvial diagram between the two community structures
data_alluvial_community <- data_131 %>%
  dplyr::select(task_irmf_LANG, rs_irmf_LANG, Region) %>%
  mutate(task_irmf_LANG = ifelse(task_irmf_LANG == "1", "Encoding-Decoding",
    ifelse(task_irmf_LANG == "2", "Control-Executive",
      ifelse(task_irmf_LANG == "3", "Abstract-Conceptual",
        "Sensorimotor"
      )
    )
  )) %>%
  mutate(rs_irmf_LANG = ifelse(rs_irmf_LANG == "1", "RS-NET 1 (DMN, FPN, Language)",
    ifelse(rs_irmf_LANG == "3", "RS-NET 3 (CON, FPN, Language)",
      ifelse(rs_irmf_LANG == "2", "RS-NET 2 (SMN)",
        ifelse(rs_irmf_LANG == "4", "RS-NET 4 (FPN)",
          "RS-NET 5 (VMM)"
        )
      )
    )
  )) %>%
  plyr::rename(c("rs_irmf_LANG" = "RS-Nets")) %>%
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
  geom_text(stat = "stratum", label = display_percentage, nudge_y = -5, size = 7) +
  geom_text(stat = "stratum", label = display_communities, size = 7) +
  scale_fill_brewer(palette = "Oranges", direction = 1) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  ggpubr::theme_pubclean()

alluvial_community
