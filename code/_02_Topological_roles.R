################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105
# 2024

source("./code/_01_Data_Wrangling.R")

library(jsonlite)
library(data.table)

################################################################################
# ~~~~~~~~~~~ Hub classification ~~~~~~~~~~~ ----

# High Participation coefficient (based on consensus group-level modular decomposition after 1000 iterations)
# High Within-z (based on consensus group-level modular decomposition after 1000 iterations)

# High_zPC/High z = connector
# High_zPC/low_z = satellite
# Low_zPC/High_z = provincial
# Low_zPC/Low_z = peripheral

# 628 Subjects, 131 Regions, one threshold = 0.15

################################################################################
################################################################################
################################################################################


setwd(paste0(getwd(), "/data/data_community_metrics"))

# All
PC_consensus <- as.data.frame(fromJSON("All_PC_norm.json")) %>%
  mutate(Subj_ID = rep(seq_len(628))) %>%
  pivot_longer(
    cols = !c("Subj_ID"),
    names_to = "Region",
    values_to = "PC_cons"
  )

Within_module_z_consensus <- as.data.frame(fromJSON("All_Wz.json")) %>%
  mutate(Subj_ID = rep(seq_len(628))) %>%
  pivot_longer(
    cols = !c("Subj_ID"),
    names_to = "Region",
    values_to = "Within_module_z_cons"
  )

nodal_metrics_cons <- cbind(PC_consensus, Within_module_z_cons = Within_module_z_consensus$Within_module_z_cons) %>%
  dplyr::select(-Region)

# Make sure dataframe is ordered identically to nodal_metrics
data_full_thresholded <- data_full %>%
  subset(threshold == "0.15") %>%
  arrange(Subj_ID, Region)

data_bind_PC_Wz <- cbind(data_full_thresholded,
  PC_cons = nodal_metrics_cons$PC_cons, Within_module_z_cons = nodal_metrics_cons$Within_module_z_cons
)
############################################################################
############################################################################
# Putting it all together ----

data_functional_role <<-
  data_bind_PC_Wz %>%
  # rbind(data_young, data_middle, data_old) %>%
  group_by(Subj_ID) %>%
  mutate(zPC_cons = as.numeric(scale(PC_cons))) %>%
  # mutate(zPC_cons = ifelse(zPC_cons == "NaN", 0, zPC_cons)) %>%
  # 1e-5 to avoid nodes with Wz = 0 to be classified as Connector or Provincial
  # 0 indicates that it forms its own module mathematically speaking
  mutate(MODULAR = ifelse(zPC_cons >= 0 & Within_module_z_cons >= 1e-5, "Connector",
    ifelse(zPC_cons >= 0 & Within_module_z_cons < 1e-5, "Satellite",
      ifelse(zPC_cons < 0 & Within_module_z_cons >= 1e-5, "Provincial",
        ifelse(zPC_cons < 0 & Within_module_z_cons < 1e-5, "Peripheral", "Isolate")
      )
    )
  )) %>%
  relocate(Subj_ID, .after = "MODULAR") %>%
  arrange(Subj_ID, Region) %>%
  ungroup()

data_functional_role$MODULAR <- factor(data_functional_role$MODULAR, levels = c(
  "Connector", "Provincial", "Satellite", "Peripheral"
))

setwd(str_replace(getwd(), "\\/data/data_community_metrics", ""))

################################################################################
Proportion <- data_functional_role %>%
  dplyr::select(
    Subj_ID, Region,
    degree, Within_module_z_cons, zPC_cons,
    `1st_network`, `RS-LANG`
  ) %>%
  # mutate(across(degree:PC, ~ rank(-.x), .names = "{.col}_rank")) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Region", "1st_network", "RS-LANG"),
    names_to = "Metric_name",
    values_to = "Metric_value"
  ) %>%
  group_by(Subj_ID, Metric_name) %>%
  group_split() %>%
  map_dfr(. %>% slice_max(Metric_value, n = 131 * 1) %>% # 131 is the number of regions in the LANG connectome
    mutate(rank = rep(seq(1:length(Region))))) %>%
  group_by(Subj_ID) %>%
  group_split()


# List of region and RSN of each subject (useful if only a fraction of all 131 regions is considered)
list_1 <- list()
# Topologico-functional profiles
list_2 <- list()
for (i in 1:length(Proportion)) {
  Hub_df <- rbindlist(Proportion[i]) %>% distinct(Region, .keep_all = TRUE)
  # Here I subset the rows specific to each subject
  tmp <- data_functional_role %>%
    filter(Region %in% Hub_df$Region) %>%
    filter(Subj_ID == i) %>%
    dplyr::select(Subj_ID, Age, Region, `1st_network`, `RS-LANG`, MODULAR)

  # Hub region of each subject
  list_1[[i]] <- tmp

  # Here I compute the proportion of each role
  tmp_bis <- tmp %>%
    group_by(MODULAR) %>%
    summarize(n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    dplyr::select(-n) %>%
    spread(MODULAR, freq)


  # Topologico-functional profiles
  list_2[[i]] <- tmp_bis
}

################################################################################
# TFP = Topological-functional profile
# Subject-level
TFP_Subject_overview <<- cbind(
  rbindlist(list_2, fill = TRUE) %>%
    mutate_all(., ~ replace(., is.na(.), 0)) %>% mutate_at(vars(Connector:Peripheral), funs(. * 100)),
  data_functional_role %>% group_by(Subj_ID) %>% summarise_at(vars(Age, Gender_c), mean) %>% arrange(Subj_ID)
)

TFP_Subject_detailed <- rbindlist(list_1)
# Select the subjects from the clusters
TFP_Subject_detailed <<- filter(
  TFP_Subject_detailed,
  Subj_ID %in% TFP_Subject_overview$Subj_ID
)

TFP_Subject_subsystem <- TFP_Subject_detailed %>%
  group_by(`RS-LANG`, Region, Subj_ID, Age, MODULAR) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  spread(MODULAR, freq) %>%
  dplyr::select(-n) %>%
  mutate_all(., ~ replace(., is.na(.), 0)) %>%
  group_by(`RS-LANG`, Subj_ID, Age) %>%
  summarize_at(vars(Connector, Provincial, Satellite, Peripheral), mean) %>%
  ungroup() %>%
  pivot_longer(cols = !c("RS-LANG", "Subj_ID", "Age"), names_to = "Functional_role", values_to = "freq") %>%
  spread(Functional_role, freq) %>%
  mutate_at(vars(Connector:Peripheral), funs(as.numeric(. * 100))) %>%
  mutate(`RS-LANG` = ifelse(`RS-LANG` == "1", "RS-NET 1 (DMN, FPN, Language)",
    ifelse(`RS-LANG` == "3", "RS-NET 3 (CON, FPN, Language)",
      ifelse(`RS-LANG` == "2", "RS-NET 2 (SMN)",
        ifelse(`RS-LANG` == "4", "RS-NET 4 (FPN)",
          "RS-NET 5 (VMM)"
        )
      )
    )
  ))
