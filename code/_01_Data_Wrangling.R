##########################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105
# 2023

##########################################################################################
# setwd("E:/Research_Projects/SENECA")

library(tidyverse)
library(readr)
library(readxl)
library(car)
library(Rmisc)
library(Hmisc)
library(tidylog)
library(rstatix)
library(data.table)

rm(list = ls())
setwd("E:/Research_Projects/SENECA")
##########################################################################################
# META-DATA ----

# Import network labeling generated by the paper InLang (Roger et al. 2022) & by our study using probabilistic voxel-to-network overlap
meta_data_0 <- read_excel("./data/LANG_atlas_RSN_overlap/Appendix_S3.xlsx",
  sheet = 2
)[, 1:8]
# RS NET1 = Associative
# RS NET2 = Sensorimotor
# RS NET3 = Bottom-up attentional
# RS NET4 = Top-down Control-executive


# Participants
meta_data_1 <- read_excel("./data/cognitive_data_628/participant_data_T1.xlsx")[, 1:8] %>%
  dplyr::select(-c("gender_code", "hand")) %>%
  rename(Subj_ID = Subject) %>%
  rename(Age = age) %>%
  replace("Subj_ID", seq_len(628)) %>%
  mutate(Gender_c = ifelse(gender_text == "MALE", -0.5, 0.5)) %>%
  dplyr::select(-gender_text)

##########################################################################################
# DATA ----
setwd(paste0(getwd(), "/data/data_graphvar_T1"))
n_subj <- 628
n_threshold <- 5 # Number of threshold we examined for reducing the density of the connectomes

# List all txt files & extract the files
listfile <- list.files(getwd(), pattern = "*.txt")

# Local Metrics-------------------------------------

# Degree centrality-------------------------------
listfile_degree <- listfile[grep("degrees", listfile)]

degree <- ldply(listfile_degree, read.table, header = T, sep = "\t") %>%
  mutate(threshold = rep(c(.1, .12, .15, .17, .2), each = n_subj)) %>%
  relocate(threshold, .after = (X)) %>%
  rename(Subj_ID = X) %>%
  replace("Subj_ID", rep(seq_len(n_subj), times = n_threshold)) %>%
  pivot_longer(
    cols = !c("Subj_ID", "threshold"),
    names_to = "Region",
    values_to = "degree"
  )

# Betweenness centrality-------------------------
listfile_BC <- listfile[grep("betweenness", listfile)]

BC <<- ldply(listfile_BC, read.table, header = T, sep = "\t") %>%
  mutate(threshold = rep(c(.1, .12, .15, .17, .2), each = n_subj)) %>%
  relocate(threshold, .after = (X)) %>%
  rename(Subj_ID = X) %>%
  replace("Subj_ID", rep(seq_len(n_subj), times = n_threshold)) %>%
  pivot_longer(
    cols = !c("Subj_ID", "threshold"),
    names_to = "Region",
    values_to = "Betweenness"
  )


# Flow centrality-------------------------------
listfile_flow <- listfile[grep("flow", listfile)]

Flow <<- ldply(listfile_flow, read.table, header = T, sep = "\t") %>%
  mutate(threshold = rep(c(.1, .12, .15, .17, .2), each = n_subj)) %>%
  relocate(threshold, .after = (X)) %>%
  rename(Subj_ID = X) %>%
  replace("Subj_ID", rep(seq_len(n_subj), times = n_threshold)) %>%
  pivot_longer(
    cols = !c("Subj_ID", "threshold"),
    names_to = "Region",
    values_to = "Flow_coeff"
  )


# Local efficiency-------------------------------
listfile_Eloc <- listfile[grep("efficiency_local", listfile)]

Eloc <<- ldply(listfile_Eloc, read.table, header = T, sep = "\t") %>%
  mutate(threshold = rep(c(.1, .12, .15, .17, .2), each = n_subj)) %>%
  relocate(threshold, .after = (X)) %>%
  rename(Subj_ID = X) %>%
  replace("Subj_ID", rep(seq_len(n_subj), times = n_threshold)) %>%
  pivot_longer(
    cols = !c("Subj_ID", "threshold"),
    names_to = "Region",
    values_to = "Eloc"
  )

# Global Metrics------------------------------------
# Global efficiency & Clustering coefficient global-------------------------
listfile_Glob <- listfile[grep("global", listfile)]

Glob_final <<- ldply(listfile_Glob, read.table, header = T, sep = "\t") %>%
  mutate(threshold = rep(c(.1, .12, .15, .17, .2), each = n_subj)) %>%
  relocate(threshold, .after = (X)) %>%
  rename(Subj_ID = X) %>%
  rename(Eglob = efficiency_bin) %>%
  rename(Clustering_coeff_glob = clusterMean_bu) %>%
  # rename(Cost_efficiency = cost_efficiency_relative_bin) %>%
  # plyr::rename(c("modularity_louvain_QOut_und" = "Modularity_Q")) %>%
  replace("Subj_ID", rep(seq_len(n_subj), times = n_threshold)) %>%
  dplyr::select(Subj_ID, threshold, Eglob, Clustering_coeff_glob)

# Removing subjects with less than 90% of connected regions
listfile_components <- list.files(getwd(), pattern = "*.txt")[grep("components", list.files(getwd(), pattern = "*.txt"))]

components <- ldply(listfile_components, read.table, header = T, sep = "\t") %>%
  # mutate(threshold = rep("OMST")) %>%
  mutate(threshold = rep(c(.1, .12, .15, .17, .2), each = n_subj)) %>%
  relocate(threshold, .after = (X)) %>%
  plyr::rename(c("X" = "Subj_ID")) %>%
  # replace("Subj_ID", rep(seq_len(n_subj))) %>%
  replace("Subj_ID", rep(seq_len(n_subj), times = n_threshold)) %>%
  pivot_longer(
    cols = !c("Subj_ID", "threshold"),
    names_to = "Region",
    values_to = "components"
  )

# Get proportion of LLC per threshold across subjects
LLC_filter <<- components %>%
  group_by(Subj_ID) %>%
  count(threshold, components) %>%
  group_by(Subj_ID, threshold) %>%
  mutate(prop = prop.table(n)) %>%
  slice_max(prop, n = 1) %>%
  filter(threshold == "0.15") %>%
  filter(prop > 0.9)

LLC_threshold <<- components %>%
  group_by(Subj_ID) %>%
  count(threshold, components) %>%
  group_by(Subj_ID, threshold) %>%
  mutate(prop = prop.table(n)) %>%
  slice_max(prop, n = 1) %>%
  group_by(threshold) %>%
  summarise_at(vars(prop), mean) %>%
  plyr::rename(c("prop" = "Largest Connected Component"))

setwd(str_replace(getwd(), "\\/data/data_graphvar_T1", ""))

################################################################################
# MERGE META-DATA & DATA

# For local metrics
tmp_local_0 <- cbind(
  degree,
  Betweenness = BC$Betweenness,
  Flow_coeff = Flow$Flow_coeff,
  Eloc = Eloc$Eloc
)

# Add subject information to local metrics
tmp_local_1 <- merge(meta_data_1, tmp_local_0, by = "Subj_ID")

# Add RSNs classification, consensus vectors & global metrics
# DIMENSIONS 628 Subjects * 131 Regions * 5 thresholds
data_full <<- merge(tmp_local_1, meta_data_0, by = "Region") %>%
  relocate(Region, .after = Subj_ID) %>%
  relocate(Index.Power264, .before = Region) %>%
  arrange(Subj_ID, Region, threshold) %>%
  merge(., Glob_final %>%
    dplyr::select(Subj_ID, threshold, Eglob, Clustering_coeff_glob),
  by = c("Subj_ID", "threshold")
  ) %>%
  arrange(Subj_ID, threshold, Region) %>%
  mutate_at(vars(Age), funs(as.numeric(.)))


# One row = one subject
data_local_per_subject <- data_full %>%
  group_by(Subj_ID, threshold) %>%
  summarize_at(vars(degree:Eloc), mean)

data_global_per_subject <- merge(meta_data_1, Glob_final, by = "Subj_ID") %>%
  arrange(Subj_ID, threshold)

# DIMENSIONS 628 Subjects * 5 thresholds
data_full_per_subject <<- merge(data_global_per_subject, data_local_per_subject, by = c("Subj_ID", "threshold")) %>%
  arrange(Subj_ID, threshold)

# One row = one region
data_local_per_region <- data_full %>%
  group_by(Region, threshold) %>%
  summarize_at(vars(degree:Eloc), mean)

data_global_per_region <- merge(data_local_per_region, Glob_final %>%
  group_by(threshold) %>%
  summarize_at(vars(Eglob:Clustering_coeff_glob), mean), by = "threshold")

# DIMENSIONS 131 Regions * 5 thresholds
data_full_per_region <<- merge(data_global_per_region, meta_data_0, by = "Region")


################################################################################
# Output-------------------------------
################################################################################

rm(list = ls()[!ls() %in% c("data_full", "data_full_per_subject", "data_full_per_region", "LLC_filter", "LLC_threshold")])
