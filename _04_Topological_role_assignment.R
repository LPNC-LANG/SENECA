################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

################################################################################

library(jsonlite)
library(data.table)

################################################################################
source("_01_Data_Wrangling.R")

################################################################################
# ~~~~~~~~~~~ Hub classification ~~~~~~~~~~~ -----------------------------------

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


    setwd(paste0(getwd(), "/data_graphvar_T1"))
    
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
    
    setwd(str_replace(getwd(), "\\/data_graphvar_T1", ""))