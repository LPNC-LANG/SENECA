################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

################################################################################

source("_04_Topological_role_assignment.R")

################################################################################
    Proportion <- data_functional_role %>% 
      dplyr::select(Subj_ID, Region, 
                    degree, Within_module_z_cons, zPC_cons,
                    `1st_network`, Consensus_vector_0.15) %>%
      # mutate(across(degree:PC, ~ rank(-.x), .names = "{.col}_rank")) %>%
      pivot_longer(
        cols = !c("Subj_ID","Region", "1st_network", "Consensus_vector_0.15"),
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
        dplyr::select(Subj_ID, Age, Region, `1st_network`, Consensus_vector_0.15, MODULAR)
      
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
      group_by(Consensus_vector_0.15, Region, Subj_ID, Age, MODULAR) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n)) %>%
      spread(MODULAR, freq) %>%
      dplyr::select(-n) %>%
      mutate_all(., ~ replace(., is.na(.), 0)) %>%
      group_by(Consensus_vector_0.15, Subj_ID, Age) %>%
      summarize_at(vars(Connector, Provincial, Satellite, Peripheral), mean) %>%
      ungroup() %>%
      pivot_longer(cols = !c("Consensus_vector_0.15","Subj_ID", "Age"), names_to = "Functional_role", values_to = "freq") %>% 
      spread(Functional_role, freq) %>% 
      mutate_at(vars(Connector:Peripheral), funs(as.numeric(. * 100))) %>% 
      mutate(Consensus_vector_0.15 = ifelse(Consensus_vector_0.15 == "1", "RS-NET 1 (DMN, FPN, Language)",
                                            ifelse(Consensus_vector_0.15 == "3", "RS-NET 3 (CON, FPN, Language)",
                                                   ifelse(Consensus_vector_0.15 == "2", "RS-NET 2 (SMN)",
                                                          ifelse(Consensus_vector_0.15 == "4", "RS-NET 4 (FPN)",
                                                                 "RS-NET 5 (VMM)"
                                                          )
                                                   )
                                            )
      ))
    
