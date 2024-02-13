##########################################################################################
# Probabilistic analysis
# Written by CG - 2024
##########################################################################################


##########################################################################################
##########################################################################################
# FUNCTION

trajectory <- function(list_community, list_RSN, threshold, AGE_MIDDLE, AGE_OLD) {
  # Get the associated Resting-state networks for all age groups
  # Hub region specific to each subject yielded by hub detection procedure
  ##############################################################################
    modular <<- TFP_Subject_detailed %>%
      mutate(Age_group = ifelse(Age < AGE_MIDDLE, "Young",
                                ifelse(Age >= AGE_MIDDLE & Age <= AGE_OLD, "Middle", "Old"))) %>% 
      group_by(`RS-LANG`, `1st_network`, Region, Age_group, Subj_ID, MODULAR) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n)) %>%
      dplyr::select(-n) %>%
      spread(MODULAR, freq) %>%
      mutate_all(., ~ replace(., is.na(.), 0)) %>%
      group_by(`RS-LANG`, `1st_network`, Region, Age_group) %>%
      summarize_at(vars(Connector, Provincial, Satellite, Peripheral), mean) %>%
      ungroup() %>%
      arrange(Region) %>%
      pivot_longer(
        cols = !c("RS-LANG", "1st_network", "Region", "Age_group"),
        names_to = "MODULAR", values_to = "freq"
      )
    
    modular$Age_group <- factor(modular$Age_group, levels = c("Young", "Middle", "Old"))
    
    if (list_community != "All" & list_RSN != "All") {
      Cond_PMF <- modular %>%
        filter(grepl(list_community, `RS-LANG`)) %>%
        filter(grepl(list_RSN, `1st_network`)) %>%
        dplyr::select(-`1st_network`) %>% 
        spread(Age_group, freq) %>%
        arrange(MODULAR) %>%
        group_by(Region) %>%
        group_split()
    } else if (list_community != "All" & list_RSN == "All") {
      Cond_PMF <- modular %>%
        filter(grepl(list_community, `RS-LANG`)) %>%
        dplyr::select(-`1st_network`) %>%
        spread(Age_group, freq) %>%
        arrange(MODULAR) %>% 
        group_by(Region) %>%
        group_split()
    } else {
      Cond_PMF <- modular %>%
        dplyr::select(-`1st_network`) %>%
        spread(Age_group, freq) %>%
        arrange(MODULAR) %>% 
        group_by(Region) %>%
        group_split()
    }
  
  ##############################################################################
  ##############################################################################
  # Converts an adjacency matrix to a 2-column dataframe
  ##############################################################################
  ##############################################################################
    adjacency_to_2col <- function(data) {
    crossdata <- lapply(rownames(data), function(x) sapply(colnames(data), function(y) list(x, y, data[x, y])))
    crossdatatmp <- matrix(unlist(crossdata), nrow = 3)
    crossdatamat <- t(crossdatatmp)
    if (colnames(data)[5] == "YM") {
      colnames(crossdatamat) <- c("Young", "Middle", "Value")
    } else {
      colnames(crossdatamat) <- c("Middle", "Old", "Value")
    }
    crossdatadf <- as.data.frame(crossdatamat, stringsAsFactors = F)
    crossdatadf[, 3] <- as.numeric(crossdatadf[, 3] %>% na.omit())
    return(crossdatadf %>% na.omit())
    }
    
  ##############################################################################
  ##############################################################################
  # For each region, finds the most probable trajectory
  ##############################################################################
  ##############################################################################
    
  Cond_PMF_list <- list()
  for (i in 1:length(Cond_PMF)) {
    tmp <<- rbindlist(Cond_PMF[i]) 
    
    ############################################################################
    # First segment - Young to Middle
    ############################################################################
    
    # Beware of the order here, first young then old, make sure of the factor levels
    # Outer compute all pairwise probabilities between each of the 4 functional roles, element-wise matrix multiplication
    outer_young_to_middle <- outer(tmp[,4] %>% as.matrix(), tmp[, 5] %>% as.matrix()) %>% as.data.frame()
    rownames(outer_young_to_middle) <- unlist(tmp$MODULAR)
    colnames(outer_young_to_middle) <- unlist(tmp$MODULAR)
    
    
    outer_young_to_middle <- outer_young_to_middle %>% mutate(YM = "YM")
    # Returning a 4 by 4 matrix which is then converted to a dataframe
    crossdatadf <- adjacency_to_2col(outer_young_to_middle)
    outer_young_to_middle_2 <- crossdatadf
    
    # Filtering out the null probabilities for each region
    tmp_young_to_middle <<- cbind(tmp %>% slice(1:nrow(outer_young_to_middle_2)) %>%
                                    dplyr::select(`RS-LANG`, Region), outer_young_to_middle_2) %>%
      plyr::rename(c("Value" = "Value_YM")) %>% 
      filter(Value_YM > 0)
    
    ############################################################################
    # Second segment - Middle to Old
    ############################################################################
    
    outer_middle_to_old <- outer(tmp_young_to_middle[, 5] %>% as.matrix(), tmp[, 6] %>% as.matrix()) %>% as.data.frame()
      # Indexes a number to each outer product/trajectory to bypass 'duplicate rows not allowed'
      # This essentially keeps trajectories if they are tied, e.g., Provincial-Connector & Provincial-Provincial if tied
      bypass_duplicate_row <- tmp_young_to_middle %>%
        mutate(helper_vector = rep(seq(1:nrow(.)))) %>%
        unite(., Young, "Young", "helper_vector", remove = FALSE) %>%
        unite(., Middle, "Middle", "helper_vector", remove = FALSE) %>%
        dplyr::select(-helper_vector)
      
      rownames(outer_middle_to_old) <- unlist(bypass_duplicate_row$Middle)
      colnames(outer_middle_to_old) <- unlist(tmp$MODULAR)

    
    outer_middle_to_old <- outer_middle_to_old %>% mutate(MO = "MO")
    crossdatadf <- adjacency_to_2col(outer_middle_to_old)
    
    # Finds the maximum probability
    max <- crossdatadf %>% dplyr::slice_max(Value, n = 1)
    
    # Keeps the probabilities within a user-defined range, 5% or 0.05 in the present study
    outer_middle_to_old_2 <- crossdatadf %>% filter(Value >= (max$Value[1] - (max$Value*0.05)))
    
    
    tmp_middle_to_old <- cbind(tmp %>% slice(1:nrow(outer_middle_to_old_2)) %>%
                                 dplyr::select(`RS-LANG`, Region), outer_middle_to_old_2) %>%
      plyr::rename(c("Value" = "Value_MO"))
    
    full_trajectory <- tmp_middle_to_old %>%
      merge(., bypass_duplicate_row %>% dplyr::select(c("Young", "Middle", "Value_YM")), by = c("Middle"))
    
    # Aggregate for all regions
    Cond_PMF_list[[i]] <- full_trajectory
  }
    
    
    Cond_PMF_final <<- rbindlist(Cond_PMF_list) %>%
      # Giving the original labels back, because they had an index number from the helper vector
      mutate(Young = ifelse(base::startsWith(Young, "Connector"), "Connector",
                            ifelse(base::startsWith(Young, "Provincial"), "Provincial",
                                   ifelse(base::startsWith(Young, "Peripheral"), "Peripheral",
                                          ifelse(base::startsWith(Young, "Satellite"), "Satellite", Young)
                                   )
                            )
      )) %>%
      mutate(Middle = ifelse(base::startsWith(Middle, "Connector"), "Connector",
                             ifelse(base::startsWith(Middle, "Provincial"), "Provincial",
                                    ifelse(base::startsWith(Middle, "Peripheral"), "Peripheral",
                                           ifelse(base::startsWith(Middle, "Satellite"), "Satellite", Middle)
                                    )
                             )
      )) %>%
      # Identify each region with a unique label
      mutate(helper_vector = rep(seq(nrow(.)))) %>%
      unite(., Region, c("Region", "helper_vector"), remove = FALSE) %>%
      # Transform  back to an alluvial data format
      pivot_longer(
        cols = c("Young", "Middle", "Old"),
        names_to = "Age_group",
        values_to = "MODULAR"
      )
    
    # Makes sure the factor order is right
    Cond_PMF_final$Age_group <- factor(Cond_PMF_final$Age_group, levels = c("Young", "Middle", "Old"))
  
  ############################################################################
  # ALLUVIAL PLOT
  ############################################################################
  library(ggalluvial)
    
    display_cluster <- Cond_PMF_final %>%
      group_by(Age_group, MODULAR) %>%
      summarize(s = n()) %>%
      arrange(Age_group, desc(MODULAR)) %>%
      .$MODULAR
    
    # display_percentage <- Cond_PMF_final %>%
    #   group_by(Age_group, MODULAR) %>%
    #   summarize(s = n()) %>%
    #   group_by(Age_group) %>%
    #   mutate(s = scales::percent(s / sum(s), accuracy = 0.1)) %>%
    #   arrange(Age_group, desc(MODULAR)) %>%
    #   .$s
    
    alluvial_cluster <- ggplot(
      Cond_PMF_final,
      aes(x = Age_group, stratum = MODULAR, alluvium = Region, fill = MODULAR)
    ) +
      geom_flow(alpha = .7, curve_type = "arctangent", width = .2, na.rm = TRUE) +
      geom_stratum(alpha = .8) +
      scale_x_discrete(expand = c(.1, .1)) +
      # geom_text(stat = "stratum", label = display_percentage, nudge_x = -0.05) +
      geom_text(stat = "stratum", label = display_cluster) +
      scale_fill_brewer(palette = "PuOr", direction = 1) +
      # labs(title = paste0("Most probable topological reconfiguration trajectory (z-scored probability threshold: ", threshold, ")")) +
      labs(x = "Age group") +
      theme_pubr(legend = "none",
                 base_size = 18) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())
    
    alluvial_cluster
  }

trajectory(list_community = "All", list_RSN = "All", AGE_MIDDLE = 45, AGE_OLD = 55)

##########################################################################################
##########################################################################################
# ANALYSIS - Section 3.2 of the article

flexibility_score <- Cond_PMF_final %>% spread(Age_group, MODULAR) %>%
  # 0 = movers; 1 = non-movers
  mutate(movers_tot = ifelse(Young == Middle & Middle == Old, 1, 0)) 

# write.csv(flexibility_score, "Qualitative_analysis.csv")


flexibility_score %>% 
  group_by(Region) %>% 
  count(movers_tot) %>% 
  mutate(n = prop.table(n))

##########################################################################################
# SUBSYSTEMS SUMMARY
flexibility_score %>% 
  group_by(`RS-LANG`) %>% 
  count(movers_tot) %>% 
  mutate(n = prop.table(n))

# Reconfiguration towards CONNECTOR
flexibility_score_2 <- flexibility_score %>%  
  filter(movers_tot == 0) %>% 
  filter((Young != "Connector" & Middle == "Connector") | (Middle != "Connector" & Old == "Connector"))

flexibility_score_2 <- flexibility_score %>%  
  filter(movers_tot == 0) %>% 
  filter((Young != "Peripheral" & Middle == "Peripheral") | (Middle != "Peripheral" & Old == "Peripheral"))

# # Reconfiguration towards PROVINCIAL
# flexibility_score_2 <- flexibility_score %>% 
#   filter(movers_tot == 0) %>% 
#   filter((Young != "Provincial" & Middle == "Provincial") | (Middle != "Provincial" & Old == "Provincial"))
# 
# # Reconfiguration towards PERIPHERAL
# flexibility_score_2 <- flexibility_score %>%  
#   filter(movers_tot == 0) %>% 
#   filter((Young != "Peripheral" & Middle == "Peripheral") | (Middle != "Peripheral" & Old == "Peripheral"))
# 
# flexibility_score_2 <- flexibility_score %>% 
#   filter(movers_tot == 0) %>% 
#   filter((Young == "Satellite" & Middle == "Peripheral") | (Middle == "Satellite" & Old == "Peripheral"))
# 
# flexibility_score_2 <- flexibility_score %>% 
#   filter(movers_tot == 0) %>% 
#   filter((Young == "Provincial" & Middle == "Peripheral") | (Middle == "Provincial" & Old == "Peripheral"))
# 
# # Reconfiguration towards SATELLITE
# flexibility_score_2 <- flexibility_score %>% 
#   filter(movers_tot == 0) %>% 
#   filter((Young != "Satellite" & Middle == "Satellite") | (Middle != "Satellite" & Old == "Satellite"))
# 
# flexibility_score_2 <- flexibility_score %>% 
#   filter(movers_tot == 0) %>% 
#   filter((Middle == "Connector" & Old == "Satellite"))
# 
# flexibility_score_2 <- flexibility_score %>% 
#   filter(movers_tot == 0) %>% 
#   filter((Young == "Provincial" & Middle == "Satellite") | (Middle == "Provincial" & Old == "Satellite"))

library(tidyverse)
flexibility_score <- rio::import("./data/Qualitative_analysis.csv")

# Provincial-to-peripheral
flexibility_score_2 <- flexibility_score %>%  
  relocate(Young, .before = Middle) %>% 
  filter(movers_tot == 0) %>% 
  filter((Young == "Provincial" & Middle == "Peripheral") | (Middle == "Provincial" & Old == "Peripheral"))


# Provincial-to-connector
flexibility_score_2 <- flexibility_score %>%  
  relocate(Young, .before = Middle) %>% 
  filter(movers_tot == 0) %>% 
  filter((Young == "Provincial" & Middle == "Connector") | (Middle == "Provincial" & Old == "Connector"))

# Satellite-to-peripheral
flexibility_score_2 <- flexibility_score %>%  
  relocate(Young, .before = Middle) %>% 
  filter(movers_tot == 0) %>% 
  filter((Young == "Satellite" & Middle == "Peripheral") | (Middle == "Satellite" & Old == "Peripheral"))

# Satellite-to-connector
flexibility_score_2 <- flexibility_score %>%  
  relocate(Young, .before = Middle) %>% 
  filter(movers_tot == 0) %>% 
  filter((Young == "Satellite" & Middle == "Connector") | (Middle == "Satellite" & Old == "Connector"))

# Connector-to-Satellite
flexibility_score_2 <- flexibility_score %>%  
  relocate(Young, .before = Middle) %>% 
  filter(movers_tot == 0) %>% 
  filter((Young == "Connector" & Middle == "Satellite") | (Middle == "Connector" & Old == "Satellite"))

# Provincial-to-satellite
flexibility_score_2 <- flexibility_score %>%  
  relocate(Young, .before = Middle) %>% 
  filter(movers_tot == 0) %>% 
  filter((Young == "Provincial" & Middle == "Satellite") | (Middle == "Provincial" & Old == "Satellite"))




############################################################################
# Easter egg because it's pretty :)
#############################################################################

# seq(from=-10, to=10, by = 0.05) %>%
#   expand.grid(x=., y=.) %>%
#   ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*sin(x)))) +
#   geom_point(alpha=.1, shape=20, size=1, color="black")+
#   theme_void()

# VENN DIAGRAM

# library(VennDiagram)
# 
# draw.quad.venn(72, 86, 50, 52, 44, 27, 32, 38, 32, 20, 18, 17, 11, 13, 6,
#                category = c("Satellite", "Peripheral", "Segregation", "Integration"),
#                fill = c("#FDCC8A", "grey", "#08519C", "#D7301F"),
#                cex = 2,
#                cat.cex = 2,
#                cat.col = c("#FDCC8A", "grey", "#08519C", "#D7301F"),
#                
# )
# 
# 
# options(max.print=200)
