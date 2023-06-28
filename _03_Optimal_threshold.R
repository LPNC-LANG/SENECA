##########################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

##########################################################################################
source("_01_Data_Wrangling.R")

################################################################################
library(plotly)

data_full_per_subject_LLC <- merge(data_full_per_subject, LLC_threshold, by = "threshold") %>%
  arrange(Subj_ID, threshold)

# Evolution of Global metrics - what is the optimal threshold?
evo <- data_full_per_subject_LLC %>%
  group_by(threshold) %>% 
  summarise_at(vars(Eglob, Clustering_coeff_glob, Eloc, `Largest Connected Component`), funs(mean)) %>%
  dplyr::rename(ClustCoeff = Clustering_coeff_glob, 
                LLC = `Largest Connected Component`) %>% 
  pivot_longer(
    cols = !c("threshold"),
    names_to = "Metrics"
  )

evo %>% 
    ggplot(aes(threshold, value, color = Metrics)) +
    geom_line(size = 1) +
    geom_point(size = 4) +
    geom_jitter(height = 0.05, alpha = 0.2) +
    xlab("Threshold level") +
    ylab("") +
    scale_x_continuous(breaks = c(0.1, 0.12, 0.15, 0.17, 0.2)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    coord_cartesian(ylim = c(0.4, 1)) +
    # geom_rect(
    #   aes(
    #     xmin = 0.14,
    #     xmax = 0.16,
    #     ymin = 0.25,
    #     ymax = 1.01
    #   ),
    #   fill = "red", alpha = 0.2, color = "red", linewidth = 0.1
    # ) +
    ggpubr::theme_pubr(base_size = 18)

# AMI across lifespan consensus partitions
# source("_NMI&AMI_functions.R")
# # Make sure this is indexed on 131 observations only
# data_AMI <- data_full_per_region %>% subset(threshold == "0.15")
# 
# a <- AMI_func(factor(data_AMI$Consensus_young), factor(data_AMI$Consensus_vector_0.15))
# b <- AMI_func(factor(data_AMI$Consensus_middle), factor(data_AMI$Consensus_vector_0.15))
# c <- AMI_func(factor(data_AMI$Consensus_old), factor(data_AMI$Consensus_vector_0.15))
# 
# lifespan_AMI <- c(a, b, c) %>% as.data.frame()
# lifespan_AMI %>% get_summary_stats(type = "full")