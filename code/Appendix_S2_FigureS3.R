##########################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

##########################################################################################
source("./code/_01_Data_Wrangling.R")
source("./code/helper_functions/_NMI&AMI_functions.R")

library(plotly)
################################################################################
# FIGURE S3 ---



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
    geom_jitter(height = 0.05, alpha = 0) +
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
    ggpubr::theme_pubclean(base_size = 18, base_family = 'sans')




# VALIDATION ANALYSIS ---
# AMI across lifespan consensus partitions
# Consensus community vectors derived from graph_var
Consensus_vector <- read_excel("./data/data_community_metrics/Consensus_vectors_CamCAN.xlsx", sheet = 2) %>%
  mutate_at(vars(starts_with("Consensus")), funs(as.character(as.numeric(.))))
# RS NET1 = Associative
# RS NET2 = Sensorimotor
# RS NET3 = Bottom-up attentional
# RS NET4 = Top-down Control-executive

a <- AMI_func(factor(Consensus_vector$Consensus_young), factor(Consensus_vector$Consensus_vector_0.15))
b <- AMI_func(factor(Consensus_vector$Consensus_middle), factor(Consensus_vector$Consensus_vector_0.15))
c <- AMI_func(factor(Consensus_vector$Consensus_old), factor(Consensus_vector$Consensus_vector_0.15))

lifespan_AMI <- c(a, b, c) %>% as.data.frame()
lifespan_AMI %>% get_summary_stats(type = "full")
