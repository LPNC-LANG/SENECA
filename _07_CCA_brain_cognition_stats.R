################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

################################################################################

source("_06_Main_Statistics.R")
################################################################################
# DESCRIPTIVES
################################################################################

participants <- read_excel("meta_data_628/participant_data_T1.xlsx")[, 1:2] %>%
  dplyr::rename(Subj_ID = Subject) %>%
  replace("Subj_ID", seq_len(628))

CAMCAN_cognitive_data <- read_excel("meta_data_628/CognitiveData_CamCAN_Apr2022.xlsx") %>%
  filter(Observations %in% participants$Observations) %>%
  dplyr::select(-gender_code) %>%
  dplyr::rename(Age_CogData = Age) %>%
  dplyr::select(c(
    Observations,
    Age_CogData,
    # Cattell Fluid intelligence
    Cattell,
    # Proverb comprehension (abstraction & EF)
    Proverbs_Summary__Score,
    # Picture-picture priming (word production)
    Picture__Primming_Summary_ACC_baseline_all,
    # Tip-of-the-tongue
    TOT_Summary_ToT_ratio
  )) %>%
  dplyr::rename(Proverb = Proverbs_Summary__Score) %>%
  dplyr::rename(Naming = Picture__Primming_Summary_ACC_baseline_all) %>%
  dplyr::rename(ToT_Ratio = TOT_Summary_ToT_ratio) 



CAMCAN_cognitive_data_supp <- read_excel("meta_data_628/CognitiveData_CamCAN_Supplement.xlsx") %>%
  filter(Observations %in% participants$Observations) %>%
  dplyr::select(c(
    Observations,
    # EF
    Hotel_Task,
    # Sentence Comprehension
    Sentence_Comprehension_c,
    # Memory
    Story_Recall,
    # Language
    Verbal_Fluency
  )) %>%
  mutate_at(vars(Hotel_Task, Sentence_Comprehension_c, Story_Recall, Verbal_Fluency), funs(as.numeric(.)))


################################################################################
################################################################################
################################################################################
# IMPUTE MEDIAN
################################################################################
################################################################################
################################################################################

exclusion <- CAMCAN_cognitive_data %>% 
  mutate(E1 = ifelse(is.na(Cattell), 1, 0),
         E2 = ifelse(is.na(Proverb), 1, 0),
         E3 = ifelse(is.na(Naming), 1, 0),
         E4 = ifelse(is.na(ToT_Ratio), 1, 0),
         E_tot = E1 + E2 + E3 + E4) %>% 
  filter(E_tot < 3) %>% arrange(Age_CogData) %>% 
  mutate(Age_CogData = as.numeric(Age_CogData),
    Age_decade = ifelse(Age_CogData <= 29, 25, 
                             ifelse(Age_CogData <= 39, 35,
                                    ifelse(Age_CogData <= 49, 45,
                                           ifelse(Age_CogData <= 59, 55,
                                                  ifelse(Age_CogData <= 69, 65,
                                                         ifelse(Age_CogData <= 79, 75, 85))))))) %>% 
  group_by(Age_decade, .all = TRUE) %>% 
  group_split()

exclusion_list <- list()
for (i in 1:length(exclusion)) {
  tmp <- rbindlist(lapply(exclusion[i], as.data.table)) %>% as.data.frame()
  tmp_select <- tmp[,3:6]
  # getting median of each column using apply()
  all_column_median <- apply(tmp_select, 2, median, na.rm=TRUE)
  
  # imputing median value with NA
  for(j in colnames(tmp_select)) {
    tmp_select[,j][is.na(tmp_select[,j])] <- all_column_median[j]
  }
  
  tmp_imputed <- cbind(Observations = tmp[,1], tmp_select)
  
  exclusion_list[[i]] <- tmp_imputed
}

CAMCAN_cognitive_data_imputed_median <- rbindlist(exclusion_list)


################################################################################
################################################################################
################################################################################

Data_CCA <- merge(participants, CAMCAN_cognitive_data_imputed_median, by = "Observations") %>%
  merge(., CAMCAN_cognitive_data_supp, by = "Observations") %>% 
  merge(., subsystem_level_stats %>% dplyr::select(
    Subj_ID, Age, Consensus_vector_0.15,
    Connector_balance, Provincial_balance, Peripheral_balance, Satellite_balance
  ), by = "Subj_ID")

Data_CCA_full <- Data_CCA %>%
  na.omit() %>%
  mutate(ToT_Ratio_inverse = ToT_Ratio * (-1)) %>%
  mutate(Hotel_Task_inverse = Hotel_Task * (-1)) %>%
  dplyr::select(-c(ToT_Ratio, Hotel_Task)) %>%
  pivot_longer(c(Connector_balance, Provincial_balance, Peripheral_balance, Satellite_balance),
    names_to = "topological_balances", values_to = "value"
  ) %>% 
  unite(BalancexRSN, "Consensus_vector_0.15", "topological_balances", remove = FALSE) %>% 
  dplyr::select(-c(topological_balances, Consensus_vector_0.15)) %>% 
  spread(BalancexRSN, value)



Data_CCA_full %>%
  pivot_longer(
    c(Cattell:Verbal_Fluency, ToT_Ratio_inverse, Hotel_Task_inverse),
    names_to = "Cognitive_assessment",
    values_to = "performance"
  ) %>%
  mutate(Cognitive_assessment = ifelse(Cognitive_assessment == "Verbal_Fluency", "Verbal Fluency", 
                                       ifelse(Cognitive_assessment == "Sentence_Comprehension_c", "Sentence Comp",
                                              ifelse(Cognitive_assessment == "Hotel_Task_inverse", "Multitasking", 
                                                     ifelse(Cognitive_assessment == "Story_Recall", "LTM",
                                                            ifelse(Cognitive_assessment == "ToT_Ratio_inverse", "Tip-of-the-tongue", Cognitive_assessment)))))) %>% 
  group_by(Cognitive_assessment) %>%
  mutate(performance = as.numeric(scale(performance))) %>%
  ggplot(aes(Age, performance, color = Cognitive_assessment)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_jitter(height = 0.05, alpha = 0.08) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x), alpha = .3) +
  scale_x_continuous(breaks = c(20, 40, 55, 70, 90)) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_color_brewer(palette = "Paired") +
  theme_pubr(base_size = 20, 
                 legend = "none") +
  theme(plot.title.position = "plot",
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.x = element_blank()) +
  labs(y = "Normalized score") +
  facet_wrap(~Cognitive_assessment, scale = "free", ncol = 4) 


cog_set <- Data_CCA_full[, c(3:8, 10:11)] %>%
  scale(.) %>% 
  as.data.frame() 

brain_set <- Data_CCA_full[, c(
  # Without Sat
  # 12:13, 15:17, 19:21, 23:25, 27
  # Without Periph
   12, 14:16, 18:20, 22:24, 26:27
  # Without Prov
  # 12:14, 16:18, 20:22, 24:26
)] %>%
  scale(.) %>% 
  as.data.frame() 

################################################################################
# CANONICAL CORRELATION  ANALYSIS
################################################################################
library(CCA)
library(CCP)

desc_cca <- matcor(brain_set, cog_set)
img.matcor(desc_cca, type = 2)


# canonical coefficients - weights used for linear combination
cc_results <- cc(brain_set, cog_set)
# Canonical correlation
cc_results$cor
# Proportion of variance explained
cc_results$cor[1]^2
cc_results$cor[2]^2

# Function coefficients
cc_results[3:4]

# VIF
1/(1-(cc_results[3:4]$xcoef[,1]^2))
1/(1-(cc_results[3:4]$ycoef[,1]^2))


# Identical because sigma = 1, already scaled beforehand
# Std function coefficients
# s1 <- diag(sqrt(diag(cov(brain_set))))
# s1 %*% cc_results$xcoef
# 
# s1 <- diag(sqrt(diag(cov(cog_set))))
# s1 %*% cc_results$ycoef


# Structure coeffs - correlation between observed variables and the latent variable
cc_loadings <- comput(brain_set, cog_set, cc_results)
cc_loadings[3:6]

cc_loadings[3:6]$corr.X.xscores

# multicollinearity
sum((cc_loadings[3:6]$corr.X.xscores^2)[,1])
sum((cc_loadings[3:6]$corr.Y.yscores^2)[,1])


# tests of canonical dimensions
rho <- cc_results$cor
# Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(brain_set)[1]
p <- length(brain_set)
q <- length(cog_set)

# Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")



################################################################################
# CCA FIGURES
################################################################################

# INTERACTION EFFECT SEMANTIC-SYNERGISTIC


mod <- mgcv::gam(Cattell~s(Age), data = Data_CCA_full)
summary(mod)


dev_explained <- c(42.9, 41.4, 14.8, 14.1, 13, 8.75, 6.02, 3.74)
linearity <- c(2.67, 5.92, 1.87, 5.36, 2.43, 2.3, 2.58, 2.32)
semantic_cor <- c(-0.8061704, -0.7698932, -0.4115551, -0.351683, -0.1656834, -0.1783413, -0.2664709, 0.06650062)
synergy_cor <- c(-0.08613661, -0.2068828, -0.07528576, -0.5258505, -0.3177736, -0.5940927, -0.04294554, -0.6831757)

df_stats <- cbind(dev_explained, linearity, semantic_cor, synergy_cor) %>% as.data.frame()
summary(lm(dev_explained ~ semantic_cor*synergy_cor, data =df_stats))


Brain_Mode <- as.matrix(brain_set) %*% cc_results$xcoef[, 1]
Brain_Mode_2 <- as.matrix(brain_set) %*% cc_results$xcoef[, 2]
Brain_Mode_resilience <- as.matrix(brain_set) %*% ((cc_results$xcoef[, 1]+cc_results$xcoef[, 2])/2)
Brain_Mode_synergy <- as.matrix(brain_set) %*% ((cc_results$xcoef[, 1]-cc_results$xcoef[, 2])/2)

    
Cognitive_Mode <- as.matrix(cog_set) %*% cc_results$ycoef[, 1]
Cognitive_Mode_2 <- as.matrix(cog_set) %*% cc_results$ycoef[, 2]
Cognitive_Mode_resilience <- as.matrix(cog_set) %*% ((cc_results$ycoef[, 1]+cc_results$ycoef[, 2])/2)
Cognitive_Mode_synergy <- as.matrix(cog_set) %*% ((cc_results$ycoef[, 1]-cc_results$ycoef[, 2])/2)

plot_cca <- Data_CCA_full %>% 
  mutate(
    Age = Age,
    Brain_Mode = Brain_Mode,
    Brain_Mode_2 = Brain_Mode_2,
    Brain_Mode_resilience = Brain_Mode_resilience,
    Brain_Mode_synergy = Brain_Mode_synergy,
    Cognitive_Mode = Cognitive_Mode * (-1),
    Cognitive_Mode_2 = Cognitive_Mode_2,
    Cognitive_Mode_resilience = Cognitive_Mode_resilience *(-1),
    Cognitive_Mode_synergy = Cognitive_Mode_synergy*(-1)
  ) 


plot_cca %>%
  dplyr::select(Age, Cognitive_Mode_resilience, Cognitive_Mode_synergy, Cognitive_Mode) %>% 
  pivot_longer(c("Cognitive_Mode_resilience", "Cognitive_Mode_synergy", "Cognitive_Mode"), names_to = "Cognition", values_to = "Values") %>% 
  ggplot(aes(Age, Values, color = Cognition)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x), alpha = 0) +
  scale_x_continuous(breaks = seq(20, 90, 15)) +
  scale_y_continuous(breaks = seq(-0.7, 0.7, 0.2)) +
  coord_cartesian(ylim = c(-0.7, 0.7)) +
  scale_color_manual(values = c("#838f93", "#FDCC8A", "#08519C")) +
  theme_pubr(base_size = 18,
             legend = "none") +
  theme(plot.title.position = "plot") +
  labs(y = "Cognitive performance") +
  ggtitle("")

plot_cca %>%
  dplyr::select(Age, Brain_Mode_resilience, Brain_Mode_synergy) %>% 
  pivot_longer(c("Brain_Mode_resilience", "Brain_Mode_synergy"), names_to = "Brain", values_to = "Values") %>% 
  ggplot(aes(Age, Values, color = Brain)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x), alpha = 0) +
  scale_x_continuous(breaks = seq(20, 90, 20)) +
  scale_y_continuous(breaks = seq(-0.7, 0.6, 0.2)) +
  coord_cartesian(ylim = c(-0.7, 0.6)) +
  scale_color_manual(values = c("#FDCC8A", "#08519C")) +
  theme_pubr(base_size = 18,
             legend = "none") +
  theme(plot.title.position = "plot") +
  labs(y = "Latent coordinates") +
  ggtitle("")


plot_loadings_y <- cc_loadings$corr.Y.yscores[, 1] %>%
  as.data.frame() %>%
  rownames_to_column("Cognitive_assessment") %>%
  plyr::rename(c("." = "loading")) %>%
  mutate(labels = ifelse(Cognitive_assessment == "Verbal_Fluency", "Verbal Fluency",
    ifelse(Cognitive_assessment == "Story_Recall", "Long-term memory",
      ifelse(Cognitive_assessment == "ToT_Ratio_inverse", "Tip-of-the-tongue",
        ifelse(Cognitive_assessment == "Hotel_Task_inverse", "Multitasking",
          ifelse(Cognitive_assessment == "Sentence_Comprehension_c", "Verbal Comprehension",
            ifelse(Cognitive_assessment == "Cattell", "Fluid Intelligence",
              ifelse(Cognitive_assessment == "Naming", "Naming",
                ifelse(Cognitive_assessment == "Proverb", "Proverb",
                  NA
                )
              )
            )
          )
        )
      )
    )
  ))

plot_loadings_y$labels <- factor(plot_loadings_y$labels) %>%
  fct_reorder(plot_loadings_y$loading, .desc = FALSE)

loading_cog <- ggplot(
  plot_loadings_y,
  aes(x = labels, y = loading)
) +
  geom_col(aes(fill = loading), alpha = .7) +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  scale_y_continuous(breaks = seq(-0.8, 0, 0.2)) +
  coord_flip() +
  geom_text(aes(y = -0.01, label = labels, fontface = "bold", hjust = "right"),
    size = 8
  ) +
  theme_pubr(
    base_size = 18,
    legend = "none",
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank()
  ) 


loading_cog
Rmisc::multiplot(loading_brain, loading_cog, cols = 1)




# ################################################################################
# # CHORD PLOTS
# ################################################################################
ncors <- cor(cog_set, brain_set)
ncors_plot <- ncors %>%
  as.data.frame()
  # Retain only correlations above the 3rd Quantile
  mutate_all(., funs(ifelse(. > quantile(ncors, na.rm = TRUE)[2], NA, .))) %>%
  as.matrix()


library(circlize)
circlize::chordDiagram(ncors_plot,
  transparency = .2,
  link.lwd = 1, # Line width
  link.lty = 1, # Line type
  link.border = 1
) # Border color)

ncors <- cor(cog_set %>% dplyr::select(ends_with("Naming")), brain_set)
ncors_plot <- ncors %>%
  as.data.frame() %>%
  # Retain only correlations above the 3rd Quantile
  mutate_all(., funs(ifelse(. > quantile(ncors, na.rm = TRUE)[3], NA, .))) %>%
  as.matrix()
