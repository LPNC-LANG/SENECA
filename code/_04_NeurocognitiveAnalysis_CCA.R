################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105
# 2024

################################################################################
source("./code/_03_ConnectomicAnalysis_Quantitative_subsystem.R")
################################################################################
# Cognitive DATA
################################################################################
participants <- read_excel("./data/cognitive_data_628/participant_data_T1.xlsx")[, 1:2] %>%
  dplyr::rename(Subj_ID = Subject) %>%
  replace("Subj_ID", seq_len(628))

CAMCAN_cognitive_data <- read_excel("./data/cognitive_data_628/CognitiveData_CamCAN_Apr2022.xlsx") %>%
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



CAMCAN_cognitive_data_supp <- read_excel("./data/cognitive_data_628/CognitiveData_CamCAN_Supplement.xlsx") %>%
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
# IMPUTE MEDIAN VALUE TO DATA ----
################################################################################

exclusion <- CAMCAN_cognitive_data %>%
  mutate(
    E1 = ifelse(is.na(Cattell), 1, 0),
    E2 = ifelse(is.na(Proverb), 1, 0),
    E3 = ifelse(is.na(Naming), 1, 0),
    E4 = ifelse(is.na(ToT_Ratio), 1, 0),
    E_tot = E1 + E2 + E3 + E4
  ) %>%
  # Exclude participants if 3 or more missing values (628 to 613 participants as a result)
  filter(E_tot < 3) %>%
  arrange(Age_CogData) %>%
  mutate(
    Age_CogData = as.numeric(Age_CogData),
    Age_decade = ifelse(Age_CogData <= 30, 25,
      ifelse(Age_CogData <= 40, 35,
        ifelse(Age_CogData <= 50, 45,
          ifelse(Age_CogData <= 60, 55,
            ifelse(Age_CogData <= 70, 65,
              ifelse(Age_CogData <= 80, 75, 85)
            )
          )
        )
      )
    )
  ) %>%
  group_by(Age_decade, .all = TRUE) %>%
  group_split()

exclusion_list <- list()
for (i in 1:length(exclusion)) {
  tmp <- rbindlist(lapply(exclusion[i], as.data.table)) %>% as.data.frame()
  tmp_select <- tmp[, 3:6]
  # getting median of each column using apply()
  all_column_median <- apply(tmp_select, 2, median, na.rm = TRUE)

  # imputing median value with NA
  for (j in colnames(tmp_select)) {
    tmp_select[, j][is.na(tmp_select[, j])] <- all_column_median[j]
  }

  tmp_imputed <- cbind(Observations = tmp[, 1], tmp_select)

  exclusion_list[[i]] <- tmp_imputed
}

CAMCAN_cognitive_data_imputed_median <- rbindlist(exclusion_list)

################################################################################
# MERGE Cognitive Data with Topological Data ----
################################################################################

Data_CCA <- merge(participants, CAMCAN_cognitive_data_imputed_median, by = "Observations") %>%
  merge(., CAMCAN_cognitive_data_supp, by = "Observations") %>%
  merge(., subsystem_level_stats %>% dplyr::select(
    Subj_ID, Age, `RS-LANG`,
    Connector_balance, Provincial_balance, Peripheral_balance, Satellite_balance
  ), by = "Subj_ID")

Data_CCA_full <- Data_CCA %>%
  na.omit() %>%
  mutate(ToT_Ratio_inverse = 1 - ToT_Ratio) %>%
  mutate(Hotel_Task_inverse = 1 - log(Hotel_Task)) %>%
  dplyr::select(-c(ToT_Ratio, Hotel_Task)) %>%
  pivot_longer(c(Connector_balance, Provincial_balance, Peripheral_balance, Satellite_balance),
    names_to = "topological_balances", values_to = "value"
  ) %>%
  unite(BalancexRSN, "RS-LANG", "topological_balances", remove = FALSE) %>%
  dplyr::select(-c(topological_balances, `RS-LANG`)) %>%
  spread(BalancexRSN, value)


# Setup datasets for CCA ----
age_contrasts <- Data_CCA_full %>% mutate(
  age_groups = ifelse(Age < 45, "young",
    ifelse(Age <= 55 & Age >= 45, "middle",
      "old"
    )
  ),
  c1 = ifelse(age_groups == "old", 1, -1 / 2),
  c2 = ifelse(age_groups == "young", -1 / 2,
    ifelse(age_groups == "middle", 1 / 2, 0)
  )
)

cog_set <- Data_CCA_full[, c(3:8, 10:11)] %>%
  scale(.) %>%
  # contrasts
  cbind(c1 = as.numeric(age_contrasts$c1), c2 = as.numeric(age_contrasts$c2)) %>%
  as.data.frame()

brain_set <- Data_CCA_full[, c(
  12:27
)] %>%
  scale(.) %>%
  as.data.frame()

################################################################################
# CANONICAL CORRELATION  ANALYSIS ----
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
1 / (1 - (cc_results[3:4]$xcoef[, 1]^2))
1 / (1 - (cc_results[3:4]$ycoef[, 1]^2))


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
cc_loadings[3:6]$corr.Y.yscores

# multicollinearity
sum((cc_loadings[3:6]$corr.X.xscores^2)[, 1])
sum((cc_loadings[3:6]$corr.Y.yscores^2)[, 1])


# tests of canonical dimensions
rho <- cc_results$cor
# Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(brain_set)[1]
p <- length(brain_set)
q <- length(cog_set)

# Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")


################################################################################
# CCA K-fold cross validation ----
################################################################################
library(ccatools)
source("helper_functions/.cca_kfold_cv.R")

set.seed(100)
customed_cca_cv_boot(as.matrix(brain_set), as.matrix(cog_set), ncomp = 4, Nfolds = 10, Nboot = 10e2, UseProcrustes = T)
