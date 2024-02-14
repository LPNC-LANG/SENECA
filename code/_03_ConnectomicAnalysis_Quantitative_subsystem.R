################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105
# 2024

################################################################################
# SUBSYSTEM LEVEL
################################################################################
source("./code/_02_Topological_roles.R")
source("./code/helper_functions/PRE.R")
library(ggpubr)
################################################################################

# COVARIATES
# TIV - covariate
participants <- read_excel("./data/cognitive_data_628/participant_data_T1.xlsx")[, c(1:2, 7)] %>%
  dplyr::rename(Subj_ID = Subject) %>%
  replace("Subj_ID", seq_len(628))
# Mean FC - covariate
covariate_FC <- fromJSON("./data/data_graphvar_T1/mean_cor.json") %>%
  as.data.frame() %>%
  rename(mean_FC = colnames(.)[1])

efficiencies_ROI <- data_functional_role %>%
  dplyr::select(Subj_ID, Region, Eglob, Eloc) %>%
  mutate(Disruption = (Eglob - Eloc) / (Eloc + Eglob)) %>%
  group_by(Subj_ID) %>%
  summarize_at(vars(Eglob, Eloc, Disruption), mean) %>%
  ungroup()


TFP_Subject_overview <- cbind(TFP_Subject_overview,
  mean_FC = covariate_FC$mean_FC,
  TIV = as.numeric(participants$tiv_cubicmm),
  Eglob = efficiencies_ROI$Eglob,
  Eloc = efficiencies_ROI$Eloc,
  Disruption = efficiencies_ROI$Disruption
)

TFP_Subject_subsystem <- merge(TFP_Subject_subsystem, TFP_Subject_overview %>% dplyr::select(Subj_ID, Gender_c, mean_FC, TIV), by = "Subj_ID")


list_TFP_Subject_subsystem <- TFP_Subject_subsystem %>%
  filter(`RS-LANG` != "RS-NET 5 (VMM)") %>%
  group_by(`RS-LANG`) %>%
  group_split()

# Bayesian Multiplicative replacement
list_imputed <- list()
for (i in 1:length(list_TFP_Subject_subsystem)) {
  library(zCompositions)
  library(compositions)

  tmp_raw <- rbindlist(list_TFP_Subject_subsystem[i]) %>% arrange(Subj_ID)

  tmp_coda_modular <- tmp_raw %>%
    dplyr::select(Connector, Satellite, Provincial, Peripheral)

  if (min(tmp_coda_modular) == 0) {
    tmp_coda_modular_bis <- tmp_coda_modular %>%
      acomp(.) %>%
      cmultRepl(., output = "prop")
  } else {
    tmp_coda_modular_bis <- tmp_coda_modular
  }

  tmp <- cbind(
    tmp_raw %>% dplyr::select(-c(Connector, Satellite, Provincial, Peripheral)),
    tmp_coda_modular_bis
  )

  list_imputed[[i]] <- tmp
}

TFP_Subject_subsystem_imputed <- rbindlist(list_imputed)
TFP_Subject_subsystem_imputed$`RS-LANG` <- factor(TFP_Subject_subsystem_imputed$`RS-LANG`,
  ordered = FALSE
)

subsystem_level_stats <- TFP_Subject_subsystem_imputed %>%
  mutate(
    Connector_balance = log(Connector / (1 - Connector)),
    Provincial_balance = log(Provincial / (1 - Provincial)),
    Satellite_balance = log(Satellite / (1 - Satellite)),
    Peripheral_balance = log(Peripheral / (1 - Peripheral))
  )


# GAM MODELS ---
# Connector
gam1_interaction <- mgcv::gam(Connector_balance ~ s(Age, `RS-LANG`, bs = "fs") + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats,
  method = "ML"
)
summary(gam1_interaction)

gam1_interaction_1 <- mgcv::gam(Connector_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 1 (DMN, FPN, Language)"),
  method = "REML"
)
gam1_interaction_2 <- mgcv::gam(Connector_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 2 (SMN)"),
  method = "REML"
)
gam1_interaction_3 <- mgcv::gam(Connector_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 3 (CON, FPN, Language)"),
  method = "REML"
)
gam1_interaction_4 <- mgcv::gam(Connector_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 4 (FPN)"),
  method = "REML"
)

summary(gam1_interaction_1)
plot(gam1_interaction_1, shade = TRUE, pages = 1, scale = 0)
summary(gam1_interaction_2)
plot(gam1_interaction_2, shade = TRUE, pages = 1, scale = 0)
summary(gam1_interaction_3)
plot(gam1_interaction_3, shade = TRUE, pages = 1, scale = 0)
summary(gam1_interaction_4)
plot(gam1_interaction_4, shade = TRUE, pages = 1, scale = 0)

p.adjust(c(0.0222, 0.0479, 0.0796, 0.115), method = "fdr")

# Provincial
gam2_interaction <- mgcv::gam(Provincial_balance ~ s(Age, `RS-LANG`, bs = "fs") + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats,
  method = "ML"
)
summary(gam2_interaction)

gam2_interaction_1 <- mgcv::gam(Provincial_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 1 (DMN, FPN, Language)"),
  method = "REML"
)
gam2_interaction_2 <- mgcv::gam(Provincial_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 2 (SMN)"),
  method = "REML"
)
gam2_interaction_3 <- mgcv::gam(Provincial_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 3 (CON, FPN, Language)"),
  method = "REML"
)
gam2_interaction_4 <- mgcv::gam(Provincial_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 4 (FPN)"),
  method = "REML"
)

summary(gam2_interaction_1)
plot(gam2_interaction_1, shade = TRUE, pages = 1, scale = 0)
summary(gam2_interaction_2)
plot(gam2_interaction_2, shade = TRUE, pages = 1, scale = 0)
summary(gam2_interaction_3)
plot(gam2_interaction_3, shade = TRUE, pages = 1, scale = 0)
summary(gam2_interaction_4)
plot(gam2_interaction_4, shade = TRUE, pages = 1, scale = 0)

p.adjust(c(0.0222, 0.0479, 0.0796, 0.115), method = "fdr")

# Peripheral
gam3_interaction <- mgcv::gam(Peripheral_balance ~ s(Age, `RS-LANG`, bs = "fs") + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats,
  method = "ML"
)
summary(gam3_interaction)

gam3_interaction_1 <- mgcv::gam(Peripheral_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 1 (DMN, FPN, Language)"),
  method = "REML"
)
gam3_interaction_2 <- mgcv::gam(Peripheral_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 2 (SMN)"),
  method = "REML"
)
gam3_interaction_3 <- mgcv::gam(Peripheral_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 3 (CON, FPN, Language)"),
  method = "REML"
)
gam3_interaction_4 <- mgcv::gam(Peripheral_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 4 (FPN)"),
  method = "REML"
)

summary(gam3_interaction_1)
plot(gam3_interaction_1, shade = TRUE, pages = 1, scale = 0)
summary(gam3_interaction_2)
plot(gam3_interaction_2, shade = TRUE, pages = 1, scale = 0)
summary(gam3_interaction_3)
plot(gam3_interaction_3, shade = TRUE, pages = 1, scale = 0)
summary(gam3_interaction_4)
plot(gam3_interaction_4, shade = TRUE, pages = 1, scale = 0)

p.adjust(c(0.0222, 0.0479, 0.0796, 0.115), method = "fdr")

# Satellite

gam4_interaction <- mgcv::gam(Satellite_balance ~ s(Age, `RS-LANG`, bs = "fs") + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats,
  method = "ML"
)
summary(gam4_interaction)

gam4_interaction_1 <- mgcv::gam(Satellite_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 1 (DMN, FPN, Language)"),
  method = "REML"
)
gam4_interaction_2 <- mgcv::gam(Satellite_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 2 (SMN)"),
  method = "REML"
)
gam4_interaction_3 <- mgcv::gam(Satellite_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 3 (CON, FPN, Language)"),
  method = "REML"
)
gam4_interaction_4 <- mgcv::gam(Satellite_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(`RS-LANG` == "RS-NET 4 (FPN)"),
  method = "REML"
)

summary(gam4_interaction_1)
plot(gam4_interaction_1, shade = TRUE, pages = 1, scale = 0)
summary(gam4_interaction_2)
plot(gam4_interaction_2, shade = TRUE, pages = 1, scale = 0)
summary(gam4_interaction_3)
plot(gam4_interaction_3, shade = TRUE, pages = 1, scale = 0)
summary(gam4_interaction_4)
plot(gam4_interaction_4, shade = TRUE, pages = 1, scale = 0)

p.adjust(c(0.0222, 0.0479, 0.0796, 0.115), method = "fdr")

# FIGURE 2C

RSNET1 <- subsystem_level_stats %>%
  filter(grepl("1", `RS-LANG`)) %>%
  dplyr::select(Subj_ID, Age, `RS-LANG`, Connector_balance, Peripheral_balance, Satellite_balance, Provincial_balance) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Age", "RS-LANG"),
    names_to = "Functional_role",
    values_to = "Score"
  ) %>%
  group_by(Functional_role) %>%
  mutate(Score = as.numeric(scale(Score))) %>%
  ggplot(aes(Age, Score, color = Functional_role)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 3, method = "gam", formula = y ~ s(x, k = 3), alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(
    plot.title.position = "plot",
    legend.title = element_blank()
  ) +
  labs(y = "Relative proportion \n (z-scored)") +
  scale_color_manual(values = c("#E66101", "#FDB863", "#B2ABD2", "#5E3C99"), labels = c("Connector", "Peripheral", "Satellite", "Provincial")) +
  ggtitle("")

RSNET1

RSNET2 <- subsystem_level_stats %>%
  filter(grepl("2", `RS-LANG`)) %>%
  dplyr::select(Subj_ID, Age, `RS-LANG`, Connector_balance, Provincial_balance, Satellite_balance, Peripheral_balance) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Age", "RS-LANG"),
    names_to = "Functional_role",
    values_to = "Score"
  ) %>%
  group_by(Functional_role) %>%
  mutate(Score = as.numeric(scale(Score))) %>%
  ggplot(aes(Age, Score, color = Functional_role)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 3, method = "gam", formula = y ~ s(x, k = 3), alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(
    plot.title.position = "plot",
    legend.title = element_blank()
  ) +
  labs(y = "") +
  scale_color_manual(values = c("#E66101", "#FDB863", "#B2ABD2", "#5E3C99"), labels = c("Connector", "Peripheral", "Satellite", "Provincial")) +
  ggtitle("")

RSNET2

RSNET3 <- subsystem_level_stats %>%
  filter(grepl("3", `RS-LANG`)) %>%
  dplyr::select(Subj_ID, Age, `RS-LANG`, Connector_balance, Provincial_balance, Peripheral_balance) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Age", "RS-LANG"),
    names_to = "Functional_role",
    values_to = "Score"
  ) %>%
  group_by(Functional_role) %>%
  mutate(Score = as.numeric(scale(Score))) %>%
  ggplot(aes(Age, Score, color = Functional_role)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 3, method = "gam", formula = y ~ s(x, k = 3), alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(
    plot.title.position = "plot",
    legend.title = element_blank()
  ) +
  labs(y = "") +
  scale_color_manual(values = c("#E66101", "#FDB863", "#B2ABD2"), labels = c("Connector", "Peripheral", "Provincial")) +
  ggtitle("")

RSNET3

RSNET4 <- subsystem_level_stats %>%
  filter(grepl("4", `RS-LANG`)) %>%
  dplyr::select(Subj_ID, Age, `RS-LANG`, Connector_balance, Provincial_balance, Satellite_balance, Peripheral) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Age", "RS-LANG"),
    names_to = "Functional_role",
    values_to = "Score"
  ) %>%
  group_by(Functional_role) %>%
  mutate(Score = as.numeric(scale(Score))) %>%
  ggplot(aes(Age, Score, color = Functional_role)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 3, method = "gam", formula = y ~ s(x, k = 3), alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(
    plot.title.position = "plot",
    legend.title = element_blank()
  ) +
  labs(y = "") +
  scale_color_manual(values = c("#E66101", "#FDB863", "#B2ABD2", "#5E3C99"), labels = c("Connector", "Peripheral", "Satellite", "Provincial")) +
  ggtitle("")

RSNET4


Rmisc::multiplot(RSNET1, RSNET2, RSNET3, RSNET4, cols = 4)
