################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105
# 2024

source("./code/_02_Topological_roles.R")
source("./code/helper_functions/PRE.R")
library(ggpubr)
################################################################################

# Density plot of the age distribution of the CamCAN cohort
ggplot(TFP_Subject_overview, aes(Age)) +
  geom_density(fill = "orange", alpha = .75, kernel = "epanechnikov", linewidth = 1, linetype = 5) +
  theme_pubclean(base_size = 18) +
  labs(x = "Age", y = "Density") +
  ggtitle("Density Plot of Age")

# COVARIATES
# TIV - covariate
participants <- read_excel("./data/cognitive_data_628/participant_data_T1.xlsx")[, c(1:2, 7)] %>%
  dplyr::rename(Subj_ID = Subject) %>%
  replace("Subj_ID", seq_len(628))
# Mean FC - covariate
covariate_FC <- fromJSON("./data/data_community_metrics/mean_cor.json") %>%
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

summary(lm(mean_FC ~ Age + Gender_c + TIV, TFP_Subject_overview))

ggplot(aes(Age, mean_FC), data = TFP_Subject_overview %>% group_by(Age) %>% summarise_at(vars(mean_FC), mean)) +
  geom_point() +
  geom_smooth()


############################################################################
# SYSTEM LEVEL
############################################################################
mod <- lm(Eloc ~ Age + mean_FC + Gender_c + TIV, TFP_Subject_overview)
summary(mod)
effectsize::eta_squared(mod, ci = .95)


mod <- lm(Eglob ~ Age + mean_FC + Gender_c + TIV, TFP_Subject_overview)
summary(mod)
effectsize::eta_squared(mod, ci = .95)

mod <- lm(Disruption ~ Age + mean_FC + Gender_c + TIV, TFP_Subject_overview)
summary(mod)
effectsize::eta_squared(mod, ci = .95)

# FIGURE 2A
TFP_Subject_overview %>%
  pivot_longer(c(Eglob, Eloc, Disruption), names_to = "Efficiencies", values_to = "value") %>%
  group_by(Efficiencies) %>%
  mutate(value = as.numeric(scale(value))) %>%
  ggplot(aes(Age, value, color = Efficiencies)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_jitter(height = 0.05, alpha = 0.05, size = 2) +
  geom_smooth(linewidth = 3, method = "lm", alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(breaks = seq(-0.5, 0.7, 0.2)) +
  coord_cartesian(ylim = c(-0.5, 0.5), xlim = c(20, 90)) +
  scale_color_viridis_d(
    option = "D", alpha = .8,
    labels = c("Balance I/S", "Global efficiency", "Local efficiency")
  ) +
  theme_pubr(base_size = 18) +
  theme(
    legend.position = "bottomleft",
    legend.title = element_blank()
  ) +
  labs(y = "Normalized efficiencies")
# geom_vline(xintercept = 54, color = "red", linewidth = 1.5, alpha = 1)


############################################################################
# Bayesian non-parametric multiplicative replacement
############################################################################
library(zCompositions)
library(compositions)

imputation <- TFP_Subject_overview %>%
  dplyr::select(Connector, Satellite, Provincial, Peripheral) %>%
  acomp(.)

TFP_Subject_overview_imputed <- cbind(
  TFP_Subject_overview[, 5:11],
  imputation
)

system_level_stats <- TFP_Subject_overview_imputed %>%
  # Connector_balance
  mutate(
    Connector_balance = log(Connector / (1 - Connector)),
    Provincial_balance = log(Provincial / (1 - Provincial)),
    Satellite_balance = log(Satellite / (1 - Satellite)),
    Peripheral_balance = log(Peripheral / (1 - Peripheral))
  )

lin1 <- lm(Connector_balance ~ scale(Age, scale = T) + mean_FC + Gender_c + TIV, data = global_level_stats)
summary(lin1)
confint(lin1)
PRE(lin1)

lin2 <- lm(Provincial_balance ~ scale(Age, scale = T) + mean_FC + Gender_c + TIV, data = global_level_stats)
summary(lin2)
confint(lin2)
PRE(lin2)

lin3 <- lm(Peripheral_balance ~ scale(Age, scale = T) + mean_FC + Gender_c + TIV, data = global_level_stats)
summary(lin3)
confint(lin3)
PRE(lin3)


lin4 <- lm(Satellite_balance ~ scale(Age, scale = T) + mean_FC + Gender_c + TIV, data = global_level_stats)
summary(lin4)
confint(lin4)
PRE(lin4)

# FIGURE 2B
system_level_stats %>%
  pivot_longer(
    c(
      Connector_balance,
      Provincial_balance,
      Peripheral_balance,
      Satellite_balance
    ),
    names_to = "topological_balances",
    values_to = "balance"
  ) %>%
  group_by(topological_balances) %>%
  mutate(balance = as.numeric(scale(balance))) %>%
  ggplot(aes(Age, balance, color = topological_balances)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_jitter(height = 0.05, alpha = 0.1) +
  geom_smooth(linewidth = 3, method = "lm", alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.2)) +
  # geom_vline(xintercept = 54, color = "red", linewidth = 1.5, alpha = 1) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(
    plot.title.position = "plot",
    legend.title = element_blank()
  ) +
  labs(y = "Relative proportion\n (z-scored)") +
  scale_color_brewer(palette = "PuOr") +
  ggtitle("")


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
gam1_interaction <- mgcv::gam(Connector_balance ~ s(Age, Consensus_vector_0.15, bs = "fs") + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats,
  method = "ML"
)
summary(gam1_interaction)

gam1_interaction_1 <- mgcv::gam(Connector_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 1 (DMN, FPN, Language)"),
  method = "REML"
)
gam1_interaction_2 <- mgcv::gam(Connector_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 2 (SMN)"),
  method = "REML"
)
gam1_interaction_3 <- mgcv::gam(Connector_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 3 (CON, FPN, Language)"),
  method = "REML"
)
gam1_interaction_4 <- mgcv::gam(Connector_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 4 (FPN)"),
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
gam2_interaction <- mgcv::gam(Provincial_balance ~ s(Age, Consensus_vector_0.15, bs = "fs") + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats,
  method = "ML"
)
summary(gam2_interaction)

gam2_interaction_1 <- mgcv::gam(Provincial_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 1 (DMN, FPN, Language)"),
  method = "REML"
)
gam2_interaction_2 <- mgcv::gam(Provincial_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 2 (SMN)"),
  method = "REML"
)
gam2_interaction_3 <- mgcv::gam(Provincial_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 3 (CON, FPN, Language)"),
  method = "REML"
)
gam2_interaction_4 <- mgcv::gam(Provincial_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 4 (FPN)"),
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
gam3_interaction <- mgcv::gam(Peripheral_balance ~ s(Age, Consensus_vector_0.15, bs = "fs") + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats,
  method = "ML"
)
summary(gam3_interaction)

gam3_interaction_1 <- mgcv::gam(Peripheral_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 1 (DMN, FPN, Language)"),
  method = "REML"
)
gam3_interaction_2 <- mgcv::gam(Peripheral_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 2 (SMN)"),
  method = "REML"
)
gam3_interaction_3 <- mgcv::gam(Peripheral_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 3 (CON, FPN, Language)"),
  method = "REML"
)
gam3_interaction_4 <- mgcv::gam(Peripheral_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 4 (FPN)"),
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

gam4_interaction <- mgcv::gam(Satellite_balance ~ s(Age, Consensus_vector_0.15, bs = "fs") + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats,
  method = "ML"
)
summary(gam4_interaction)

gam4_interaction_1 <- mgcv::gam(Satellite_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 1 (DMN, FPN, Language)"),
  method = "REML"
)
gam4_interaction_2 <- mgcv::gam(Satellite_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 2 (SMN)"),
  method = "REML"
)
gam4_interaction_3 <- mgcv::gam(Satellite_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 3 (CON, FPN, Language)"),
  method = "REML"
)
gam4_interaction_4 <- mgcv::gam(Satellite_balance ~ s(Age, k = 3) + mean_FC + Gender_c + TIV,
  data = subsystem_level_stats %>% filter(Consensus_vector_0.15 == "RS-NET 4 (FPN)"),
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
  filter(grepl("1", Consensus_vector_0.15)) %>%
  dplyr::select(Subj_ID, Age, Consensus_vector_0.15, Connector_balance, Peripheral_balance, Satellite_balance, Provincial_balance) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Age", "Consensus_vector_0.15"),
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
  filter(grepl("2", Consensus_vector_0.15)) %>%
  dplyr::select(Subj_ID, Age, Consensus_vector_0.15, Connector_balance, Provincial_balance, Satellite_balance, Peripheral_balance) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Age", "Consensus_vector_0.15"),
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
  filter(grepl("3", Consensus_vector_0.15)) %>%
  dplyr::select(Subj_ID, Age, Consensus_vector_0.15, Connector_balance, Provincial_balance, Peripheral_balance) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Age", "Consensus_vector_0.15"),
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
  filter(grepl("4", Consensus_vector_0.15)) %>%
  dplyr::select(Subj_ID, Age, Consensus_vector_0.15, Connector_balance, Provincial_balance, Satellite_balance, Peripheral) %>%
  pivot_longer(
    cols = !c("Subj_ID", "Age", "Consensus_vector_0.15"),
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
