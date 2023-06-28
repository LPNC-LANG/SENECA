################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

################################################################################

source("_05_Topological_role_calculation.R")
source("_radarplotting_function.R")
source("_geometricmeanCruz.R")



ggplot(TFP_Subject_overview, aes(Age)) +
  geom_density(fill = "orange", alpha = .75, kernel = "epanechnikov", linewidth = 1, linetype = 5) +
  theme_pubclean(base_size = 18) +
  labs(x = "Age", y = "Density") +
  ggtitle("Density Plot of Age")

################################################################################
# GLOBAL LEVEL ----
################################################################################
covariate_FC <- fromJSON("D:/Analyses/1_Analyse_R/RS_CamCAN_LANG131_R/RS_LANG_CamCAN/OMST/mean_cor.json") %>% 
  as.data.frame() %>% 
  rename(mean_FC = colnames(.)[1])

efficiencies_ROI <- data_functional_role %>%
  dplyr::select(Subj_ID, Region, Eglob, Eloc) %>%
  mutate(Disruption = (Eglob - Eloc) / (Eloc + Eglob)) %>%
  group_by(Subj_ID) %>%
  summarize_at(vars(Eglob, Eloc, Disruption), mean) %>%
  ungroup()


TFP_Subject_overview <- cbind(TFP_Subject_overview, mean_FC = covariate_FC$mean_FC,
                     Eglob = efficiencies_ROI$Eglob,
                     Eloc = efficiencies_ROI$Eloc,
                     Disruption = efficiencies_ROI$Disruption)

summary(lm(mean_FC ~ Age + Gender_c, TFP_Subject_overview))


############################################################################
# Global Disruption
############################################################################
# # At the ROI level

mod <- lm(Eloc ~  Age + mean_FC + Gender_c, TFP_Subject_overview)
summary(mod)
effectsize::eta_squared(mod, ci = .95)


mod <- lm(Eglob ~ Age + mean_FC + Gender_c, TFP_Subject_overview)
summary(mod)
effectsize::eta_squared(mod, ci = .95)

mod <- lm(Disruption ~ Age + mean_FC + Gender_c, TFP_Subject_overview)
summary(mod)
effectsize::eta_squared(mod, ci = .95)


TFP_Subject_overview %>%  
  pivot_longer(c(Eglob, Eloc, Disruption), names_to = "Efficiencies", values_to = "value") %>% 
  group_by(Efficiencies) %>% 
  mutate(value = as.numeric(scale(value))) %>% 
  ggplot(aes(Age, value, color = Efficiencies)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_jitter(height = 0.05, alpha = 0.05, size = 4) +
  geom_smooth(linewidth = 2.5, method = "lm", alpha = .25) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(breaks = seq(-0.5, 0.7, 0.2)) +
  coord_cartesian(ylim = c(-0.5, 0.7), xlim = c(20, 90)) +
  scale_color_viridis_d(option = "D", alpha = .8,
                  labels = c("Balance I/S", "Global efficiency", "Local efficiency")) +
  theme_pubr(base_size = 18) +
  theme(legend.position = "bottomleft",
        legend.title = element_blank()) +
  labs(y = "Normalized efficiencies")
  # geom_vline(xintercept = 54, color = "red", linewidth = 1.5, alpha = 1) 


############################################################################
# Bayesian non-parametric multiplicative replacement
############################################################################
library(zCompositions)
library(compositions)

projection_to_euclidean_space <- TFP_Subject_overview %>%
  dplyr::select(Connector, Satellite, Provincial, Peripheral) %>% 
  acomp(.)

TFP_Subject_overview_imputed <- cbind(TFP_Subject_overview[,5:11],
                             projection_to_euclidean_space)

TFP_Subject_overview_imputed %>%  
  dplyr::select(Subj_ID, Age, Connector:Peripheral) %>%
  # dplyr::select(Subj_ID, Age, `Global Interface`:`Sub-Interface`) %>% 
  pivot_longer(
    cols = !c("Subj_ID", "Age"),
    names_to = "Functional_role",
    values_to = "Score"
  ) %>% 
  group_by(Functional_role) %>% 
  mutate(Score = as.numeric(scale(Score))) %>% 
  ggplot(aes(Age, Score, color = Functional_role)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_jitter(height = 0.05, alpha = 0.1) +
  geom_smooth(linewidth = 2, method = "lm", alpha = .3) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  coord_cartesian(ylim = c(-0.4, 0.4)) +
  scale_color_brewer(palette = "PuOr") +
  geom_vline(xintercept = 54, color = "red", linewidth = 1.5, alpha = 1) +
  theme_pubclean(base_size = 18) +
  theme(plot.title.position = "plot",
        legend.title = element_blank()
  ) +
  labs(y = "Raw proportions") +
  
  ggtitle("A\n\n Lifespan dynamic of topological roles with raw proportions")


############################################################################
# DEFINING ILRs (ISOMETRIC LOG-RATIOS)
############################################################################

global_level_stats <- TFP_Subject_overview_imputed %>% 
  # Connector_balance
  mutate(Connector_balance = ((3/4)^0.5)*log((Connector)/((Peripheral*Provincial*Satellite)^(1/3)))) %>%
  # Provincial_balance
  mutate(Provincial_balance = ((3/4)^0.5)*log((Provincial)/((Connector*Satellite*Peripheral)^(1/3)))) %>% 
  mutate(Satellite_balance = ((3/4)^0.5)*log(Satellite/(Connector*Satellite*Peripheral)^(1/3))) %>% 
  # Peripheral_balance
  mutate(Peripheral_balance = ((3/4)^0.5)*log((Peripheral)/((Connector*Provincial*Satellite)^(1/3))))



gam1 <- mgcv::gam(Connector_balance~s(Age) + mean_FC + Gender_c, data = global_level_stats, method = "ML")
lin1 <- lm(Connector_balance~ scale(Age, scale = T) + mean_FC + Gender_c, data = global_level_stats)
summary(lin1)
confint(lin1)

source("PRE.R")
PRE(lin1)

vif(lin1)
plot(ggeffects::ggpredict(gam1)$Age)

AIC(gam1, lin1)
effectsize::eta_squared(lin1, ci = .95)
# performance::check_model(lin1)

gam2 <- mgcv::gam(Provincial_balance~s(Age) + mean_FC + Gender_c, data = global_level_stats, method = "ML")
lin2 <- lm(Provincial_balance~scale(Age, scale = T) + mean_FC + Gender_c, data = global_level_stats)
summary(lin2)
confint(lin2)

source("PRE.R")
PRE(lin2)

gam3 <- mgcv::gam(Peripheral_balance~s(Age) + mean_FC + Gender_c, data = global_level_stats, method = "ML")
lin3 <- lm(Peripheral_balance~scale(Age, scale = T) + mean_FC + Gender_c, data = global_level_stats)
summary(lin3)
confint(lin3)

source("PRE.R")
PRE(lin3)


lin3 <- lm(Satellite_balance~scale(Age, scale = T) + mean_FC + Gender_c, data = global_level_stats)
summary(lin3)
confint(lin3)

source("PRE.R")
PRE(lin3)


global_level_stats %>%  
  pivot_longer(c(Connector_balance, 
                 Provincial_balance,
                 Peripheral_balance,
                 Satellite_balance),
               names_to = "topological_balances",
               values_to = "balance") %>% 
  group_by(topological_balances) %>% 
  # mutate(balance = as.numeric(scale(balance))) %>% 
  ggplot(aes(Age, balance, color = topological_balances)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_hline(yintercept = 0, color = "red") +
  
  geom_jitter(height = 0.05, alpha = 0.1) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x), alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.2)) +
  # geom_vline(xintercept = 54, color = "red", linewidth = 1.5, alpha = 1) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(plot.title.position = "plot",
        legend.title = element_blank()) +
  labs(y = "ilr-coordinates") +
  scale_color_brewer(palette = "PuOr") +
  ggtitle("")


################################################################################
################################################################################
# SUBSYSTEM LEVEL

covariate_FC_subsystem <- fromJSON("D:/Analyses/1_Analyse_R/RS_CamCAN_LANG131_R/RS_LANG_CamCAN/OMST/mean_cor.json") %>% 
  as.data.frame() %>% 
  rename(mean_FC = colnames(.)[1])

TFP_Subject_subsystem <- merge(TFP_Subject_subsystem, covariate_FC_subsystem %>% mutate(Subj_ID = rep(seq(628))), by = "Subj_ID") %>% 
  merge(., TFP_Subject_overview %>% dplyr::select(Subj_ID, Gender_c), by = "Subj_ID")

list_TFP_Subject_subsystem <- TFP_Subject_subsystem %>%  
  filter(Consensus_vector_0.15 != "RS-NET 5 (VMM)") %>% 
  group_by(Consensus_vector_0.15) %>% group_split()

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
  
  tmp <- cbind(tmp_raw %>% dplyr::select(-c(Connector, Satellite, Provincial, Peripheral)),
                           tmp_coda_modular_bis)
  
  list_imputed[[i]] <- tmp

}

TFP_Subject_subsystem_imputed <- rbindlist(list_imputed)
TFP_Subject_subsystem_imputed$Consensus_vector_0.15 <- factor(TFP_Subject_subsystem_imputed$Consensus_vector_0.15,
                                                          ordered = FALSE)

############################################################################
# Nodal Disruption
############################################################################

subsystem_level_stats <- TFP_Subject_subsystem_imputed %>%  
  # Connector_balance
  mutate(Connector_balance = ((3/4)^0.5)*log((Connector)/((Peripheral*Provincial*Satellite)^(1/3)))) %>%
  # Provincial_balance
  mutate(Provincial_balance = ((3/4)^0.5)*log((Provincial)/((Connector*Satellite*Peripheral)^(1/3)))) %>% 
  mutate(Satellite_balance = ((3/4)^0.5)*log(Satellite/(Connector*Satellite*Peripheral)^(1/3))) %>% 
  # Peripheral_balance
  mutate(Peripheral_balance = ((3/4)^0.5)*log((Peripheral)/((Connector*Provincial*Satellite)^(1/3))))





gam1_interaction <- mgcv::gam(Connector_balance~s(Age, Consensus_vector_0.15, bs = "fs") + mean_FC + Gender_c,
                              data = subsystem_level_stats,
                              method = "REML") 
summary(gam1_interaction)

gam1_interaction_detailed <- mgcv::gam(Connector_balance~Consensus_vector_0.15 + s(Age, by = Consensus_vector_0.15) + mean_FC + Gender_c,
          data = subsystem_level_stats,
          method = "REML") 

summary(gam1_interaction_detailed)
p.adjust(c(0.142285, 0.013859, 0.602495, 0.000201), method = "fdr")

plot(gam1_interaction_detailed, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
mgcv::gam.check(gam1_interaction_detailed)
mgcv::vis.gam(gam1_interaction_detailed, theta = 30, view = c("Consensus_vector_0.15", "Age"))
gratia::draw(gam1_interaction_detailed)
gam.vcomp(gam1_interaction_detailed, rescale = T)

fd <- gratia::fderiv(gam1_interaction_detailed)
ci <- confint(fd, type = "confidence")
head(ci)
ci <- cbind(ci, x = as.vector(fd[['eval']]))
  
ggplot(ci, aes(x = x.Age, y = est, group = term)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line() +
  facet_wrap( ~ term)


sumup <- subsystem_level_stats %>% mutate(Age_group = ifelse(Age <= 50, "Young",
                                                              ifelse(Age > 55, "Old", "Middle"))) %>% 
  group_by(Consensus_vector_0.15, Age_group) %>% 
  get_summary_stats(Connector, type = "full") %>% as.data.frame()


# Provincial_balance
gam2_interaction <- mgcv::gam(Provincial_balance~s(Age, Consensus_vector_0.15, bs = "fs") + mean_FC + Gender_c,
                              data = subsystem_level_stats,
                              method = "REML") 
summary(gam2_interaction)

gam2_interaction_detailed <- mgcv::gam(Provincial_balance~Consensus_vector_0.15 + s(Age, by = Consensus_vector_0.15) + mean_FC + Gender_c,
                                       data = subsystem_level_stats,
                                       method = "REML") 

summary(gam2_interaction_detailed)
p.adjust(c(0.24413,  0.00463, 0.23770,2.29e-07), method = "fdr")
plot(gam2_interaction_detailed, shade = TRUE, pages = 1, scale = 0)
mgcv::gam.check(gam2_interaction_detailed)

# Peripheral_balance
gam3_interaction <- mgcv::gam(Peripheral_balance~s(Age, Consensus_vector_0.15, bs = "fs") + mean_FC + Gender_c,
                              data = subsystem_level_stats,
                              method = "REML") 
summary(gam3_interaction)

gam3_interaction_detailed <- mgcv::gam(Peripheral_balance~Consensus_vector_0.15 + s(Age, by = Consensus_vector_0.15) + mean_FC + Gender_c,
                                       data = subsystem_level_stats,
                                       method = "REML") 

summary(gam3_interaction_detailed)
p.adjust(c(0.0002941, 0.128094, 0.336297,0.016742), method = "fdr")
plot(gam3_interaction_detailed, shade = TRUE, pages = 1, scale = 0)
gratia::draw(gam3_interaction_detailed)

# Satellite_balance
gam4_interaction_detailed <- mgcv::gam(Satellite_balance~Consensus_vector_0.15 + s(Age, by = Consensus_vector_0.15) + mean_FC + Gender_c,
                                       data = subsystem_level_stats,
                                       method = "REML") 

summary(gam4_interaction_detailed)
p.adjust(c(4.36e-05,0.58361, 0.60913,0.00685), method = "fdr")
plot(gam4_interaction_detailed, shade = TRUE, pages = 1, scale = 0)

gratia::draw(gam4_interaction_detailed)




# PLOTS

RSNET1 <- subsystem_level_stats %>%
  filter(grepl("1",Consensus_vector_0.15)) %>% 
  dplyr::select(Subj_ID, Age, Consensus_vector_0.15, Connector_balance, Peripheral_balance, Satellite_balance) %>% 
  pivot_longer(cols = !c("Subj_ID", "Age", "Consensus_vector_0.15"),
               names_to = "Functional_role",
               values_to = "Score"
) %>%  
  group_by(Functional_role) %>% 
  mutate(Score = as.numeric(scale(Score))) %>% 
  ggplot(aes(Age, Score, color = Functional_role)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x), alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 20)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(plot.title.position = "plot",
        legend.title = element_blank()) +
  labs(y = "Raw ilr-coordinates") +
  scale_color_manual(values = c("#E66101", "#FDB863", "#B2ABD2"), labels = c("Connector", "Peripheral", "Satellite")) +
  ggtitle("")

RSNET1

RSNET2 <- subsystem_level_stats %>%
  filter(grepl("2",Consensus_vector_0.15)) %>% 
  dplyr::select(Subj_ID, Age, Consensus_vector_0.15, Connector_balance, Provincial_balance) %>% 
  pivot_longer(cols = !c("Subj_ID", "Age", "Consensus_vector_0.15"),
               names_to = "Functional_role",
               values_to = "Score"
  ) %>% 
  group_by(Functional_role) %>% 
  mutate(Score = as.numeric(scale(Score))) %>% 
  ggplot(aes(Age, Score, color = Functional_role)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x), alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 20)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(plot.title.position = "plot",
        legend.title = element_blank()) +
  labs(y = "Raw ilr-coordinates") +
  scale_color_manual(values = c("#E66101", "#5E3C99"), labels = c("Connector", "Provincial")) +
  ggtitle("")

RSNET2

RSNET4 <- subsystem_level_stats %>%
  filter(grepl("4",Consensus_vector_0.15)) %>% 
  dplyr::select(Subj_ID, Age, Consensus_vector_0.15, Connector_balance, Provincial_balance, Satellite_balance) %>% 
  pivot_longer(cols = !c("Subj_ID", "Age", "Consensus_vector_0.15"),
               names_to = "Functional_role",
               values_to = "Score"
  ) %>% 
  group_by(Functional_role) %>% 
  mutate(Score = as.numeric(scale(Score))) %>% 
  ggplot(aes(Age, Score, color = Functional_role)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x), alpha = .1) +
  scale_x_continuous(breaks = seq(20, 90, 20)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_pubr(base_size = 18) +
  theme(plot.title.position = "plot",
        legend.title = element_blank()) +
  labs(y = "ilr-coordinates \n (z-scored)") +
  scale_color_manual(values = c("#E66101","#5E3C99", "#B2ABD2"), labels = c("Connector", "Provincial", "Satellite")) +
  ggtitle("")

RSNET4


Rmisc::multiplot(RSNET1, RSNET2, RSNET4, cols = 3)
