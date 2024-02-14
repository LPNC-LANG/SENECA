################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105
# 2024

################################################################################
source("./code/_04_NeurocognitiveAnalysis_CCA.R")
library(ggchicklet)
library(ggpubr)
################################################################################

# LIFESPAN COG PERF ----
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
          ifelse(Cognitive_assessment == "ToT_Ratio_inverse", "Tip-of-the-tongue", Cognitive_assessment)
        )
      )
    )
  )) %>%
  group_by(Cognitive_assessment) %>%
  mutate(performance = as.numeric(scale(performance))) %>%
  ggplot(aes(Age, performance, color = Cognitive_assessment)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_jitter(height = 0.05, alpha = 0.08) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x), alpha = .3) +
  scale_x_continuous(breaks = c(20, 40, 55, 70, 90)) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_color_brewer(palette = "Paired") +
  theme_pubr(
    base_size = 20,
    legend = "none"
  ) +
  theme(
    plot.title.position = "plot",
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.title.x = element_blank()
  ) +
  labs(y = "Normalized score") +
  facet_wrap(~Cognitive_assessment, scale = "free", ncol = 4)

# CCA FIGURE 4 ----

Brain_Mode <- as.matrix(brain_set) %*% cc_results$xcoef[, 1]
Brain_Mode_2 <- as.matrix(brain_set) %*% cc_results$xcoef[, 2]
Cognitive_Mode <- as.matrix(cog_set) %*% cc_results$ycoef[, 1]
Cognitive_Mode_2 <- as.matrix(cog_set) %*% cc_results$ycoef[, 2]

plot_cca <- Data_CCA_full %>%
  mutate(
    Age = Age,
    Brain_Mode = Brain_Mode * (-1),
    Brain_Mode_2 = Brain_Mode_2 * (-1),
    Cognitive_Mode = Cognitive_Mode * (-1),
    Cognitive_Mode_2 = Cognitive_Mode_2 * (-1),
  )


# plot_cca %>%
#   dplyr::select(Age, Brain_Mode, Brain_Mode_2, Cognitive_Mode, Cognitive_Mode_2) %>%
#   pivot_longer(c("Brain_Mode", "Brain_Mode_2", "Cognitive_Mode", "Cognitive_Mode_2"), names_to = "Variates", values_to = "Values") %>%
#   ggplot(aes(Age, Values, color = Variates)) +
#   geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
#   geom_jitter(height = 0.05, alpha = 0) +
#   geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x, k = 3), alpha = .1) +
#   scale_x_continuous(breaks = seq(20, 90, 5)) +
#   scale_y_continuous(breaks = seq(-3/2, 3/2, 1/2)) +
#   coord_cartesian(ylim = c(-3/2, 3/2)) +
#   # scale_color_manual(values = c("#838f93", "#FDCC8A", "#08519C")) +
#   theme_pubr(base_size = 18) +
#   theme(plot.title.position = "plot") +
#   labs(y = "Cognitive performance") +
#   ggtitle("")

# Combining both latent components ----
library(FactoMineR)
library(ggpubr)

plot_cca$pca_L1 <- (FactoMineR::PCA(plot_cca[, c(28, 30)]))$ind$coord[, 1]
plot_cca$pca_L2 <- (FactoMineR::PCA(plot_cca[, c(29, 31)]))$ind$coord[, 1]

mod <- mgcv::gam(pca_L1 ~ s(Age, k = 3), data = plot_cca)
summary(mod)
plot(mod)

mod <- mgcv::gam(pca_L2 ~ s(Age, k = 3), data = plot_cca)
summary(mod)
plot(mod)


# plot_cca <- plot_cca %>% mutate(
#   age_groups = ifelse(Age < 45, 'young',
#                       ifelse(Age <=55 & Age >= 45, 'middle',
#                              'old')))
#
# plot_cca$age_groups <- factor(plot_cca$age_groups, levels = c("middle", "young", "old"))
#
# plot_cca %>%
#   ggplot(aes(x = pca_L2, y = pca_L1, color = age_groups)) +
#   geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
#   geom_vline(xintercept = 0, size = 1, linetype = "dashed") +
#   # geom_smooth(method = "loess", color = 'black', size = 2, alpha = 0.3) +
#   geom_point(aes(color = age_groups), size = 3) +
#   scale_color_manual(values = c("#E66101", "#FDB863", "darkviolet")) +
#   coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)) +
#   # scale_color_viridis(name = 'Cognitive age', discrete = T, option = "magma")+
#   theme_pubclean(base_size = 16) +
#   labs(y = 'LC1 (domain-general)',
#        x = 'LC2 (language-specific)')


# Figure 4A ----

plot_cca$interplay <- (plot_cca$pca_L1 + plot_cca$pca_L2) / 2
plot_cca$z_Naming <- (plot_cca$pca_L1 * (.49 / (.49 + .40)) + plot_cca$pca_L2 * (0.40 / (0.49 + 0.40))) / 2


plot_cca_plot <- plot_cca %>%
  dplyr::select(Age, pca_L1, pca_L2, interplay, z_Naming) %>%
  pivot_longer(c("pca_L1", "pca_L2", "interplay", "z_Naming"), names_to = "latent", values_to = "Values")

plot_cca_plot %>%
  ggplot(aes(Age, Values, color = latent)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x, k = 3), alpha = 0.2) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(breaks = seq(-1.5, 1.7, 0.8)) +
  coord_cartesian(ylim = c(-1.5, 1.7)) +
  scale_color_manual(values = c("red", "#08519C", "#FDCC8A", "black")) +
  theme_pubr(
    base_size = 16,
    legend = "none"
  ) +
  theme(plot.title.position = "plot") +
  labs(y = "", x = "") +
  # labs(y = "Latent component score", x = "Age") +
  ggtitle("")

# Figure 4B ----
plot_cca$DMN_deactivation <- (plot_cca$pca_L1 * (-0.2 / (0.2 + 0.14)) + plot_cca$pca_L2 * (0.14 / (0.2 + 0.14))) / 2
plot_cca$BFPN_integration <- (plot_cca$pca_L1 * (-0.22 / (0.22 + 0.06)) + plot_cca$pca_L2 * (0.06 / (0.22 + 0.06))) / 2
plot_cca$ASMN_subcortical_integration <- (plot_cca$pca_L1 * (-0.09 / (0.09 + 0.23)) + plot_cca$pca_L2 * (0.23 / (0.09 + 0.23))) / 2

# DMN FPN coupling
# I use *(-1) because integration mitigates executive decline i.e., opposite to the direction of Variate I
plot_cca$DMN_FPN_coupling <- (plot_cca$BFPN_integration * (-1) + plot_cca$DMN_deactivation)

plot_cca_plot <- plot_cca %>%
  dplyr::select(Age, DMN_deactivation, BFPN_integration, ASMN_subcortical_integration, DMN_FPN_coupling) %>%
  pivot_longer(c("DMN_deactivation", "BFPN_integration", "ASMN_subcortical_integration", "DMN_FPN_coupling"), names_to = "latent", values_to = "Values")

plot_cca_plot %>%
  ggplot(aes(Age, Values, color = latent)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 2, method = "gam", formula = y ~ s(x, k = 3), alpha = 0.2) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(breaks = seq(-.75, 0.75, 0.5)) +
  coord_cartesian(ylim = c(-.75, 1)) +
  scale_color_manual(values = c("#FDCC8A", "#08519C", "red", "black")) +
  theme_pubr(
    base_size = 16,
    legend = "none"
  ) +
  theme(plot.title.position = "plot") +
  labs(y = "", x = "") +
  # labs(y = "Latent component score", x = "Age") +
  ggtitle("")





# Get derivatives ----

library(gratia)
ub <- 0.01
lb <- -0.05
this_font_size <- 12
# For FIGURE 4A
modobj <- mgcv::gam(interplay ~ s(Age, k = 3),
  data = plot_cca,
  method = "REML"
)
# For FIGURE 4B
modobj <- mgcv::gam(DMN_FPN_coupling ~ s(Age, k = 3),
  data = plot_cca,
  method = "REML"
)

# get model derivatives
derv <- derivatives(modobj, interval = "confidence", unconditional = F, order = 2L, eps = 1e-6) # from gratia. "confidence" for point-wise intervals

# add significance variable (true or false)
derv <- derv %>%
  mutate(sig = !(0 > lower & 0 < upper)) # derivative is sig if the lower CI is not < 0 while the upper CI is > 0 (i.e., when the CI does not include 0)
# new variable with only significant derivatives (non-sig. ones are set to 0)
derv$sig_deriv <- derv$derivative * derv$sig

# print changes range if significant
if (all(derv$sig_deriv == 0)) {
  cat(sprintf("\n No significant change in %s \n", nmf_network))
} else {
  cat(sprintf("\nSig change: %1.2f - %1.2f\n", min(derv$data[derv$sig == T]), max(derv$data[derv$sig == T])))
}

# plot change
derv[derv == 0] <- NA
if (is.null(lb) & is.null(ub)) {
  d <- ggplot(data = derv) +
    geom_tile(aes(x = data, y = .5, fill = sig_deriv)) +
    scale_fill_gradient(
      low = "darkblue", high = "darkorange", na.value = "white",
      limits = c(min(derv$sig_deriv), max(derv$sig_deriv))
    )
} else {
  d <- ggplot(data = derv) +
    geom_tile(aes(x = data, y = .5, fill = sig_deriv)) +
    scale_fill_gradient(
      low = "darkblue", high = "darkorange", na.value = "white",
      limits = c(min(derv$sig_deriv), max(derv$sig_deriv))
    )
}


d +
  coord_cartesian(xlim = c(45, 60)) +
  labs(x = "", fill = "") +
  scale_x_continuous(breaks = seq(45, 60, 1)) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = this_font_size),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = this_font_size),
    legend.text = element_text(size = this_font_size),
    legend.title = element_text(size = this_font_size),
    axis.title = element_text(size = this_font_size),
    legend.key.width = unit(1, "cm"),
    legend.position = "right",
    plot.margin = unit(c(0, 0, 0.5, 0), "cm"), # Top, left,Bottom, right
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour = "black", size = 1.5),
    axis.line.y = element_line(colour = "black", size = 1.5),
    axis.ticks.length = unit(.25, "cm"),
    axis.text = element_text(size = 50)
  ) +
  guides(fill = guide_colorbar(
    ticks = T,
    ticks.linewidth = 1,
    ticks.colour = "black",
    draw.ulim = T,
    frame.colour = "black",
    frame.linetype = 1,
    frame.linewidth = 1,
    reverse = T,
    direction = "horizontal",
    title.position = "top"
  )) +
  geom_rect(aes(ymin = 0, ymax = 1, xmin = min(data), xmax = max(data)), color = "black", fill = "white", alpha = 0)

# Graphical ABSTRACT ----
graph_abst <- plot_cca %>%
  # add Global efficiency representing global homeostasis
  merge(., TFP_Subject_overview[, c(5, 10)], by = "Subj_ID") %>%
  dplyr::select(Age, DMN_deactivation, z_Naming, Eglob) %>%
  pivot_longer(c("DMN_deactivation", "z_Naming", "Eglob"), names_to = "latent", values_to = "Values")
graph_abst %>%
  ggplot(aes(Age, Values, color = latent)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter(height = 0.05, alpha = 0) +
  geom_smooth(linewidth = 4, method = "gam", formula = y ~ s(x, k = 3), alpha = 0.2) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(breaks = seq(-.5, 0.5, 0.25)) +
  coord_cartesian(ylim = c(-.5, .5)) +
  scale_color_manual(values = c("red", "darkgreen", "black")) +
  theme_pubr(base_size = 30, ) +
  theme(plot.title.position = "plot") +
  labs(y = "", x = "") +
  # labs(y = "Latent component score", x = "Age") +
  ggtitle("")

# Figure 4C ----
# COGNITIVE VARIATE 1 - DG MECHANISM
mod <- mgcv::gam(Cognitive_Mode ~ s(Age, k = 3), data = plot_cca)
summary(mod)
plot(mod)

plot_loadings_cog <- cc_loadings$corr.Y.yscores[, 1] %>%
  as.data.frame() %>%
  rownames_to_column("Cognitive_assessment") %>%
  plyr::rename(c("." = "loading")) %>%
  mutate(loading = loading * (-1)) %>%
  mutate(labels = ifelse(Cognitive_assessment == "Verbal_Fluency", "Verbal Fluency",
    ifelse(Cognitive_assessment == "Story_Recall", "LTM",
      ifelse(Cognitive_assessment == "ToT_Ratio_inverse", "Tip-of-the-tongue",
        ifelse(Cognitive_assessment == "Hotel_Task_inverse", "Multitasking",
          ifelse(Cognitive_assessment == "Sentence_Comprehension_c", "Sen. Comp.",
            ifelse(Cognitive_assessment == "Cattell", "Fluid Intell.",
              ifelse(Cognitive_assessment == "Naming", "Naming",
                ifelse(Cognitive_assessment == "Proverb", "Semantic Abstraction",
                  NA
                )
              )
            )
          )
        )
      )
    )
  ))

plot_loadings_cog$labels <- factor(plot_loadings_cog$labels) %>%
  fct_reorder(plot_loadings_cog$loading, .desc = FALSE)

ggplot(
  plot_loadings_cog %>% na.omit(),
  aes(x = labels, y = loading)
) +
  # geom_col(aes(fill = loading), alpha = .7) +
  # scale_fill_distiller(palette = "RdBu", direction = 1) +
  geom_chicklet(radius = grid::unit(5, "mm"), aes(fill = loading, alpha = .8, color = loading)) +
  scale_fill_fermenter(palette = "Blues", direction = 1) +
  scale_y_continuous(breaks = seq(0, 0.8, 0.1)) +
  coord_flip() +
  geom_text(aes(y = -0.01, label = labels, fontface = "bold", hjust = "left"),
    nudge_y = .025,
    size = 7
  ) +
  theme_pubr(
    base_size = 22,
    legend = "none",
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank()
  )


# COGNITIVE VARIATE 2 - LS MECHANISM
mod <- mgcv::gam(Cognitive_Mode_2 ~ s(Age, k = 3), data = plot_cca)
summary(mod)
plot(mod)

plot_loadings_cog <- cc_loadings$corr.Y.yscores[, 2] %>%
  as.data.frame() %>%
  rownames_to_column("Cognitive_assessment") %>%
  plyr::rename(c("." = "loading")) %>%
  mutate(loading = loading * (-1)) %>%
  mutate(labels = ifelse(Cognitive_assessment == "Verbal_Fluency", "Verbal Fluency",
    ifelse(Cognitive_assessment == "Story_Recall", "LTM",
      ifelse(Cognitive_assessment == "ToT_Ratio_inverse", "Tip-of-the-tongue",
        ifelse(Cognitive_assessment == "Hotel_Task_inverse", "Multitasking",
          ifelse(Cognitive_assessment == "Sentence_Comprehension_c", "Sen. Comp.",
            ifelse(Cognitive_assessment == "Cattell", "Fluid Intell.",
              ifelse(Cognitive_assessment == "Naming", "Naming",
                ifelse(Cognitive_assessment == "Proverb", "Semantic Abstraction",
                  NA
                )
              )
            )
          )
        )
      )
    )
  ))

plot_loadings_cog$labels <- factor(plot_loadings_cog$labels) %>%
  fct_reorder(plot_loadings_cog$loading, .desc = FALSE)

ggplot(
  plot_loadings_cog %>% na.omit(),
  aes(x = labels, y = loading)
) +
  # geom_col(aes(fill = loading), alpha = .7) +
  # scale_fill_distiller(palette = "RdBu", direction = 1) +
  geom_chicklet(radius = grid::unit(5, "mm"), aes(fill = loading, alpha = .8, color = loading)) +
  scale_fill_fermenter(palette = "Oranges", direction = 1) +
  scale_y_continuous(breaks = seq(-0.5, 0.8, 0.1)) +
  coord_flip() +
  geom_text(aes(y = -0.01, label = labels, fontface = "bold", hjust = "left"),
    nudge_y = .025,
    size = 7
  ) +
  theme_pubr(
    base_size = 22,
    legend = "none",
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank()
  )


# Figure 4D ----
# BRAIN VARIATES - Derived from Proxy Correlations

labels <- c("DMN deactivation\n (NET1)", "FPN integration\n (NET4)", "CON integration\n (thalamus & putamen; NET3)")
loadings_V1 <- c(-.2, -.2, -0.09)
loadings_V2 <- c(.14, 0.06, 0.23)

plot_loadings_brain <- cbind(labels, loadings_V1, loadings_V2) %>%
  as.data.frame() %>%
  mutate_at(vars(loadings_V1, loadings_V2), funs(as.numeric(.)))

plot_loadings_brain$labels <- factor(plot_loadings_brain$labels) %>%
  fct_reorder(plot_loadings_brain$loadings_V1, .desc = TRUE)

library(ggchicklet)

ggplot(
  plot_loadings_brain,
  aes(x = labels, y = loadings_V1)
) +
  geom_chicklet(radius = grid::unit(5, "mm"), aes(fill = loadings_V1, alpha = .8, color = loadings_V1)) +
  scale_fill_fermenter(palette = "Blues", direction = -1) +
  scale_y_continuous(breaks = seq(-0.2, 0, 0.05)) +
  coord_flip() +
  geom_text(aes(y = -0.01, label = labels, fontface = "bold", hjust = "right"),
    nudge_y = .005,
    size = 7
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


plot_loadings_brain$labels <- factor(plot_loadings_brain$labels) %>%
  fct_reorder(plot_loadings_brain$loadings_V2, .desc = FALSE)

library(ggchicklet)

ggplot(
  plot_loadings_brain,
  aes(x = labels, y = loadings_V2)
) +
  geom_chicklet(radius = grid::unit(5, "mm"), aes(fill = loadings_V2, alpha = .8, color = loadings_V2)) +
  scale_fill_fermenter(palette = "Oranges", direction = 1) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.05)) +
  coord_flip() +
  geom_text(aes(y = -0.01, label = labels, fontface = "bold", hjust = "left"),
    nudge_y = .015,
    size = 7
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
