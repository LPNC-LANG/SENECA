################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105
# 2024

################################################################################

library(pacman)
library(ggseg)
library(ggsegPower)
library(ggsegGlasser)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(readxl)

# For FIGURE 3 -----------

library(rio)
RSNET <- rio::import("./data/ggseg/RSNET_visualization_glasser.csv") %>%
  mutate(RSNET = ifelse(is.na(RSNET), 0, RSNET))
# RS NET1 = Associative
# RS NET2 = Sensorimotor
# RS NET3 = Low-level attentional
# RS NET4 = Control-executive

atlas_plot <- cbind(atlas, RSNET = RSNET[, 6]) %>%
  as_brain_atlas() %>%
  as.tibble()


# MECHANISMS

synergistic_integration_YM <- ggplot() +
  geom_brain(
    atlas = atlas_plot %>%
      mutate(ACTIVATION = ifelse(hemi == "left" & region == "45", 4,
        ifelse(hemi == "left" & region == "IFSp", 4,
          ifelse(hemi == "left" & region == "IFja", 4,
            ifelse(hemi == "left" & region == "6v", 4,
              ifelse(hemi == "right" & region == "PF", 4,
                ifelse(hemi == "right" & region == "PFop", 4,
                  0
                )
              )
            )
          )
        )
      )),
    mapping = aes(fill = as.factor(ACTIVATION)),
    position = position_brain("horizontal"),
    side = "lateral",
    size = 0.5,
    color = "black",
    show.legend = F
  ) +
  scale_fill_manual(values = c("lightgrey", "#08519C")) +
  theme_void()

synergistic_integration_YM

###############
deactivation_YM <- ggplot() +
  geom_brain(
    atlas = atlas_plot %>%
      mutate(ACTIVATION = ifelse(hemi == "left" & region == "5L", 1,
        ifelse(hemi == "left" & region == "PHA3", 1,
          ifelse(hemi == "left" & region == "RSC", 1,
            0
          )
        )
      )),
    mapping = aes(fill = as.factor(ACTIVATION)),
    position = position_brain("horizontal"),
    size = 0.5,
    color = "black",
    show.legend = F
  ) +
  scale_fill_manual(values = c("lightgrey", "#EED6B5")) +
  theme_void()

deactivation_YM

###################


provincial_peripheral <- ggplot() +
  geom_brain(
    atlas = atlas_plot %>%
      mutate(
        ACTIVATION =
        # From young to middle
        # ifelse(hemi == "right" & region == "STSvp", 1,
        # ifelse(hemi == "right" & region == "47l", 1,
        # ifelse(hemi == "left" & region == "p47r", 4,
        # From middle to old
          ifelse(hemi == "left" & region == "6mp", 2,
            ifelse(hemi == "right" & region == "SCEF", 2,
              ifelse(hemi == "left" & region == "47s", 1,
                0
              )
            )
          )
      ),
    # ))),
    mapping = aes(fill = as.factor(ACTIVATION)),
    position = position_brain("horizontal"),
    size = 0.5,
    color = "black",
    show.legend = F
  ) +
  scale_fill_manual(values = c("lightgrey", "black", "black", "black")) +
  theme_void()

provincial_peripheral

redundant_integration_MO <- ggplot() +
  geom_brain(
    atlas = atlas_plot %>%
      mutate(ACTIVATION = ifelse(hemi == "left" & region == "31a", 1,
        ifelse(hemi == "left" & region == "31pv", 1,
          ifelse(hemi == "left" & region == "31pd", 1,
            ifelse(hemi == "left" & region == "PFcm", 2,
              ifelse(hemi == "left" & region == "AIP", 2,
                ifelse(hemi == "left" & region == "PBELT", 3,
                  ifelse(hemi == "left" & region == "A4", 3,
                    ifelse(hemi == "left" & region == "A5", 3,
                      ifelse(hemi == "left" & region == "55b", 4,
                        ifelse(hemi == "left" & region == "p47r", 4,
                          ifelse(hemi == "left" & region == "a9-46v", 4,
                            0
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )),
    mapping = aes(fill = as.factor(ACTIVATION)),
    position = position_brain("horizontal"),
    size = 0.5,
    color = "black",
    show.legend = F
  ) +
  scale_fill_manual(values = c("lightgrey", "#EB8B72", "#EB8B72", "#EB8B72", "#EB8B72", "#EB8B72")) +
  theme_void()

redundant_integration_MO


# Thalamus and putamen
atlas_subcortical <- ggseg::aseg %>% as.data.frame()
plot(aseg)

subcortical_MO <- ggplot() +
  geom_brain(
    atlas = atlas_subcortical %>%
      mutate(
        ACTIVATION =
          ifelse(hemi == "left" & region == "thalamus proper", 3,
            ifelse(hemi == "right" & region == "thalamus proper", 3,
              ifelse(hemi == "left" & region == "putamen", 3,
                0
              )
            )
          )
      ),
    # ))),
    mapping = aes(fill = as.factor(ACTIVATION)),
    position = position_brain("horizontal"),
    size = 0.5,
    color = "black",
    show.legend = F
  ) +
  scale_fill_manual(values = c("lightgrey", "darkgreen", "darkgreen", "darkgreen")) +
  theme_void()

subcortical_MO
