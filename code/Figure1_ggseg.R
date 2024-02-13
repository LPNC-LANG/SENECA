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


# For FIGURE 1 -------------
## Load in atlas data provided by ggseg package
atlas <-  ggsegGlasser::glasser %>% as.data.frame()
subcortical_atlas <- ggseg::aseg %>% as.data.frame()

# write.csv(atlas[,3:6], "RSNET_visualization_glasser.csv")
# write.csv(subcortical_atlas[,3:6], "RSNET_visualization_subcortical.csv")
library(rio)
RSNET <- rio::import("./data/ggseg/RSNET_visualization_glasser.csv") %>% 
  mutate(RSNET = ifelse(is.na(RSNET), 0, RSNET))
  # RS NET1 = Associative
  # RS NET2 = Sensorimotor
  # RS NET3 = Low-level attentional
  # RS NET4 = Control-executive

atlas_plot <- cbind(atlas, RSNET = RSNET[,6]) %>% 
  as_brain_atlas() %>% as.tibble()

## Plot atlas:
LH <- ggplot() + geom_brain(
  atlas       = atlas_plot,
  mapping     = aes(fill=as.factor(RSNET)),
  position    = position_brain("horizontal"),
  hemi = "left",
  size = 0.5,
  color = "black",
  show.legend =T) +
  scale_fill_manual(values = c("lightgrey", "#D7301F", "#FC8D59", "#FDCC8A", "#08519C", "black")) +
  theme_void()



RH <- ggplot() + geom_brain(
  atlas       = atlas_plot,
  mapping     = aes(fill=as.factor(RSNET)),
  position    = position_brain("horizontal"),
  hemi = "right",
  size = 0.5,
  color = "black",
  show.legend = T) +
  scale_fill_manual(values = c("lightgrey", "#D7301F", "#FC8D59", "#FDCC8A", "#08519C", "black")) +
  theme_void()

Rmisc::multiplot(LH, RH, cols = 1)

subcortical <- rio::import("./data/ggseg/RSNET_visualization_subcortical.csv") %>% 
mutate(RSNET = ifelse(is.na(RSNET), 0, RSNET))

subatlas_plot <- cbind(subcortical_atlas, RSNET = subcortical[,7]) %>%
  as_brain_atlas() %>% as.tibble()

ggplot() + geom_brain(
  atlas       = subatlas_plot,
  mapping     = aes(fill=as.factor(RSNET)),
  position    = position_brain("horizontal"),
  size = 0.5,
  color = "black",
  show.legend = T) +
  scale_fill_manual(values = c("lightgrey", "#D7301F", "#FDCC8A")) +
  theme_void()






## Plot RS NETs ----
atlas_plot <- cbind(atlas, RSNET = RSNET[,6]) %>% 
  as_brain_atlas() %>% as.tibble()

ggplot() + geom_brain(
  atlas       = atlas_plot,
  mapping     = aes(fill=as.factor(RSNET)),
  position    = position_brain("horizontal"),
  hemi = "left",
  size = 0.5,
  color = "black",
  show.legend = T) +
  scale_fill_manual(values = c("lightgrey", "#D7301F", "#FC8D59", "#FDCC8A", "#08519C", "black")) +
  theme_void()

rsnet1 <- ggplot() + geom_brain(
  atlas       = atlas_plot %>% filter(grepl("0|1", RSNET)),
  mapping     = aes(fill=as.factor(RSNET)),
  position    = position_brain("horizontal"),
  size = 0.5,
  color = "black",
  show.legend = F) +
  scale_fill_manual(values = c("lightgrey", "#D7301F")) +
  theme_void()

rsnet2 <- ggplot() + geom_brain(
  atlas       = atlas_plot %>% filter(grepl("0|2", RSNET)),
  mapping     = aes(fill=as.factor(RSNET)),
  position    = position_brain("horizontal"),
  size = 0.5,
  color = "black",
  show.legend = F) +
  scale_fill_manual(values = c("lightgrey", "#FC8D59")) +
  theme_void()

rsnet3 <- ggplot() + geom_brain(
  atlas       = atlas_plot %>% filter(grepl("0|3", RSNET)),
  mapping     = aes(fill=as.factor(RSNET)),
  position    = position_brain("horizontal"),
  size = 0.5,
  color = "black",
  show.legend = F) +
  scale_fill_manual(values = c("lightgrey", "#FDCC8A")) +
  theme_void()

rsnet4 <- ggplot() + geom_brain(
  atlas       = atlas_plot %>% filter(grepl("0|4", RSNET)),
  mapping     = aes(fill=as.factor(RSNET)),
  position    = position_brain("horizontal"),
  size = 0.5,
  color = "black",
  show.legend = F) +
  scale_fill_manual(values = c("lightgrey", "#08519C")) +
  theme_void()


Rmisc::multiplot(rsnet1, rsnet2, rsnet3, rsnet4, cols = 1)
