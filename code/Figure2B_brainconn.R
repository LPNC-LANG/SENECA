################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2024

################################################################################
library(brainconn)

#########################
# For FIGURE 2B ---------
#########################

# Viz brain conn

movers_data <- rio::import("./data/Qualitative_analysis.csv") %>% 
  # Plot only movers
  filter(movers_tot == 0) 

LANG_Atlas_young <- rio::import("./data/ggseg/LANG_131_for_RViz.xlsx") %>% 
  mutate(index = seq_len(nrow(.))) %>%
  mutate(x.mni = as.integer(x.mni),
         y.mni = as.integer(y.mni),
         z.mni = as.integer(z.mni)) %>% 
  dplyr::rename(ROI.name = Glasser_assign)



young_subj <- read_excel("./data/brainconn/brainconn_viz_R.xlsx", 
                         col_names = FALSE, 
                         sheet = "young") %>%  
  rownames_to_column() %>% 
  slice(row_number(c(5, 7, 11, 15, 16, 18,  19,  22,  23,  25,  36,  42,
                     43,  44,  47,  48, 49,  50,  55,  57,  58,  59,
                     60,  61,  62,  64,  67,  72,  80,  82,  85,  88,
                     95,  96, 101, 106, 109, 110 , 114, 121, 126,
                     127, 129))) %>% 
  remove_rownames() %>% column_to_rownames("rowname") %>% 
  dplyr::select(LANG_Atlas_young$index) %>% 
  as.data.frame()

# brainconn::check_atlas(LANG_Atlas_young)

p_young <- brainconn_custom(atlas = "LANG_Atlas_young", conmat=young_subj, view="top",
                            all.nodes = TRUE, edge.alpha = 0.8, edge.color = "black", edge.width = .3,
                            node.size = 8, title = "", show.legend = F)



LANG_Atlas_middle <- rio::import("D:/Analyses/1_Analyse_R/RS_CamCAN_LANG131_R/ggseg/LANG_131_for_RViz.xlsx") %>%
  dplyr::rename(ROI.Name = Region,
                network = Middle) %>%
  mutate(index = seq_len(nrow(.))) %>%
  mutate(x.mni = as.integer(x.mni),
         y.mni = as.integer(y.mni),
         z.mni = as.integer(z.mni)) %>%
  # Plot only movers
  filter(movers == 1)

middle_subj <- read_excel("D:/Analyses/1_Analyse_R/RS_CamCAN_LANG131_R/ggseg/brainconn_viz_R.xlsx",
                          col_names = FALSE,
                          sheet = "middle") %>%
  rownames_to_column() %>%
  slice(row_number(c(5, 7, 11, 15, 16, 18,  19,  22,  23,  25,  36,  42,
                     43,  44,  47,  48, 49,  50,  55,  57,  58,  59,
                     60,  61,  62,  64,  67,  72,  80,  82,  85,  88,
                     95,  96, 101, 106, 109, 110 , 114, 121, 126,
                     127, 129))) %>%
  remove_rownames() %>% column_to_rownames("rowname") %>% 
  dplyr::select(LANG_Atlas_middle$index)


p_middle <- brainconn_custom(atlas ="LANG_Atlas_middle", conmat=middle_subj, view="top",
                             all.nodes = TRUE, edge.alpha = 0.8, edge.color = "black", edge.width = .3,
                             node.size = 8, title = "", show.legend = F)


LANG_Atlas_old <- rio::import("D:/Analyses/1_Analyse_R/RS_CamCAN_LANG131_R/ggseg/LANG_131_for_RViz.xlsx") %>%
  dplyr::rename(ROI.Name = Region,
                network = Old) %>%
  mutate(index = seq_len(nrow(.))) %>%
  mutate(x.mni = as.integer(x.mni),
         y.mni = as.integer(y.mni),
         z.mni = as.integer(z.mni)) %>%
  # Plot only movers
  filter(movers == 1)

old_subj <- read_excel("D:/Analyses/1_Analyse_R/RS_CamCAN_LANG131_R/ggseg/brainconn_viz_R.xlsx",
                       col_names = FALSE,
                       sheet = "old") %>%
  rownames_to_column() %>%
  slice(row_number(c(5, 7, 11, 15, 16, 18,  19,  22,  23,  25,  36,  42,
                     43,  44,  47,  48, 49,  50,  55,  57,  58,  59,
                     60,  61,  62,  64,  67,  72,  80,  82,  85,  88,
                     95,  96, 101, 106, 109, 110 , 114, 121, 126,
                     127, 129))) %>%
  remove_rownames() %>% column_to_rownames("rowname") %>% 
  dplyr::select(LANG_Atlas_old$index)

p_old <- brainconn_custom(atlas ="LANG_Atlas_old", conmat=old_subj, view="top",
                          all.nodes = TRUE, edge.alpha = 0.8, edge.color = "black", edge.width = .3,
                          node.size = 8, title = "", show.legend = F)



Rmisc::multiplot(p_young, p_middle, p_old, cols = 3)


#  Modify brainconn's functions ----
library(grid)
library(cowplot)

brainconn_custom <- function (atlas, background = "ICBM152", view = "top", conmat = NULL, 
                              node.size = 4, node.color = "network", all.nodes = FALSE, 
                              edge.color = "black", edge.alpha = 0.8, edge.width = 1, 
                              edge.color.weighted = FALSE, labels = FALSE, show.legend = TRUE, 
                              thr = NULL, uthr = NULL, scale.edge.width = NULL, label.size = 1.5, 
                              label.edge.weight = FALSE, background.alpha = 1, bg_xmax = 0, 
                              bg_xmin = 0, bg_ymax = 0, bg_ymin = 0, title = title) 
{
  ifelse(is.character(atlas), data <- get(atlas), data <- atlas)
  if (background != "ICBM152" && view == "ortho") {
    stop("Custom background image detected, view cannot be 'ortho', please select top,\n        bottom, left, right, front or back.")
  }
  if (!is.null(thr)) {
    conmat[conmat < thr] <- 0
  }
  if (!is.null(uthr)) {
    conmat[conmat > thr] <- 0
  }
  if (view == "ortho") {
    ortho_list <- list()
    ortho_views <- c("top", "left", "front")
    for (v in 1:3) {
      view <- ortho_views[v]
      bg <- paste0("ICBM152_", view)
      m <- get(bg)
      w <- matrix(rgb(m[, , 1], m[, , 2], m[, , 3], m[, 
                                                      , 4] * background.alpha), nrow = dim(m)[1])
      background <- rasterGrob(w)
      nparc <- dim(data)[1]
      if (!exists("conmat")) {
        conmat <- matrix(0L, nrow = nparc, ncol = nparc)
      }
      conmat <- as.matrix(conmat)
      rownames(conmat) <- colnames(conmat)
      ifelse(isSymmetric.matrix(conmat) == FALSE, directed <- FALSE, 
             directed <- TRUE)
      if (all.nodes == FALSE && directed == FALSE) {
        include.vec <- vector(length = dim(data)[1])
        for (i in 1:dim(conmat)[1]) {
          ifelse(any(conmat[i, ] != 0), include.vec[i] <- 1, 
                 include.vec[i] <- 0)
        }
        data <- data[as.logical(include.vec), , drop = F]
        conmat <- conmat[which(rowSums(conmat, na.rm = T) != 
                                 0), which(colSums(conmat, na.rm = T) != 0), 
                         drop = F]
      }
      if (all.nodes == FALSE && directed == TRUE) {
        include.vec <- vector(length = dim(data)[1])
        for (i in 1:dim(conmat)[1]) {
          ifelse(any(conmat[i, ] != 0) | any(conmat[, 
                                                    i] != 0), include.vec[i] <- 1, include.vec[i] <- 0)
        }
      }
      if (all.nodes == TRUE) {
        include.vec <- vector(length = dim(data)[1])
        include.vec <- rep(1, length = dim(data)[1])
      }
      ifelse(v == 1, show.legend <- T, show.legend <- F)
      ortho_list[[v]] <- build_plot(conmat = conmat, data = data, 
                                    background = background, node.size = node.size, 
                                    view = view, node.color = node.color, thr = thr, 
                                    uthr = uthr, edge.color = edge.color, edge.alpha = edge.alpha, 
                                    edge.width = edge.width, scale.edge.width = scale.edge.width, 
                                    show.legend = show.legend, labels = labels, 
                                    label.size = label.size, include.vec = include.vec, 
                                    edge.color.weighted = edge.color.weighted, label.edge.weight = label.edge.weight,
                                    title = title)
      if (is.environment(edge.color) == T) {
        ortho_list[[v]] <- ortho_list[[v]] + edge.color
      }
    }
    right_col <- plot_grid(ortho_list[[2]], ortho_list[[3]], 
                           nrow = 2, rel_heights = c(1, 1.45))
    p <- plot_grid(ortho_list[[1]], right_col, rel_widths = c(1.8, 
                                                              1.2))
    return(p)
  }
  if (background == "ICBM152") {
    bg <- paste0("ICBM152_", view)
    m <- get(bg)
    w <- matrix(rgb(m[, , 1], m[, , 2], m[, , 3], m[, , 
                                                    4] * background.alpha), nrow = dim(m)[1])
  }
  if (background != "ICBM152") {
    m <- OpenImageR::readImage(background)
  }
  w <- matrix(rgb(m[, , 1], m[, , 2], m[, , 3], m[, , 4] * 
                    background.alpha), nrow = dim(m)[1])
  background <- rasterGrob(w)
  nparc <- dim(data)[1]
  if (!exists("conmat")) {
    conmat <- matrix(0L, nrow = nparc, ncol = nparc)
  }
  conmat <- as.matrix(conmat)
  rownames(conmat) <- colnames(conmat)
  ifelse(isSymmetric.matrix(conmat) == FALSE, directed <- FALSE, 
         directed <- TRUE)
  if (all.nodes == FALSE && directed == FALSE) {
    include.vec <- vector(length = dim(data)[1])
    for (i in 1:dim(conmat)[1]) {
      ifelse(any(conmat[i, ] != 0), include.vec[i] <- 1, 
             include.vec[i] <- 0)
    }
    data <- data[as.logical(include.vec), , drop = F]
    conmat <- conmat[which(rowSums(conmat, na.rm = T) != 
                             0), which(colSums(conmat, na.rm = T) != 0), drop = F]
  }
  if (all.nodes == FALSE && directed == TRUE) {
    include.vec <- vector(length = dim(data)[1])
    for (i in 1:dim(conmat)[1]) {
      ifelse(any(conmat[i, ] != 0) | any(conmat[, i] != 
                                           0), include.vec[i] <- 1, include.vec[i] <- 0)
    }
  }
  if (all.nodes == TRUE) {
    include.vec <- vector(length = dim(data)[1])
    include.vec <- rep(1, length = dim(data)[1])
  }
  p <- build_plot(conmat = conmat, data = data, background = background, 
                  node.size = node.size, view = view, node.color = node.color, 
                  thr = thr, uthr = uthr, edge.color = edge.color, edge.alpha = edge.alpha, 
                  edge.width = edge.width, scale.edge.width = scale.edge.width, 
                  show.legend = show.legend, labels = labels, label.size = label.size, 
                  include.vec = include.vec, edge.color.weighted = edge.color.weighted, 
                  label.edge.weight = label.edge.weight, bg_xmax = bg_xmax, 
                  bg_xmin = bg_xmin, bg_ymax = bg_ymax, bg_ymin = bg_ymin, title = title) 
  return(p)
}




build_plot <- function (conmat, data, data.row = NULL, data.col = NULL, background, 
                        node.size, node.color = "network", thr = NULL, uthr = NULL, 
                        view, edge.color, edge.alpha, edge.width, show.legend, label.size, 
                        labels, include.vec = NULL, scale.edge.width, edge.color.weighted, 
                        label.edge.weight, bg_xmin = 0, bg_ymin = 0, bg_xmax = 0, 
                        bg_ymax = 0, title = NULL, ...) 
{
  if (view == "top") {
    x.mni <- data$x.mni
    y.mni <- data$y.mni
    depth <- data$z.mni
    xmax = 70 + bg_xmax
    xmin = -75 + bg_xmin
    ymax = 73 + bg_ymax
    ymin = -107 + bg_ymin
  }
  if (view == "bottom") {
    x.mni <- data$x.mni * -1
    y.mni <- data$y.mni
    depth <- data$z.mni * -1
    xmax = 70 + bg_xmax
    xmin = -70 + bg_xmin
    ymax = 73 + bg_ymax
    ymin = -107 + bg_ymin
  }
  if (view == "front") {
    x.mni <- data$x.mni
    y.mni <- data$z.mni
    depth <- data$y.mni
    xmax = 70 + bg_xmax
    xmin = -70 + bg_xmin
    ymax = 80 + bg_ymax
    ymin = -48 + bg_ymin
  }
  if (view == "back") {
    x.mni <- data$x.mni * -1
    y.mni <- data$z.mni
    depth <- data$y.mni * -1
    xmax = 70 + bg_xmax
    xmin = -70 + bg_xmin
    ymax = 80 + bg_ymax
    ymin = -48 + bg_ymin
  }
  if (view == "left") {
    x.mni <- data$y.mni * -1
    y.mni <- data$z.mni
    depth <- data$x.mni
    xmax = 103 + bg_xmax
    xmin = -72 + bg_xmin
    ymax = 77 + bg_ymax
    ymin = -50 + bg_ymin
  }
  if (view == "right") {
    x.mni <- data$y.mni
    y.mni <- data$z.mni
    depth <- data$x.mni * -1
    xmax = 103 + bg_xmax
    xmin = -140 + bg_xmin
    ymax = 77 + bg_ymax
    ymin = -50 + bg_ymin
  }
  ifelse(isSymmetric.matrix(conmat) == TRUE, directed <- FALSE, 
         directed <- FALSE)
  ifelse(all(conmat %in% c(0, 1)) == TRUE, weighted <- FALSE, 
         weighted <- TRUE)
  if (!exists("conmat")) 
    stop(print("Please enter a valid connectivity matrix"))
  if (directed == F) {
    conmat[upper.tri(conmat)] <- 0
    layout <- ggraph::create_layout(graph = conmat, layout = "stress", 
                                    circular = TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
  }
  if (directed == F && weighted == F) {
    p <- ggraph(layout, circular = FALSE) + annotation_custom(background, 
                                                              xmax = xmax, xmin = xmin, ymax = ymax, ymin = ymin) + 
      geom_edge_link(color = edge.color, edge_width = edge.width, 
                     edge_alpha = edge.alpha) + coord_fixed(xlim = c(-70, 
                                                                     70), ylim = c(-107, 73))
  }
  if (view == "left") {
    p <- p + coord_fixed(xlim = c(-64, 98), ylim = c(-44, 
                                                     76)) 
  }
  if (view == "right") {
    p <- p + coord_fixed(xlim = c(-98, 64), ylim = c(-44, 
                                                     76))
  }
  if (directed == F) {
    ifelse(node.color == "network", p <- p + geom_node_point(size = node.size, 
                                                             aes(colour = as.factor(data$network))), p <- p + 
             geom_node_point(size = node.size, colour = node.color))
  }
  if (directed == F && labels == T) {
    p <- p + geom_node_label(aes(label = data$ROI.Name), 
                             size = label.size, repel = TRUE, nudge_x = node.size + 
                               2, nudge_y = node.size - 2)
  }
  p <- p + theme_bw(base_size = 25, base_family = "serif") + theme(panel.grid.major = element_blank(), 
                                                                   panel.grid.minor = element_blank(), panel.border = element_blank(), 
                                                                   panel.background = element_blank(), axis.title.x = element_blank(), 
                                                                   axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
                                                                   axis.title.y = element_blank(), axis.text.y = element_blank(), 
                                                                   axis.ticks.y = element_blank(), )
  if (show.legend == F) {
    p <- p + theme(legend.position = "none") +
      ggtitle(paste0(title))
  }
  if (show.legend == T) {
    p <- p +
      ggtitle(paste0(title))
  }
  p + scale_color_brewer(name = "Topological role", palette = "PuOr")
}
