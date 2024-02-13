################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2024

################################################################################

source("./code/_04_NeurocognitiveAnalysis_CCA.R")
library(ggforce)
library(ggrepel)
library(ggtext)

################################################################################
# LATENT SPACE
label = c(
  "Cattell",
  "Proverb",
  "Naming",
  "Sen. Compr",
  "LTM",
  "Verbal Fluency",
  "ToT",
  "Multitasking"
)

color_plot = c("black",
               "black",
               "black",
               "black",
               "black",
               "black",
               "black",
               "black"
)


Dimension1 <- c(cc_loadings$corr.Y.yscores[, 1][1],
                cc_loadings$corr.Y.yscores[, 1][2],
                cc_loadings$corr.Y.yscores[, 1][3],
                cc_loadings$corr.Y.yscores[, 1][4],
                cc_loadings$corr.Y.yscores[, 1][5],
                cc_loadings$corr.Y.yscores[, 1][6],
                cc_loadings$corr.Y.yscores[, 1][7],
                cc_loadings$corr.Y.yscores[, 1][8]
                ) %>% as.data.frame() %>% plyr::rename(c("." = "Dim1"))

Dimension2 <- c(cc_loadings$corr.Y.yscores[, 2][1],
                cc_loadings$corr.Y.yscores[, 2][2],
                cc_loadings$corr.Y.yscores[, 2][3],
                cc_loadings$corr.Y.yscores[, 2][4],
                cc_loadings$corr.Y.yscores[, 2][5],
                cc_loadings$corr.Y.yscores[, 2][6],
                cc_loadings$corr.Y.yscores[, 2][7],
                cc_loadings$corr.Y.yscores[, 2][8]
) %>% as.data.frame() %>% plyr::rename(c("." = "Dim2"))

Dim1 <-  Dimension1 %>% mutate(label = label)
Dim2 <- Dimension2 %>% mutate(label = label)

correlational_space <- merge(Dim1, Dim2, by = c("label")) %>% as.data.frame()


ggplot(correlational_space,
       aes(Dim2*(-1), Dim1*(-1), label = label)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 4, shape = 19, color = color_plot) +
  ggrepel::geom_label_repel(size = 7, fontface = "bold", nudge_x = -.03, max.overlaps = 30, color = color_plot) +
  # scale_x_continuous(breaks = seq(-0.8, 0, 0.2)) +
  # scale_y_continuous(breaks = seq(-0.8, 0, 0.2)) +
  # coord_cartesian(xlim = c(-0.8, 0.1), ylim = c(-0.8, 0)) +
  theme_pubr(legend = "none", 
             base_size = 20) +
  labs(
    title = "",
    x = "",
    y = "" 
  ) 



# AGE-BRAIN-COGNITION
library(ggforce)
library(ggrepel)
library(ggtext)


# LATENT SPACE
label = c(        
  colnames(cog_set)[1],
  colnames(cog_set)[2],
  colnames(cog_set)[3],
  colnames(cog_set)[4],
  colnames(cog_set)[5],
  colnames(cog_set)[6],
  colnames(cog_set)[7],
  colnames(cog_set)[8], 
  
          colnames(brain_set)[1],
          colnames(brain_set)[2],
          colnames(brain_set)[3],
          colnames(brain_set)[4],
          
  colnames(brain_set)[5],
  colnames(brain_set)[6],
  colnames(brain_set)[7],
  colnames(brain_set)[8],
  
          colnames(brain_set)[9],
          colnames(brain_set)[10],
          colnames(brain_set)[11],
          colnames(brain_set)[12],
  
          colnames(brain_set)[13],
          colnames(brain_set)[14],
          colnames(brain_set)[15],
          colnames(brain_set)[16]
          )



# WITH BRAIN VARIATES

Dimension1 <- c(cc_loadings$corr.Y.yscores[, 1][1],
                cc_loadings$corr.Y.yscores[, 1][2],
                cc_loadings$corr.Y.yscores[, 1][3],
                cc_loadings$corr.Y.yscores[, 1][4],
                cc_loadings$corr.Y.yscores[, 1][5],
                cc_loadings$corr.Y.yscores[, 1][6],
                cc_loadings$corr.Y.yscores[, 1][7],
                cc_loadings$corr.Y.yscores[, 1][8],
                
                # RS NET1
                cc_loadings$corr.X.yscores[, 1][1], # Connector
                cc_loadings$corr.X.yscores[, 1][2], # Peripheral
                cc_loadings$corr.X.yscores[, 1][3], # Provincial
                cc_loadings$corr.X.yscores[, 1][4], # Satellite
                
                # RS NET2
                cc_loadings$corr.X.yscores[, 1][5],
                cc_loadings$corr.X.yscores[, 1][6],
                cc_loadings$corr.X.yscores[, 1][7],
                cc_loadings$corr.X.yscores[, 1][8],
                
                # RS NET3
                cc_loadings$corr.X.yscores[, 1][9],
                cc_loadings$corr.X.yscores[, 1][10],
                cc_loadings$corr.X.yscores[, 1][11],
                cc_loadings$corr.X.yscores[, 1][12],
                
                # RS NET4
                cc_loadings$corr.X.yscores[, 1][13],
                cc_loadings$corr.X.yscores[, 1][14],
                cc_loadings$corr.X.yscores[, 1][15],
                cc_loadings$corr.X.yscores[, 1][16]
                ) %>% as.data.frame() %>% plyr::rename(c("." = "Dim1"))

Dimension2 <- c(cc_loadings$corr.Y.yscores[, 2][1],
                cc_loadings$corr.Y.yscores[, 2][2],
                cc_loadings$corr.Y.yscores[, 2][3],
                cc_loadings$corr.Y.yscores[, 2][4],
                cc_loadings$corr.Y.yscores[, 2][5],
                cc_loadings$corr.Y.yscores[, 2][6],
                cc_loadings$corr.Y.yscores[, 2][7],
                cc_loadings$corr.Y.yscores[, 2][8],
                
                cc_loadings$corr.X.yscores[, 2][1],
                cc_loadings$corr.X.yscores[, 2][2],
                cc_loadings$corr.X.yscores[, 2][3],
                cc_loadings$corr.X.yscores[, 2][4],
                
                cc_loadings$corr.X.yscores[, 2][5],
                cc_loadings$corr.X.yscores[, 2][6],
                cc_loadings$corr.X.yscores[, 2][7],
                cc_loadings$corr.X.yscores[, 2][8],
                
                cc_loadings$corr.X.yscores[, 2][9],
                cc_loadings$corr.X.yscores[, 2][10],
                cc_loadings$corr.X.yscores[, 2][11],
                cc_loadings$corr.X.yscores[, 2][12],
                
                cc_loadings$corr.X.yscores[, 2][13],
                cc_loadings$corr.X.yscores[, 2][14],
                cc_loadings$corr.X.yscores[, 2][15],
                cc_loadings$corr.X.yscores[, 2][16]
) %>% as.data.frame() %>% plyr::rename(c("." = "Dim2"))

correlational_space <- cbind(Dim1 = Dimension1$Dim1, Dim2= Dimension2$Dim2, label) %>% as.data.frame() %>% 
  mutate_at(vars(c("Dim1", "Dim2")), funs(as.numeric(.)*(-1)))



# PROXY CORRELATION between MECHANISMS and VARIATES ----
variate_I <- c(1-0, 0-0)
variate_II <- c(0-0, 1-0)

provincial_peripheral_1 <- c(correlational_space$Dim1[10] - correlational_space$Dim1[11], correlational_space$Dim2[10] - correlational_space$Dim2[11])
provincial_peripheral_2 <- c(correlational_space$Dim1[14] - correlational_space$Dim1[15], correlational_space$Dim2[14] - correlational_space$Dim2[15])
provincial_peripheral_3 <- c(correlational_space$Dim1[18] - correlational_space$Dim1[19], correlational_space$Dim2[18] - correlational_space$Dim2[19])
provincial_peripheral_4 <- c(correlational_space$Dim1[22] - correlational_space$Dim1[23], correlational_space$Dim2[22] - correlational_space$Dim2[23])

provincial_peripheral_1
provincial_peripheral_2
provincial_peripheral_3
provincial_peripheral_4

provincial_connector_1 <- c(correlational_space$Dim1[9] - correlational_space$Dim1[11], correlational_space$Dim2[9] - correlational_space$Dim2[11])
provincial_connector_2 <- c(correlational_space$Dim1[13] - correlational_space$Dim1[15], correlational_space$Dim2[13] - correlational_space$Dim2[15])
provincial_connector_3 <- c(correlational_space$Dim1[17] - correlational_space$Dim1[19], correlational_space$Dim2[17] - correlational_space$Dim2[19])
provincial_connector_4 <- c(correlational_space$Dim1[21] - correlational_space$Dim1[23], correlational_space$Dim2[21] - correlational_space$Dim2[23])

provincial_connector_1
provincial_connector_2
provincial_connector_3
provincial_connector_4

satellite_peripheral_1 <- c(correlational_space$Dim1[10] - correlational_space$Dim1[12], correlational_space$Dim2[10] - correlational_space$Dim2[12])
satellite_peripheral_2 <- c(correlational_space$Dim1[14] - correlational_space$Dim1[16], correlational_space$Dim2[14] - correlational_space$Dim2[16])
satellite_peripheral_3 <- c(correlational_space$Dim1[18] - correlational_space$Dim1[20], correlational_space$Dim2[18] - correlational_space$Dim2[20])
satellite_peripheral_4 <- c(correlational_space$Dim1[22] - correlational_space$Dim1[24], correlational_space$Dim2[22] - correlational_space$Dim2[24])

satellite_peripheral_1
satellite_peripheral_2
satellite_peripheral_3
satellite_peripheral_4

satellite_connector_1 <- c(correlational_space$Dim1[9] - correlational_space$Dim1[12], correlational_space$Dim2[9] - correlational_space$Dim2[12])
satellite_connector_2 <- c(correlational_space$Dim1[13] - correlational_space$Dim1[16], correlational_space$Dim2[13] - correlational_space$Dim2[16])
satellite_connector_3 <- c(correlational_space$Dim1[17] - correlational_space$Dim1[18], correlational_space$Dim2[17] - correlational_space$Dim2[20])
satellite_connector_4 <- c(correlational_space$Dim1[21] - correlational_space$Dim1[24], correlational_space$Dim2[21] - correlational_space$Dim2[24])

satellite_connector_1
satellite_connector_2
satellite_connector_3
satellite_connector_4

# Subcortical integration in Thalamus & Putamen
peripheral_connector_3 <- c(correlational_space$Dim1[17] - correlational_space$Dim1[18], correlational_space$Dim2[17] - correlational_space$Dim2[18])
peripheral_connector_3

# Threshold for interpreting cross-correlations of Variate I
total_cor <- c(provincial_peripheral_1[1],
               provincial_peripheral_2[1],
               provincial_peripheral_3[1],
               provincial_peripheral_4[1],
               
               provincial_connector_1[1],
               provincial_connector_2[1],
               provincial_connector_3[1],
               provincial_connector_4[1],
               
               satellite_peripheral_1[1],
               satellite_peripheral_2[1],
               satellite_peripheral_3[1],
               satellite_peripheral_4[1],
               
               satellite_connector_1[1],
               satellite_connector_2[1],
               satellite_connector_3[1],
               satellite_connector_4[1]
               
  
) 

quantile(abs(total_cor))
median(abs(total_cor))

# Threshold for interpreting cross-correlations of Variate II
total_cor <- c(provincial_peripheral_1[2],
               provincial_peripheral_2[2],
               provincial_peripheral_3[2],
               provincial_peripheral_4[2],
               
               provincial_connector_1[2],
               provincial_connector_2[2],
               provincial_connector_3[2],
               provincial_connector_4[2],
               
               satellite_peripheral_1[2],
               satellite_peripheral_2[2],
               satellite_peripheral_3[2],
               satellite_peripheral_4[2],
               
               satellite_connector_1[2],
               satellite_connector_2[2],
               satellite_connector_3[2],
               satellite_connector_4[2]
               
               
) 

quantile(abs(total_cor))
median(abs(total_cor))

# Plots for intuitive viz ----

color_plot = c("#08519C",
               "#08519C",
               "#08519C",
               "#08519C",
               
               "#08519C",
               "#08519C",
               "#08519C",
               "#08519C",
               # 
               # "#D7301F",
               # "#D7301F",
               # "#D7301F",
               # "#D7301F",
               # 
               # "#D7301F",
               # "#D7301F",
               # "#D7301F",
               # "#D7301F",
               # 
               # "#D7301F",
               # "#D7301F",
               # "#D7301F",
               # "#D7301F",
               # 
               "#D7301F",
               "#D7301F",
               "#D7301F",
               "#D7301F"
)



# RS NET1
ggplot(correlational_space %>% slice(1:8, 9:12),
       aes(Dim2, Dim1, label = label)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 4, shape = 19, color = color_plot) +
  ggrepel::geom_label_repel(size = 5, fontface = "bold", nudge_y = -.01, max.overlaps = 30, color = color_plot) +
  # coord_fixed(xlim = c(-0.4, 0.5), ylim = c(-0.4, 0.2)) + 
  theme_pubr(legend = "none", 
             base_size = 18) +
  labs(
    title = "",
    x = "Dim2",
    y = "Dim1" 
  ) 


# RS NET2
ggplot(correlational_space %>% slice(1:8, 13:16),
       aes(as.numeric(Dim2), as.numeric(Dim1), label = label)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 4, shape = 19, color = color_plot) +
  ggrepel::geom_label_repel(size = 5, fontface = "bold", nudge_y = -.01, max.overlaps = 30, color = color_plot) +
  # coord_fixed(xlim = c(-0.4, 0.5), ylim = c(-0.4, 0.2)) + 
  theme_pubr(legend = "none", 
             base_size = 18) +
  labs(
    title = "",
    x = "Dim2",
    y = "Dim1" 
  ) 

# RS NET3
ggplot(correlational_space %>% slice(1:8, 17:20),
       aes(as.numeric(Dim2), as.numeric(Dim1), label = label)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 4, shape = 19, color = color_plot) +
  ggrepel::geom_label_repel(size = 5, fontface = "bold", nudge_y = -.01, max.overlaps = 30, color = color_plot) +
  # coord_fixed(xlim = c(-0.4, 0.5), ylim = c(-0.4, 0.2)) + 
  theme_pubr(legend = "none", 
             base_size = 18) +
  labs(
    title = "",
    x = "Dim2",
    y = "Dim1" 
  ) 


# RS NET4
ggplot(correlational_space %>% slice(1:8, 21:24),
       aes(as.numeric(Dim2), as.numeric(Dim1), label = label)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 4, shape = 19, color = color_plot) +
  ggrepel::geom_label_repel(size = 5, fontface = "bold", nudge_y = -.01, max.overlaps = 30, color = color_plot) +
  # coord_fixed(xlim = c(-0.4, 0.5), ylim = c(-0.4, 0.2)) + 
  theme_pubr(legend = "none", 
             base_size = 18) +
  labs(
    title = "",
    x = "Dim2",
    y = "Dim1" 
  ) 

# angle <- function(x,y){
#   dot.prod <- x%*%y 
#   norm.x <- norm(x,type="2")
#   norm.y <- norm(y,type="2")
#   theta <- acos(dot.prod / (norm.x * norm.y))
#   as.numeric(theta)
# }
# 
# angle(RSNET1_pivotal, RSNET1_integrative) * 180 / pi