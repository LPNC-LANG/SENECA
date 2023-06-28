################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

################################################################################

source("_06_Main_Statistics.R")

################################################################################

library(CCA)
library(CCP)

cog_set <- Data_CCA_full[, c(3:8, 10:11)]

brain_set <- Data_CCA_full[, c(
  # Without Sat
  # 12:13, 15:17, 19:21, 23:25, 27
  # Without Periph
  12, 14:16, 18:20, 22:24, 26:27
  # Without Prov
  # 12:14, 16:18, 20:22, 24:26
)]

# canonical coefficients - weights used for linear combination
cc_results <- cc(brain_set, cog_set)

# Structure coeffs - correlation between observed variables and the latent variable
cc_loadings <- comput(rs_measures_ComStruct, cog_measures_ComStruct, cc_results)

library(ggforce)
library(ggrepel)
library(ggtext)


# LATENT SPACE
label = c(
  "Cattell",
  "Proverb",
  "Naming",
  "Comprehension",
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


cor.test(Cognitive_Mode_synergy, Data_CCA_full$Cattell)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$Proverb)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$Naming)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$Sentence_Comprehension_c)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$Story_Recall)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$Verbal_Fluency)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$ToT_Ratio_inverse)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$Hotel_Task_inverse)



Dimension2 <- c(-0.08613661,
                -0.6831757,
                -0.2068828,
                -0.04294554,
                -0.07528576,
                -0.5258505,
                -0.3177736,
                -0.5940927
                ) %>% as.data.frame() %>% plyr::rename(c("." = "Dim2"))

cor.test(Cognitive_Mode_resilience, Data_CCA_full$Cattell)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Proverb)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Naming)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Sentence_Comprehension_c)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Story_Recall)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Verbal_Fluency)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$ToT_Ratio_inverse)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Hotel_Task_inverse)

Dimension1 <- c(-0.8061704,
                0.06650062,
                -0.7698932,
                -0.2664709,
                -0.4115551,
                -0.351683,
                -0.1656834,
                -0.1783413) %>% as.data.frame() %>% plyr::rename(c("." = "Dim1"))

Dim1 <-  Dimension1 %>% mutate(label = label)
Dim2 <- Dimension2 %>% mutate(label = label)

correlational_space <- merge(Dim1, Dim2, by = c("label")) %>% as.data.frame()



ggplot(correlational_space,
       aes(Dim1, Dim2, label = label)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 4, shape = 19, color = color_plot) +
  ggrepel::geom_label_repel(size = 7, fontface = "bold", nudge_x = -.03, max.overlaps = 30, color = color_plot) +
  scale_x_continuous(breaks = seq(-0.8, 0, 0.2)) +
  scale_y_continuous(breaks = seq(-0.8, 0, 0.2)) +
  coord_cartesian(xlim = c(-0.8, 0.1), ylim = c(-0.8, 0)) +
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
          "RS NET1\nConnector",
          "RS NET1\nPeripheral",
          "RS NET1\nSatellite",
          "RS NET1\nProvincial",
          
          "RS NET2\nConnector",
          "RS NET2\nPeripheral",
          "RS NET2\nSatellite",
          "RS NET2\nProvincial",
        
          "RS NET3\nConnector",
          "RS NET3\nPeripheral",
          "RS NET3\nSatellite",
          "RS NET3\nProvincial",
          
          "RS NET4\nConnector",
          "RS NET4\nPeripheral",
          "RS NET4\nSatellite",
          "RS NET4\nProvincial",
          
          "Cattell",
          "Proverb",
          "Naming",
          "Comprehension",
          "LTM",
          "Verbal Fluency",
          "ToT",
          "Multitasking",
          
          "18-49",
          "50-60",
          "60-88"
          )



# WITH BRAIN VARIATES
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 1 (DMN, FPN, Language)_Connector_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 1 (DMN, FPN, Language)_Peripheral_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 1 (DMN, FPN, Language)_Satellite_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 1 (DMN, FPN, Language)_Provincial_balance`)

cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 2 (SMN)_Connector_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 2 (SMN)_Peripheral_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 2 (SMN)_Satellite_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 2 (SMN)_Provincial_balance`)

cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 3 (CON, FPN, Language)_Connector_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 3 (CON, FPN, Language)_Peripheral_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 3 (CON, FPN, Language)_Satellite_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 3 (CON, FPN, Language)_Provincial_balance`)

cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 4 (FPN)_Connector_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 4 (FPN)_Peripheral_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 4 (FPN)_Satellite_balance`)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$`RS-NET 4 (FPN)_Provincial_balance`)

cor.test(Cognitive_Mode_resilience, Data_CCA_full$Cattell)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Proverb)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Naming)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Sentence_Comprehension_c)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Story_Recall)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Verbal_Fluency)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$ToT_Ratio_inverse)
cor.test(Cognitive_Mode_resilience, Data_CCA_full$Hotel_Task_inverse)



Cognitive_Mode <- as.matrix(cog_measures_ComStruct) %*% cc_results$ycoef[, 1]
Cognitive_Mode_2 <- as.matrix(cog_measures_ComStruct) %*% cc_results$ycoef[, 2]
Cognitive_Mode_resilience <- as.matrix(cog_measures_ComStruct) %*% ((cc_results$ycoef[, 1]+cc_results$ycoef[, 2])/2)
Cognitive_Mode_synergy <- as.matrix(cog_measures_ComStruct) %*% ((cc_results$ycoef[, 1]-cc_results$ycoef[, 2])/2)


plot_ComStruct <- Data_CCA_full %>% 
  mutate(
    Age = Age,
    Cognitive_Mode_2 = Cognitive_Mode_2,
    Cognitive_Mode_resilience = Cognitive_Mode_resilience,
    Cognitive_Mode_synergy = Cognitive_Mode_synergy
  ) 

a <- plot_ComStruct %>% filter(Age <= 49)
cor.test(a$Cognitive_Mode_resilience, a$Age)
b <- plot_ComStruct %>% filter(Age <= 60 & Age > 50)
cor.test(b$Cognitive_Mode_resilience, b$Age)
c <- plot_ComStruct %>% filter(Age > 60)
cor.test(c$Cognitive_Mode_resilience, c$Age)

Dimension1 <- c(-0.01666637, 0.1257109, -0.07229147, -0.02952829,
                0.01136937, 0.1334632, -0.09970274, -0.0626087,
                0.08298302 , 0.01593042, -0.06049631, -0.09306003,
                0.1228923, -0.02387481, -0.005217147, -0.1568189,
                -0.8061704,
                0.06650062,
                -0.7698932,
                -0.2664709,
                -0.4115551,
                -0.351683,
                -0.1656834,
                -0.1783413,
                
                0.07879471,
                0.2729955,
                0.4404376
                ) %>% as.data.frame() %>% plyr::rename(c("." = "Dim1"))

cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 1 (DMN, FPN, Language)_Connector_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 1 (DMN, FPN, Language)_Peripheral_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 1 (DMN, FPN, Language)_Satellite_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 1 (DMN, FPN, Language)_Provincial_balance`)

cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 2 (SMN)_Connector_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 2 (SMN)_Peripheral_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 2 (SMN)_Satellite_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 2 (SMN)_Provincial_balance`)


cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 3 (CON, FPN, Language)_Connector_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 3 (CON, FPN, Language)_Peripheral_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 3 (CON, FPN, Language)_Satellite_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 3 (CON, FPN, Language)_Provincial_balance`)


cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 4 (FPN)_Connector_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 4 (FPN)_Peripheral_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 4 (FPN)_Satellite_balance`)
cor.test(Cognitive_Mode_synergy, Data_CCA_full$`RS-NET 4 (FPN)_Provincial_balance`)

a <- plot_ComStruct %>% filter(Age <= 49)
cor.test(a$Cognitive_Mode_synergy, a$Age)
b <- plot_ComStruct %>% filter(Age <= 60 & Age > 50)
cor.test(b$Cognitive_Mode_synergy, b$Age)
c <- plot_ComStruct %>% filter(Age > 60)
cor.test(c$Cognitive_Mode_synergy, c$Age)

Dimension2 <- c(0.1072959, -0.00503611, -0.07472321, -0.1309121,
                -0.002689689, -0.08568644, 0.05232241, 0.04405015,
                -0.01411595, 0.1585352, -0.1220187, -0.04530742,
                0.0547551, 0.1056959, -0.1226985, -0.08979472,
                -0.08613661,
                -0.6831757,
                -0.2068828,
                -0.04294554,
                -0.07528576,
                -0.5258505,
                -0.3177736,
                -0.5940927,
                
                -0.1725672,
                -0.1530274,
                0.06239137
) %>% as.data.frame() %>% plyr::rename(c("." = "Dim2"))

Dim1 <-  Dimension1 %>% mutate(label = label)
Dim2 <- Dimension2 %>% mutate(label = label) 

correlational_space <- merge(Dim1, Dim2, by = c("label")) %>% as.data.frame()



color_plot = c("purple",
               "purple",
               "purple",
               
               "#D7301F",
               "#D7301F",
               "#D7301F",
               "#D7301F",
               
               "#FC8D59",
               "#FC8D59",
               
               "#FDCC8A",
               "#FDCC8A",
               "#FDCC8A",
               "#FDCC8A",
               
               "#08519C",
               "#08519C",
               "#08519C",
               "#08519C"
)



ggplot(correlational_space %>% slice(1:3, 10:15, 18:21, 22:25),
       aes(Dim1, Dim2, label = label)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 4, shape = 19, color = color_plot) +
  ggrepel::geom_label_repel(size = 5, fontface = "bold", nudge_y = -.01, max.overlaps = 30, color = color_plot) +
  coord_fixed(xlim = c(-0.4, 0.5), ylim = c(-0.4, 0.2)) + 
  theme_pubr(legend = "none", 
             base_size = 20) +
  labs(
    title = "",
    x = "",
    y = "" 
  ) 





color_plot = c(
  "purple",
  "purple",
  "purple",
               "#D7301F", #Satellite
               "#D7301F", #Connector
  "#D7301F", #Peripheral
          
               
               "#FC8D59", #Connector
               "#FC8D59", #Provincial
               
               "#08519C", #Connector
               "#08519C", #Provincial
               "#08519C" #Satellite
               
               
)

ggplot(correlational_space %>% slice(1:3, 10:11, 13, 18, 20, 22, 24, 25), 
       aes(Dim1, Dim2, label = label)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 4, shape = 19) +
  ggrepel::geom_label_repel(size = 5, fontface = "bold", nudge_x = -0.02, max.overlaps = 20, color = color_plot) +
  coord_fixed(xlim = c(-0.4, 0.4), ylim = c(-0.4, 0.4)) + 
  theme_pubr(legend = "none", 
             base_size = 20) +
  labs(
    title = "",
    x = "",
    y = "" 
  ) 



# COSINE SIMILARITIES between each mechanism and each axis (not included in the final analyses)
# library(lsa)
# 
# RSNET1_pre_alloc <- c(correlational_space$Dim1[8] - correlational_space$Dim1[9], correlational_space$Dim2[8] - correlational_space$Dim2[9])
# RSNET1_integrative <- c(correlational_space$Dim1[7] - correlational_space$Dim1[10], correlational_space$Dim2[7] - correlational_space$Dim2[10])
# 
# cosine(cbind(RSNET1_pre_alloc, RSNET1_integrative))
# 
# RSNET2_pre_alloc <- c(correlational_space$Dim1[12] - correlational_space$Dim1[13], correlational_space$Dim2[12] - correlational_space$Dim2[13])
# RSNET2_integrative <- c(correlational_space$Dim1[11] - correlational_space$Dim1[14], correlational_space$Dim2[11] - correlational_space$Dim2[14])
# 
# cosine(cbind(RSNET2_pre_alloc, RSNET2_integrative))
# 
# RSNET3_pre_alloc <- c(correlational_space$Dim1[16] - correlational_space$Dim1[17], correlational_space$Dim2[16] - correlational_space$Dim2[17])
# RSNET3_integrative <- c(correlational_space$Dim1[15] - correlational_space$Dim1[18], correlational_space$Dim2[15] - correlational_space$Dim2[18])
# 
# cosine(cbind(RSNET3_pre_alloc, RSNET3_integrative))
# 
# RSNET4_pre_alloc <- c(correlational_space$Dim1[20] - correlational_space$Dim1[21], correlational_space$Dim2[20] - correlational_space$Dim2[21])
# RSNET4_integrative <- c(correlational_space$Dim1[19] - correlational_space$Dim1[22], correlational_space$Dim2[19] - correlational_space$Dim2[22])
# 
# cosine(cbind(RSNET4_pre_alloc, RSNET4_integrative))
# 
# 
# # COSINES between diagonal tandems and each dimension
# 
# resilience_axis <- c(1, 1)
# synergy_axis <- c(-1, 1)
# 
# 
# cosine(cbind(RSNET1_pre_alloc, resilience_axis))
# cosine(cbind(RSNET1_pre_alloc, synergy_axis))
# 
# a <- cosine(cbind(RSNET1_pre_alloc, resilience_axis))
# b <- cosine(cbind(RSNET1_pre_alloc, synergy_axis))
# 
# (abs(a)-abs(b))/(abs(a)+abs(b))
# 
# a <- cosine(cbind(RSNET1_integrative, resilience_axis))
# b <- cosine(cbind(RSNET1_integrative, synergy_axis))
# 
# (abs(a)-abs(b))/(abs(a)+abs(b))
# 
# 
# a <- cosine(cbind(RSNET2_pre_alloc, resilience_axis))
# b <- cosine(cbind(RSNET2_pre_alloc, synergy_axis))
# 
# (abs(a)-abs(b))/(abs(a)+abs(b))
# 
# a <- cosine(cbind(RSNET2_integrative, resilience_axis))
# b <- cosine(cbind(RSNET2_integrative, synergy_axis))
# 
# (abs(a)-abs(b))/(abs(a)+abs(b))
# 
# a <- cosine(cbind(RSNET3_pre_alloc, resilience_axis))
# b <- cosine(cbind(RSNET3_pre_alloc, synergy_axis))
# 
# (abs(a)-abs(b))/(abs(a)+abs(b))
# 
# a <- cosine(cbind(RSNET3_integrative, resilience_axis))
# b <- cosine(cbind(RSNET3_integrative, synergy_axis))
# 
# (abs(a)-abs(b))/(abs(a)+abs(b))
# 
# 
# a <- cosine(cbind(RSNET4_pre_alloc, resilience_axis))
# b <- cosine(cbind(RSNET4_pre_alloc, synergy_axis))
# 
# (abs(a)-abs(b))/(abs(a)+abs(b))
# 
# a <- cosine(cbind(RSNET4_integrative, resilience_axis))
# b <- cosine(cbind(RSNET4_integrative, synergy_axis))
# 
# (abs(a)-abs(b))/(abs(a)+abs(b))
# 
# 
# 
# 
# # PIVOTAL ROLE of RS NET1
# 
# RSNET1_pre_alloc <- c(correlational_space$Dim1[8] - correlational_space$Dim1[9], correlational_space$Dim2[8] - correlational_space$Dim2[9])
# RSNET1_pivotal <- c(correlational_space$Dim1[7] - correlational_space$Dim1[9], correlational_space$Dim2[7] - correlational_space$Dim2[9])
# 
# cosine(cbind(RSNET1_pre_alloc, RSNET1_pivotal))
# 
# angle <- function(x,y){
#   dot.prod <- x%*%y 
#   norm.x <- norm(x,type="2")
#   norm.y <- norm(y,type="2")
#   theta <- acos(dot.prod / (norm.x * norm.y))
#   as.numeric(theta)
# }
# 
# angle(RSNET1_pivotal, RSNET1_integrative) * 180 / pi
