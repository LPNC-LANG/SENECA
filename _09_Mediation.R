################################################################################
# Written by Clément Guichet, PhD Student
# LPNC - CNRS UMR 5105 
# 2023

################################################################################

source("_06_Main_Statistics.R")

################################################################################
# MEDIATION
################################################################################
# Scale the other variables
mediation_data <- TFP_Subject_overview %>% filter(Subj_ID %in% Data_CCA_full$Subj_ID) %>% 
  mutate(Brain_Mode = Brain_Mode,
         Cognitive_Mode = Cognitive_Mode) %>% 
  mutate_at(vars(Age, Brain_Mode, Cognitive_Mode, mean_FC, Disruption), funs(as.numeric(scale(.)))) %>% 
  dplyr::select(Subj_ID, Gender_c, Age, Brain_Mode, Cognitive_Mode, mean_FC, Disruption)

# write.csv(mediation_ComStruct, "MEDIATION_DF.csv")

flexplot::flexplot(Cognitive_Mode ~ Age, mediation_data) # make sure to interpret the coefficients correctly, positive means worse performances


library(lavaan)
library(sem)
library(semPlot)
library(processR)

# Hayes (2018) - Slope analysis
quantile(mediation_data$Disruption, probs = c(.16, .50, .84))

MEDIATION_MODEL <- "

##########################################
Cognitive_Mode ~ b1*Brain_Mode + b2*Brain_Mode:Disruption + c1*Age + c2*Disruption + c3*Age:Disruption + cov1*mean_FC + cov2*Gender_c
Brain_Mode ~ a*Age + covmed1*mean_FC + covmed2*Gender_c
Disruption ~ d1*Brain_Mode + a2*Age + covmed1*mean_FC + covmed2*Gender_c
##########################################

##########################################
# For Main mediation (Age -> Brain -> Cog)
# For Inner mediation (Age --> Brain --> Efficiency)
##########################################
direct_main:= c1
direct_inner:= a2

# Indirect effects
Indirect_mainMed:= a*b1
Indirect_innermed:= a*d1

# direct inner
direct_inner:= direct_inner + Indirect_innermed

# Index of moderated mediation
Index_MedMod:= a*b2
# Index_MedMod_inner:= a*d1*b2

# simple slope of Cognitive_Mode on Brain_Mode is b1+b2*Disruption
bLow: = b1+b2*(-1.014791822)
bMedian: = b1+b2*(0.008879843)
bMean: = b1+b2*(0)
bHigh: = b1+b2*(1.044607287)

# conditional indirect effects is a*d1*(b1+b2*Disruption)
abLow: = Index_MedMod*bLow
abMedian: = Index_MedMod*bMedian
abMean: = Index_MedMod*bMean
abHigh: = Index_MedMod*bHigh

# direct effects
direct_Low: = c1+c3*(-1.014791822)
direct_Median: = c1+c3*(0.008879843)
direct_Mean: = c1+c3*(0)
direct_High: = c1+c3*(1.044607287)

# total effects

tot_below:= direct_Low + abLow
tot_Median:= direct_Median + abMedian
tot_Mean:= direct_Mean + abMean
tot_High:= direct_High + abHigh

# prop.mediated
prop_below:= abLow / tot_below
prop_Median:= abMedian / tot_Median
prop_Mean:=  abMean / tot_Mean
prop_High:=  abHigh / tot_High
"


set.seed(1)
fit_dual <- lavaan::sem(MEDIATION_MODEL,
                        data = mediation_data,
                        se = "bootstrap", bootstrap = 1000,
                        fixed.x = FALSE, meanstructure = TRUE
)


summary(fit_dual, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE, ci = TRUE)
parameterEstimates(fit_dual, level = 0.95, boot.ci.type = "bca.simple", standardized = FALSE)
AIC(fit_dual)

# ################################################################################
# ################################################################################
# Nonlinear interactions
# ################################################################################
# ################################################################################

library(mgcv)
source("_vis_gam_modified.R")

visualization <- mediation_data %>% dplyr::select(Subj_ID, mean_FC, Gender_c, Brain_Mode, Cognitive_Mode) %>% 
  mutate(Cognitive_Mode = Cognitive_Mode*(-1)) %>% 
  mutate(Brain_Mode = Brain_Mode*(-1)) %>% 
  merge(., efficiencies_ROI, by = "Subj_ID") %>% 
  merge(., TFP_Subject_overview %>% dplyr::select(Subj_ID, Age), by = "Subj_ID")



moderation_AgeBrainMode <- mgcv::gam(Cognitive_Mode~ ti(Age) + ti(Brain_Mode) + ti(Age, Brain_Mode)
                                     + mean_FC + Gender_c, 
                                     data = visualization)
summary(moderation_AgeBrainMode)

cor.test(visualization$Brain_Mode, visualization$Disruption)


visualization$b2 <- visualization$Brain_Mode*(visualization$Disruption)

mod_lin_B <- mgcv::gam(Cognitive_Mode~ 
                         ti(b2, Age) +
                         + mean_FC + Gender_c, 
                       data = visualization)

summary(mod_lin_B)

vis.gam.1(mod_lin_B, theta = 25, view = c("b2", "Age"),  
          phi = 20, n.grid = 20, ticktype = "detailed",
          zlab.1 = "", color = "custom")
