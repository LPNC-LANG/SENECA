# Intellectual property of Pr. Dominique Muller; UGA LIP/PC2S, Grenoble, France


# Description:
# Save Studentized deleted residuals, Cook D's and lever within a dataframe that
# contains the variables implied in the regression model.
# (NB: require to use row.names to identify observations)

# Usage:
# outliers(formula,df,order="sdr")

# Arguments:
# formula	the regression equation to be used
# df		data frame where variables are stored and where indices will be stored
# order 	which criteria for ordering (sdr,default): "sdr" (for studentized deleted
# 			residuals), "cookd", or "leverage".

# Example:
# source("outliersFunction.R") # To load the function (need to specify the path if a
# 								default working directory is not set up)
# auto <- read.table("autoNA.txt",header=T,sep="\t",row.names="obs") # Note that it
# is critical to use the row.names statment (often with "subject" or "participant")
# to be able to identify the outliers.
# outliers(fatrate ~ drink + jantemp + density,auto,"cookd")


outliers <- function(formule, DF, order = "sdr") {
  monModel <- formule
  fit <- lm(monModel, DF)

  DF <- model.frame(monModel, DF)

  DF$sdr <- round(rstudent(fit), 2)
  DF$fstar <- round(DF$sdr^2, 2)
  DF$cookd <- round(cooks.distance(fit), 2)
  DF$leverage <- round(hatvalues(fit), 2)

  if (order == "sdr") {
    DF[order(-DF$fstar), ]
  } else {
    if (order == "cookd") {
      DF[order(-DF$cookd), ]
    } else {
      DF[order(-DF$leverage), ]
    }
  }
}
