.R2quickcalc = function(X,Y){
  # browser()
  intercept = rep(1,613)
  X = base::cbind(intercept, X)
  out1= Rfast::lmfit(x=X,y=Y)
  Y = as.numeric(Y)
  return(1- sum(out1$residuals^2)/sum((Y-mean(Y))^2))
}

# Estimate p=value from Bootstrap distribution ---------------------------------------------------------------------------------
.boot_pval = function(x, null_val=0){
  x = stats::na.omit(x)
  perc = length(which(x<null_val))/length(x)
  p_val = 1-abs(.50-perc)*2
  return(p_val)
}


#'  Cross-Validtation Bootstrap CCA
#'
#' Algorithm for using cross-validation & bootstrap resampling to find unbiased canonical correlation and their
#' sampling error. On each iteration, N-fold (default is 10 fold) cross-validation is used to generated predicted canonical
#' variates for the complete sample. Following this, the predicted variates are bootstrap resampled and
#' canonical correlations are estimated from them.
#' Because bootstrap resampling can change the order of canonical variates that are extracted, or sign
#' flipping can occur in some cases (i.e. a very similar latent variable is extracted but on some occasions
#' the loadings are mostly positive or negative), we rotate the loadings in each during cross-validation to
#' map onto the loadings generated from a smaller dataset (ProcrustX & ProcrustY)
#'
#' @param ProcrustX Numeric Matrix [ncomp, P1] containing target matrix for Procrustes Analysis. All CCA predictor raw coefficients obtained during the bootstrap resampling will be rotated to this target matrix.
#' @param ProcrustY Numeric Matrix [ncomp, P2] containing target matrix for Procrustes Analysis. All CCA outcome raw coefficients obtained during the bootstrap resampling will be rotated to this target matrix.
#' @param X_FIT Numeric Matrix or Data Frame [N, P1] containing the predictor variables.
#' @param Y_FIT Numeric Matrix or Data Frame [N, P2] containing the outcome variables.
#' @param ncomp Numeric Scalar. Number of CCA components to keep in analyses. Must be equal to or less than min(P1,P2).
#' @param Nboot Numeric Scalar. Number of times to repeat k-fold cross-validation (yes its confusing it says "boot").
#' @param Nfolds Numeric Scalar. Number of
#' @param UseProgressBar Logical. Whether to show progress bar.
#' @param UseProcrustes  Logical. Whether to use procrustes analysis.
#'
#' @export
#'
customed_cca_cv_boot = function(X_FIT,Y_FIT, ncomp=10, Nboot=30, Nfolds=10,ProcrustX = NULL, ProcrustY = NULL, UseProgressBar=TRUE, UseProcrustes=TRUE){
  if (is.data.frame(X_FIT)) X_FIT  = as.matrix(X_FIT)
  if (is.data.frame(Y_FIT)) Y_FIT  = as.matrix(Y_FIT)
  
  # browser()
  if (UseProgressBar){
    pb <- utils::txtProgressBar(min = 0, max = Nboot, style = 3)
  }
  
  CCA_OriginalData = .cca(X_FIT=X_FIT, Y_FIT=Y_FIT, X_PRED=NULL, Y_PRED=NULL, ncomp=ncomp, ProcrustX = ProcrustX, ProcrustY = ProcrustY)
  
  
# CCA_OriginalData = ccatools_res
# ncomp = 2
# Nfolds = 10
# Nboot = 10
# 
# X_FIT = as.matrix(brain_set)
# Y_FIT = as.matrix(cog_set)

  if (UseProcrustes==FALSE){
    CCA_OriginalData$xcoef = NULL # By setting this to NULL, the .cca function called below will not use procrustes rotations
    CCA_OriginalData$ycoef = NULL
  }
  
  cc_CVboot = list()
  cc_CV = list()
  R2_matrix = list()
  
  for(b in 1:Nboot){
    #Divide data into folds...
    df_folds = caret::createFolds(1:nrow(X_FIT), k=Nfolds)
    # df_folds = cut(sample(nrow(X_FIT)), breaks=Nfolds, labels=FALSE)
    
    Variate_predictions = list()
    for (f in 1:Nfolds){
      
      # Fit_Index = (df_folds!=f) #Rows to select to fit data to
      # Pred_Index = (df_folds==f) #Rows to select to make predictions for
      #
      Fit_Index = base::unlist(df_folds[-f], FALSE, FALSE) #Row numbers - Training Data
      Pred_Index = base::unlist(df_folds[f], FALSE, FALSE) #Row numbers - Hold-Out Data
      
      #Estimate CCA in trainning dataset and generate list of predictions for hold-out data - and append to Variate_predictions list
      Variate_predictions =  c(Variate_predictions,
                               list(.cca(X_FIT  = X_FIT[Fit_Index,],  Y_FIT  = Y_FIT[Fit_Index,],
                                         X_PRED = X_FIT[Pred_Index,], Y_PRED = Y_FIT[Pred_Index,], ncomp=ncomp,
                                         ProcrustX = CCA_OriginalData$xcoef, ProcrustY = CCA_OriginalData$ycoef)$variates)
      )
      
    }
    
    #Put cross-validated predictions back in original order and in a data frame
    Variates_CrossValidated = data.table::rbindlist(Variate_predictions)
    Variates_CrossValidated = Variates_CrossValidated[base::order(unlist(df_folds, FALSE, FALSE)),]
    
    #Bootstrap cross-validation predictions
    boot_i = base::sample(nrow(Variates_CrossValidated),nrow(Variates_CrossValidated), TRUE, NULL)
    Variates_CrossValidated_b = as.matrix(Variates_CrossValidated[boot_i,])
    
    #Estimate canonical correlations
    cc_CVboot[[b]] = Rfast::corpairs(as.matrix(Variates_CrossValidated_b[,1:ncomp]),as.matrix(Variates_CrossValidated_b[,(ncomp+1):(2*ncomp)]))
    # cc_CVboot = c(cc_CVboot , list(Rfast::corpairs(as.matrix(Variates_CrossValidated_b[,1:ncomp]),as.matrix(Variates_CrossValidated_b[,(ncomp+1):(2*ncomp)])))) #Not necessary
    
    #Estimate R2 for all outcome variables (with boot)
    Variates_CV_Scaled = as.matrix(Rfast::standardise(Variates_CrossValidated_b, center = TRUE, scale = TRUE))
    Y_FIT_Scaled =       as.matrix(Rfast::standardise(Y_FIT[boot_i,],center = TRUE, scale = TRUE))
    
    R2_matrix[[b]] =
      sapply(1:ncomp, function(ncomp_i)
        sapply(1:ncol(Y_FIT), function(y_i)
          .R2quickcalc(X=Variates_CV_Scaled[,1:ncomp_i],Y=Y_FIT_Scaled[,y_i])
        )
      )
    
    #Standard Cross-Validation canonical correlation
    cc_CV[[b]] = Rfast::corpairs(as.matrix(Variates_CrossValidated[,1:ncomp]),as.matrix(Variates_CrossValidated[,(ncomp+1):(2*ncomp)]))
    
    
    if (UseProgressBar){
      utils::setTxtProgressBar(pb, value=b)
    }
  }
  
  
  # R2 Means
  R2_matrix = apply(base::simplify2array(R2_matrix), 1:2, mean)
  #Prettify output
  R2_matrix = data.frame(R2_matrix)
  rownames(R2_matrix) = colnames(Y_FIT)
  colnames(R2_matrix) = paste0("NVariates_",1:ncomp)
  
  # Cross-Validation Quantiles
  cc_CV_quantiles = do.call("rbind.data.frame", cc_CV)
  colnames(cc_CV_quantiles) = paste0("cc",1:ncomp)
  cc_CV_quantiles = apply(cc_CV_quantiles, 2, function(x) stats::quantile(x, probs = c(0.025,.5,.975), type=6))
  # cc_CV_pval = apply(cc_CV_quantiles, 2, function(x) boot_pval(x))
  
  # Cross-validation + Bootstrap Quantiles
  cc_CVBoot_quantiles = do.call("rbind.data.frame", cc_CVboot)
  colnames(cc_CVBoot_quantiles) = paste0("cc",1:ncomp)
  cc_CVBoot_quantiles2 = apply(cc_CVBoot_quantiles, 2, function(x) stats::quantile(x, probs = c(0.025,.5,.975), type=6))
  cc_CVBoot_pval = apply(cc_CVBoot_quantiles, 2, function(x) .boot_pval(x))
  
  
  return(list(
    R2_matrix = R2_matrix,
    CrossValidationQuantiles = cc_CV_quantiles,
    CrossValidationBootstrapQuantiles = cc_CVBoot_quantiles2,
    CrossValidationBootstrapPvalues = cc_CVBoot_pval
    
  ))
  
  
}
