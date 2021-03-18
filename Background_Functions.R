## Internship project
## Tasos Psychogyiopoulos
## BACKGROUND_FUNCTIONS
## c.17/02/2021/ m.16/3/2021

## 'GGM_search' Functions: Performs model search to extract
## a GGM Network model, given a specified sample (Kan et al, 2020) 
GGM_search <- function(dat,
                       from = c("US", "GER", "HUN"),
                       alpha_prune = 0.01, fast = TRUE) {
  ## Specify a saturated Network Model according to 'from' WAIS data
  NWmodel_sat <- ggm(covs = (dat[['size']][[from]] - 1)/dat[['size']][[from]] * dat[['Data']][[from]],
                     omega = "Full",
                     nobs = dat[['size']][[from]])
  ## Removing the insignificant partial correlations (alpha = .01) 
  ## & Further improvement until minimizes BIC criterion
  NWmodel_imp <- NWmodel_sat %>%
    prune(alpha = alpha_prune, recursive = TRUE)
  if (fast) NWmodel_imp %>% stepup 
  if (!fast) NWmodel_imp %>% modelsearch #different algorithm; slower
  ## Extract adjacency matrix & save in .GlobalEnv *automatically*
  adj.matrix <<- 1*(getmatrix(NWmodel_imp, "omega")!=0)
  return(NWmodel_imp)
}
# ___________________________________________________________

## 'ModelsFitFun' Function: Fit the 3 models to our data.
ModelsFitFun <- function (HFmodel = FALSE,
                          BFmodel = FALSE,
                          NWmodel = FALSE,
                          Raw = FALSE,
                          dat, 
                          which = c("US", "GER", "HUN")){
  if(!Raw){
    if (HFmodel){
      hmodel <<- lvm(lambda = lambda_h,
                     beta = beta_h,
                     sigma_zeta = "empty",
                     covs = (dat[['size']][[which]] - 1) / 
                             dat[['size']][[which]] * dat[['Data']][[which]],
                     nobs = dat[['size']][[which]],
                     identification = "variance",
                     optimizer = "ucminf") %>% runmodel
      }
    if (BFmodel){
      BFmodel <<- lvm(lambda = lambda_bi,
                      sigma_zeta = "empty", # Ψ
                      covs = (dat[['size']][[which]] - 1) /
                              dat[['size']][[which]] * dat[['Data']][[which]],
                      nobs = dat[['size']][[which]],
                      identification = "variance",
                      optimizer = "ucminf") %>% runmodel
      }
    if (NWmodel){
      NWmodel <<- ggm(covs = (dat[['size']][[which]] - 1) /
                              dat[['size']][[which]] * dat[['Data']][[which]],
                      omega = adj.matrix,
                      nobs = dat[['size']][[which]],
                      optimizer = "ucminf") %>% runmodel
    }
  }
   if(Raw){
     if (HFmodel){
      hmodelraw <- lvm(lambda = lambda_h,
                       beta = beta_h,
                       sigma_zeta = "empty",
                       data = dat,
                       identification = "variance",
                       optimizer = "ucminf") %>% runmodel
      return(hmodelraw)
      }
    if (BFmodel){
      BFmodelraw <- lvm(lambda = lambda_bi,
                        sigma_zeta = "empty", # Ψ
                        data = dat,
                        identification = "variance",
                        optimizer = "ucminf") %>% runmodel
      return(BFmodelraw)
      }
    if (NWmodel) {
      NWmodelraw <- ggm(data = dat,
                        omega = adj.matrix,
                        optimizer = "ucminf") %>% runmodel
      return(NWmodelraw)
    }
  }
}
# ___________________________________________________________

## 'GetCorMat' Function: Gives correlation implied matrix from a psychonetrics model
GetCorMat <- function(x){
  tempCor <- cov2cor(getmatrix(x, "sigma"))
  dimnames(tempCor) <- list(obsvars, obsvars)
  return(tempCor)
}
# ___________________________________________________________

## 'Decomp' Function: Takes the output from the simulations, and a vector
##  of the desired fit statistics and transform them to readable data frame.
Decomp <- function(simout, fitMeasures){
  # Apply the vector arg to the output to extract all the fit measures
  tmp <- sapply(fitMeasures,
                function(m){
                  lapply(simout, sapply, sapply,
                         function(skiptheque) skiptheque@fitmeasures[[m]])
                }, simplify = F)
  # Transform it to data.frame
  tmp <- do.call(rbind, Map(data.frame, df = tmp$df, chisq = tmp$chisq, 
                            pvalue = tmp$pvalue, rmsea = tmp$rmsea, 
                            cfi = tmp$cfi, tli = tmp$tli, 
                            aic = tmp$aic, bic = tmp$bic))
  # Create an empty list
  stat <- list()
  # Decompose the output and store the fit measures in data frames
  # For True Model = HF
  stat$trueHF$fitHF <- tmp[seq(1, nrow(tmp) - 2, by = 3), seq(1, ncol(tmp) - 2, by = 3)]
  stat$trueHF$fitBF <- tmp[seq(2, nrow(tmp) - 1, by = 3), seq(1, ncol(tmp) - 2, by = 3)]
  stat$trueHF$fitNW <- tmp[seq(3, nrow(tmp)    , by = 3), seq(1, ncol(tmp) - 2, by = 3)]
  # For True Model = BF
  stat$trueBF$fitHF <- tmp[seq(1, nrow(tmp) - 2, by = 3), seq(2, ncol(tmp) - 1, by = 3)]
  stat$trueBF$fitBF <- tmp[seq(2, nrow(tmp) - 1, by = 3), seq(2, ncol(tmp) - 1, by = 3)]
  stat$trueBF$fitNW <- tmp[seq(3, nrow(tmp)    , by = 3), seq(2, ncol(tmp) - 1, by = 3)]
  # For True Model = NW
  stat$trueNW$fitHF <- tmp[seq(1, nrow(tmp) - 2, by = 3), seq(3, ncol(tmp)    , by = 3)]
  stat$trueNW$fitBF <- tmp[seq(2, nrow(tmp) - 1, by = 3), seq(3, ncol(tmp)    , by = 3)]
  stat$trueNW$fitNW <- tmp[seq(3, nrow(tmp)    , by = 3), seq(3, ncol(tmp)    , by = 3)]
  # Give as rownames the Number of Replication
  stat <- lapply(stat, lapply, "rownames<-", paste("Rep", 1:nrep, sep=""))
  # define the colnames more clear. !NOTE: follows the order of the Map function above!
  stat <- lapply(stat, lapply, "colnames<-", c("df", "chisq", "p_chisq", "RMSEA", 
                                               "CFI", "TLI", "AIC", "BIC"))
  return(stat)
}


