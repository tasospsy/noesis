## Internship project
## Tasos Psychogyiopoulos
## DRAFT - ANALYSIS
## c.17/02/2021 / m.04/04/2021

# ---- required sources
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))

load("out.Rda") 
## 
hoi <- Decomp2(output = out, extract = "fitmeasures")
## Example Structure: hoi$trueHF$fitHF[['chisq']]

## Set Graphics parameter 3x3
par(mfrow=c(3,3))

# Check histograms
histfun <- function(index){
  for (i in 1:3) {
    for(j in 1:3){
      hist(hoi[[i]][[j]][[index]], xlab = index, 
           main = paste(names(hoi[i]),names(hoi[[i]][j])))
    }
  }
}
indices <- c('chisq', 'pvalue', 'rmsea', 'tli', 'cfi','nfi', 'bic','aic.ll') #
sapply(indices, histfun) %>% invisible

# Test: Do fit Indices pick the right model?
(rmsea_test <- TableL(index = 'rmsea' , Min = TRUE,  truemodel = 'ALL', compare_models = 'ALL'))
(bic_test   <- TableL(index = 'bic'   , Min = TRUE,  truemodel = 'ALL', compare_models = 'ALL'))
(aic_test   <- TableL(index = 'aic.ll', Min = TRUE,  truemodel = 'ALL', compare_models = 'ALL'))
(tli_test   <- TableL(index = 'tli'   , Min = FALSE, truemodel = 'ALL', compare_models = 'ALL'))
(cfi_test   <- TableL(index = 'cfi'   , Min = FALSE, truemodel = 'ALL', compare_models = 'ALL'))
(nfi_test   <- TableL(index = 'nfi'   , Min = FALSE, truemodel = 'ALL', compare_models = 'ALL'))

# Hypothesis: HF VS BF, if NW is the true model
(rmsea_hypo <- TableL(index = 'rmsea' , Min = TRUE,  truemodel = 'NW', compare_models = 'HFvsBF'))
(bic_hypo   <- TableL(index = 'bic'   , Min = TRUE,  truemodel = 'NW', compare_models = 'HFvsBF'))
(aic_hypo   <- TableL(index = 'aic.ll', Min = TRUE,  truemodel = 'NW', compare_models = 'HFvsBF'))
(tli_hypo   <- TableL(index = 'tli'   , Min = FALSE, truemodel = 'NW', compare_models = 'HFvsBF'))
(cfi_hypo   <- TableL(index = 'cfi'   , Min = FALSE, truemodel = 'NW', compare_models = 'HFvsBF'))
(nfi_hypo   <- TableL(index = 'nfi'   , Min = FALSE, truemodel = 'NW', compare_models = 'HFvsBF'))

# Model Comparison HF Vs BF
# ABSOLUTE FIT: 
Dchisq <- lapply(hoi, sapply, function(.) .[['chisq']]) %>%
  lapply(., function(.).[ ,1] - .[,2]) %>%
  lapply(., median)
Ddf   <- lapply(hoi, sapply, function(.) .[['df']]) %>%
  lapply(., function(.).[ ,1] - .[,2]) %>%
  lapply(., median)

# DIORTHWSE TO !
## Dpchisq   <- lapply(hoi, sapply, function(.) .[['pvalue']]) %>%
##   lapply(., function(.).[ ,1] - .[,2]) %>%
##   lapply(., hist)


