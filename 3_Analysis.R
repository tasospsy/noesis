## Internship project
## Tasos Psychogyiopoulos
## DRAFT - ANALYSIS
## c.17/02/2021 / m.30/3/2021

# ---- required sources
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))

load("out.Rda") 
## 
hoi <- Decomp2(output = out, extract = "fitmeasures")

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
indices <- c('chisq', 'pvalue', 'rmsea', 'tli', 'cfi','nfi', 'bic','aic.ll')
sapply(indices, histfun) %>% invisible

# Test: Do fit Indices pick the right model?
  TableZ <- function(index, list = hoi, Min = FALSE){
    st <- lapply(list, sapply, function(q) q[[index]]) %>%
      sapply (., function(.)
        apply(., 1, function(.)
          ifelse(Min, which.min(.), which.max(.))) %>%
        table)
}

rmsea_test <- TableZ(index = 'rmsea' , Min = TRUE) 
bic_test   <- TableZ(index = 'bic'   , Min = TRUE) 
aic_test   <- TableZ(index = 'aic.ll', Min = TRUE)

tli_test   <- TableZ(index = 'tli'   , Min = FALSE)
cfi_test   <- TableZ(index = 'cfi'   , Min = FALSE)
nfi_test   <- TableZ(index = 'nfi'   , Min = FALSE)

# Hypothesis: HF VS BF, if NW is the true model
TableH <- function(index, list = hoi, Min = FALSE){
  st <- lapply(list, sapply, function(q) q[[index]])$trueNW[ ,1:2] %>%
      apply(., 1, function(.)
        ifelse(Min, which.min(.), which.max(.))) %>%
        table
}

rmsea_hypo <- TableH(index = 'rmsea' , Min = TRUE) 
bic_hypo   <- TableH(index = 'bic'   , Min = TRUE) 
aic_hypo   <- TableH(index = 'aic.ll', Min = TRUE)

tli_hypo   <- TableH(index = 'tli'   , Min = FALSE)
cfi_hypo   <- TableH(index = 'cfi'   , Min = FALSE)
nfi_hypo   <- TableH(index = 'nfi'   , Min = FALSE)






 
