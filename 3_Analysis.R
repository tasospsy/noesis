## Internship project
## Tasos Psychogyiopoulos
## DRAFT - ANALYSIS
## c.17/02/2021 / m.04/04/2021

# ---- required sources
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))

##  load("out.Rda") 
hoi <- Decomp2(output = out, extract = "fitmeasures")
## Example Structure: hoi$trueHF$fitHF$chisq

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
## Set Graphics parameter 3x3
par(mfrow=c(3,3))
sapply(indices, histfun) %>% invisible

# Test: Do fit Indices pick the right model?
## For 'TableL' function see, 'background_Funcions.R'
## UPDATE: see, '4_VISUALIZATIONS' for and updated table.

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


# VISUALIZATION (see. part 4. Visualization)

         ## Old code
# Draw the boxplot and the histogram 
## HistBox <- function(index, truemodel = c('trueHF', 'trueBF', 'trueNW'),
##                     fit = c('fitHF', 'fitBF', 'fitNW')) {
##   boxplot(hoi[[truemodel]][[fit]][[index]] ,horizontal=TRUE, frame=FALSE, axes = FALSE )
##   text(x = boxplot.stats(hoi[[truemodel]][[fit]][[index]])$stats[3], 
##        labels = round(boxplot.stats(hoi[[truemodel]][[fit]][[index]])$stats[3] ,4), y = 1.25)
##   hist(hoi[[truemodel]][[fit]][[index]]  , border=FALSE  , main= paste(truemodel,fit) , xlab=index)
## }
## # Graphics layout
## par(mfcol = c(4,3), mar=c(4, 3.1, 1.1, 2.1))
## sapply(indices[1:6], HistBox, truemodel = 'trueBF', fit = 'fitBF') %>% invisible
## sapply(indices[1:6], HistBox, truemodel = 'trueNW', fit = 'fitBF') %>% invisible

# graphics.off()

