## Internship project
## Tasos Psychogyiopoulos
## DRAFT - ANALYSIS
## c.17/02/2021 / m.01/06/2021

# ---- required sources
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))
library(tidyverse)

## The simulation results have been store in an object called 'out'.
## If you had already run the simulation process, then you can just
## load them as below. 
load("out.Rda") 

# Use the 'Decomp2' function that I created to decompose
# the simulation results and store the fit measures given by 
# 'psychonetrics'.

hoi <- Decomp2(output = out, extract = "fitmeasures")
## Example Structure: hoi$trueHF$fitHF$chisq

## TIDY UP THE SIMULATION RESULTS

# Selected fit measures for this study
exact.fit   <-  c("chisq", "df", "pvalue")
approx.fit  <-  c("rmsea", "cfi", "tli", "nfi")
comp.criteria    <-  c("aic.ll", "bic")

## TRUE HF - FIT HF
HFHF <-  hoi$trueHF$fitHF %>% as_tibble() %>% 
  add_column(FitModel = "Higher-order factor") %>% 
  rownames_to_column("Rep")
## TRUE HF - FIT BF
HFBF <-  hoi$trueHF$fitBF %>% as_tibble() %>% 
  add_column(FitModel = "Bi-factor") %>% 
  rownames_to_column("Rep")
## TRUE HF - FIT NW
HFNW <-  hoi$trueHF$fitNW %>% as_tibble()%>% 
  add_column(FitModel = "Network")%>% 
  rownames_to_column("Rep")
## merge
data1 <- bind_rows(HFHF, HFBF, HFNW) %>% 
  dplyr::select(all_of(c(exact.fit, approx.fit, comp.criteria)),
                FitModel, Rep) %>% 
  add_column(TrueModel = "Higher-order factor") 

## TRUE BF - FIT HF
BFHF <-  hoi$trueBF$fitHF %>% as_tibble() %>% 
  add_column(FitModel = "Higher-order factor") %>% 
  rownames_to_column("Rep")
## TRUE BF - FIT BF
BFBF <-  hoi$trueBF$fitBF %>% as_tibble() %>% 
  add_column(FitModel = "Bi-factor") %>% 
  rownames_to_column("Rep")
## TRUE BF - FIT NW
BFNW <-  hoi$trueBF$fitNW %>% as_tibble()%>% 
  add_column(FitModel = "Network") %>% 
  rownames_to_column("Rep")
## merge
data2 <- bind_rows(BFHF, BFBF, BFNW) %>% 
  dplyr::select(all_of(c(exact.fit, approx.fit, comp.criteria)),
                FitModel, Rep) %>% 
  add_column(TrueModel = "Bi-factor") 

## TRUE NW - FIT HF
NWHF <-  hoi$trueNW$fitHF %>% as_tibble() %>% 
  add_column(FitModel = "Higher-order factor") %>% 
  rownames_to_column("Rep")
## TRUE NW - FIT BF
NWBF <-  hoi$trueNW$fitBF %>% as_tibble() %>% 
  add_column(FitModel = "Bi-factor") %>% 
  rownames_to_column("Rep")
## TRUE NW - FIT NW
NWNW <-  hoi$trueNW$fitNW %>% as_tibble()%>% 
  add_column(FitModel = "Network") %>% 
  rownames_to_column("Rep")
## merge
data3 <- bind_rows(NWHF, NWBF, NWNW) %>% 
  dplyr::select(all_of(c(exact.fit, approx.fit, comp.criteria)),
                FitModel, Rep) %>% 
  add_column(TrueModel = "Network") 

# Cucina et al 2017 data
Cucina.table <- tibble(CFI = c(.975, .977, .975, .967, .989, .979, .990),
                       TLI = c(.965, .967, .965, .954, .982, .968, .966),
                       NFI = c(.951, .963, .954, .948, .986, .976, .983),
                       RMSEA = c(.049, .052, .053, .063, .046, .056, .035)) 

Cucina <-  Cucina.table %>% 
  add_column(TrueModel = "Empirical") %>% 
  add_column(FitModel = "Bi-factor") 

# merge all datasets
alldat <-  bind_rows(data1, data2, data3) %>% 
  dplyr::rename(Chi.squared = chisq, RMSEA = rmsea, CFI = cfi, p.value = pvalue,
                TLI = tli, NFI = nfi, AIC = aic.ll, BIC = bic) %>% 
  bind_rows(Cucina)

#head(alldat)

# Test: Do fit Indices pick the right model?

pickFun <- function(truemodel = c("Higher-order factor", 
                                  "Bi-factor",
                                  "Network")) {
  testall <- alldat %>% 
    filter(TrueModel == truemodel) %>% 
    dplyr::select(RMSEA, CFI, TLI, NFI, AIC, BIC, Rep, FitModel) %>% 
    group_by(Rep) %>% 
    summarize(RMSEA = FitModel[which.min(RMSEA)],
              AIC   = FitModel[which.min(AIC)],
              BIC   = FitModel[which.min(BIC)],
              TLI   = FitModel[which.max(TLI)],
              CFI   = FitModel[which.max(CFI)],
              NFI   = FitModel[which.max(NFI)]
    ) %>% 
    ungroup() %>% 
    gather(key = "Chosen_Index", value = "Fitting_Model", - Rep) %>% 
    group_by(Chosen_Index) %>% 
    count(Fitting_Model) %>% 
    mutate(percent = n /10)
}

all_true_HF <- pickFun(truemodel = "Higher-order factor") 
all_true_BF <- pickFun(truemodel = "Bi-factor")
all_true_NW <- pickFun(truemodel = "Network")

# HYPOTHESIS: True: NW - Fitting: HF Vs BF

trueNW_HFvsBF <- alldat %>% 
  filter(TrueModel == "Network", !FitModel == "Network") %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI, AIC, BIC, Rep, FitModel) %>% 
  group_by(Rep) %>% 
  summarize(RMSEA = FitModel[which.min(RMSEA)],
            AIC   = FitModel[which.min(AIC)],
            BIC   = FitModel[which.min(BIC)],
            TLI   = FitModel[which.max(TLI)],
            CFI   = FitModel[which.max(CFI)],
            NFI   = FitModel[which.max(NFI)]
  ) %>% 
  ungroup() %>% 
  gather(key = "Chosen_Index", value = "Fitting_Model", - Rep) %>% 
  group_by(Chosen_Index) %>% 
  count(Fitting_Model) %>% 
  mutate(percent = n /10)


# Model Comparison HF Vs BF
# ABSOLUTE FIT: 
Dchisq <- lapply(hoi, sapply, function(.) .[['chisq']]) %>%
  lapply(., function(.).[ ,1] - .[,2]) 
Ddf   <- lapply(hoi, sapply, function(.) .[['df']]) %>%
  lapply(., function(.).[ ,1] - .[,2]) 

# I need p.value of Chisq_diff!!

# VISUALIZATION (see. part 4. Visualization)


## Old code to check the distribution of the data

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
## ar(mfcol = c(1,1), mar=c(0,0,0,0))
# graphics.off()


## OLD CODE for 'contigency tables'
## ## For 'TableL' function see, 'background_Funcions.R'
## (rmsea_test <- TableL(index = 'rmsea' , Min = TRUE,  truemodel = 'ALL', compare_models = 'ALL'))
## (bic_test   <- TableL(index = 'bic'   , Min = TRUE,  truemodel = 'ALL', compare_models = 'ALL'))
## (aic_test   <- TableL(index = 'aic.ll', Min = TRUE,  truemodel = 'ALL', compare_models = 'ALL'))
## (tli_test   <- TableL(index = 'tli'   , Min = FALSE, truemodel = 'ALL', compare_models = 'ALL'))
## (cfi_test   <- TableL(index = 'cfi'   , Min = FALSE, truemodel = 'ALL', compare_models = 'ALL'))
## (nfi_test   <- TableL(index = 'nfi'   , Min = FALSE, truemodel = 'ALL', compare_models = 'ALL'))
## 
## # Hypothesis: HF VS BF, if NW is the true model
## (rmsea_hypo <- TableL(index = 'rmsea' , Min = TRUE,  truemodel = 'NW', compare_models = 'HFvsBF'))
## (bic_hypo   <- TableL(index = 'bic'   , Min = TRUE,  truemodel = 'NW', compare_models = 'HFvsBF'))
## (aic_hypo   <- TableL(index = 'aic.ll', Min = TRUE,  truemodel = 'NW', compare_models = 'HFvsBF'))
## (tli_hypo   <- TableL(index = 'tli'   , Min = FALSE, truemodel = 'NW', compare_models = 'HFvsBF'))
## (cfi_hypo   <- TableL(index = 'cfi'   , Min = FALSE, truemodel = 'NW', compare_models = 'HFvsBF'))
## (nfi_hypo   <- TableL(index = 'nfi'   , Min = FALSE, truemodel = 'NW', compare_models = 'HFvsBF'))
