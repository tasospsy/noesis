## Internship project
## Tasos Psychogyiopoulos
## 4. VISUALIZATION (draft)
## c. 10/5/2021
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/3_Analysis.R"))
library(tidyverse)

## setwd("/Users/tasospsy/Google Drive/_UvA/Research Internship/Noesis/")
## load('out.rda')

## "Tidy" the simulation results

# Selected fit measures for this study
fitmeasures <- c("chisq", "df", "pvalue", "rmsea", "aic.ll", "bic", "cfi", "tli", "nfi")

## TRUE HF - FIT HF
HFHF <-  hoi$trueHF$fitHF %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures))
## TRUE HF - FIT BF
HFBF <-  hoi$trueHF$fitBF %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures))  
## TRUE HF - FIT NW
HFNW <-  hoi$trueHF$fitNW %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures)) 

## TRUE BF - FIT HF
BFHF <-  hoi$trueBF$fitHF %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures))
## TRUE BF - FIT BF
BFBF <-  hoi$trueBF$fitBF %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures))  
## TRUE BF - FIT NW
BFNW <-  hoi$trueBF$fitNW %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures))

## TRUE NW - FIT HF
NWHF <-  hoi$trueNW$fitHF %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures))
## TRUE NW - FIT BF
NWBF <-  hoi$trueNW$fitBF %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures))  
## TRUE NW - FIT NW
NWNW <-  hoi$trueNW$fitNW %>% as_tibble() %>% 
  dplyr::select(all_of(fitmeasures))


# 1st plot: FITTING BI-FACTOR MODELS

## TRUE BF - FIT BF
BFBF1 <-  BFBF %>% 
  dplyr::select(chisq, rmsea, cfi, pvalue, tli, nfi) %>% 
  rename(χ2 = chisq, RMSEA = rmsea, CFI = cfi, p_value = pvalue,
         TLI = tli, NFI = nfi)

# Calculating summary statistics to put them on plots
SumBFBF <- BFBF1 %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(medianBFBF = median(value), meanBFBF = mean(value)) %>% 
  mutate(lab = paste("median = ", round(medianBFBF,4), "\nmean =", round(meanBFBF,4))
         )
# Histogram Plot - TrueBF/FitBF
BFBF_hists <- BFBF1 %>% gather() %>% 
  ggplot(aes(x= value)) + 
  geom_histogram(bins = 15, show.legend = FALSE, fill = "coral", color = "white", alpha = 0.8) + 
  facet_wrap(~key, scales = 'free', ncol = 3)+
  geom_text(data = SumBFBF, aes(label = lab), 
            x=Inf, y=Inf, hjust=1, vjust=1, size=3) +
  ggtitle("True model Bi-factor Model - Fitting Bi-factor Model")+
  xlab("Fit Indices")+
  ylab("") +
  theme_minimal()
BFBF_hists

## TRUE NW - FIT BF
NWBF1 <-  NWBF %>% 
  dplyr::select(chisq, rmsea, cfi, pvalue, tli, nfi) %>% 
  rename(χ2 = chisq, RMSEA = rmsea, CFI = cfi, p_value = pvalue,
         TLI = tli, NFI = nfi)
# Summary statistics
SumNWBF <- NWBF1 %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(medianNWBF = median(value), meanNWBF = mean(value)) %>% 
  mutate(lab = paste("median = ", round(medianNWBF,4), 
                     "\nmean =", round(meanNWBF,4))
         )

# Histogram Plot - TrueNW/FitBF
NWBF_hists <- NWBF1 %>% gather() %>% 
  ggplot(aes(x= value)) + 
  geom_histogram(bins = 15, show.legend = FALSE, color = "white", fill = "deepskyblue", alpha = 0.6) + 
  facet_wrap(~key, scales = 'free', ncol = 3) +
  geom_text(data = SumNWBF, aes(label = lab), 
            x=Inf, y=Inf, hjust=1, vjust=1, size=3) +
  ggtitle("True model Network Model - Fitting Bi-factor Model")+
  xlab("Fit Indices") +
  ylab("") +
  theme_minimal()
NWBF_hists

## Comparison Plot

BFBFt <- BFBF %>% add_column(TrueModel = "Bi-factor")
NWBFt <- NWBF %>% add_column(TrueModel = "Network")
HFBFt <- HFBF %>% add_column(TrueModel = "Higher-order factor")

comp <- bind_rows(BFBFt, NWBFt, HFBFt) %>% 
  rename(χ2 = chisq, RMSEA = rmsea, CFI = cfi, p_value = pvalue,
         TLI = tli, NFI = nfi) %>% 
  dplyr::select(TrueModel, RMSEA, TLI, CFI, NFI, p_value, χ2) %>% 
  gather(key = "Index", value = "value", -TrueModel)

ggcomp <- ggplot(comp) + 
  geom_histogram(aes(x = value, color = TrueModel,fill = TrueModel),
                 bins = 70, alpha = .2) +
  facet_wrap(~Index, scales = 'free')+
  ggtitle("Fitting Bi-factor Models")+
  xlab("Fit Indices Comparison") +
  ylab("") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggcomp


# Cucina et al 2017 data
Cucina <- tibble(CFI = c(.975, .977, .975, .967, .989, .979, .990),
                 TLI = c(.965, .967, .965, .954, .982, .968, .966),
                 NFI = c(.951, .963, .954, .948, .986, .976, .983),
                 RMSEA = c(.049, .052, .053, .063, .046, .056, .035)) 

ggcuc <- gather(Cucina) %>% 
  ggplot() +
  geom_boxplot(aes(x = value, color = key, fill = key), alpha = .4) +
  facet_wrap(~key, scales = 'free') +
  theme_minimal()
  
ggcuc

## TABLE VISUALIZATION CHECK 
## TRUE MODEL: HF

## TRUE HF - FIT HF
HFHFf <- HFHF %>% 
  dplyr::select(chisq, df, rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Higher-Order factor") %>% 
  rownames_to_column("Rep")
## TRUE HF - FIT BF
HFBFf <-  HFBF %>% 
  dplyr::select(chisq, df, rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Bi-factor")%>% 
  rownames_to_column("Rep")
## TRUE HF - FIT NW
HFNWf <-  HFNW %>% 
  dplyr::select(chisq, df, rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Network")%>% 
  rownames_to_column("Rep")

## Comparison Hf Vs BF when the true model is HF
Vs_all_true_HF <- bind_rows(HFHFf, HFBFf, HFNWf) %>% 
  group_by(Rep) %>% 
  summarize(RMSEA = FitModel[which.min(rmsea)],
            AIC   = FitModel[which.min(aic.ll)],
            BIC   = FitModel[which.min(bic)],
            TLI   = FitModel[which.max(tli)],
            CFI   = FitModel[which.max(cfi)],
            NFI   = FitModel[which.max(nfi)]
            )

count_all_true_HF <- Vs_all_true_HF %>% 
  gather(key = "Chosen_Index", value = "Model", - Rep) %>% 
  group_by(Chosen_Index) %>% 
  filter(Model == "Higher-Order factor") %>% 
  count(Model) %>% 
  mutate(per = n /10) 


pie1 <- count_all_true_HF %>% 
  ggplot(aes(x=per, y=Chosen_Index, fill= Chosen_Index))+
  geom_bar(width = 1, stat = "identity", color="white", show.legend = FALSE) +
  coord_polar("x", start=0)+
  geom_text(aes(label= paste0(Chosen_Index," ", per,  "%")), 
            color = "black", size=3 ) +
  theme_void() +
  scale_fill_brewer(palette = "Greens") +
  labs(
    title = "",
    subtitle = "A. When the true model is \n Higher-Order Factor Model",
    caption = "") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "darkgreen", size = 11, face = "bold")
  )
pie1

## TRUE MODEL: BF

## TRUE BF - FIT HF
BFHFf <-  BFHF %>% 
  dplyr::select(chisq, df, rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Higher-Order factor") %>% 
  rownames_to_column("Rep")
## TRUE BF - FIT BF
BFBFf <-  BFBF %>% 
  dplyr::select(chisq, df, rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Bi-factor")%>% 
  rownames_to_column("Rep")
## TRUE BF - FIT NW
BFNWf <- BFNW %>% 
  dplyr::select(chisq, df, rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Network")%>% 
  rownames_to_column("Rep")

## Comparison Hf Vs BF when the true model is BF
Vs_all_true_BF <- bind_rows(BFHFf, BFBFf, BFNWf) %>% 
  group_by(Rep) %>% 
  summarize(RMSEA = FitModel[which.min(rmsea)],
            AIC   = FitModel[which.min(aic.ll)],
            BIC   = FitModel[which.min(bic)],
            TLI   = FitModel[which.max(tli)],
            CFI   = FitModel[which.max(cfi)],
            NFI   = FitModel[which.max(nfi)]
  )

count_all_true_BF <- Vs_all_true_BF %>% 
  gather(key = "Chosen_Index", value = "Model", - Rep) %>% 
  group_by(Chosen_Index) %>% 
  filter(Model == "Bi-factor") %>% 
  count(Model) %>% 
  mutate(per = n /10) 


pie2 <- count_all_true_BF %>% 
  ggplot(aes(x=per, y=Chosen_Index, fill= Chosen_Index))+
  geom_bar(width = 1, stat = "identity", color="white", show.legend = FALSE) +
  coord_polar("x", start=0) +
  geom_text(aes(label= paste0(Chosen_Index," ", per,  "%")), 
            color = "black", size=3) +
  theme_void() +
  scale_fill_brewer(palette = "Oranges") +
  labs(
    title = "",
    subtitle = "B. When the true model is \n Bi-factor Model",
    caption = "") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "darkorange2", size = 11, face = "bold")
  )
pie2

## TRUE MODEL: NW

## TRUE NW - FIT HF
NWHFf <-  NWHF %>% 
  dplyr::select(chisq, rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Higher-Order factor") %>% 
  rownames_to_column("Rep")
## TRUE NW - FIT BF
NWBFf <-  NWBF %>% 
  dplyr::select(chisq,rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Bi-factor")%>% 
  rownames_to_column("Rep")
## TRUE NW - FIT NW
NWNWf <-  NWNW %>% 
  dplyr::select(chisq,rmsea, aic.ll, bic, cfi, tli, nfi) %>% 
  add_column(FitModel = "Network")%>% 
  rownames_to_column("Rep")

## Comparison Hf Vs BF when the true model is NW
Vs_all_true_NW <- bind_rows(NWHFf, NWBFf, NWNWf) %>% 
  group_by(Rep) %>% 
  summarize(RMSEA = FitModel[which.min(rmsea)],
            AIC   = FitModel[which.min(aic.ll)],
            BIC   = FitModel[which.min(bic)],
            TLI   = FitModel[which.max(tli)],
            CFI   = FitModel[which.max(cfi)],
            NFI   = FitModel[which.max(nfi)]
  )

count_all_true_NW <- Vs_all_true_NW %>% 
  gather(key = "Chosen_Index", value = "Model", - Rep) %>% 
  group_by(Chosen_Index) %>% 
  filter(Model == "Network") %>% 
  count(Model) %>% 
  mutate(per = n /10) 


pie3 <- count_all_true_NW %>% 
  ggplot(aes(x=per, y=Chosen_Index, fill= Chosen_Index))+
  geom_bar(width = 1, stat = "identity", color="white", show.legend = FALSE) +
  coord_polar("x", start=0)+
  geom_text(aes(label= paste0(Chosen_Index," ", per,  "%")), 
            color = "black", size=3, x = 100) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "",
    subtitle = "C. When the true model is \n Network Model",
    caption = "") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "blue4", size = 11, face = "bold")
  )
pie3

## Final Plot with all 3 pies
library(patchwork)
finalpie <- pie1 + pie2 + pie3 + 
  plot_annotation(title = "Do Fit Indices pick the right model?")
finalpie  

## WAFFLE CHARTS
test <- Vs_all_true_HF %>% 
  gather(key = "Chosen_Index", value = "Model", - Rep) %>% 
  group_by(Chosen_Index) %>% 
  count(Model) %>% 
  mutate(per = n /10) %>% 
  ungroup()

library(waffle)
ggw <- test %>% 
  ggplot(aes(fill = Model, values = n)) +
  geom_waffle(color = "white", size = 0.15, n_rows = 10, flip = TRUE) +
  facet_wrap(~Chosen_Index, nrow = 1, strip.position = "bottom") +
  scale_x_discrete(expand=c(0,0)) +
  scale_fill_brewer(palette = "Set2") +
  coord_equal() +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = ""
  ) +
  theme_minimal() 
ggw

NWHF3 <- NWHFf %>% 
  add_column(TrueModel = "Network")
NWBF3 <- NWHFf %>% 
  add_column(TrueModel = "Network")




