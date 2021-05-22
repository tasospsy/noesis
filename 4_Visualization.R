## Internship project
## Tasos Psychogyiopoulos
## 4. VISUALIZATION (draft)
## c. 10/5/2021 / m. 22/5/2021

source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/3_Analysis.R"))
library(tidyverse)
library(patchwork)

## setwd("/Users/tasospsy/Google Drive/_UvA/Research Internship/Noesis/")
## load('out.rda')

## TIDY UP' THE SIMULATION RESULTS

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

# merge all datasets
alldat <-  bind_rows(data1, data2, data3) %>% 
  rename(Chi.squared = chisq, RMSEA = rmsea, CFI = cfi, p.value = pvalue,
         TLI = tli, NFI = nfi, AIC = aic.ll, BIC = bic)

# 1st plot: FITTING BI-FACTOR MODELS

## Version. 1: All fit measures together
## Distribution Comparison Plot

ggcomp <- alldat %>% filter(FitModel == "Bi-factor") %>%  
    dplyr::select(TrueModel, RMSEA, TLI, CFI, NFI, p.value, Chi.squared) %>% 
    gather(key = "Index", value = "value", -TrueModel) %>% 
    ggplot() + 
    geom_histogram(aes(x = value, color = TrueModel, fill = TrueModel),
                   bins = 50, alpha = .3) +
    facet_wrap(~Index, scales = 'free') +
      ggtitle(label = "Fitting Bi-factor Models",
            subtitle = "Fit Measures Comparison") +
    xlab("") +
    ylab("") +
    theme_minimal(base_size = 15) +
    theme(text = element_text(family = "mono", color = "grey15"),
          plot.title = element_text(face = "bold", size = 20),
          panel.grid.major.x = element_line(size = 0.4),
          panel.grid.major.y = element_line(size = 0.4),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.title = element_text(face = "bold"),
          strip.text = element_text(colour = "grey15", size = 12),
          legend.position = "bottom") +
    scale_fill_manual(values= c("coral","chartreuse3","turquoise3"),
                      aesthetics = c("color", "fill"))
ggcomp

## # Grey theme
## theme(text = element_text(family = "mono", color = "grey90"),
##       plot.background = element_rect(fill = "grey15", color = NA),
##       plot.title = element_text(face = "bold", size = 20),
##       panel.grid.major.x = element_line(size = 0.1),
##       panel.grid.major.y = element_line(size = 0.1),
##       panel.grid.minor.x = element_blank(),
##       panel.grid.minor.y = element_blank(),
##       legend.title = element_text(face = "bold"),
##       strip.text = element_text(colour = "grey80", size = 12),
##       legend.position = "bottom") +
##   scale_fill_manual(values= c("coral","aquamarine","lightyellow1"),
##                     aesthetics = c("color", "fill"))

## Version. 2: separately exact and approx. fit measures
ex.plot <- alldat %>% filter(FitModel == "Bi-factor") %>%  
  dplyr::select(TrueModel, p.value, Chi.squared) %>% 
  gather(key = "Index", value = "value", -TrueModel) %>% 
  ggplot() + 
  geom_histogram(aes(x = value, color = TrueModel, fill = TrueModel),
                 bins = 50, alpha = .4, show.legend = FALSE) +
  facet_wrap(~Index, scales = 'free', nrow = 1) +
  ggtitle(label = "Fitting Bi-factor Models",
          subtitle = "Exact Fit Measures") +
  xlab("") +
  ylab("") +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "mono", color = "grey15"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        panel.grid.major.x = element_line(size = 0.4),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(colour = "grey15", size = 11)) +
  scale_fill_manual(values= c("coral","chartreuse3","turquoise3"),
                    aesthetics = c("color", "fill"))
ex.plot

approx.plot <- alldat %>% filter(FitModel == "Bi-factor") %>%  
  dplyr::select(TrueModel, RMSEA, CFI, TLI, NFI) %>% 
  gather(key = "Index", value = "value", -TrueModel) %>% 
  ggplot() + 
  geom_histogram(aes(x = value, color = TrueModel, fill = TrueModel),
                 bins = 50, alpha = .4, show.legend = TRUE) +
  facet_wrap(~Index, scales = 'free', nrow = 2) +
  ggtitle(label = "",
          subtitle = "Approximate Fit Indices") +
  xlab("") +
  ylab("") +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "mono", color = "grey15"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        panel.grid.major.x = element_line(size = 0.4),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(colour = "grey15", size = 11),
        legend.position = "bottom") +
  scale_fill_manual(values= c("coral","chartreuse3","turquoise3"),
                    aesthetics = c("color", "fill"))
approx.plot

ggcomp.v2 <- ex.plot / approx.plot
ggcomp.v2

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

## -- VISUALIZATION CHECK: DO FIT INDICES PICK THE RIGHT MODEL?  

## TRUE MODEL: HF

all_true_HF <- alldat %>% 
  filter(TrueModel == "Higher-order factor") %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI, AIC, BIC, Rep, FitModel) %>% 
  group_by(Rep) %>% 
  summarize(RMSEA = FitModel[which.min(RMSEA)],
            AIC   = FitModel[which.min(AIC)],
            BIC   = FitModel[which.min(BIC)],
            TLI   = FitModel[which.max(TLI)],
            CFI   = FitModel[which.max(CFI)],
            NFI   = FitModel[which.max(NFI)]
            ) %>% 
  ungroup()

count_all_true_HF <- all_true_HF %>% 
  gather(key = "Chosen_Index", value = "Model", - Rep) %>% 
  group_by(Chosen_Index) %>% 
  filter(Model == "Higher-order factor") %>% 
  count(Model) %>% 
  mutate(per = n /10) 

pie1 <- count_all_true_HF %>% 
  ggplot(aes(x=per, y=Chosen_Index, fill= Chosen_Index)) +
  geom_bar(width = 0.8, stat = "identity", color="white", show.legend = FALSE, alpha = 1) +
  coord_polar("x", start=0) +
  geom_label(aes(label = paste0(Chosen_Index," ", per,  "%")), 
             color = "grey15", size=3.5, family = "mono", hjust = 0.5, 
             show.legend = FALSE, alpha = 0.8) +
  theme_void() +
  scale_fill_brewer(palette = "YlGn") +
  labs(title = "",
       subtitle = "A. When the true model is \n Higher-Order Factor Model",
       caption = "") +
  theme(text = element_text(family = "mono"),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgreen", size = 13)
  )
pie1

## TRUE MODEL: BF
all_true_BF <- alldat %>% 
  filter(TrueModel == "Bi-factor") %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI, AIC, BIC, Rep, FitModel) %>% 
  group_by(Rep) %>% 
  summarize(RMSEA = FitModel[which.min(RMSEA)],
            AIC   = FitModel[which.min(AIC)],
            BIC   = FitModel[which.min(BIC)],
            TLI   = FitModel[which.max(TLI)],
            CFI   = FitModel[which.max(CFI)],
            NFI   = FitModel[which.max(NFI)]
  ) %>% 
  ungroup()

count_all_true_BF <- all_true_BF %>% 
  gather(key = "Chosen_Index", value = "Model", - Rep) %>% 
  group_by(Chosen_Index) %>% 
  filter(Model == "Bi-factor") %>% 
  count(Model) %>% 
  mutate(per = n /10) 

pie2 <- count_all_true_BF %>% 
  ggplot(aes(x=per, y=Chosen_Index, fill= Chosen_Index)) +
  geom_bar(width = 0.8, stat = "identity", color="white", show.legend = FALSE, alpha = 1) +
  coord_polar("x", start=0) +
  geom_label(aes(label = paste0(Chosen_Index," ", per,  "%")), 
             color = "grey15", size=3.5, family = "mono", hjust = 0, 
             show.legend = FALSE, alpha = 0.8) +
  theme_void() +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "",
       subtitle = "B. When the true model is \n Bi-factor Model",
       caption = "") +
  theme(text = element_text(family = "mono"),
        plot.subtitle = element_text(hjust = 0.5, color = "darkorange2", size = 13)
  )
pie2


## TRUE MODEL: NW
## TRUE MODEL: BF
all_true_NW <- alldat %>% 
  filter(TrueModel == "Network") %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI, AIC, BIC, Rep, FitModel) %>% 
  group_by(Rep) %>% 
  summarize(RMSEA = FitModel[which.min(RMSEA)],
            AIC   = FitModel[which.min(AIC)],
            BIC   = FitModel[which.min(BIC)],
            TLI   = FitModel[which.max(TLI)],
            CFI   = FitModel[which.max(CFI)],
            NFI   = FitModel[which.max(NFI)]
  ) %>% 
  ungroup()

count_all_true_NW <- all_true_NW %>% 
  gather(key = "Chosen_Index", value = "Model", - Rep) %>% 
  group_by(Chosen_Index) %>% 
  filter(Model == "Network") %>% 
  count(Model) %>% 
  mutate(per = n /10) 

pie3 <- count_all_true_NW %>% 
  ggplot(aes(x=per, y=Chosen_Index, fill= Chosen_Index)) +
  geom_bar(width = 0.8, stat = "identity", color="white", 
           show.legend = FALSE, alpha = 1) +
  coord_polar("x", start=0) +
  geom_label(aes(label = paste0(Chosen_Index," ", per,  "%")), 
             color = "grey15", size=3.5, family = "mono", hjust = 0, 
             show.legend = FALSE, alpha = 0.8) +
  theme_void() +
  scale_fill_brewer(palette = "GnBu") +
  labs(title = "",
       subtitle = "C. When the true model is \n Network Model",
       caption = "") +
  theme(text = element_text(family = "mono"),
        plot.subtitle = element_text(hjust = 0.5, color = "turquoise3", size = 13)
  ) 
pie3

## Final Plot with all 3 pies

finalpie <- pie1 + pie2 + pie3
finalpie  


# --
## EXTRA PLOTS (DRAFT)

## TRUE BF - FIT BF
BFBF1 <-  BFBF %>% 
  dplyr::select(chisq, rmsea, cfi, pvalue, tli, nfi) %>% 
  rename(χ2 = chisq, RMSEA = rmsea, CFI = cfi, p_value = pvalue,
          TLI = tli, NFI = nfi)

SumBFBF <- BFBF1 %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(medianBFBF = median(value), meanBFBF = mean(value)) %>% 
  mutate(lab = paste("median = ", round(medianBFBF,4), "\nmean =", round(meanBFBF,4))
  )

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
SumNWBF <- NWBF1 %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(medianNWBF = median(value), meanNWBF = mean(value)) %>% 
  mutate(lab = paste("median = ", round(medianNWBF,4), 
                     "\nmean =", round(meanNWBF,4))
  )
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
