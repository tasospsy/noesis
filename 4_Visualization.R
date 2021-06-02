## Internship project
## Tasos Psychogyiopoulos
## 4. VISUALIZATION (draft)
## c. 10/5/2021 / m.01/06/2021

source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/3_Analysis.R"))
library(patchwork)

# 1st plot: FITTING BI-FACTOR MODELS

## Version. 2: separately exact and approx. fit measures
ex.plot <- alldat %>% filter(FitModel == "Bi-factor") %>%  
  dplyr::select(TrueModel, p.value, Chi.squared) %>% 
  gather(key = "Index", value = "value", -TrueModel) %>% 
  ggplot() + 
  geom_histogram(aes(x = value, color = TrueModel, fill = TrueModel),
                 bins = 40, alpha = .2, show.legend = TRUE, position = "identity") +
  facet_wrap(~Index, scales = 'free', nrow = 2) +
  ggtitle(label = "Fitting Bi-factor Models",
          subtitle = "Exact Fit Measures") +
  xlab("") +
  ylab("") +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "mono", color = "grey15"),
        plot.title = element_text(hjust = 0, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        panel.grid.major.x = element_line(size = 0.4),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(colour = "grey15", size = 11),
        legend.position = "bottom") +
  scale_fill_manual(values= c("coral","chartreuse3","turquoise3"),
                    aesthetics = c("color", "fill"))
ex.plot


approx.plot <- alldat %>% 
  filter(FitModel == "Bi-factor") %>%
  
  dplyr::select(TrueModel, RMSEA, CFI, TLI, NFI) %>% 
  gather(key = "Index", value = "value", -TrueModel) %>% 
  ggplot() + 
  geom_histogram(data = . %>% filter(!TrueModel == "Empirical"),aes(x = value, color = TrueModel, fill = TrueModel),
                 bins = 40, alpha = .1, show.legend = FALSE, position = "identity") +
  geom_point(data = . %>% filter(TrueModel == "Empirical"),
             aes(x = value, y = 1, color = TrueModel, fill = TrueModel), 
             alpha = 1, show.legend = TRUE) +
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
  scale_fill_manual(values= c("coral","gold","chartreuse3","turquoise3"),
                    aesthetics = c("color", "fill"),
                    breaks = c("Empirical"),
                    guide = guide_legend(title = ""))

approx.plot 


ggcomp.v2 <- ex.plot + approx.plot
ggcomp.v2

## -- VISUALIZATION CHECK: DO FIT INDICES PICK THE RIGHT MODEL?  

## TRUE MODEL: HF

## all_true_HF 

pie1 <- all_true_HF %>% 
  filter(Model == "Higher-order factor") %>% 
  ggplot(aes(x=percent, y=Chosen_Index, fill= Chosen_Index)) +
  geom_bar(width = 0.8, stat = "identity", color="white", show.legend = FALSE, alpha = 1) +
  coord_polar("x", start=0) +
  geom_label(aes(label = paste0(Chosen_Index," ", percent,  "%")), 
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
## all_true_BF 

pie2 <- all_true_BF %>% 
  filter(Fitting_Model == "Bi-factor") %>% 
  ggplot(aes(x=percent, y=Chosen_Index, fill= Chosen_Index)) +
  geom_bar(width = 0.8, stat = "identity", color="white", show.legend = FALSE, alpha = 1) +
  coord_polar("x", start=0) +
  geom_label(aes(label = paste0(Chosen_Index," ", percent,  "%")), 
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

## all_true_NW 

pie3 <- all_true_NW %>% 
  filter(Fitting_Model == "Network") %>% 
  ggplot(aes(x=percent, y=Chosen_Index, fill= Chosen_Index)) +
  geom_bar(width = 0.8, stat = "identity", color="white", 
           show.legend = FALSE, alpha = 1) +
  coord_polar("x", start=0) +
  geom_label(aes(label = paste0(Chosen_Index," ", percent,  "%")), 
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
