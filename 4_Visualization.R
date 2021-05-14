
## TRUE BF - FIT BF
BFBF <-  hoi$trueBF$fitBF %>% as_tibble() %>% 
  dplyr::select(chisq, rmsea, cfi, pvalue, tli, nfi) %>% 
  rename(χ2 = chisq, RMSEA = rmsea, CFI = cfi, p_value = pvalue,
         TLI = tli, NFI = nfi)

SumBFBF <- BFBF %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(medianBFBF = median(value), meanBFBF = mean(value)) %>% 
  mutate(lab = paste("median = ", round(medianBFBF,4), "\nmean =", round(meanBFBF,4))
         )

BFBF_hists <- BFBF %>% gather() %>% 
  ggplot(aes(x= value)) + 
  geom_histogram(bins = 15, show.legend = FALSE, , fill = "orange", alpha = 0.9) + 
  facet_wrap(~key, scales = 'free', ncol = 3)+
  geom_text(data = SumBFBF, aes(label = lab), 
            x=Inf, y=Inf, hjust=1, vjust=1, size=3) +
  ggtitle("True model Bi-factor Model - Fitting Bi-factor Model")+
  xlab("Fit Indices")+
  ylab("") +
  theme_minimal()
BFBF_hists


## TRUE NW - FIT BF
NWBF <-  hoi$trueNW$fitBF %>% as_tibble() %>% 
  dplyr::select(chisq, rmsea, cfi, pvalue, tli, nfi) %>% 
  rename(χ2 = chisq, RMSEA = rmsea, CFI = cfi, p_value = pvalue,
         TLI = tli, NFI = nfi)
SumNWBF <- NWBF %>% 
  dplyr::select(RMSEA, CFI, TLI, NFI) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(medianNWBF = median(value), meanNWBF = mean(value)) %>% 
  mutate(lab = paste("median = ", round(medianNWBF,4), 
                     "\nmean =", round(meanNWBF,4))
         )
NWBF_hists <- NWBF %>% gather() %>% 
  ggplot(aes(x= value)) + 
  geom_histogram(bins = 15, show.legend = FALSE, fill = "blue", alpha = 0.8) + 
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

comp <- bind_rows(BFBFt, NWBFt) %>% 
  dplyr::select(TrueModel, RMSEA, TLI, CFI, NFI) %>% 
  group_by(TrueModel) %>% 
  gather(key= "Index", value = "value", -TrueModel)

ggcomp <- ggplot(comp) + 
  geom_histogram(aes(x = value, color = TrueModel, fill = TrueModel),bins = 50, alpha = .2) +
  facet_wrap(~Index, scales = 'free')+
  ggtitle("Fitting Bi-factor Models")+
  xlab("Fit Indices Comparison") +
  ylab("") +
  theme_minimal()
ggcomp



