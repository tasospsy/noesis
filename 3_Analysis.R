## Internship project
## Tasos Psychogyiopoulos
## DRAFT - ANALYSIS
## c.17/02/2021 / m.18/3/2021

# ---- required sources
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))

# load("out.Rda") # The simulations output. Very big file; I cannot store it online(yet)
## 
hoi <- Decomp2(out)

## Set Graphics parameter 3x3
par(mfrow=c(3,3))

# hist fun
histfun <- function(index){
  for (i in 1:3) {
    for(j in 1:3){
      hist(hoi[[i]][[j]][[index]], xlab = index, main = paste(names(hoi[i]),names(hoi[[i]][j])))
    }
  }
}
indices <- c('chisq', 'pvalue', 'rmsea', 'tli', 'cfi', 'bic','aic.ll')
sapply(indices, histfun) %>% invisible

# To Be Continued...




 