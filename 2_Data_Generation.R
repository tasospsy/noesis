## Internship project
## Tasos Psychogyiopoulos
## DATA GENERATION PROCEDURE
## c.15/3/2021

# ---- required sources
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/BACKGROUND_FUNCTIONS.R"))
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))

## Data generation procedure
set.seed(1992) # Set a seed
Rawdata <- replicate(n = 1000,                            # 1000 replications
                     lapply(lapply(modslist, GetCorMat),  # requir. function 
                            function(s){
                              mvrnorm(n = WAIS$size$GER,  # N = 1425 
                                      mu = rep(0, nobs),  # nobs = 15
                                      Sigma = s,
                                      empirical = FALSE)
                            }
                     ),
                     simplify = FALSE)

# save(Rawdata, file = "Rawdata.Rda")