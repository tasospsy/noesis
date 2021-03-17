## Internship project
## Tasos Psychogyiopoulos
## SIMULATIONS
## c.15/3/2021

# ---- required sources
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/BACKGROUND_FUNCTIONS.R"))
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))
# Rawdata ??

# Let's specify the arguments to combine later with mapply function
args <-  list(fitHF = c(HFmodel = T, BFmodel = F, NWmodel = F),
              fitBF = c(HFmodel = F, BFmodel = T, NWmodel = F),
              fitNW = c(HFmodel = F, BFmodel = F, NWmodel = T))

# Perform the simulations
startt <- Sys.time()
out <- lapply(Rawdata, lapply, function(d) mapply(ModelsFitFun,
                                                  args$fitHF,
                                                  args$fitBF,
                                                  args$fitNW,
                                                  MoreArgs = list(Raw = T,
                                                                  dat = d),
                                                  SIMPLIFY = FALSE))
endt <- Sys.time()
endt-startt

#  save(out, file = "out.Rda")


# NOTE:
# Time difference of 53.93103 mins
# We can use 'mcmapply' and 'mclapply' instead of 'mapply' and 'lapply'
# for parallel computing using multiple cores at once. 

