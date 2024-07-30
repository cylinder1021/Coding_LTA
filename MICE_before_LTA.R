rm(list=ls())
load("final_data.dat")
library(dplyr)
glimpse(data)

#########MICE#############

## remove ID, BioSex_C, and Age
vars_keep <-  c( "Warmth_M", "Warmth_F",
"AcaExpec_M", "AcaExpec_F", "SatisRela_M", "SatisRela_F",
"SatisComm_M", "SatisComm_F", "Comm_M", "Comm_F",
"Timetogether_M", 
"HurtOthers_w1", "FreqGroup_w1", "H1FS3", "H1FS6", "H1FS16",
"FreqGroup_w2", "HurtOthers_w2", "H2FS3", "H2FS6", "H2FS16",
"FreqGroup_w3", "HurtOthers_w3", "H3SP6", "H3SP9", "H3SP12")
data <- data[, vars_keep]
glimpse(data)
# make all variables as factor
data <- data.frame(lapply(data, as.factor))
glimpse(data)
library(mice)
imp = mice(data, m = 5, method = 'polr', seed=2024)
lca <- complete(imp)

sum(is.na(lca))

#########Execute LCA Separately###########
