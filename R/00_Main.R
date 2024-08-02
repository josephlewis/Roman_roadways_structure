library(sf)
library(terra)
library(leastcostpath)

library(dplyr)
library(gtools)
library(tidyr)

library(foreach)
library(doParallel)

library(ggplot2)
library(ggdist)
library(ggnewscale)
library(ggthemes)
library(tmap)

source("./R/Functions.R")

#### TACTICAL SIMULATION ####

### 01 - simulate routes with known parameters
#source("./R/01_tactical_sim.R")
### 02 - fit hierarchical movement model to simulated routes infer known parameters
#source("./R/02_ABC_tactical_sim.R")
### 03 - produce figures to assess fit of hierarchical movement model to simulated routes
source("./R/03_analyse_ABC_tactical_sim.R")

### 04 - fit hierarchical movement model to Roman roads to infer parameters
#source("./R/04_ABC_RR_known.R")

source("./R/05_analyse_ABC_RR_known.R")
source("./R/06_roadways_critical_slopes.R")

source("./R/07_roadways_empire_slopes.R")