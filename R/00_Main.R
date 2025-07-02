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
library(patchwork)
library(ggnewscale)
library(ggthemes)
library(geomtextpath)

library(brms)

options(scipen = 999)

source("./R/Functions.R")

# number of neighbours to use when calculating least-cost paths. This can be 4,8,16,32,48. But larger number increases both computational load and time
neigh <- 8
ncores <- 40
# number of simulations to run for each Roman road
nsims <- 250000
# number of accepted values in the posterior. This should be less than nsims, i.e. the total number of values to simulate from.
no_post_rows <- 250

#############################
#### TACTICAL SIMULATION ####
#############################

#### 01 - simulate routes with known parameters ####
#source("./R/01_tactical_sim.R")

### 02 - fit hierarchical movement model to simulated routes to infer known parameters ####
#source("./R/02_ABC_tactical_sim.R")

### 03 - produce figures to assess fit of hierarchical movement model to simulated routes ####
source("./R/03_analyse_ABC_tactical_sim.R")

### 04 - fit hierarchical movement model to Roman roads to infer parameters ####
#source("./R/04_ABC_RR_known.R")

### 05 - produce figures to assess fit of hierarchical movement model to simulated routes ####
source("./R/05_analyse_ABC_RR_known.R")

### 06 - calculate sinuosity and produce b-sinuosity plots ####
source("./R/06_b_sinuosity_calculation.R")

### 07 - estimate b from critical slope gradient ###
source("./R/07_b_critical_gradient_slope.R")
