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

options(scipen = 999)

source("./R/Functions.R")

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

### 06 - infer parameter b from empirical slope gradients of Roman roads across the Roman empire ####
source("./R/06_critical_slope_gradient_to_b.R")

### 07 - plot relationship between parameter b and critical slope gradient (Extended Data Figure 1) ####
source("./R/07_b_critical_gradient_slope.R")