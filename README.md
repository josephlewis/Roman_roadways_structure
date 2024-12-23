# Process-explicit model reveals variability in the influence of slope gradient on roads in the Roman empire

This repository contains all the data and scripts required to fully reproduce all analyses presented in the paper "_Process-explicit model reveals variability in the influence of slope gradient on roads in the Roman empire_" authored by **Lewis**, J.

Getting Started
---------------

1. Open project using Roman_roadways_structure.Rproj to ensure relative paths work.
2. Run the _00_Main.R_ R script in the R folder to reproduce the analysis.
   * _01_tactical_sim.R_ generates 62 simulated routes with known parameter values
   * _02_ABC_tactical_sim.R_ fits the Bayesian hierarchical movement model to the 62 simulated routes
   * _03_analyse_ABC_tactical_sim.R_ produces summary tables and figures for the tactical simulation
   * _04_ABC_RR_known.R_ fits the Bayesian hierarchical movement model to the 62 known Roman roadways in Wales
   * _05_analyse_ABC_RR_known.R_ produces summary tables and figures for the Roman roadways analysis
   * _06_critical_slope_gradient_to_b_ infers parameter b values from empirical slope gradients of Roman roads across the Roman empire
   * _07_b_critical_gradient_slope.R_ produces plot to show relationship between parameter b and critical slope gradient (Extended Data Figure 1)

**Data**
1. DEM
  * _OS_50m_Wales.tif_ - Ordnance Survey 50 metre Digital Elevation Model of Wales
2. extent
  * _roman_empire_ad_117.gpkg_ - Extent of the Roman empire.
2. _RR_known.gpkg_ - 62 Known Roman roadways digitised from 'Roman frontiers in Wales and the Marches’

**Output**
1. tactical_simuation
  * _simulated_routes_ - simulated routes produced via _01_tactical_sim.R_
  * _simulated_routes_abc_ - fitted least-cost paths to the 62 simulated routes using the Bayesian hierarchical movement model
2. _simulated_roads_ - fitted least-cost paths to the 62 known Roman roadways using the Bayesian hierarchical movement model
3. _tables_
  * tactical_simulation
      * _sim_routes_abc_posterior_summary1.csv_ - estimated b parameter for the 62 simulated routes
      * _sim_routes_abc_posterior_summary2.csv_ - estimated b bar parameter for population-level simulated routes
  * simulated_roads
      * _road_sims_posterior_summary_1.csv_ - estimated b parameter for 62 known Roman roadways
      * _sim_routes_abc_posterior_summary2.csv_ - estimated b bar parameter for population-level Roman roadways across the Roman empire
      * _road_sims_posterior_summary_3.csv_ - Path Deviation Index for 62 known Roman roadways
4. roadway_system
  *  _road_sims_posterior.gpkg_ - 15,500 least-cost paths based on the estimated posterior b value for each Roman roadway in Wales (250 accepted parameter values * 62 = 15,500)
  *  _road_sims_posterior_median.gpkg_ - 62 least-cost paths based on the estimated median posterior b value for each Roman roadway in Wales
5. figures

**Analysis**

Please note that running the Bayesian hierarchical movement model via _02_ABC_tactical_sim.R_ or _04_ABC_RR_known.R_ has a long processing time and high memory requirements. The calculation of 250,000 least-cost paths for each of the 62 simulated routes / known Roman roadways (15,500,000 least-cost paths in total) takes approximtely 4 days to complete when using 85 cores and ~50GB memory. 

Fitted Bayesian hierarchical movement model to the 62 known Roman roads and simulated routes are available in the _Output_ folder.
1. tactical_simuation
  * _simulated_routes_abc_ - fitted least-cost paths to the 62 simulated routes using the Bayesian hierarchical movement model
2. _simulated_roads_ - fitted least-cost paths to the 62 known Roman roadways using the Bayesian hierarchical movement model
    
License
---------------
CC-BY 3.0 unless otherwise stated (see Licenses.md)
