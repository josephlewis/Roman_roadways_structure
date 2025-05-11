# Generative inference model reveals labour mobilisation towards route construction in past societies


This repository contains all the data and scripts required to fully reproduce all analyses presented in the paper "_Generative inference model reveals labour mobilisation towards route construction in past societies_" authored by **Lewis**, J and Crema, E.R.

Getting Started
---------------

1. Open project using Roman_roadways_structure.Rproj to ensure relative paths work.
2. Run the _00_Main.R_ R script in the R folder to reproduce the analysis
   * _01_tactical_sim.R_ generates 62 simulated routes with known parameter values
   * _02_ABC_tactical_sim.R_ fits the Bayesian hierarchical movement model to the 62 simulated routes
   * _03_analyse_ABC_tactical_sim.R_ produces summary tables and figures for the tactical simulation
   * _04_ABC_RR_known.R_ fits the Bayesian hierarchical movement model to the 62 known Roman roads in Wales
   * _05_analyse_ABC_RR_known.R_ produces summary tables and figures for the Roman roads in Wales study
   * _06_b_sinuosity_calculation.R_ calculate sinuosity of simulated Roman roads and relate this to the influence of slope gradient (parameter b). Produce figures for the unified framework
   * _07_b_critical_gradient_slope.R_ produces plot to show relationship between parameter b and critical slope gradient

**Data**
1. DEM
  * _OS_50m_Wales.tif_ - Ordnance Survey 50 metres Digital Elevation Model of Wales
2. extent
  * _roman_empire_ad_117.gpkg_ - Extent of the Roman empire.
2. _RR_known.gpkg_ - 62 Known Roman roads digitised from 'Roman frontiers in Wales and the Marches’

**Output**
1. tactical_simuation
  * _simulated_routes_ - simulated routes produced via _01_tactical_sim.R_
  * _simulated_routes_abc_ - fitted least-cost paths to the 62 simulated routes using the Bayesian hierarchical movement model
  * _simulated_routes_sf_ - 15,500 least-cost paths based on the estimated posterior b value for each simulated road (250 accepted parameter values * 62 = 15,500); 62 least-cost paths based on the estimated median posterior b value for each simulated road
2. _simulated_roads_ - fitted least-cost paths to the 62 known Roman roadways using the Bayesian hierarchical movement model
3. _tables_
  * simulated_roads
      * _road_sims_posterior_summary_1.csv_ - estimated b parameter for the 62 Roman roads
      * _road_sims_posterior_summary_2.csv_ - estimated b bar parameter for the population-level of the 62 Roman roads 
        * _road_sims_posterior_summary_3.csv_ - Path Deviation Index for the 62 Roman roads
  * tactical_simulation
      * _road_sims_posterior_summary_1.csv_ - estimated b parameter for the 62 simulated routes (tactical simulation)
      * _sim_routes_abc_posterior_summary2.csv_ - estimated b bar parameter for population-level for the 62 simulated routes (tactical simulation)
  * _b_from_critical_slope_ - estimated b value for theoretical limits when using wheeled vehicles
4. roadway_system
  *  _road_sims_posterior.gpkg_ - 15,500 least-cost paths based on the estimated posterior b value for each Roman roadway in Wales (250 accepted parameter values * 62 = 15,500)
  *  _road_sims_posterior_median.gpkg_ - 62 least-cost paths based on the estimated median posterior b value for each Roman roadway in Wales
5. figures

**Analysis**

Please note that running the Bayesian hierarchical movement model via _02_ABC_tactical_sim.R_ or _04_ABC_RR_known.R_ has a long processing time and high memory requirements. The calculation of 250,000 least-cost paths for each of the 62 simulated routes / known Roman roadways (15,500,000 least-cost paths in total) takes approximately 4 days to complete when using 85 cores and ~50GB memory.

For testing purposes, the processing time and memory requirements can be reduced by changing the parameters in _00_Main.R_

* _nsims_ - total number of simulated per Roman road
* _no_post_rows_ - Total number of accepted parameter to create the posterior distribution

Note that _nsims_ value should be greater than the _no_post_rows_ value

License
---------------
CC-BY 3.0 unless otherwise stated (see Licenses.md)
