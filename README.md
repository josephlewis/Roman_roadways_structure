# Process-Explicit Model Reveals Structure of Roman Roads in the Roman Empire

This repository contains all the data and scripts required to fully reproduce all analyses presented in the paper "_Process-Explicit Model Reveals Structure of Roman Roads in the Roman Empire_" authored by Lewis, J.

Getting Started
---------------

1. Open project using Roman_roadways_structure.Rproj to ensure relative paths work.
2. Run the 00_Main R script in the R folder to reproduce the analysis.
   * _01_tactical_sim.R_ generates 62 simulated routes with known parameter values
   * _02_ABC_tactical_sim.R_ fits the Bayesian hierarchical movement model to the 62 simulated routes
   * _03_analyse_ABC_tactical_sim.R_ produces summary tables and figures for the tactical simulation
   * _04_ABC_RR_known.R_ fits the Bayesian hierarchical movement model to the 62 known Roman roadways in Wales
   * _05_analyse_ABC_RR_known.R_ produces summary tables and figures for the Roman roadways analysis
   * _06_roadways_critical_slopes.R_ produces mathematical slope gradient figures for the Roman roadways analysis
   * _07_roadways_empire_slopes.R_ produces summary tables and figures for Roman roadways across the Roman empire

License
---------------
CC-BY 3.0 unless otherwise stated (see Licenses.md)
