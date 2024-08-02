set.seed(NULL)
set.seed(1)

ncores <- 85
nsims <- 250000

neigh <- 8

r <- terra::rast("./Data/DEM/OS_50m_Wales.tif")
tactical_sim_routes <- readRDS("./Output/tactical_simulation/simulated_routes/tactical_simulation_routes.rds")

b_mean <- truncnorm::rtruncnorm(n = nsims, mean = 1, sd = 10, a = 0)
b_sd <- rexp(n = nsims, rate = 1)
b <- truncnorm::rtruncnorm(n = nsims, mean = b_mean, sd = b_sd, a = 0)

df_vals <- cbind(b_mean, b_sd, b)

for(road_indx in 1:nrow(tactical_sim_routes)) {
  
  print(road_indx)
  
  input_data <- slope_calc(r = r, route = tactical_sim_routes[road_indx,], neighbours = neigh)

  message("calculating routes...")
  
  ts_routes_abc <- abc(input_data = input_data, df_vals = df_vals, route = tactical_sim_routes[road_indx,], cf = exp_abc_cf, spatial = FALSE, ncores = ncores, cost = FALSE)
  ts_routes_abc$road_indx <- road_indx
  
  saveRDS(ts_routes_abc, paste0("./Output/tactical_simulation/simulated_routes_abc/tactical_sim_routes_abc_", road_indx, ".rds"))
  
}
