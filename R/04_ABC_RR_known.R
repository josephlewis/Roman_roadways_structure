set.seed(NULL)
set.seed(1)

ncores <- 85
nsims <- 250000

neigh <- 8

r <- terra::rast("./Data/DEM/OS_50m_Wales.tif")
roads <- sf::st_read("./Data/RR_known.gpkg")
sf::st_geometry(roads) <- "geometry"

b_mean <- truncnorm::rtruncnorm(n = nsims, mean = 1, sd = 10, a = 0)
b_sd <- rexp(n = nsims, rate = 1)
b <- truncnorm::rtruncnorm(n = nsims, mean = b_mean, sd = b_sd, a = 0)

df_vals <- cbind(b_mean, b_sd, b)

for(road_indx in 1:nrow(roads)) {
  
  print(road_indx)

  input_data <- slope_calc(r = r, route = roads[road_indx,], neighbours = neigh)
  
  message("calculating routes...")

  lcps_abc <- abc(input_data = input_data, df_vals = df_vals, route = roads[road_indx,], cf = exp_abc_cf,  spatial = FALSE, ncores = ncores,cost = TRUE)
  lcps_abc$road_indx <- road_indx
  saveRDS(lcps_abc, paste0("./Output/simulated_roads/rw_road_8neigh_", road_indx, ".rds"))

}