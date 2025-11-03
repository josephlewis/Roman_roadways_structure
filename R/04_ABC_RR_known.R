set.seed(NULL)
set.seed(1)

r <- terra::rast("./Data/DEM/OS_50m_Wales.tif")
roads <- sf::st_read("./Data/RR_known.gpkg")
sf::st_geometry(roads) <- "geometry"

# crop DEM to Roman road ID 17 for Figure
terra::writeRaster(terra::crop(r, sf::st_buffer(roads[roads$RR_ID == "RR642",][2,], 5000)), "./Data/DEM/OS_50m_Wales_RR17.tif", overwrite = TRUE)
sf::st_write(roads[roads$RR_ID == "RR642",][2,], "./Data/RR17_known.gpkg", append = TRUE)

terra::writeRaster(terra::crop(r, sf::st_buffer(roads[roads$RR_ID == "rr62c_3",], 5000)), "./Data/DEM/OS_50m_Wales_RR43.tif", overwrite = TRUE)
sf::st_write(roads[roads$RR_ID == "rr62c_3",], "./Data/RR43_known.gpkg", append = TRUE)

terra::writeRaster(terra::crop(r, sf::st_buffer(roads[roads$RR_ID == "RR64_2",], 5000)), "./Data/DEM/OS_50m_Wales_RR20.tif", overwrite = TRUE)
sf::st_write(roads[roads$RR_ID == "RR64_2",], "./Data/RR20_known.gpkg", append = TRUE)

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