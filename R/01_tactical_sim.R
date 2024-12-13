set.seed(NULL)
set.seed(1)

r <- terra::rast("./Data/DEM/OS_50m_Wales.tif")
roads <- sf::st_read("./Data/RR_known.gpkg")
sf::st_geometry(roads) <- "geometry"

b_mean <- 10
b_sd <- 5
b <- truncnorm::rtruncnorm(n = nrow(roads), mean = b_mean, sd = b_sd, a = 0)

df_vals <- cbind(b_mean, b_sd, b)

sim_routes <- calculate_tactical_sim_routes(roads = roads, values = df_vals)

sf::st_write(sim_routes, "./Output/tactical_simulation/simulated_routes/tactical_simulation_routes.gpkg", append = FALSE)
saveRDS(sim_routes, "./Output/tactical_simulation/simulated_routes/tactical_simulation_routes.rds")