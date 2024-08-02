calculate_tactical_sim_routes <- function(roads, values) { 
  
  tactical_sim_routes <- list()
  
  for(road_indx in 1:nrow(roads)) {
    
    print(road_indx)
    print(paste0("b value = ", values[road_indx,3]))
    
    road2_od <- origin_destination(route = roads[road_indx,])
    
    route_pts <- origin_destination(road2_od)
    route_pts_cells <- terra::cellFromXY(r, sf::st_coordinates(route_pts))
    route_pts_XY <- terra::xyFromCell(r, route_pts_cells)
    route_pts_centre <- route_pts_XY %>% 
      as.data.frame %>% 
      sf::st_as_sf(coords = c(1,2))
    
    route_pts_centre_bbox <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(route_pts_centre)))
    
    r2 <- terra::crop(r, sf::st_buffer(route_pts_centre_bbox, dist = max(terra::res(r))*50))
    
    cf = function(x) {(1 * exp(-values[road_indx,3] * abs(x + 0.05))) / 3.6}
    
    slope_cs <- leastcostpath::create_slope_cs(x = r2, cost_function = cf, neighbours = 8)
    
    tactical_sim_routes[[road_indx]] <- leastcostpath::create_lcp(x = slope_cs, origin = road2_od[1,], destination = road2_od[2,])
    tactical_sim_routes[[road_indx]]$road_id <- road_indx
    tactical_sim_routes[[road_indx]]$b <- values[road_indx,3]
    
  }
  
  tactical_sim_routes <- do.call(rbind, tactical_sim_routes)
  
  return(tactical_sim_routes)
  
}

slope_calc <- function(r, route, max_slope = NULL, neighbours = 8) {

  route_pts <- origin_destination(route)
  route_pts_cells <- terra::cellFromXY(r, sf::st_coordinates(route_pts))
  route_pts_XY <- terra::xyFromCell(r, route_pts_cells)
  route_pts_centre <- route_pts_XY %>% 
    as.data.frame %>% 
    sf::st_as_sf(coords = c(1,2))
  
  route_pts_centre_bbox <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(route_pts_centre)))
  
  r2 <- terra::crop(r, sf::st_buffer(route_pts_centre_bbox, dist = max(terra::res(r))*50))
  
  neighbours <- leastcostpath::neighbourhood(neighbours = neighbours)
  
  cells <- which(!is.na(terra::values(r2)))
  na_cells <- which(is.na(terra::values(r2)))
  
  adj <- terra::adjacent(x = r2, cells = cells, directions = neighbours, pairs = TRUE)
  adj <- adj[!adj[,2] %in% na_cells,]
  
  elev_values <- terra::values(r2)[,1]
  
  message("calculating slope...")
  
  rise <- (elev_values[adj[,2]] - elev_values[adj[,1]])
  run <- leastcostpath::calculate_distance(x = r2, adj = adj)
  
  mathematical_slope <- rise/run
  
  ncells <- length(cells) + length(na_cells)
  
  cs_matrix <- Matrix::Matrix(data = 0, nrow = ncells, ncol = ncells, sparse = TRUE)
  cs_matrix[adj] <- mathematical_slope
  
  cs <- list("conductanceMatrix" = cs_matrix, 
             "costFunction" = NA,
             "maxSlope" = ifelse(!is.null(max_slope), paste0(max_slope*100, "%"), NA), 
             "exaggeration" = NA,
             "criticalSlope" = NA,
             "neighbours" = sum(neighbours, na.rm = TRUE),
             "resolution" = terra::res(r2), 
             "nrow" = terra::nrow(r2), 
             "ncol" = terra::ncol(r2), 
             "extent" = as.vector(terra::ext(r2)), 
             "crs" = terra::crs(r2, proj = TRUE))
  
  class(cs) <- "conductanceMatrix"
  
  route_list <- list(cs, run, adj)
  
  return(route_list)
}

abc <- function(input_data, df_vals, route, cf, ncores, spatial = TRUE, ...) {
  
  od <- origin_destination(route)
  
  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)
  
  lcps <- foreach::foreach(i = 1:nrow(df_vals), .packages= c("sf", "leastcostpath", "Matrix"), .combine = "rbind") %dopar% {
    
    input_data2 <- input_data
    
    y <- df_vals[i,, drop = FALSE]
    colnames(y) <- paste0("p.", colnames(y))
    
    input_data2[[1]]$conductanceMatrix[input_data2[[3]]] <- cf(x =  input_data2[[1]]$conductanceMatrix[input_data2[[3]]], y = y)
    
    input_data2[[1]]$conductanceMatrix[input_data2[[3]]] <-   input_data2[[1]]$conductanceMatrix[input_data2[[3]]] / input_data2[[2]]
    
    lcp <- leastcostpath::create_lcp(x = input_data2[[1]], origin = od[1,], destination = od[2,], ...)
    sf::st_crs(lcp) <- sf::st_crs(route)
    
    pdis <- leastcostpath::PDI_validation(lcp = lcp, comparison = route)
    normalised_pdi <- pdis$normalised_pdi
    pdi <- pdis$pdi
    max_dist <- max(as.numeric(sf::st_distance(sf::st_cast(lcp, "POINT"), y = route)))
    
    euc_dist_od <- as.numeric(sf::st_distance(od[1,], y = od[2,]))
    
    lcp_sinuosity <- sf::st_length(lcp) / euc_dist_od
    route_sinuosity <- sf::st_length(route) / euc_dist_od
    
    sinuosity_diff <- as.numeric(abs(lcp_sinuosity-route_sinuosity))
    
    lcp_index <- i
    lcp <- cbind(lcp, lcp_index, y, normalised_pdi, pdi, max_dist, sinuosity_diff)
    
    if(spatial != TRUE) {
      lcp <- sf::st_drop_geometry(lcp)
    }
    
    return(lcp)
  }
  
  parallel::stopCluster(myCluster)
  
  return(lcps)
}

exp_cf <- function(x, y) {(1 * exp(-y * abs(x + 0.05))) / 3.6}

exp_abc_cf <- function(x, y) {(1 * exp(-y[3] * abs(x + 0.05))) / 3.6}

tobler_cf <- function(x) {(1 * exp(-3.5 * abs(x + 0.05))) / 3.6}

range01 <- function(x, min = NULL, max = NULL){
  
  if(is.null(min)) { 
    min <- min(x)
  }
  
  if(is.null(max)) { 
    max <- max(x)
  }
  
  (x-min(x))/(max(x)-min(x))
}

origin_destination <- function(route) {
  
  route_pts <- sf::st_cast(route, "POINT")
  
  origin <- route_pts[1,]
  destination <- route_pts[nrow(route_pts),]
  
  od <- rbind(origin, destination)
  
  return(od)
  
}

calculate_posterior_cf <- function(road_sims_post) {
  
  cf_roads_post <- list()
  
  for(road_indx in unique(road_sims_post$road_indx)) { 
    
    road_sims_post2 <- road_sims_post[road_sims_post$road_indx == road_indx,]$p.b
    
    road_post_cf_df <- list()
    
    slope <- seq(-0.9, 0.9, 0.01)
    
    for(sim_indx in 1:length(road_sims_post2)) {
      
      road_post_cf = function(x) {(1 * exp(-road_sims_post2[sim_indx] * abs(x + 0.05))) / 3.6}
      
      post_vals <- road_post_cf(slope)
      road_post_df <- data.frame(slope, vals = post_vals)
      road_post_cf_df[[sim_indx]] <- road_post_df
      road_post_cf_df[[sim_indx]]$param_id <- sim_indx
      
    }
    
    cf_roads_post[[road_indx]] <- do.call(rbind, road_post_cf_df)
    cf_roads_post[[road_indx]]$type <- "Posterior"
    cf_roads_post[[road_indx]]$road_id <- road_indx
    
  }
  
  cf_roads_post <- do.call(rbind, cf_roads_post)
  
  return(cf_roads_post)
  
}

calculate_posterior_median_cf <- function(road_sims_post) {
  
  cf_roads_posterior_median <- list()
  
  slope <- seq(-0.9, 0.9, 0.01)
  
  for(road_indx in unique(road_sims_post$road_indx)) { 
    
    road_sims_posterior_median_b <- median(road_sims_post[road_sims_post$road_indx == road_indx,]$p.b)
    
    road_posterior_median_cf <- function(x) {(1 * exp(-road_sims_posterior_median_b * abs(x + 0.05))) / 3.6}
    road_posterior_median_vals <- road_posterior_median_cf(slope)
    road_posterior_median_df <- data.frame(slope, vals = road_posterior_median_vals)
    cf_roads_posterior_median[[road_indx]] <- road_posterior_median_df
    cf_roads_posterior_median[[road_indx]]$param_id <- road_indx
    
    cf_roads_posterior_median[[road_indx]]$type <- "Posterior Median"
    cf_roads_posterior_median[[road_indx]]$road_id <- road_indx
    
  }
  
  cf_roads_posterior_median <- do.call(rbind, cf_roads_posterior_median)
  
  return(cf_roads_posterior_median)
  
}

calculate_prior_cf <- function(road_sims, ndraws = 100) {
  
  cf_roads_prior <- list()
  
  road_sims_prior <- road_sims[[1]]
  road_sims_prior_b <- road_sims_prior[sample(1:nrow(road_sims_prior), size = ndraws, replace = FALSE),]$p.b
  
  slope <- seq(-0.9, 0.9, 0.01)
  
  for(prior_indx in 1:length(road_sims_prior_b)) { 
    
    prior_cf = function(x) {(1 * exp(-road_sims_prior_b[prior_indx] * abs(x + 0.05))) / 3.6}
    
    vals <- prior_cf(slope)
    road_prior_df <- data.frame(slope, vals)
    cf_roads_prior[[prior_indx]] <- road_prior_df
    cf_roads_prior[[prior_indx]]$param_id <- prior_indx
    cf_roads_prior[[prior_indx]]$type <- "Prior"
    cf_roads_prior[[prior_indx]]$road_id <- "Prior"
    
  }
  
  cf_roads_prior <- do.call(rbind, cf_roads_prior)
  
  return(cf_roads_prior)
  
}

calculate_prediction_cf <- function(road_sims_post, ndraws = 100) {
  
  cf_roads_prediction <- list()
  
  slope <- seq(-0.9, 0.9, 0.01)
  
  for(sim_indx in 1:ndraws) {
    
    road_sims_prediction_b <- truncnorm::rtruncnorm(n = 1, 
                                                    mean = sample(road_sims_post$p.b_mean, 1), 
                                                    sd = sample(road_sims_post$p.b_sd, 1),
                                                    a = 0)
    
    road_predict_cf <- function(x) {(1 * exp(-road_sims_prediction_b * abs(x + 0.05))) / 3.6}
    road_predict_vals <- road_predict_cf(slope)
    road_predict_df <- data.frame(slope, vals = road_predict_vals)
    cf_roads_prediction[[sim_indx]] <- road_predict_df
    cf_roads_prediction[[sim_indx]]$param_id <- sim_indx
    
  }
  
  cf_roads_prediction <- do.call(rbind, cf_roads_prediction)
  
  cf_roads_prediction$type <- "Prediction"
  cf_roads_prediction$road_id <- "Prediction"
  
  return(cf_roads_prediction)
  
}

calculate_slope_gradient <- function(road, r = r, max_slope = FALSE) { 
  
  slope_vals_df <- list()
  
  for(road_indx in 1:nrow(road)) {
    
    road2 <- road[road_indx,]
    input_data <- slope_calc(r = r, route = road2)
    
    road2_pts <- sf::st_cast(road2, "POINT")
    road2_pts_cells <- terra::cellFromXY(object = leastcostpath::rasterise(input_data[[1]]), xy = sf::st_coordinates(road2_pts))
    
    road2_pts_cells2 <- cbind(from = road2_pts_cells[1:(length(road2_pts_cells)-1)],
                              to = road2_pts_cells[2:length(road2_pts_cells)])
    
    slope_vals <- input_data[[1]]$conductanceMatrix[road2_pts_cells2]
    
    slope_vals_df[[road_indx]] <- data.frame(road_indx = road_indx,
                                             slope_value = abs(slope_vals))
    
  }
  slope_vals_df2 <- do.call(rbind, slope_vals_df)
  
  if(max_slope == TRUE) { 
    slope_vals_df2 <- slope_vals_df2 %>%
      group_by(road_indx) %>%
      summarise(max = max(slope_value))
  }
  
  return(slope_vals_df2)
}

calculate_known_roads <- function(road_post, roads, r = r) {
  
  roads_df <- list()
  
  for(road_indx in 1:length(unique(road_post$road_indx))) {
    
    print(road_indx)
    
    road_post2 <- road_post[road_post$road_indx == road_indx,]
    
    roads_list <- list()
    
    for(sim_indx in 1:nrow(road_post2)) {
      
      b <- road_post2[sim_indx,]$p.b
      
      road_od <- origin_destination(roads[roads$road_indx == road_indx,])
      
      input_data <- slope_calc(r = r, route = roads[roads$road_indx == road_indx,], neighbours = 8)
      roads_list[[sim_indx]] <- calculate_lcp(input_data = input_data,
                                                   param_value = b,
                                                   hyp_roads_od = road_od)
      
    }
    
    roads_df[[road_indx]] <- do.call(rbind, roads_list)
    
  }
  
  roads_df <- do.call(rbind, roads_df)
  roads_df$type <- "Known"
  
  return(roads_df)
  
}

calculate_lcp <- function(input_data, param_value, hyp_roads_od) {
  
  lcps <- list()
  
  for(b_indx in 1:length(param_value)) { 
    
    input_data2 <- input_data
    
    cf = function(x,y) {(1 * exp(-y * abs(x + 0.05))) / 3.6}
    
    input_data2[[1]]$conductanceMatrix[input_data2[[3]]] <- cf(x =  input_data2[[1]]$conductanceMatrix[input_data2[[3]]], y = param_value[b_indx])
    
    input_data2[[1]]$conductanceMatrix[input_data2[[3]]] <-   input_data2[[1]]$conductanceMatrix[input_data2[[3]]] / input_data2[[2]]
    
    lcps[[b_indx]] <- leastcostpath::create_lcp(x = input_data2[[1]], origin = hyp_roads_od[1,], destination = hyp_roads_od[2,], cost_distance = TRUE)
    sf::st_crs(lcps[[b_indx]]) <- sf::st_crs(hyp_roads_od)
    
    lcps[[b_indx]]$sim_indx <- b_indx
    lcps[[b_indx]]$b <- param_value[b_indx]
    lcps[[b_indx]]$origin <- hyp_roads_od$ID[1]
    lcps[[b_indx]]$destination <- hyp_roads_od$ID[2]
    
  }
  
  lcps <- do.call(rbind, lcps)
  
  return(lcps)
  
}

calculate_population_critical_slope <- function(road_sims_posterior, n_draws) { 
  
  population_level_critical_slope_df <- data.frame(b_value = NA,
                                                   critical_slope_value = rep(NA, nrow(road_sims_posterior)))
  
  for(i in 1:nrow(road_sims_posterior)) {
    
    population_level_b <- truncnorm::rtruncnorm(n = n_draws, a = 0, 
                                                mean = road_sims_posterior$p.b_mean[i],
                                                sd = road_sims_posterior$p.b_sd[i])
    
    population_level_b <- mean(population_level_b)
    
    population_level_cf = function(x, y) {(1 * exp(-y * abs(x + 0.05))) / 3.6}
    
    population_level_df <- data.frame(road_indx = 1, p.b = population_level_b)
    
    population_level_critical_slope_df$b_value[i] <- population_level_b
    population_level_critical_slope_df$critical_slope_value[i] <- calculate_critical_slope(population_level_df)$critical_slope
    
  }
  
  return(population_level_critical_slope_df)
}

calculate_critical_slope <- function(road_sims_post) { 
  
  critical_slope_vals <- list()
  
  for(road_indx in unique(road_sims_post$road_indx)) {
    
    road_sims_post2 <- road_sims_post[road_sims_post$road_indx == road_indx,]
    
    slope <- seq(-0.9, 0.9, 0.01)
    
    individual_critical_slope_vals <- list()
    
    for(sim_indx in 1:nrow(road_sims_post2)) { 
      
      cf = function(x,y) {(1 * exp(-road_sims_post2$p.b[sim_indx] * abs(x + 0.05))) / 3.6}
      
      speed_vals <- cf(slope)
      pace_vals <- 1/speed_vals
      
      height_gain_rate <- slope / pace_vals
      critical_slope <- slope[which.max(height_gain_rate)]
      
      individual_critical_slope_vals[[sim_indx]] <- data.frame(road_indx = as.numeric(road_indx), sim_indx = sim_indx, critical_slope = critical_slope)
      
    }
    
    individual_critical_slope_vals <- do.call(rbind, individual_critical_slope_vals)
    
    critical_slope_vals[[road_indx]] <- individual_critical_slope_vals
    
  }
  
  critical_slope_vals2 <- do.call(rbind, critical_slope_vals)
  
  return(critical_slope_vals2)
  
}