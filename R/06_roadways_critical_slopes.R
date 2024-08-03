set.seed(NULL)
set.seed(1)

r <- terra::rast("./Data/DEM/OS_50m_Wales.tif")
roads <- sf::st_read("./Data/RR_known.gpkg")
sf::st_geometry(roads) <- "geometry"
roads$road_indx <- 1:nrow(roads)

sim_list <- list.files("./Output/simulated_roads", full.names = TRUE, recursive = FALSE, pattern = ".rds")
sim_list <- gtools::mixedsort(sim_list)

road_sims <- lapply(sim_list, readRDS)

no_post_rows <- 250
tol <- no_post_rows/nrow(road_sims[[1]])

road_sims <- lapply(road_sims, function(x) { x[order(x$max_dist),]})
road_sims_posterior <- lapply(road_sims, function(x) { x[1:(nrow(road_sims[[1]])*tol),]})

road_sims_posterior <- do.call(rbind, road_sims_posterior)
road_sims_posterior$road_indx <- rep(1:length(road_sims), each = no_post_rows)
road_sims_posterior$road_indx <- factor(road_sims_posterior$road_indx)

road_sims_posterior_lcp <- calculate_known_roads(road_post = road_sims_posterior, r = r, roads = roads)
sf::st_write(road_sims_posterior_lcp, "./Output/roadway_system/road_sims_posterior.gpkg", append = FALSE)

road_sims_posterior_median_b <- road_sims_posterior %>%
  group_by(road_indx) %>%
  summarise(p.b = median(p.b))

road_sims_posterior_median <- calculate_known_roads(road_post = road_sims_posterior_median_b, r = r, roads = roads)
road_sims_posterior_median$road_indx <- 1:nrow(road_sims_posterior_median)

sf::st_write(road_sims_posterior_median, "./Output/roadway_system/road_sims_posterior_median.gpkg", append = FALSE)

tobler_b <- road_sims_posterior_median_b
tobler_b$p.b <- 3.5
tobler_b$road_indx <- 1:nrow(tobler_b)

tobler_routes <- calculate_known_roads(road_post = tobler_b, r = r, roads = roads)
sf::st_write(tobler_routes, "./Output/roadway_system/tobler_routes.gpkg", append = FALSE)

road_sims_posterior_median_slope_gradient <- calculate_slope_gradient(road = road_sims_posterior_median, r = r, max_slope = FALSE) %>%
  left_join(sf::st_drop_geometry(road_sims_posterior_median[c("road_indx", "b")]))

write.csv(road_sims_posterior_median_slope_gradient, "./Output/tables/simulated_roads/road_sims_posterior_median_slope_gradient.csv", row.names = FALSE)

tobler_routes_slope_gradient <- calculate_slope_gradient(road = tobler_routes, r = r, max_slope = FALSE) %>%
  mutate(b = 3.5)

road_sims_posterior_median_slope_gradient$type <- "Known Roman roadways"
tobler_routes_slope_gradient$type <- "Tobler's Hiking Function (b parameter = 3.5)"

road_sims_posterior_median_tobler_slope_gradient <- rbind(road_sims_posterior_median_slope_gradient, tobler_routes_slope_gradient)

road_sims_posterior_median_tobler_slope_gradient$type <- factor(road_sims_posterior_median_tobler_slope_gradient$type, levels = unique(road_sims_posterior_median_tobler_slope_gradient$type))

road_sims_posterior_median_tobler_slope_gradient_plot <- ggplot() + 
  stat_ecdf(data = road_sims_posterior_median_tobler_slope_gradient, aes(x = slope_value, colour = type)) + 
  scale_colour_manual(values = c("#E74C3C", "#2980B9")) +
  scale_x_continuous(breaks = seq(0,1,0.1), limits = c(0,0.5), expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.1)) + 
  labs(x = "Mathematical slope gradient (rise/run)", y = "Cumulative Probability", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

ggplot2::ggsave(road_sims_posterior_median_tobler_slope_gradient_plot, filename = "./Output/figures/road_sims_posterior_median_tobler_slope_gradient.png", dpi = 300, width = 8, height = 4)

road_sims_posterior_median_tobler_slope_gradient$road_indx2 <- paste0("Roadway Index: ", road_sims_posterior_median_tobler_slope_gradient$road_indx)

road_sims_posterior_median_tobler_slope_gradient$road_indx2 <- factor(road_sims_posterior_median_tobler_slope_gradient$road_indx2, levels = unique(road_sims_posterior_median_tobler_slope_gradient$road_indx2))

road_sims_posterior_median_tobler_slope_gradient_individual_plot <- ggplot() + 
  stat_ecdf(data = road_sims_posterior_median_tobler_slope_gradient, aes(x = slope_value, colour = type)) + 
  scale_colour_manual(values = c("#E74C3C", "#2980B9")) +
  facet_wrap(~road_indx2, ncol = 6) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25)) + 
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 0.9)) +
  labs(x = "Mathematical slope gradient (rise/run)", y = "Cumulative Probability", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

ggplot2::ggsave(road_sims_posterior_median_tobler_slope_gradient_individual_plot, filename = "./Output/figures/road_sims_posterior_median_tobler_slope_gradient_individual.png", dpi = 300, width = 8, height = 14)

critical_slopes_df <- data.frame(type = c("Critical slope gradient while hiking",
                                          "Estimated critical slope gradient for Roman roadways in South-West England",
                                          "Critical slope gradient for wheeled vehicles",
                                          "Maximum slope gradient for wheeled vehicles"),
                                 slope_value = c(0.29, 0.08, 0.15, 0.20))

median_slope_gradients_plot <- ggplot() +
  ggdist::stat_interval(data = road_sims_posterior_median_slope_gradient, aes(x = factor(road_indx), y = slope_value), show.legend = TRUE, .width = c(0.5, 0.8, 0.95)) +
  scale_colour_brewer(palette = "Reds") +
  stat_summary(data = road_sims_posterior_median_slope_gradient, aes(x = factor(road_indx), y = slope_value), fun = "median", colour = "black", size = 0.25) + 
  geom_hline(data = critical_slopes_df, aes(yintercept = slope_value, linetype = type)) + 
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) + 
  labs(x = "Roadway Index", y = "Mathematical slope gradient (rise/run)", colour = "Credible Interval", linetype = NULL) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0)), legend.box="vertical") + 
  guides(linetype=guide_legend(nrow=2,byrow=TRUE))

ggplot2::ggsave(median_slope_gradients_plot, filename = "./Output/figures/road_sims_posterior_median_slope_gradients.png", dpi = 300, width = 12, height = 8)

road_sims_posterior_median_slope_gradient_max <- road_sims_posterior_median_slope_gradient %>%
  group_by(road_indx) %>%
  summarise(max_slope_value = max(slope_value))

road_sims_posterior_median_slope_gradient_max$above_029 <- road_sims_posterior_median_slope_gradient_max$max_slope_value > 0.29
road_sims_posterior_median_slope_gradient_max$above_025 <- road_sims_posterior_median_slope_gradient_max$max_slope_value > 0.25
road_sims_posterior_median_slope_gradient_max$above_02 <- road_sims_posterior_median_slope_gradient_max$max_slope_value > 0.2
road_sims_posterior_median_slope_gradient_max$above_015 <- road_sims_posterior_median_slope_gradient_max$max_slope_value > 0.15
road_sims_posterior_median_slope_gradient_max$above_0125 <- road_sims_posterior_median_slope_gradient_max$max_slope_value > 0.125
road_sims_posterior_median_slope_gradient_max$above_008 <- road_sims_posterior_median_slope_gradient_max$max_slope_value > 0.08

write.csv(road_sims_posterior_median_slope_gradient_max, "./Output/tables/simulated_roads/road_sims_posterior_median_slope_gradient_max.csv", row.names = FALSE)

### 

road_sims_posterior_median_slope_gradient_increasing_res <- list()

for(res_multiplier in 1:10) { 
  
  print(res_multiplier)
  
  r2 <- terra::aggregate(x = r, fact = res_multiplier, fun = "mean", na.rm = TRUE)
  
  road_sims_posterior_median_slope_gradient_increasing_res[[res_multiplier]] <- calculate_slope_gradient(road = road_sims_posterior_median, r = r2, max_slope = FALSE) %>%
    mutate(res_multiplier = res_multiplier)
  
}

road_sims_posterior_median_slope_gradient_increasing_res2 <- do.call(rbind, road_sims_posterior_median_slope_gradient_increasing_res)
road_sims_posterior_median_slope_gradient_increasing_res2$res_multiplier <- road_sims_posterior_median_slope_gradient_increasing_res2$res_multiplier*50

road_sims_posterior_median_slope_gradient_increasing_res2$res_multiplier <- factor(road_sims_posterior_median_slope_gradient_increasing_res2$res_multiplier)

road_sims_posterior_median_slope_gradient_increasing_res_plot <- ggplot(road_sims_posterior_median_slope_gradient_increasing_res2, aes(x = res_multiplier, y = slope_value)) + 
  ggdist::stat_interval() +
  stat_summary(fun = "median", colour = "black", size = 0.25) + 
  scale_color_brewer(palette = "Reds") +
  labs(x = "Digital Elevation Model resolution (metres)", y = "Mathematical slope gradient (rise/run)", colour = "Credible Interval (%)") + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave(road_sims_posterior_median_slope_gradient_increasing_res_plot, filename = "./Output/figures/road_sims_posterior_median_slope_gradient_increasing_res.png", dpi = 300, width = 12, height = 8)

