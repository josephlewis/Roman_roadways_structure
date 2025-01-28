set.seed(NULL)
set.seed(1)

r <- terra::rast("./Data/DEM/OS_50m_Wales.tif")
sim_roads <- readRDS("./Output/tactical_simulation/simulated_routes/tactical_simulation_routes.rds")
sf::st_geometry(sim_roads) <- "geometry"
sim_roads$road_indx <- 1:nrow(sim_roads)

sim_routes_list <- list.files("./Output/tactical_simulation/simulated_routes/", full.names = TRUE, recursive = FALSE, pattern = ".rds")
sim_routes_list <- gtools::mixedsort(sim_routes_list)
sim_routes <- lapply(sim_routes_list, readRDS)
sim_routes <- do.call(rbind, sim_routes)
sim_routes$road_indx <- 1:(nrow(sim_routes))
sim_routes <- sf::st_drop_geometry(sim_routes)

sim_routes_abc_list <- list.files("./Output/tactical_simulation/simulated_routes_abc/", full.names = TRUE, recursive = FALSE, pattern = ".rds")
sim_routes_abc_list <- gtools::mixedsort(sim_routes_abc_list)
sim_routes_abc <- lapply(sim_routes_abc_list, readRDS)

no_post_rows <- 250
tol <- no_post_rows/nrow(sim_routes_abc[[1]])

sim_routes_abc <- lapply(sim_routes_abc, function(x) { x[order(x$pdi),]})

sim_routes_abc_posterior <- lapply(sim_routes_abc, function(x) { x[1:(nrow(sim_routes_abc[[1]])*tol),]})
sim_routes_abc_posterior <- do.call(rbind, sim_routes_abc_posterior)

# # calculate least-cost paths using all posterior b values for each road
sim_routes_abc_posterior_lcp <- calculate_known_roads(road_post = sim_routes_abc_posterior, r = r, roads = sim_roads)
sim_routes_abc_posterior_lcp$road_indx <- rep(1:length(unique(sim_roads$road_indx)), each = no_post_rows)
sim_routes_abc_posterior_lcp$sim_indx <- rep(1:length(unique(sim_roads$road_indx)), by = no_post_rows)
sf::st_write(sim_routes_abc_posterior_lcp, "./Output/tactical_simulation/simulated_routes_sf/tactical_sims_posterior.gpkg", append = FALSE)

sim_routes_abc_posterior_summary1 <- sim_routes_abc_posterior %>%
  group_by(road_indx) %>%
  summarise(lower95 = quantile(p.b, 0.025),
            upper_95 = quantile(p.b, 0.975),
            median = median(p.b),
            mean = mean(p.b))

# summarise posterior_median to their median
sim_routes_abc_posterior_median_b <- sim_routes_abc_posterior %>%
  group_by(road_indx) %>%
  summarise(p.b = median(p.b))

# calculate least-cost paths using median posterior b values for each road
route_sims_posterior_median <- calculate_known_roads(road_post = sim_routes_abc_posterior_median_b, r = r, roads = sim_roads)
route_sims_posterior_median$road_indx <- 1:nrow(route_sims_posterior_median)
sf::st_write(route_sims_posterior_median, "./Output/tactical_simulation/simulated_routes_sf/tactical_sims_posterior_median.gpkg", append = FALSE)

sim_routes_abc_posterior_summary1 <- sim_routes_abc_posterior_summary1 %>%
  left_join(sim_routes[c("road_indx", "b")], by = c("road_indx")) %>%
  mutate(b_within_interval = b >= lower95 & b <= upper_95)

write.csv(sim_routes_abc_posterior_summary1, "./Output/tables/tactical_simulation/sim_routes_abc_posterior_summary1.csv", row.names = FALSE)

sim_routes_abc_posterior_summary2 <- sim_routes_abc_posterior %>%
  summarise(b_mean_lower_95 = quantile(p.b_mean, 0.025),
            b_mean_upper_95 = quantile(p.b_mean, 0.975),
            b_mean_median = median(p.b_mean),
            b_mean_mean = mean(p.b_mean),
            b_sd_lower_95 = quantile(p.b_sd, 0.025),
            b_sd_upper_95 = quantile(p.b_sd, 0.975),
            b_sd_median = median(p.b_sd),
            b_sd_mean = mean(p.b_sd))

write.csv(sim_routes_abc_posterior_summary2, "./Output/tables/tactical_simulation/sim_routes_abc_posterior_summary2.csv", row.names = FALSE)

sim_routes_abc_posterior <- sim_routes_abc_posterior %>%
  left_join(sim_routes[c("fromCell", "toCell", "b")], by = c("fromCell", "toCell"))

sim_routes_abc_posterior$road_indx <- factor(sim_routes_abc_posterior$road_indx)

sim_routes_abc_posterior <- sim_routes_abc_posterior %>%
  group_by(road_indx) %>%
  mutate(low = quantile(p.b, 0.025),
         high = quantile(p.b, 0.975),
         within = between(b, 
                          left = quantile(p.b, 0.025),
                          right = quantile(p.b, 0.975)
         ))

sim_routes_abc_posterior$within <- factor(sim_routes_abc_posterior$within, levels = c(TRUE, FALSE))

sim_routes_abc_posterior_interval_plot <- ggplot() +
  ggdist::stat_interval(data = sim_routes_abc_posterior, aes(x = road_indx, y = p.b), show.legend = TRUE, .width = c(0.5, 0.8, 0.95)) +
  scale_colour_brewer(palette = "Reds") +
  stat_summary(data = sim_routes_abc_posterior, aes(x = road_indx, y = b, shape = within), size = 2.5,  fun = "median", geom = "point") +
  scale_shape_manual(values = c(16,1)) +
  labs(x = "Road Index", y = expression(paste("Estimated ", italic("b"), " parameter")), colour = "Credible Interval", shape = "Within 95% Credible Interval") + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0)))

ggplot2::ggsave(plot = sim_routes_abc_posterior_interval_plot, filename = "./Output/figures/sim_routes_abc_posterior_interval.png", dpi = 300, width = 12, height = 8)

sim_routes_posterior_cf <- calculate_posterior_cf(road_sims_post = sim_routes_abc_posterior)
sim_routes_posterior_cf$road_id <- paste0("Road Index: ", sim_routes_posterior_cf$road_id)

sim_routes_cf_df <- list()

for(road_indx in 1:nrow(sim_routes)) {
  
  sim_routes_cf_df[[road_indx]] <- data.frame(slope = rep(seq(-0.9, 0.9, 0.01), times = 1),
                                              vals = rep(exp_cf(x = seq(-0.9, 0.9, 0.01), y = sim_routes[sim_routes$road_indx == road_indx,]$b), 1),
                                              param_id = "Known",
                                              type = "Known",
                                              road_id = road_indx)
}

sim_routes_cf_df <- do.call(rbind, sim_routes_cf_df)
sim_routes_cf_df$road_id <- paste0("Road Index: ", sim_routes_cf_df$road_id)

sim_routes_posterior_median_cf <- calculate_posterior_median_cf(road_sims_post = sim_routes_abc_posterior)
sim_routes_posterior_median_cf$road_id <- paste0("Road Index: ", sim_routes_posterior_median_cf$road_id)

sim_routes_abc_posterior_median <- sim_routes_abc_posterior %>%
  group_by(road_indx) %>%
  summarise(b_median = median(p.b))

sim_routes_abc_posterior_median_diff <- data.frame(road_id = paste0("Road Index: ", 1:nrow(sim_routes)), 
                                                   diff = round(sim_routes_abc_posterior_median$b_median - sim_routes$b, 2))

sim_routes_posterior_cf2 <- rbind(sim_routes_posterior_cf, sim_routes_cf_df, sim_routes_posterior_median_cf) %>%
  mutate(vals = vals * 3.6) %>%
  left_join(sim_routes_abc_posterior_median_diff) %>%
  mutate(road_indx = as.numeric(gsub("\\D", "", road_id))) %>%
  left_join(sim_routes[c("road_indx", "b")]) %>%
  left_join(sim_routes_abc_posterior %>%
              group_by(road_indx) %>%
              summarise(within = unique(within),
                        low = unique(low),
                        high = unique(high)) %>%
              mutate(road_indx = as.numeric(road_indx))) %>%
  group_by(road_indx) %>%
  mutate(road_id2 = ifelse(within == TRUE,
                           paste0(road_id, " (0)"),
                           paste0(road_id, " (", round(min(c(abs(low - b), abs(high - b))), 4), ")")),
         abs_diff = round(min(c(abs(low - b), abs(high - b))), 4))

# mean absolute difference for true parameter b value not within the estimated parameter b 95% Credible Interval
round(mean(sim_routes_posterior_cf2 %>%
             filter(within == FALSE) %>% 
             group_by(road_indx) %>%
             summarise(abs_diff = unique(abs_diff)) %>%
             pull(abs_diff)), 2)

sim_routes_posterior_cf2$road_id2 <- factor(sim_routes_posterior_cf2$road_id2, levels = unique(sim_routes_posterior_cf2$road_id2))

sim_routes_posterior_cf_plot1 <- ggplot() + 
  stat_lineribbon(data = sim_routes_posterior_cf2[sim_routes_posterior_cf2$type == "Posterior",], aes(x = slope, y = vals), show.legend = TRUE, linewidth = 0.5, colour = "#7B191E", linetype = "dashed") + 
  scale_fill_brewer(palette = "Reds") +
  labs(x = "Mathematical slope gradient (rise/run)", y = "Influence of slope gradient", caption = "Median posterior estimate shown with dashed red line\nTrue parameter b shown with dashed black line\nAbsolute difference between true b parameter and estimated b parameter (95% credible interval) shown with numbers in brackets", fill = "Credible Interval") +
  ggnewscale::new_scale_fill() + 
  geom_line(data = sim_routes_posterior_cf2[sim_routes_posterior_cf2$type == "Known",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
  facet_wrap(~road_id2, ncol = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) +
  labs(x = "Mathematical slope gradient (rise/run)", y = "Influence of slope gradient", caption = "Median posterior estimate shown with dashed red line\nKnown parameter b shown with dashed black line\nAbsolute difference between true b parameter and estimated b parameter (95% credible interval) shown with numbers in brackets", fill = "Credible Interval") +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

ggplot2::ggsave(plot = sim_routes_posterior_cf_plot1, filename = "./Output/figures/sim_routes_abc_posterior_cf.png", dpi = 300, width = 10, height = 12)

