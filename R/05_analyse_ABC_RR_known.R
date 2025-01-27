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

# # calculate least-cost paths using all posterior b values for each road
road_sims_posterior_lcp <- calculate_known_roads(road_post = road_sims_posterior, r = r, roads = roads)
road_sims_posterior_lcp$road_indx <- rep(1:length(road_sims), each = no_post_rows)
road_sims_posterior_lcp$sim_indx <- rep(1:length(road_sims), by = no_post_rows)
sf::st_write(road_sims_posterior_lcp, "./Output/roadway_system/road_sims_posterior.gpkg", append = FALSE)

# summarise posterior_median to their median
road_sims_posterior_median_b <- road_sims_posterior %>%
  group_by(road_indx) %>%
  summarise(p.b = median(p.b))

# calculate least-cost paths using median posterior b values for each road
road_sims_posterior_median <- calculate_known_roads(road_post = road_sims_posterior_median_b, r = r, roads = roads)
road_sims_posterior_median$road_indx <- 1:nrow(road_sims_posterior_median)
sf::st_write(road_sims_posterior_median, "./Output/roadway_system/road_sims_posterior_median.gpkg", append = FALSE)

round(c(median = median(road_sims_posterior$p.b_mean), 
                        quantile(road_sims_posterior$p.b_mean, 0.025),
                        quantile(road_sims_posterior$p.b_mean, 0.975)), 2)

round(c(median = median(road_sims_posterior$p.b_sd), 
                        quantile(road_sims_posterior$p.b_sd, 0.025),
                        quantile(road_sims_posterior$p.b_sd, 0.975)), 2)

road_sims_posterior_summary_1 <- road_sims_posterior %>%
  group_by(road_indx) %>%
  summarise(lower_95 = quantile(p.b, 0.025),
            upper_95 = quantile(p.b, 0.975),
            median = median(p.b),
            mean = mean(p.b),
            min = min(p.b),
            max = max(p.b))

write.csv(road_sims_posterior_summary_1, "./Output/tables/simulated_roads/road_sims_posterior_summary_1.csv", row.names = FALSE)

road_sims_posterior_summary_1$road_indx <- factor(road_sims_posterior_summary_1$road_indx)

road_sims_posterior_predict_values <- data.frame(id = "Population-level\nposterior prediction", values = rep(NA, nrow(road_sims_posterior)))

for(sim_indx in 1:nrow(road_sims_posterior_predict_values)) {
  
  road_sims_posterior_predict_values[sim_indx, 2] <- truncnorm::rtruncnorm(n = 1, 
                                                       mean = sample(road_sims_posterior$p.b_mean, 1), 
                                                       sd = sample(road_sims_posterior$p.b_sd, 1),
                                                       a = 0)
}

plot_b_df <- ggplot() + 
  stat_interval(data = road_sims_posterior, aes(x = road_indx, y = p.b), show.legend = FALSE) + 
  stat_summary(data = road_sims_posterior, aes(x = road_indx, y = p.b), fun = "median", colour = "black", size = 0.25, pch = 16) + 
  geom_point(data = road_sims_posterior_summary_1, aes(x = road_indx, y = median), size = 2) + 
  scale_colour_brewer(palette = "Reds") +
  ggnewscale::new_scale_colour() + 
  stat_interval(data = road_sims_posterior_predict_values, aes(x = id, y = values)) + 
  stat_summary(data = road_sims_posterior_predict_values, aes(x = id, y = values), fun = "median", colour = "black", size = 0.25, pch = 16) + 
  scale_colour_brewer(palette = "Greens") +
  geom_point(data = road_sims_posterior_summary_1, aes(x = road_indx, y = median), size = 2) +
  labs(x = "Road Index", y = expression(paste("Influence of slope gradient (parameter ", italic("b"), ")")), colour = "Credible Interval") + 
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_text(vjust=15))

plot_b_df2 <- ggplot() + 
  stat_interval(data = road_sims_posterior, aes(x = road_indx, y = p.b), show.legend = FALSE) + 
  stat_summary(data = road_sims_posterior, aes(x = road_indx, y = p.b), fun = "median", colour = "black", size = 0.25, pch = 16) + 
  geom_point(data = road_sims_posterior_summary_1, aes(x = road_indx, y = median), size = 2) + 
  scale_colour_brewer(palette = "Reds") +
  labs(x = "Road Index", y = expression(paste("Influence of slope gradient (parameter ", italic("b"), ")")), colour = "Credible Interval") + 
  scale_y_continuous(breaks = seq(0, 50, 2), limits = c(0, 50)) +
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave(plot_b_df2, filename = "./Output/figures/road_sims_posterior_b_2.png", dpi = 300, width = 12, height = 8)

road_sims_posterior_summary_2 <- road_sims_posterior %>%
  summarise(b_mean_lower_95 = quantile(p.b_mean, 0.025),
            b_mean_upper_95 = quantile(p.b_mean, 0.975),
            b_mean_median = median(p.b_mean),
            b_mean_mean = mean(p.b_mean),
            b_sd_lower_95 = quantile(p.b_sd, 0.025),
            b_sd_upper_95 = quantile(p.b_sd, 0.975),
            b_sd_median = median(p.b_sd),
            b_sd_mean = mean(p.b_sd)) %>%
  mutate(parameter = "population level")

write.csv(road_sims_posterior_summary_2, "./Output/tables/simulated_roads/road_sims_posterior_summary_2.csv", row.names = FALSE)

road_sims_posterior_summary_3 <- road_sims_posterior %>%
  group_by(road_indx) %>%
  summarise(pdi_min = min(pdi, na.rm = TRUE),
            pdi_max = min(pdi, na.rm = TRUE),
            pdi_lower_95 = quantile(pdi, 0.025),
            pdi_upper_95 = quantile(pdi, 0.975),
            pdi_median = median(pdi, na.rm = TRUE))

write.csv(road_sims_posterior_summary_3, "./Output/tables/simulated_roads/road_sims_posterior_summary_3.csv", row.names = FALSE)

road_sims_posterior_summary_3$road_indx <- factor(road_sims_posterior_summary_3$road_indx)
road_sims_posterior$road_indx <- factor(road_sims_posterior$road_indx)

plot_pdi_df <- ggplot(data = road_sims_posterior, aes(x = road_indx, y = pdi)) +
  ggdist::stat_interval(show.legend = TRUE, .width = c(0.5, 0.8, 0.95)) +
  scale_colour_brewer(palette = "Reds") +
  stat_summary(fun = "median", colour = "black", size = 0.25) + 
  labs(x = "Road Index", y = expression(paste("Path Deviation Index (", italic("pdi"), ")")), colour = "Credible Interval") + 
  scale_y_continuous(breaks = seq(0, 1200, 100)) +
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave(plot_pdi_df, filename = "./Output/figures/road_sims_posterior_pdi.png", dpi = 300, width = 12, height = 8)

###### CALCULATE COST FUNCTION FOR EACH SIMULATED KNOWN ROMAN ROAD ######

prior_cf <- calculate_prior_cf(road_sims = road_sims, ndraws = 250)
posterior_cf <- calculate_posterior_cf(road_sims_post = road_sims_posterior)
posterior_cf$road_id <- paste0("Road Index: ", posterior_cf$road_id)
prediction_cf <- calculate_prediction_cf(road_sims_post = road_sims_posterior, ndraws = 250)
posterior_cf_median <- calculate_posterior_median_cf(road_sims_post = road_sims_posterior)
posterior_cf_median$road_id <- paste0("Road Index: ", posterior_cf_median$road_id)

posterior_cf_median_b <- road_sims_posterior %>%
  group_by(road_indx) %>%
  summarise(median_p.b = round(median(p.b), 1)) %>%
  mutate(road_id = paste0("Road Index: ", road_indx))

cf_dfs <- rbind(prior_cf, posterior_cf, prediction_cf, posterior_cf_median)

cf_dfs <- cf_dfs %>%
  left_join(posterior_cf_median_b) %>%
  mutate(road_id2 = NA) %>%
  mutate(vals = vals * 3.6)

cf_dfs[is.na(cf_dfs$median_p.b) & cf_dfs$type == "Prior",]$road_id2 <- "Prior"
cf_dfs[is.na(cf_dfs$median_p.b) & cf_dfs$type == "Prediction",]$road_id2 <- "Posterior prediction"
cf_dfs[is.na(cf_dfs$road_id2),]$road_id2 <- paste0(cf_dfs[is.na(cf_dfs$road_id2),]$road_id, " (", cf_dfs[is.na(cf_dfs$road_id2),]$median_p.b, ")")

cf_dfs$road_id2 <- factor(cf_dfs$road_id2, levels = unique(cf_dfs$road_id2))

cf_dfs1 <- cf_dfs

plot_cf_dfs1 <- ggplot() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Prior",], aes(x = slope, y = vals), show.legend = FALSE, linewidth = 0.5, colour = "#193875", linetype = "dashed") + 
  scale_fill_brewer(palette = "Blues") +
  new_scale_fill() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Posterior",], aes(x = slope, y = vals), show.legend = FALSE, linewidth = 0.5, colour = "#6A191E", linetype = "dashed") + 
  scale_fill_brewer(palette = "Reds") +
  new_scale_fill() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Prediction",], aes(x = slope, y = vals), show.legend = TRUE, linewidth = 0.5, colour = "#287719", linetype = "dashed") + 
  scale_fill_brewer(palette = "Greens") +
  facet_wrap(~road_id2, ncol = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) +
  labs(x = "Mathematical slope gradient (rise/run)", caption = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b) shown with dashed line", y = "Influence of slope gradient", fill = "Credible Interval") +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

plot_cf_dfs2 <- ggplot() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Posterior",], aes(x = slope, y = vals), show.legend = TRUE, colour = "#6A191E", linewidth = 0.5, linetype = "dashed") + 
  scale_fill_brewer(palette = "Reds") +
  new_scale_fill() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Prediction",], aes(x = slope, y = vals), show.legend = FALSE, colour = "#287719", linewidth = 0.5, linetype = "dashed") + 
  scale_fill_brewer(palette = "Greens") +
  facet_wrap(~road_id2, ncol = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) +
  labs(x = "Mathematical slope gradient (rise/run)", caption = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b) shown with dashed line", y = "Influence of slope gradient", fill = "Credible Interval") +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

plot_cf_dfs3 <- ggplot() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Posterior",], aes(x = slope, y = vals), show.legend = TRUE, colour = "#6A191E", linewidth = 0.5, linetype = "dashed") + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(~road_id2, ncol = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) +
  labs(x = "Mathematical slope gradient (rise/run)", caption = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b) shown with dashed line", y = "Influence of slope gradient", fill = "Credible Interval") +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

ggplot2::ggsave(plot_cf_dfs1, filename = "./Output/figures/road_sims_post_cfs.png", dpi = 300, width = 10, height = 12)
ggplot2::ggsave(plot_cf_dfs2, filename = "./Output/figures/road_sims_post_cfs2.png", dpi = 300, width = 10, height = 12)
ggplot2::ggsave(plot_cf_dfs3, filename = "./Output/figures/road_sims_post_cfs3.png", dpi = 300, width = 10, height = 12)

