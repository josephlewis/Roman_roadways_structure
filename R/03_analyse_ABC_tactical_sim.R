set.seed(NULL)
set.seed(1)

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

sim_routes_abc_posterior_summary1 <- sim_routes_abc_posterior %>%
  group_by(road_indx) %>%
  summarise(lower95 = quantile(p.b, 0.025),
            upper_95 = quantile(p.b, 0.975),
            median = median(p.b),
            mean = mean(p.b))

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
  mutate(within = between(b, 
                          left = quantile(p.b, 0.025),
                          right = quantile(p.b, 0.975)
                          ))

sim_routes_abc_posterior$within <- factor(sim_routes_abc_posterior$within, levels = c(TRUE, FALSE))

sim_routes_abc_posterior_interval_plot <- ggplot() +
  ggdist::stat_interval(data = sim_routes_abc_posterior, aes(x = road_indx, y = p.b), show.legend = TRUE, .width = c(0.5, 0.8, 0.95)) +
  scale_colour_brewer(palette = "Reds") +
  stat_summary(data = sim_routes_abc_posterior, aes(x = road_indx, y = b, shape = within), size = 2.5,  fun = "median", geom = "point") +
  scale_shape_manual(values = c(16,1)) +
  labs(x = "Roadway Index", y = expression(paste("Estimated ", italic("b"), " parameter")), colour = "Credible Interval", shape = "Within 95% Credible Interval") + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0)))

ggplot2::ggsave(plot = sim_routes_abc_posterior_interval_plot, filename = "./Output/figures/sim_routes_abc_posterior_interval.png", dpi = 300, width = 12, height = 8)

sim_routes_posterior_cf <- calculate_posterior_cf(road_sims_post = sim_routes_abc_posterior)
sim_routes_posterior_cf$road_id <- paste0("Roadway Index: ", sim_routes_posterior_cf$road_id)

sim_routes_cf_df <- list()

for(road_indx in 1:nrow(sim_routes)) { 

sim_routes_cf_df[[road_indx]] <- data.frame(slope = rep(seq(-0.9, 0.9, 0.01), times = 1),
                           vals = rep(exp_cf(x = seq(-0.9, 0.9, 0.01), y = sim_routes[sim_routes$road_indx == road_indx,]$b), 1),
                           param_id = "Known",
                           type = "Known",
                           road_id = road_indx)
}

sim_routes_cf_df <- do.call(rbind, sim_routes_cf_df)
sim_routes_cf_df$road_id <- paste0("Roadway Index: ", sim_routes_cf_df$road_id)

sim_routes_posterior_median_cf <- calculate_posterior_median_cf(road_sims_post = sim_routes_abc_posterior)
sim_routes_posterior_median_cf$road_id <- paste0("Roadway Index: ", sim_routes_posterior_median_cf$road_id)

sim_routes_abc_posterior_median <- sim_routes_abc_posterior %>%
  group_by(road_indx) %>%
  summarise(b_median = median(p.b))

sim_routes_abc_posterior_median_diff <- data.frame(road_id = paste0("Roadway Index: ", 1:nrow(sim_routes)), 
                                                   diff = round(sim_routes_abc_posterior_median$b_median - sim_routes$b, 2))

sim_routes_posterior_cf2 <- rbind(sim_routes_posterior_cf, sim_routes_cf_df, sim_routes_posterior_median_cf) %>%
  mutate(vals = vals * 3.6) %>%
  left_join(sim_routes_abc_posterior_median_diff) %>%
  mutate(road_id = paste0(road_id, " (", abs(diff), ")"))

sim_routes_posterior_cf3 <- sim_routes_posterior_cf2 %>%
  mutate(vals = log(1/vals)) %>%
  mutate(vals = vals / max(vals))

sim_routes_posterior_cf2$road_id <- factor(sim_routes_posterior_cf2$road_id, levels = unique(sim_routes_posterior_cf2$road_id))

sim_routes_posterior_cf3$road_id <- factor(sim_routes_posterior_cf3$road_id, levels = unique(sim_routes_posterior_cf3$road_id))

sim_routes_posterior_cf_plot1 <- ggplot() + 
  stat_lineribbon(data = sim_routes_posterior_cf2[sim_routes_posterior_cf2$type == "Posterior",], aes(x = slope, y = vals), show.legend = TRUE, linewidth = 0.5, colour = "#7B191E", linetype = "dashed") + 
  scale_fill_brewer(palette = "Reds") +
  geom_line(data = sim_routes_posterior_cf2[sim_routes_posterior_cf2$type == "Known",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
  facet_wrap(~road_id, ncol = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
    scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) +
    labs(x = "Mathematical slope gradient (rise/run)", y = "Relative preference for slope gradient", caption = "Median posterior estimate shown with dashed red line\nKnown parameter b shown with dashed black line", fill = "Credible Interval") +
    theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

sim_routes_posterior_cf_plot2 <- ggplot() + 
  stat_lineribbon(data = sim_routes_posterior_cf3[sim_routes_posterior_cf3$type == "Posterior",], aes(x = slope, y = vals), show.legend = TRUE, linewidth = 0.5, colour = "#7B191E", linetype = "dashed") + 
  scale_fill_brewer(palette = "Reds") +
  geom_line(data = sim_routes_posterior_cf3[sim_routes_posterior_cf3$type == "Known",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
    facet_wrap(~road_id, ncol = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) + 
    scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) +
    labs(x = "Mathematical slope gradient (rise/run)", y = "Relative influence of topographic constraints", caption = "Median posterior estimate shown with dashed red line\nKnown parameter b shown with dashed black line", fill = "Credible Interval") +
    theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

# sim_routes_posterior_cf_plot1 <- ggplot() +
#   geom_line(data = sim_routes_posterior_cf2[sim_routes_posterior_cf2$type == "Posterior",], aes(x = slope, y = vals, group = param_id), alpha = 0.25, colour = "#D3ADAE") +
#   geom_line(data = sim_routes_posterior_cf2[sim_routes_posterior_cf2$type == "Posterior Median",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "#7B191E", linetype = "dashed") +
#   geom_line(data = sim_routes_posterior_cf2[sim_routes_posterior_cf2$type == "Known",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed") +
#   facet_wrap(~road_id, ncol = 5) +
#   scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) + 
#   labs(x = "Mathematical slope gradient (rise/run)", y = "Relative preference for slope gradient", caption = "Median posterior estimate shown with dashed red line\nKnown parameter b shown with dashed black line") + 
#   theme_clean()
# 
# sim_routes_posterior_cf_plot2 <- ggplot() +
#   geom_line(data = sim_routes_posterior_cf3[sim_routes_posterior_cf3$type == "Posterior",], aes(x = slope, y = vals, group = param_id), alpha = 0.25, colour = "#D3ADAE") +
#   geom_line(data = sim_routes_posterior_cf3[sim_routes_posterior_cf3$type == "Posterior Median",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "#7B191E", linetype = "dashed") +
#   geom_line(data = sim_routes_posterior_cf3[sim_routes_posterior_cf3$type == "Known",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed") +
#   facet_wrap(~road_id, ncol = 5) +
#   scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) + 
#   labs(x = "Mathematical slope gradient (rise/run)", y = "Relative influence of topographic constraints", caption = "Median posterior estimate shown with dashed red line\nKnown parameter b shown with dashed black line") + 
#   theme_clean()

ggplot2::ggsave(plot = sim_routes_posterior_cf_plot1, filename = "./Output/figures/sim_routes_abc_posterior_cf1.png", dpi = 300, width = 10, height = 12)
ggplot2::ggsave(plot = sim_routes_posterior_cf_plot2, filename = "./Output/figures/sim_routes_abc_posterior_cf2.png", dpi = 300, width = 10, height = 12)

