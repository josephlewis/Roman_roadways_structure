set.seed(NULL)
set.seed(1)

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
            min = min(p.b))

road_sims_posterior_summary_1$road_indx <- factor(road_sims_posterior_summary_1$road_indx)

plot_b_tobler_df <- ggplot(road_sims_posterior) + 
  stat_interval(aes(x = road_indx, y = p.b)) + 
  geom_point(data = road_sims_posterior_summary_1, aes(x = road_indx, y = median), size = 2) + 
  geom_hline(yintercept = 3.5, linetype = "longdash") + 
  scale_colour_brewer(palette = "Reds") +
  labs(x = "Roadway Index", y = expression(paste("Estimated ", italic("b"), " parameter")), colour = "Credible Interval", caption = "b parameter (3.5) while hiking shown with black dashed line") +  
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave(plot_b_tobler_df, filename = "./Output/figures/road_sims_posterior_b_tobler.png", dpi = 300, width = 12, height = 8)

min(round(road_sims_posterior_summary_1$median, 2))
max(round(road_sims_posterior_summary_1$median, 2))
table(road_sims_posterior_summary_1$median > 3.6)
round(table(road_sims_posterior_summary_1$median > 3.6) / sum(table(road_sims_posterior_summary_1$median > 3.6))*100, 1)

write.csv(road_sims_posterior_summary_1, "./Output/tables/simulated_roads/road_sims_posterior_summary_1.csv", row.names = FALSE)

road_sims_posterior_summary_2 <- road_sims_posterior %>%
  summarise(b_mean_lower_95 = quantile(p.b_mean, 0.025),
            b_mean_upper_95 = quantile(p.b_mean, 0.975),
            b_mean_median = median(p.b_mean),
            b_mean_mean = mean(p.b_mean),
            b_sd_lower_95 = quantile(p.b_sd, 0.025),
            b_sd_upper_95 = quantile(p.b_sd, 0.975),
            b_sd_median = median(p.b_sd),
            b_sd_mean = mean(p.b_sd))

write.csv(road_sims_posterior_summary_2, "./Output/tables/simulated_roads/road_sims_posterior_summary_2.csv", row.names = FALSE)

round(road_sims_posterior_summary_2, 2)

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
  labs(x = "Roadway Index", y = expression(paste("Path Deviation Index (", italic("pdi"), ")")), colour = "Credible Interval") + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave(plot_pdi_df, filename = "./Output/figures/road_sims_posterior_pdi.png", dpi = 300, width = 12, height = 8)

sim_roads_post_interval_plot <- ggplot(data = road_sims_posterior, aes(x = road_indx, y = p.b)) +
  ggdist::stat_interval(show.legend = TRUE, .width = c(0.5, 0.8, 0.95)) +
  scale_colour_brewer(palette = "Reds") +
  stat_summary(fun = "median", colour = "black", size = 0.25) + 
  labs(x = "Roadway Index", y = expression(paste("Estimated ", italic("b"), " parameter")), colour = "Credible Interval") + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right", strip.text.x = element_text(hjust = 0, margin=margin(l=0))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave(plot = sim_roads_post_interval_plot, filename = "./Output/figures/road_sims_post_interval.png", dpi = 300, width = 12, height = 8)

###### CALCULATE COST FUNCTION FOR EACH SIMULATED KNOWN ROMAN ROAD ######

prior_cf <- calculate_prior_cf(road_sims = road_sims, ndraws = 250)
posterior_cf <- calculate_posterior_cf(road_sims_post = road_sims_posterior)
posterior_cf$road_id <- paste0("Roadway Index: ", posterior_cf$road_id)
prediction_cf <- calculate_prediction_cf(road_sims_post = road_sims_posterior, ndraws = 250)
posterior_cf_median <- calculate_posterior_median_cf(road_sims_post = road_sims_posterior)
posterior_cf_median$road_id <- paste0("Roadway Index: ", posterior_cf_median$road_id)

posterior_cf_median_b <- road_sims_posterior %>%
  group_by(road_indx) %>%
  summarise(median_p.b = round(median(p.b), 1)) %>%
  mutate(road_id = paste0("Roadway Index: ", road_indx))

tobler_cf_df <- data.frame(slope = rep(seq(-0.9, 0.9, 0.01), times = length(road_sims)),
                 vals = rep(tobler_cf(seq(-0.9, 0.9, 0.01)), times = length(road_sims)),
                 param_id = "Tobler",
                 type = "Tobler",
                 road_id = rep(1:length(road_sims), each = length(seq(-0.9, 0.9, 0.01))))
tobler_cf_df$road_id <- paste0("Roadway Index: ", tobler_cf_df$road_id)

cf_dfs <- rbind(prior_cf, posterior_cf, prediction_cf, posterior_cf_median, tobler_cf_df)

cf_dfs <- cf_dfs %>%
  left_join(posterior_cf_median_b) %>%
  mutate(road_id2 = NA) %>%
  mutate(vals = vals * 3.6)

cf_dfs[is.na(cf_dfs$median_p.b) & cf_dfs$type == "Prior",]$road_id2 <- "Prior"
cf_dfs[is.na(cf_dfs$median_p.b) & cf_dfs$type == "Prediction",]$road_id2 <- "Prediction"
cf_dfs[is.na(cf_dfs$road_id2),]$road_id2 <- paste0(cf_dfs[is.na(cf_dfs$road_id2),]$road_id, " (", cf_dfs[is.na(cf_dfs$road_id2),]$median_p.b, ")")

cf_dfs$road_id2 <- factor(cf_dfs$road_id2, levels = unique(cf_dfs$road_id2))

cf_dfs1 <- cf_dfs

cf_dfs2 <- cf_dfs %>%
  mutate(vals = log(1/vals)) %>%
  mutate(vals = vals / max(vals))

plot_cf_dfs1 <- ggplot() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Prior",], aes(x = slope, y = vals), show.legend = FALSE, linewidth = 0.5, colour = "#193875", linetype = "dashed") + 
  scale_fill_brewer(palette = "Blues") +
  new_scale_fill() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Posterior",], aes(x = slope, y = vals), show.legend = FALSE, linewidth = 0.5, colour = "#6A191E", linetype = "dashed") + 
  scale_fill_brewer(palette = "Reds") +
  new_scale_fill() + 
  stat_lineribbon(data = cf_dfs1[cf_dfs1$type == "Prediction",], aes(x = slope, y = vals), show.legend = TRUE, linewidth = 0.5, colour = "#287719", linetype = "dashed") + 
  scale_fill_brewer(palette = "Greens") +
  geom_line(data = cf_dfs1[cf_dfs1$type == "Tobler",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
    facet_wrap(~road_id2, ncol = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
    scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) +
    labs(x = "Mathematical slope gradient (rise/run)", caption = "Median posterior estimate shown with dashed red line\nInfluence of topographic constraint while hiking (parameter b = 3.5) shown with dashed black line", y = "Relative preference for slope gradient", colour = "Credible Interval") +
    theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

plot_cf_dfs2 <- ggplot() + 
  stat_lineribbon(data = cf_dfs2[cf_dfs2$type == "Prior",], aes(x = slope, y = vals), show.legend = FALSE, linewidth = 0.5, colour = "#193875", linetype = "dashed") + 
  scale_fill_brewer(palette = "Blues") +
  new_scale_fill() + 
  stat_lineribbon(data = cf_dfs2[cf_dfs2$type == "Posterior",], aes(x = slope, y = vals), show.legend = TRUE, linewidth = 0.5, colour = "#6A191E", linetype = "dashed") + 
  scale_fill_brewer(palette = "Reds") +
  new_scale_fill() + 
  stat_lineribbon(data = cf_dfs2[cf_dfs2$type == "Prediction",], aes(x = slope, y = vals), show.legend = FALSE, linewidth = 0.5, colour = "#287719", linetype = "dashed") + 
  scale_fill_brewer(palette = "Greens") +
  geom_line(data = cf_dfs2[cf_dfs2$type == "Tobler",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
    facet_wrap(~road_id2, ncol = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
    scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) +
    labs(x = "Mathematical slope gradient (rise/run)", caption = "Median posterior estimate shown with dashed red line\nInfluence of topographic constraint while hiking (parameter b = 3.5) shown with dashed black line", y = "Relative influence of topographic constraints", fill = "Credible Interval", colour = "Credible Interval") +
    theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

# plot_cf_dfs1 <- ggplot() +
#   geom_line(data = cf_dfs1[cf_dfs1$type == "Prior",], aes(x = slope, y = vals, group = param_id), alpha = 0.25, colour = "#0065CC") +
#   geom_line(data = cf_dfs1[cf_dfs1$type == "Posterior",], aes(x = slope, y = vals, group = param_id), alpha = 0.25, colour = "#D3ADAE") +
#   geom_line(data = cf_dfs1[cf_dfs1$type == "Prediction",], aes(x = slope, y = vals, group = param_id), alpha = 0.25, colour = "#7F8629") +
#   geom_line(data = cf_dfs1[cf_dfs1$type == "Posterior Median",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "#7B191E", linetype = "dashed") +
#   geom_line(data = cf_dfs1[cf_dfs1$type == "Tobler",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed") +
#   facet_wrap(~road_id2, ncol = 6) +
#   scale_y_continuous(breaks = seq(0, 1, 0.25)) + 
#   scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) + 
#   labs(x = "Mathematical slope gradient (rise/run)", caption = "Median posterior estimate shown with dashed red line\nInfluence of topographic constraint while hiking (parameter b = 3.5) shown with dashed black line", y = "Relative preference for slope gradient") + 
#   theme_clean()
# 
# plot_cf_dfs2 <- ggplot() +
#   geom_line(data = cf_dfs2[cf_dfs2$type == "Prior",], aes(x = slope, y = vals, group = param_id), alpha = 0.25, colour = "#0065CC") +
#   geom_line(data = cf_dfs2[cf_dfs2$type == "Posterior",], aes(x = slope, y = vals, group = param_id), alpha = 0.25, colour = "#D3ADAE") +
#   geom_line(data = cf_dfs2[cf_dfs2$type == "Prediction",], aes(x = slope, y = vals, group = param_id), alpha = 0.25, colour = "#7F8629") +
#   geom_line(data = cf_dfs2[cf_dfs2$type == "Posterior Median",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "#7B191E", linetype = "dashed") +
#   geom_line(data = cf_dfs2[cf_dfs2$type == "Tobler",], aes(x = slope, y = vals, group = param_id), alpha = 1, colour = "black", linetype = "dashed") +
#   facet_wrap(~road_id2, ncol = 6) +
#   scale_y_continuous(breaks = seq(0, 1, 0.25)) + 
#   scale_x_continuous(breaks = c(seq(-0.8, -0.4, 0.4), 0, seq(0.4, 0.8, 0.4))) + 
#   labs(x = "Mathematical slope gradient (rise/run)", caption = "Median posterior estimate shown with dashed red line\nInfluence of topographic constraint while hiking (parameter b = 3.5) shown with dashed black line", y = "Relative influence of topographic constraints") +
#   theme_clean()

ggplot2::ggsave(plot_cf_dfs1, filename = "./Output/figures/road_sims_post_cfs.png", dpi = 300, width = 10, height = 12)
ggplot2::ggsave(plot_cf_dfs2, filename = "./Output/figures/road_sims_post_cfs_inverse.png", dpi = 300, width = 10, height = 12)
