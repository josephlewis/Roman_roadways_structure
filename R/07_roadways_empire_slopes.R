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

roadways_empire_critical_slopes <- calculate_population_critical_slope(road_sims_posterior = road_sims_posterior, n_draws = 1000)

roadways_empire_critical_slopes2 <- roadways_empire_critical_slopes %>%
  group_by(critical_slope_value) %>%
  summarise(n = n())

critical_slopes_df <- data.frame(type = c("Hiking",
                                          "Wheeled vehicles"),
                                 slope_value = c(0.29, 0.15))

roman_empire_roadways_critical_slope_plot <- ggplot(roadways_empire_critical_slopes2) + 
  geom_bar(aes(x = critical_slope_value, y = n/sum(n)), stat = "identity", fill  = "grey85") +
  geom_vline(data = critical_slopes_df, aes(xintercept = slope_value, colour = type), linetype = "dashed", lwd = 0.75) + 
  scale_colour_manual(values = c("#0065CC", "#7B191E")) +
  scale_x_continuous(breaks = seq(0, 0.9, 0.05)) + 
  labs(x = "Critical slope gradient", y = "Probability density", colour = NULL) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom", legend.justification = "right")

ggplot2::ggsave(roman_empire_roadways_critical_slope_plot, filename = "./Output/figures/roman_empire_roadways_critical_slope.png", dpi = 300, width = 8, height = 4)

write.csv(roadways_empire_critical_slopes, "./Output/tables/simulated_roads/roman_empire_roadways_critical_slope.csv", row.names = FALSE)

round(table(roadways_empire_critical_slopes$critical_slope_value < 0.29) / length(roadways_empire_critical_slopes$critical_slope_value)*100, 2)

round(table(roadways_empire_critical_slopes$critical_slope_value < 0.15) / length(roadways_empire_critical_slopes$critical_slope_value)*100, 2)



