b_values_df <- list()
b_values <- seq(0, 100, 0.01)
slope <- seq(-0.9, 0.9, 1e-4)

for(i in 1:length(b_values)) { 
  
  b_values_df[[i]] <- data.frame(slope = slope,
                                 value  = cf(x = slope,y = b_values[i]),
                                 b_indx = i,
                                 b_value = b_values[i],
                                 critical_slope_value = b_to_critical_slope(slope = slope, y = b_values[i]))
}

b_values_df <- do.call(rbind, b_values_df)

plot_b_critical_slope1 <- ggplot(b_values_df) + 
  geom_line(aes(x = slope, y = value, colour = b_value, group = b_indx)) +
  viridis::scale_color_viridis(option = "H", breaks = seq(0, 100, 25), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(-0.9, 0.9, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(x = "Mathematical slope gradient (rise/run)", y = "Influence of slope gradient", colour = expression(paste("Rate of decline from maximum influence (parameter", italic(" b"), ")")), title = "A") + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.justification = "right")

plot_b_critical_slope2 <- ggplot(b_values_df) + 
  geom_line(aes(x = slope, y = value, colour = critical_slope_value, group = b_indx)) +
  viridis::scale_color_viridis(option = "H", breaks = seq(0, 0.9, 0.3), limits = c(0, 0.9)) + 
  scale_x_continuous(breaks = seq(-0.9, 0.9, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(x = "Mathematical slope gradient (rise/run)", y = "Influence of slope gradient", colour = "Critical slope gradient (percent)", title = "B") + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.justification = "right")

plot_b_critical_slope3 <- ggplot(data = b_values_df %>%
         select(-c(value, slope, b_indx)) %>%
         distinct()) + 
  geom_line(aes(x = critical_slope_value, y = b_value), lwd = 1.25) + 
  scale_x_continuous(breaks = seq(0, 0.9, 0.1), limits = c(0, 0.9)) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  labs(x = "Critical slope gradient (rise/run)", y = expression(paste("Rate of decline from maximum influence (parameter ", italic("b"), ")")), title = "C") +
  theme_minimal()

plot_b_critical_slope4 <- (plot_b_critical_slope1 | plot_b_critical_slope2) / plot_b_critical_slope3

ggplot2::ggsave(plot_b_critical_slope4, filename = "./Output/figures/b_critical_slope_plot.png", dpi = 300, width = 14, height = 14)
