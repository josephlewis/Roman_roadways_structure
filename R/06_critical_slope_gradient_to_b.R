slope <- seq(-0.9, 0.9, 1e-6)

b_critical_slope_df <- data.frame(scenario = c("loaded wheeled vehicle drawn by two mules", "traversed by a wheeled vehicle directly", "road connecting two forts in Wales", "Roman Britain", "Italy Low", "Italy High", "Hispania Low", "Hispania High", "Germany", "Scotland Low", "Scotland High", "Judea"), 
                                  critical_slope_value = c(0.09, 0.15, 0.25, 0.125, 0.07, 0.1, 0.06, 0.08, 0.08, 0.167, 0.25, 0.0099),
                                  estimated_b_value = NA)

for(i in 1:nrow(b_critical_slope_df)) { 
  b_critical_slope_df$estimated_b_value[i] <- round(uniroot(critical_slope_to_b,interval = c(0, 250),target_critical_slope = b_critical_slope_df$critical_slope_value[i])$root, 1)
}

write.csv(b_critical_slope_df, "./Output/tables/b_from_critical_slope.csv", row.names = FALSE)