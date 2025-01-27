set.seed(NULL)
set.seed(1)

road_sims_posterior <- sf::st_read("./Output/roadway_system/road_sims_posterior.gpkg")

road_sims_posterior$sinuosity <- calculate_sinuosity(line = road_sims_posterior)

road_sims_posterior <- road_sims_posterior %>%
  left_join(data.frame(road_indx = 1:nrow(distinct(sf::st_drop_geometry(road_sims_posterior)[c("fromCell", "toCell")])), distinct(sf::st_drop_geometry(road_sims_posterior)[c("fromCell", "toCell")])))

road_sims_posterior$road_indx2 <- paste0("Road Index: ", road_sims_posterior$road_indx)

road_sims_posterior$road_indx2 <- factor(road_sims_posterior$road_indx2, levels = unique(road_sims_posterior$road_indx2))

plot_b_sinuosity1 <- ggplot(road_sims_posterior, aes(x  = b, y = sinuosity)) + 
  geom_line(colour = "black") + 
  labs(x = "Posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity") + 
  facet_wrap(~road_indx2, ncol = 5) +
  theme_clean()

ggplot2::ggsave(plot_b_sinuosity1, filename = "./Output/figures/road_sims_post_b_sinuosity.png", dpi = 300, width = 10, height = 12)

road_sims_posterior_b_sinuosity_median <- road_sims_posterior %>%
  sf::st_drop_geometry() %>%
  group_by(road_indx, road_indx2) %>%
  summarise(sinuosity = median(sinuosity),
            min_b = min(b),
            max_b = max(b),
            low = quantile(b, 0.025),
            high = quantile(b, 0.975),
            b = median(b)) %>%
  mutate(quadrant = case_when(b >= 0 & b <= 6.7 ~ "Wheeled vehicles difficult to ascend/descend directly",
                              b > 6.7 & b <= 11.1 ~ "Wheeled vehicle can ascend/descend directly",
                              b > 11.1 ~ "Loaded wheeled vehicle drawn by two mules can ascend/descend directly"),
         quadrant2 = case_when(min_b >= 0 & max_b <= 6.7 ~ "Wheeled vehicles difficult to ascend/descend directly",
                               min_b > 6.7 ~ "Wheeled vehicle can ascend/descend",
                               min_b > 6.7 & max_b <= 11.1 ~ "Wheeled vehicle can ascend/descend directly",
                               min_b > 11.1 ~ "Loaded wheeled vehicle drawn by two mules can ascend/descend directly",
                               TRUE ~ "Uncertain"))

table(road_sims_posterior_b_sinuosity_median$quadrant)
round(table(road_sims_posterior_b_sinuosity_median$quadrant)/nrow(road_sims_posterior_b_sinuosity_median)*100, 2)

road_sims_posterior_b_sinuosity_median %>%
  group_by(quadrant) %>%
  summarise(min = round(min(sinuosity), 2),
            max = round(max(sinuosity), 2),
            median = round(median(sinuosity), 2),
            mean = round(mean(sinuosity), 2))

road_sims_posterior_b_sinuosity_median %>%
  ungroup() %>%
  filter(quadrant %in% c("Wheeled vehicle can ascend/descend directly", "Loaded wheeled vehicle drawn by two mules can ascend/descend directly")) %>%
  summarise(min = round(min(sinuosity), 2),
            max = round(max(sinuosity), 2),
            median = round(median(sinuosity), 2),
            mean = round(mean(sinuosity), 2))

lm1 <- summary(lm(sinuosity ~ b, data = road_sims_posterior_b_sinuosity_median))
lm2 <- summary(lm(sinuosity ~ b, data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$quadrant2 != "Uncertain",]))

lm_bayes1 <- brms::brm(formula = sinuosity ~ b, data = road_sims_posterior_b_sinuosity_median, family = "gaussian", iter = 2500)
lm_bayes2 <- brms::brm(formula = sinuosity ~ b, data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$quadrant2 != "Uncertain",], family = "gaussian", iter = 2500)

brms::bayes_R2(lm_bayes1)
brms::bayes_R2(lm_bayes2)

fixef(lm_bayes1)
fixef(lm_bayes2)

lm_bayes1_predict <- lm_bayes1 %>%
  tidybayes::add_epred_draws(newdata = data.frame(b = seq(0, 40, 0.1)), ndraws = 5000)

lm_bayes2_predict <- lm_bayes2 %>%
  tidybayes::add_epred_draws(newdata = data.frame(b = seq(0, 40, 0.1)), ndraws = 5000)

road_sims_posterior_b_sinuosity_median$quadrant <- factor(road_sims_posterior_b_sinuosity_median$quadrant, levels = (c("Wheeled vehicles difficult to ascend/descend directly", "Wheeled vehicle can ascend/descend directly", "Loaded wheeled vehicle drawn by two mules can ascend/descend directly")))

road_sims_posterior_b_sinuosity_median$quadrant2 <- factor(road_sims_posterior_b_sinuosity_median$quadrant2, levels = c("Wheeled vehicles difficult to ascend/descend directly", "Wheeled vehicle can ascend/descend", "Uncertain"))

road_sims_posterior_b_sinuosity_median_vline <- data.frame(quadrant = unique(road_sims_posterior_b_sinuosity_median$quadrant)[c(1,1,2,2,3)],
                                                           value = c(0, 6.7, 6.7, 11.1, 11.1))

road_sims_posterior_b_sinuosity_median_vline$quadrant <- factor(road_sims_posterior_b_sinuosity_median_vline$quadrant, levels = c("Wheeled vehicles difficult to ascend/descend directly", "Wheeled vehicle can ascend/descend directly", "Loaded wheeled vehicle drawn by two mules can ascend/descend directly"))

plot_b_sinuosity2 <- ggplot(road_sims_posterior_b_sinuosity_median) + 
  geom_point(aes(x = b, y = sinuosity), shape = 21, colour = "black", fill = "white") + 
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline, aes(xintercept = value), linetype = "dashed") + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity") +
  facet_wrap(~quadrant, ncol = 1) + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right") + 
  guides(colour = guide_legend(nrow = 3))

ggplot2::ggsave(plot_b_sinuosity2, filename = "./Output/figures/road_sims_post_b_sinuosity2.png", dpi = 300, width = 12, height = 8)

plot_b_sinuosity2b <- ggplot(road_sims_posterior_b_sinuosity_median) + 
  geom_point(aes(x = b, y = sinuosity), shape = 21, colour = "black", fill = "white") + 
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline[road_sims_posterior_b_sinuosity_median_vline$value %in% c(6.7),], aes(xintercept = value), linetype = "dashed") + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity") +
  facet_wrap(~quadrant2, ncol = 1) + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right") + 
  guides(colour = guide_legend(nrow = 3))

ggplot2::ggsave(plot_b_sinuosity2b, filename = "./Output/figures/road_sims_post_b_sinuosity2b.png", dpi = 300, width = 12, height = 8)

plot_b_sinuosity3 <- ggplot(road_sims_posterior_b_sinuosity_median) + 
  geom_point(aes(x = b, y = sinuosity, colour = quadrant), size = 3.5, shape = 21, fill = NA) +
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline, aes(xintercept = value), linetype = "dashed") +
  scale_colour_manual(values = c("#e41a1c", "#4daf4a", "#377eb8")) + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity3, filename = "./Output/figures/road_sims_post_b_sinuosity3.png", dpi = 300, width = 12, height = 8)

plot_b_sinuosity3b <- ggplot(road_sims_posterior_b_sinuosity_median) + 
  geom_point(aes(x = b, y = sinuosity, colour = quadrant2), size = 3.5, shape = 21, fill = NA) +
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline[road_sims_posterior_b_sinuosity_median_vline$value %in% c(6.7),], aes(xintercept = value), linetype = "dashed") + 
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#4daf4a")) + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity3b, filename = "./Output/figures/road_sims_post_b_sinuosity3b.png", dpi = 300, width = 12, height = 8)

plot_b_sinuosity3c <- ggplot(road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$quadrant2 != "Uncertain",]) + 
  geom_point(aes(x = b, y = sinuosity, colour = quadrant2), size = 3.5, shape = 21, fill = NA) +
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline[road_sims_posterior_b_sinuosity_median_vline$value %in% c(6.7),], aes(xintercept = value), linetype = "dashed") + 
  scale_colour_manual(values = c("#e41a1c", "#377eb8")) + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity3c, filename = "./Output/figures/road_sims_post_b_sinuosity3c.png", dpi = 300, width = 12, height = 8)

plot_b_sinuosity4 <- ggplot() +
  geom_point(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity, colour = quadrant), size = 3.5, shape = 21, fill = NA) +
  geom_smooth(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity), method = "lm", colour = "black", fill = "grey85") +
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline, aes(xintercept = value), linetype = "dashed") +
geom_text(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$road_indx %in% c(17, 20, 43),], aes(x = b, y = sinuosity, label = road_indx)) +
  annotate("text", x=35, y=1.01, label = paste0("R^2 = ", round(lm1$r.squared, 2), ", p.val < 0.001"), size = 2)  +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity4, filename = "./Output/figures/road_sims_post_b_sinuosity4.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity4a <- ggplot() +
  geom_point(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity, colour = quadrant), size = 3.5, alpha = 0.5) +
  geom_smooth(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity), method = "lm", colour = "black", fill = "grey85") +
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline, aes(xintercept = value), linetype = "dashed") +
  geom_text(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$road_indx %in% c(17, 20, 43),], aes(x = b, y = sinuosity, label = road_indx), size = 7) +
  annotate("text", x=32, y=1.01, label = paste0("R^2 = ", round(lm1$r.squared, 2), ", p.val < 0.001"), size = 4)  +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity4a, filename = "./Output/figures/road_sims_post_b_sinuosity4a.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity_bayesian_4a <- ggplot() +
  geom_point(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity, colour = quadrant), size = 3.5, alpha = 0.5) +
  stat_lineribbon(data = lm_bayes1_predict, aes(x = b, y = .epred), alpha = 0.5) +
  scale_fill_brewer(palette = "Greys") + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL, fill = "Credible Interval") +
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline, aes(xintercept = value), linetype = "dashed") +
  geom_text(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$road_indx %in% c(17, 20, 43),], aes(x = b, y = sinuosity, label = road_indx), size = 7) +
  annotate("text", x=30, y=1.01, label = paste0("b = ", round(fixef(lm_bayes1)[2], 3), " [", round(fixef(lm_bayes1)[6], 3), " - ", round(fixef(lm_bayes1)[8], 3), "]; ", "R^2 = ", round(brms::bayes_R2(lm_bayes1)[,1], 3), " [", round(brms::bayes_R2(lm_bayes1)[,3], 3), " - ", round(brms::bayes_R2(lm_bayes1)[,4], 3), "]"), size = 4)  +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL, fill = "Credible Interval") +
  theme_clean() + 
  theme(legend.position="bottom", legend.box="vertical", legend.box.just = "right", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity_bayesian_4a, filename = "./Output/figures/road_sims_post_b_sinuosity_bayesian_4a.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity4b <- ggplot(road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity)) + 
  geom_point(aes(colour = quadrant2), size = 3.5, shape = 21, fill = NA) +
  geom_smooth(method = "lm", colour = "black", fill = "grey85") + 
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline[road_sims_posterior_b_sinuosity_median_vline$value %in% c(6.7),], aes(xintercept = value), linetype = "dashed") + 
  geom_text(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$road_indx %in% c(17, 20, 43),], aes(x = b, y = sinuosity, label = road_indx)) + 
  annotate("text", x=35, y=1.01, label = paste0("R^2 = ", round(lm1$r.squared, 2), ", p.val < 0.001"), size = 2)  +
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity4b, filename = "./Output/figures/road_sims_post_b_sinuosity4b.png", dpi = 300, width = 10, height = 6)

road_sims_posterior <- road_sims_posterior %>%
  group_by(road_indx) %>%
  mutate(sinuosity_median = median(sinuosity)) %>%
  left_join(road_sims_posterior_b_sinuosity_median[c("road_indx2", "quadrant", "quadrant2")])

plot_b_sinuosity4b2 <- ggplot() + 
  ggdist::stat_interval(data = road_sims_posterior, aes(x = b, y = sinuosity_median, group = road_indx2), show.legend = TRUE, .width = c(0.5, 0.8, 0.95), alpha = 1) +
  geom_point(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity), size = 2, show.legend = FALSE, colour = "black") +
  scale_colour_brewer(palette = "Reds") +
  ggnewscale::new_scale_color() + 
  ggdist::stat_interval(data = road_sims_posterior[road_sims_posterior$quadrant2 == "Uncertain",], aes(x = b, y = sinuosity_median, group = road_indx2), show.legend = TRUE, .width = c(0.5, 0.8, 0.95), alpha = 1) +
  scale_colour_brewer(palette = "Greens") +
  geom_point(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$quadrant2 == "Uncertain",], aes(x = b, y = sinuosity), size = 2, show.legend = FALSE, colour = "black") +
  ggnewscale::new_scale_color() + 
  ggdist::stat_interval(data = road_sims_posterior[road_sims_posterior$quadrant2 == "Wheeled vehicle can ascend/descend",], aes(x = b, y = sinuosity_median, group = road_indx2), show.legend = TRUE, .width = c(0.5, 0.8, 0.95), alpha = 1) +
  scale_colour_brewer(palette = "Blues") +
  geom_point(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$quadrant2 == "Wheeled vehicle can ascend/descend",], aes(x = b, y = sinuosity), size = 2, show.legend = FALSE, colour = "black") +
#  geom_smooth(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity), method = "lm", colour = "black", fill = "grey85") + 
 geom_vline(data = road_sims_posterior_b_sinuosity_median_vline[road_sims_posterior_b_sinuosity_median_vline$value %in% c(6.7),], aes(xintercept = value), linetype = "dashed") +
  facet_wrap(~quadrant2, ncol = 1) + 
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity") + 
  theme_clean() +
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity4b2, filename = "./Output/figures/road_sims_post_b_sinuosity4b2.png", dpi = 300, width = 10, height = 12)

plot_b_sinuosity4c <- ggplot(road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$quadrant2 != "Uncertain",], aes(x = b, y = sinuosity)) + 
  geom_point(aes(colour = quadrant2), size = 3.5, shape = 21, fill = NA) +
  geom_smooth(method = "lm", colour = "black", fill = "grey85") + 
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline[road_sims_posterior_b_sinuosity_median_vline$value %in% c(6.7),], aes(xintercept = value), linetype = "dashed") + 
  geom_text(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$road_indx %in% c(17, 20, 43),], aes(x = b, y = sinuosity, label = road_indx)) + 
  annotate("text", x=32, y=1.01, label = paste0("R^2 = ", round(lm2$r.squared, 2), ", p.val < 0.001"), size = 2)  +
  scale_colour_manual(values = c("#e41a1c", "#377eb8")) + 
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity4c, filename = "./Output/figures/road_sims_post_b_sinuosity4c.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity_bayesian_4c <- ggplot() + 
  geom_point(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$quadrant2 != "Uncertain",], aes(x = b, y = sinuosity, colour = quadrant2), size = 3.5, alpha = 0.5) +
  stat_lineribbon(data = lm_bayes2_predict, aes(x = b, y = .epred), alpha = 0.5) +
  scale_fill_brewer(palette = "Greys") + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL, fill = "Credible Interval") +
  geom_vline(data = road_sims_posterior_b_sinuosity_median_vline[road_sims_posterior_b_sinuosity_median_vline$value %in% c(0, 6.7),], aes(xintercept = value), linetype = "dashed") + 
  geom_text(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$road_indx %in% c(17, 20, 43),], aes(x = b, y = sinuosity, label = road_indx), size = 7) + 
  annotate("text", x=30, y=1.01, label = paste0("b = ", round(fixef(lm_bayes2)[2], 3), " [", round(fixef(lm_bayes2)[6], 3), " - ", round(fixef(lm_bayes2)[8], 3), "]; ", "R^2 = ", round(brms::bayes_R2(lm_bayes2)[,1], 3), " [", round(brms::bayes_R2(lm_bayes2)[,3], 3), " - ", round(brms::bayes_R2(lm_bayes2)[,4], 3), "]"), size = 4) + 
  scale_colour_manual(values = c("#F8766D", "#619CFF")) + 
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  theme_clean() + 
  theme(legend.position="bottom", legend.box="vertical", legend.box.just = "right", legend.justification = "right", legend.text=element_text(size=8)) + 
  guides(colour = guide_legend(order = 0),
         fill  = guide_legend(order = 1))

ggplot2::ggsave(plot_b_sinuosity_bayesian_4c, filename = "./Output/figures/road_sims_post_b_sinuosity_bayesian_4c.png", dpi = 300, width = 10, height = 6)

b_sinuosity5_df <- data.frame(xmin = c(0, 6.7, 11.1), xmax = c(6.7, 11.1, 40), ymin = c(1,1, 1), ymax = c(1.5, 1.5, 1.5), quadrant = c("Wheeled vehicles difficult to ascend/descend directly", "Wheeled vehicle can ascend/descend directly", "Loaded wheeled vehicle drawn by two mules can ascend/descend directly"))

b_sinuosity5_df1 <- expand.grid(
  x = seq(from = 0, to = 6.7, by = 0.01),
  y = seq(from = 1, to = 1.25, by = 0.001))

b_sinuosity5_df2 <- expand.grid(
  x = seq(from = 0, to = 6.7, by = 0.01),
  y = seq(from = 1.25, to = 1.5, by = 0.001))

b_sinuosity5_df3 <- expand.grid(
  x = seq(from = 6.7, to = 45, by = 0.01),
  y = seq(from = 1, to = 1.25, by = 0.001))

b_sinuosity5_df4 <- expand.grid(
  x = seq(from = 6.7, to = 45, by = 0.01),
  y = seq(from = 1.25, to = 1.5, by = 0.001))

plot_b_sinuosity5 <- ggplot() + 
  geom_tile(data = b_sinuosity5_df1, aes(x = x, y = y), fill = "#fb8072", show.legend = FALSE) + 
#  scale_fill_gradient(low = "#8dd3c7", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicles difficult to ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df2, aes(x = x, y = y), fill = "#ffffb3", show.legend = FALSE) + 
#  scale_fill_gradient(low = "#ffffb3", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicles difficult to ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df3, aes(x = x, y = y), fill = "#8dd3c7", show.legend = FALSE) + 
#  scale_fill_gradient(low = "#fb8072", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicle can ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df4, aes(x = x, y = y), fill = "#bebada", show.legend = FALSE) + 
#  scale_fill_gradient(low = "#bebada", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Loaded wheeled vehicle drawn by two mules can ascend/descend directly") +
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1, ymax = 1.25), fill = NA, colour = "white") +
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") +
  geom_rect(aes(xmin = 6.7, xmax = 42, ymin = 1, ymax = 1.25), fill = NA, colour = "white") +
  geom_rect(aes(xmin = 6.7, xmax = 42, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") +
  geom_point(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity, colour = quadrant), size = 3.5, alpha = 0.5) +
  geom_textcurve(aes(x = 3, y = 1.5, xend = 3, yend = 1.11), arrow = arrow(length = unit(0.03, "npc")), angle = 180, label = "                         ", size = 5, fontface = "italic")  +
  annotate(geom = "text", x=3.1, y= 1.305, label="Barriers to\nmovement\nother than\nslope gradient\novercome", colour = "black", size = 5, fontface = 'italic') +
  geom_textcurve(aes(x = 7, y = 1.45, xend = 37, yend = 1.45), arrow = arrow(length = unit(0.03, "npc")), angle = 0, label = "Roads better facilitate wheeled vehicles", size = 5, fontface = "italic")  +
  geom_textcurve(aes(x = 7, y = 1.1, xend = 37.5, yend = 1.45), arrow = arrow(length = unit(0.03, "npc")), angle = 135, label = "Economic motivation", size = 5, fontface = "italic")  +
  geom_textcurve(aes(x = 7, y = 1.1, xend = 37.5, yend = 1), arrow = arrow(length = unit(0.03, "npc")), angle = 190, curvature = -0.5, label = "Political motivation", size = 5, fontface = "italic")  +
  geom_textcurve(aes(x = 37.5, y = 1.35, xend = 37.5, yend = 1.05), arrow = arrow(length = unit(0.03, "npc")), angle = 0, curvature = -0.5, label = "                           ", size = 5, fontface = "italic")  +
  annotate(geom = "text", x=37.5, y= 1.2, label="Increase in\ncost of\nconstruction\nand required\npolitical impetus", colour = "black", size = 5, fontface = 'italic') +
  scale_colour_manual(values = c("#e41a1c", "#4daf4a", "#377eb8")) + 
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 42)) +
  scale_y_continuous(limits = c(1, 1.5)) + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity5, filename = "./Output/figures/road_sims_post_b_sinuosity5.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity5b <- ggplot() + 
  geom_tile(data = b_sinuosity5_df1, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#e41a1c", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicles difficult to ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df2, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#377eb8", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicle can ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df3, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#377eb8", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Loaded wheeled vehicle drawn by two mules can ascend/descend directly") +
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
  geom_point(data = road_sims_posterior_b_sinuosity_median[road_sims_posterior_b_sinuosity_median$quadrant2 != "Uncertain",], aes(x = b, y = sinuosity, colour = quadrant2), size = 3.5, shape = 21, fill = NA, alpha = 1) +
  geom_textcurve(aes(x = 3, y = 1.5, xend = 3, yend = 1.11), arrow = arrow(length = unit(0.03, "npc")), angle = 180, label = "                         ", size = 3, fontface = "italic")  +
  annotate(geom = "text", x=3, y= 1.305, label="Barriers to\nmovement\nother than\nslope gradient\novercome", colour = "black", size = 3, fontface = 'italic') +
  geom_textcurve(aes(x = 7, y = 1.45, xend = 37, yend = 1.45), arrow = arrow(length = unit(0.03, "npc")), angle = 0, label = "Roads better facilitate wheeled vehicles", size = 3, fontface = "italic")  +
  geom_textcurve(aes(x = 7, y = 1.1, xend = 37.5, yend = 1.45), arrow = arrow(length = unit(0.03, "npc")), angle = 135, label = "Economic motivation", size = 3, fontface = "italic")  +
  geom_textcurve(aes(x = 7, y = 1.1, xend = 37.5, yend = 1), arrow = arrow(length = unit(0.03, "npc")), angle = 190, curvature = -0.5, label = "Political motivation", size = 3, fontface = "italic")  +
  geom_textcurve(aes(x = 37.5, y = 1.35, xend = 37.5, yend = 1.05), arrow = arrow(length = unit(0.03, "npc")), angle = 0, curvature = -0.5, label = "                     ", size = 3, fontface = "italic")  +
  annotate(geom = "text", x=37.5, y= 1.2, label="Increase in\ncost of construction\nand required\npolitical impetus", colour = "black", size = 3, fontface = 'italic') +
  scale_colour_manual(values = c("#e41a1c", "#377eb8")) + 
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 40)) +
  scale_y_continuous(limits = c(1, 1.5)) + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity5b, filename = "./Output/figures/road_sims_post_b_sinuosity5b.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity6 <- ggplot() + 
  geom_tile(data = b_sinuosity5_df1, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#e41a1c", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicles difficult to ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df2, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#4daf4a", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicle can ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df3, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#377eb8", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Loaded wheeled vehicle drawn by two mules can ascend/descend directly") +
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
#  geom_point(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity, colour = quadrant), size = 3.5, shape = 21, fill = NA, alpha = 1) +
  annotate(geom = "text", x=15, y= 1.02, label="Roads more likely given fewer topographic constraints that could cause the road to deviate from straightness", colour = "black", size = 3, fontface = 'italic') +
  annotate(geom = "text", x=15.5, y= 1.375, label="Roads less likely given fewer topographic constraints that could cause the road to deviate from straightness", colour = "black", size = 3, fontface = 'italic') +
  geom_textcurve(aes(x = 8, y = 1.2, xend = 37.5, yend = 1.05), arrow = arrow(length = unit(0.03, "npc")), curvature = 0.1, label = "                      ", size = 3, fontface = "italic")  +
  annotate(geom = "text", x=22.5, y= 1.09, label="Roads better\nfacilitate\nwheeled vehicles", colour = "black", size = 3, fontface = 'italic') +
  scale_colour_manual(values = c("#e41a1c", "#4daf4a", "#377eb8")) + 
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 40)) +
  scale_y_continuous(limits = c(1, 1.5)) + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity6, filename = "./Output/figures/road_sims_post_b_sinuosity6.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity6b <- ggplot() + 
  geom_tile(data = b_sinuosity5_df1, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#e41a1c", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicles difficult to ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df2, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#377eb8", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicle can ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df3, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#377eb8", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Loaded wheeled vehicle drawn by two mules can ascend/descend directly") +
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
  annotate(geom = "text", x=15, y= 1.02, label="Roads more likely given fewer topographic constraints that could cause the road to deviate from straightness", colour = "black", size = 3, fontface = 'italic') +
  annotate(geom = "text", x=15.5, y= 1.375, label="Roads less likely given fewer topographic constraints that could cause the road to deviate from straightness", colour = "black", size = 3, fontface = 'italic') +
  geom_textcurve(aes(x = 8, y = 1.2, xend = 37.5, yend = 1.05), arrow = arrow(length = unit(0.03, "npc")), curvature = 0.1, label = "                      ", size = 3, fontface = "italic")  +
  annotate(geom = "text", x=22.5, y= 1.09, label="Roads better\nfacilitate\nwheeled vehicles", colour = "black", size = 3, fontface = 'italic') +
  scale_colour_manual(values = c("#e41a1c", "#377eb8")) + 
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 40)) +
  scale_y_continuous(limits = c(1, 1.5)) + 
  labs(x = NULL, y = "Sinuosity", colour = NULL) +
  ggtitle("A") + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity6b, filename = "./Output/figures/road_sims_post_b_sinuosity6b.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity7 <- ggplot() + 
  geom_tile(data = b_sinuosity5_df1, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#e41a1c", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicles difficult to ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df2, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#4daf4a", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicle can ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df3, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#377eb8", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Loaded wheeled vehicle drawn by two mules can ascend/descend directly") +
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
#  geom_point(data = road_sims_posterior_b_sinuosity_median, aes(x = b, y = sinuosity, colour = quadrant), size = 3.5, shape = 21, fill = NA, alpha = 1) +
  annotate(geom = "text", x=3.35, y= 1.125, label="Roads less likely\ngiven a preference\n for faciliating\nwheeled vehicles", colour = "black", size = 3, fontface = 'italic') +
  annotate(geom = "text", x=3.35, y= 1.375, label="Roads less likely\ngiven a preference\nfor straightness", colour = "black", size = 3, fontface = 'italic') +
  geom_textcurve(aes(x = 8, y = 1.2, xend = 37.5, yend = 1.05), arrow = arrow(length = unit(0.03, "npc")), curvature = 0.1, label = "                      ", size = 3, fontface = "italic")  +
  annotate(geom = "text", x=22.5, y= 1.09, label="Roads better\nfacilitate\nwheeled vehicles", colour = "black", size = 3, fontface = 'italic') +
  scale_colour_manual(values = c("#e41a1c", "#4daf4a", "#377eb8")) + 
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 40)) +
  scale_y_continuous(limits = c(1, 1.5)) + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity7, filename = "./Output/figures/road_sims_post_b_sinuosity7.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity7b <- ggplot() + 
  geom_tile(data = b_sinuosity5_df1, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#e41a1c", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicles difficult to ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df2, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#377eb8", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Wheeled vehicle can ascend/descend directly") +
  new_scale_fill() +
  geom_tile(data = b_sinuosity5_df3, aes(x = x, y = y, fill = y), show.legend = FALSE) + 
  scale_fill_gradient(low = "#377eb8", high = "white") +
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", fill = "Loaded wheeled vehicle drawn by two mules can ascend/descend directly") +
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 0, xmax = 6.7, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1, ymax = 1.25), fill = NA, colour = "white") + 
  geom_rect(aes(xmin = 6.7, xmax = 40, ymin = 1.25, ymax = 1.5), fill = NA, colour = "white") + 
  annotate(geom = "text", x=3.35, y= 1.125, label="Roads less likely\ngiven a preference\n for faciliating\nwheeled vehicles", colour = "black", size = 3, fontface = 'italic') +
  annotate(geom = "text", x=3.35, y= 1.375, label="Roads less likely\ngiven a preference\nfor straightness", colour = "black", size = 3, fontface = 'italic') +
  geom_textcurve(aes(x = 8, y = 1.2, xend = 37.5, yend = 1.05), arrow = arrow(length = unit(0.03, "npc")), curvature = 0.1, label = "                      ", size = 3, fontface = "italic")  +
  annotate(geom = "text", x=22.5, y= 1.09, label="Roads better\nfacilitate\nwheeled vehicles", colour = "black", size = 3, fontface = 'italic') +
  scale_colour_manual(values = c("#e41a1c", "#377eb8")) + 
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 40)) +
  scale_y_continuous(limits = c(1, 1.5)) + 
  labs(x = "Median posterior estimate for rate of decline from maximum influence of slope gradient (parameter b)", y = "Sinuosity", colour = NULL) +
  ggtitle("B") + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=8))

ggplot2::ggsave(plot_b_sinuosity7b, filename = "./Output/figures/road_sims_post_b_sinuosity7b.png", dpi = 300, width = 10, height = 6)

plot_b_sinuosity67b <- plot_b_sinuosity6b / plot_b_sinuosity7b

ggplot2::ggsave(plot_b_sinuosity67b, filename = "./Output/figures/road_sims_post_b_sinuosity67b.png", dpi = 300, width = 10, height = 12)
