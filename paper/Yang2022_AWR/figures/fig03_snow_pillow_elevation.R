# Fig. 3 — elevation distribution of the snow pillow stations used in the model.
# From _original/plots/7_snow_pillow_distribution.R (land-cover exploration lines dropped).
source("fig_config.R")

data <- read.csv(file.path(P2_ROOT, "Stat", "snow_pillows_used_20210131.txt"))
n_sta <- 113  # value used in the paper to scale the density curve to station counts

ggplot(data, aes(x = Elev_m_)) +
  geom_histogram(aes(y = after_stat(count)), binwidth = 100, colour = "black", fill = "white") +
  geom_density(aes(y = after_stat(count) * n_sta), alpha = .2, fill = "#FF6666") +
  ylab("Number of Stations") + xlab("Elevation (m)") +
  theme_bw() +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.line = element_line(colour = "black"))
ggsave(file.path(FIG_OUT, "fig03_snow_pillow_elevation.jpg"), width = 8, height = 5, units = "in", dpi = 300)
