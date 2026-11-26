# Fig. 10 — 14-year (WY2004-2017) mean pixel-wise peak SWE by elevation (100 m DEM bins of 5 m).
# Fig. 11 — maps of mean pixel-wise peak SWE and of each product's anomaly from the four-product mean.
# From _original/plots/6_peakSWE_mean.R (Fig. 10) and 5_pixelwise_peaSWE_maps.R (Fig. 11).
# Inputs: data/final/pixel_wise_peak_mean/plot/<n>PeakSWE_<product>.tif — per-pixel peak SWE averaged
#   over WY2004-2017 on the 500 m UTM grid (LRM in m, SNODAS/NWM in mm; made by
#   _original/pipeline/5_pixelwise_peakSWE_mean.R), SNM_500.tif domain mask, SNM_DEM.tif.
source("fig_config.R")
suppressPackageStartupMessages({ library(outliers); library(ggpubr) })

pid <- c("LRM-FSCA", "LRM-baseline", "SNODAS", "NWM-SWE")
files <- c("1PeakSWE_LRM_FSCA.tif", "2PeakSWE_LRM_baseline.tif", "4PeakSWE_SNODAS.tif", "5PeakSWE_NWM_SWE.tif")
colors <- setNames(c("#7F9F00", "#FC4E07", "#00AFBB", "#8106A9"), pid)
shapes <- setNames(c(15, 16, 17, 10), pid)

tem <- raster(file.path(P2_ROOT, "data", "template", "LRM_SNM_tem", "SNM_500.tif"))
dem <- raster(file.path(P2_ROOT, "figures", "6_peakSWE", "SNM_DEM.tif")) * tem
rstack <- stack(file.path(P2_ROOT, "data", "final", "pixel_wise_peak_mean", "plot", files)) * tem
rstack[[1]] <- rstack[[1]] * 100; rstack[[2]] <- rstack[[2]] * 100   # m -> cm
rstack[[3]] <- rstack[[3]] / 10;  rstack[[4]] <- rstack[[4]] / 10    # mm -> cm
names(rstack) <- make.names(pid)

# ---- Fig. 10 ----
# The published version ("_WM") also masks water bodies (SNM_500_WM.tif), which removes lake-surface
# elevation bins (e.g. Lake Tahoe ~1,897 m).
wm <- raster(file.path(P2_ROOT, "data", "template", "LRM_SNM_tem", "SNM_500_WM.tif"))
N_inter <- 5
df <- cbind.data.frame(Elevation = getValues(dem * wm), getValues(rstack))
names(df) <- c("Elevation", pid)
df <- df[complete.cases(df) & df$Elevation >= 1500, ]
df$group <- floor((df$Elevation - 1499) / N_inter)
long <- pivot_longer(df, all_of(pid), names_to = "id", values_to = "value")
a <- long %>% group_by(group, id) %>%
  filter(!value %in% outlier(value)) %>%           # drop the single most extreme pixel per bin (as in the paper)
  summarise(m = mean(value), sd = sd(value), sum_pixel = n(), .groups = "drop") %>%
  mutate(Elevation = group * N_inter + 1500, area = sum_pixel * 0.25, id = factor(id, levels = pid))
k <- max(a$area) / max(a$m + 35)                    # secondary-axis scale used in the paper
ggplot(a) +
  geom_ribbon(aes(x = Elevation, ymin = m - sd, ymax = m + sd, fill = id), alpha = 0.2) +
  geom_point(aes(x = Elevation, y = area / k), alpha = .6, color = "red") +
  geom_point(aes(x = Elevation, y = m, color = id, shape = id), alpha = .8) +
  scale_y_continuous(name = "14-year average pixel-wise peak SWE (cm)",
                     sec.axis = sec_axis(~ . * k, name = expression(paste(Area (km^2))))) +
  scale_color_manual(NULL, values = colors) + scale_fill_manual(NULL, values = colors) + scale_shape_manual(NULL, values = shapes) +
  scale_x_continuous(breaks = seq(1500, 4200, by = 300)) + xlab("Elevation (m)") +
  theme_classic() +
  theme(legend.text = element_text(size = 11), legend.position = c(0.3, 0.85), legend.background = element_blank(),
        axis.title = element_text(size = 13, colour = "black"), axis.text = element_text(size = 12, colour = "black"),
        panel.grid.major = element_line(colour = "grey90"), panel.border = element_rect(linetype = "solid", fill = NA),
        axis.text.y.right = element_text(color = "red"), axis.title.y.right = element_text(colour = "red"))
ggsave(file.path(FIG_OUT, "fig10_peak_swe_elevation.jpg"), width = 10, height = 5, units = "in", dpi = 300)

# ---- Fig. 11 ----
cap_peak <- 200; cap_anom <- 50                      # colour-scale caps of the published figure
rmean <- mean(rstack)
to_df <- function(r, what) {
  x <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  pivot_longer(x, -c(x, y), names_to = "id", values_to = "v") %>%
    mutate(id = factor(pid[match(id, make.names(pid))], levels = pid), what = what)
}
peak <- to_df(rstack, "peak") %>% mutate(v = pmin(v, cap_peak))
ranom <- rstack - rmean; names(ranom) <- names(rstack)
anom <- to_df(ranom, "anomaly") %>% mutate(v = pmax(pmin(v, cap_anom), -cap_anom))
th <- theme_void() + theme(strip.text = element_text(size = 13), legend.position = "right")
p1 <- ggplot(peak) + geom_raster(aes(x, y, fill = v)) + facet_wrap(~id, nrow = 1) + coord_equal() +
  scale_fill_viridis_c("pixel-wise peak\nSWE (cm)", direction = -1, limits = c(0, cap_peak)) + th
p2 <- ggplot(anom) + geom_raster(aes(x, y, fill = v)) + facet_wrap(~id, nrow = 1) + coord_equal() +
  scale_fill_gradientn("pixel-wise peak SWE\nanomaly (cm)", colours = c("#FF0000", "white", "#000099"),
                       limits = c(-cap_anom, cap_anom)) + th
ggarrange(p1, p2, ncol = 1)
ggsave(file.path(FIG_OUT, "fig11_peak_swe_maps.jpg"), width = 12, height = 10, units = "in", dpi = 300)
