# Fig. 7 — normalised SWE residuals (product minus ASO) over the Tuolumne basin on five ASO dates.
# From _original/plots/3_1_validation_ASO_error_maps.R (rasterVis/rgdal replaced by ggplot2/sf).
# Inputs: figures/3_validation_ASO/data/images/ASO_Diff/<product>/<product>_<date>.tif — residuals in cm
#   on the ASO 500 m UTM grid (made by _original/plots/3_1_validation_ASO_error_analysis.R, step 1).
# Residuals are divided, per date, by the largest absolute residual of that date.
# Notes on the published figure:
#   - the fourth column header reads 2016-03-03, a typo: there was no ASO flight that day, and the
#     maps are from the 2016-03-26 flight (first 2016 flight; same as the original script's datelist);
#   - the "Error range" printed under each date also included the LRM-noREC run (not shown), which
#     only matters for 2017-04-01 (max 210 cm from noREC; 155 cm for the four products shown).
source("fig_config.R")
suppressPackageStartupMessages(library(sf))

path_diff <- file.path(P2_ROOT, "figures", "3_validation_ASO", "data", "images", "ASO_Diff")
basin <- st_read(file.path(SHAPE_DIR, "TuolumneBasin_ASO_UTM11.shp"), quiet = TRUE)
products <- c(wfscadm = "LRM-FSCA", wofsca = "LRM-baseline", SNODAS = "SNODAS", NWM = "NWM-SWE")
dates <- c("20130403", "20140407", "20150217", "20160326", "20170401")

df <- list(); ranges <- list()
for (d in dates) {
  r <- stack(file.path(path_diff, c(names(products), "norec"), paste0(c(names(products), "norec"), "_", d, ".tif")))
  v <- getValues(r)
  ranges[[d]] <- data.frame(date = d, max = max(v, na.rm = TRUE), min = min(v, na.rm = TRUE))
  scale_d <- max(abs(range(v, na.rm = TRUE)))
  for (k in seq_along(products)) {
    x <- as.data.frame(r[[k]], xy = TRUE, na.rm = TRUE)
    names(x)[3] <- "res"
    df[[length(df) + 1]] <- transform(x, res = res / scale_d, product = products[k], date = d)
  }
}
df <- bind_rows(df) %>% mutate(product = factor(product, levels = products),
                               date = factor(format(as.Date(date, "%Y%m%d")), levels = format(as.Date(dates, "%Y%m%d"))))
ranges <- bind_rows(ranges) %>% mutate(lab = sprintf("Max: %.0f cm\nMin: %.0f cm", max, min))
print(ranges)

ggplot(df) +
  geom_raster(aes(x, y, fill = res)) +
  geom_sf(data = basin, fill = NA, colour = "gray40", linewidth = 0.3) +
  facet_grid(product ~ date, switch = "y") +
  scale_fill_gradientn("Normalized SWE residuals", colours = c("#FF0000", "white", "#000099"),
                       limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  labs(caption = paste(format(as.Date(ranges$date, "%Y%m%d")), gsub("\n", ", ", ranges$lab), collapse = "    ")) +
  theme_void() +
  theme(strip.text = element_text(size = 13, face = "bold"), strip.text.y.left = element_text(angle = 0),
        legend.position = "bottom", legend.key.width = unit(3, "cm"), legend.title.position = "bottom",
        legend.title = element_text(size = 14, hjust = 0.5), plot.caption = element_text(size = 9, hjust = 0.5))
ggsave(file.path(FIG_OUT, "fig07_ASO_error_maps.jpg"), width = 13, height = 11, units = "in", dpi = 300)
