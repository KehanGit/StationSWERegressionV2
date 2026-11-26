# Fig. 8 — Sierra-wide snow water storage (SWS), WY2004-2017.
# Fig. 9 — annual maximum SWS and its date, per product.
# From _original/plots/6_peakSWE_mean.R (first two sections).
# Inputs: Stat/streamflow/sierramean/*_SM_mean_200428.txt — daily domain-mean SWE (mm) on the common
#   500 m UTM grid (made by _original/pipeline/4_streamflow_sierramean*.R); the domain mask SNM_500.tif.
source("fig_config.R")
suppressPackageStartupMessages({ library(ggpubr); library(forcats); library(cowplot) })

path_sm <- file.path(P2_ROOT, "Stat", "streamflow", "sierramean")
files <- c(`LRM-FSCA`     = "20200509_LRM_V1_pl_wfsca_dm_UTM500_SM_mean_200428.txt",
           `LRM-baseline` = "20200509_LRM_V1_pl_wofsca_UTM500_SM_mean_200428.txt",
           SNODAS         = "SNODAS_UTM500_SM_mean_200428.txt",
           `NWM-SWE`      = "NWM_SWE_UTM500_SM_mean_200428.txt")
pid <- names(files)
colors <- setNames(c("#7F9F00", "#FC4E07", "#00AFBB", "#8106A9"), pid)
shapes <- setNames(c(15, 16, 17, 10), pid)

tem <- raster(file.path(P2_ROOT, "data", "template", "LRM_SNM_tem", "SNM_500.tif"))
area_km2 <- sum(!is.na(tem[])) * 500 * 500 / 1e6
wdata <- bind_rows(lapply(pid, function(p) {
  d <- read.csv(file.path(path_sm, files[[p]]))
  data.frame(date = d$dates, meanSWE = d$SM_mean, id = p)
})) %>%
  mutate(Date = as.Date(as.character(date), "%Y%m%d"), id = factor(id, levels = pid),
         volume = meanSWE * area_km2 / 1e6) %>%          # mm x km2 -> Gt
  filter(Date >= as.Date("2003-10-01"), Date < as.Date("2017-10-01"))

# ---- Fig. 8 ----
ggplot(wdata) +
  annotate("rect", xmin = as.Date("2011-10-01"), xmax = as.Date("2016-09-30"), ymin = -Inf, ymax = Inf, fill = "gray50", alpha = 0.4) +
  annotate("text", x = as.Date("2014-01-01"), y = 40, label = "2012-2016 drought", size = 5) +
  geom_line(aes(x = Date, y = volume, color = id), alpha = 0.8, linewidth = 1) +
  scale_color_manual(values = colors) +
  ylab("Sierra-wide snow water storage (Gt)") + xlab("Date") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  theme_bw() +
  theme(legend.text = element_text(size = 11), legend.position = c(0.1, 0.87),
        legend.title = element_blank(), legend.background = element_blank())
ggsave(file.path(FIG_OUT, "fig08_snow_storage.jpg"), width = 10, height = 4, units = "in", dpi = 300)

# ---- Fig. 9 ----
pw <- wdata %>% mutate(WY = as.numeric(format(Date, "%Y")) + (as.numeric(format(Date, "%m")) >= 10)) %>%
  group_by(WY, id) %>%
  summarise(peakv = volume[which.max(volume)], peakdate = Date[which.max(volume)], .groups = "drop") %>%
  mutate(doy = as.numeric(format(peakdate, "%j")), doy = ifelse(doy > 200, doy - 365, doy))  # Dec peaks -> negative
med <- pw %>% group_by(id) %>% summarise(doy = round(median(doy)), peakv = median(peakv))
print(med)

drought <- list(annotate("rect", xmin = 2011.5, xmax = 2016.5, ymin = -Inf, ymax = Inf, fill = "gray50", alpha = 0.4))
sc <- list(scale_color_manual(NULL, values = colors), scale_shape_manual(NULL, values = shapes))
f1 <- ggplot(pw, aes(WY, peakv, color = id)) + drought +
  annotate("text", x = 2014, y = 55, label = "2012-2016 drought") +
  geom_point(aes(shape = id), size = 2, alpha = 0.8) + geom_line(alpha = 0.8, linewidth = 0.5) + sc +
  ylab("Maximum SWS (Gt)") + ylim(0, 60) + scale_x_continuous(breaks = seq(2004, 2017, 2)) +
  theme_cowplot() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())
f2 <- ggplot(pw, aes(WY, doy, color = id)) + drought +
  geom_point(aes(shape = id), size = 2, alpha = 0.8) + geom_line(alpha = 0.8, linewidth = 0.5) + sc +
  ylab("Day of year") + xlab("Water Year") + ylim(-10, 120) + scale_x_continuous(breaks = seq(2004, 2017, 2)) +
  theme_cowplot() + theme(legend.text = element_text(size = 9), legend.position = c(0.05, 0.3), legend.background = element_blank())
f3 <- ggplot(pw) + geom_boxplot(aes(id, peakv, color = id), show.legend = FALSE) +
  geom_text(data = med, aes(x = id, y = 60, label = sprintf("%.1f Gt", peakv)), size = 3.6) + sc + ylim(0, 60) +
  theme_cowplot() + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks.x = element_blank())
f4 <- ggplot(pw) + geom_boxplot(aes(id, doy, color = id), show.legend = FALSE) +
  geom_text(data = med, aes(x = id, y = 120, label = paste(doy, "DOY")), size = 3.6) + sc + ylim(-10, 120) +
  theme_cowplot() + theme(axis.title = element_blank(), axis.text.y = element_blank(),
                          axis.text.x = element_text(angle = 30, hjust = 1))
p <- plot_grid(f1, f3, f2, f4, align = "hv", axis = "tblr", nrow = 2, rel_widths = c(7, 4),
               labels = c("A", "B", "C", "D"), label_fontface = "plain", label_x = c(0.12, 0.85, 0.12, 0.85), label_y = 0.92)
print(p)
ggsave(file.path(FIG_OUT, "fig09_max_snow_storage.jpg"), p, width = 11, height = 5.5, units = "in", dpi = 300)
