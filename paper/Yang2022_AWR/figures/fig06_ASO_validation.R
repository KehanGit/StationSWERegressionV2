# Fig. 6 — daily accuracy against ASO lidar SWE (Tuolumne): time series (A-D), box plots of the
# four products on common dates (a1-d1) and of the two LRM models on all dates (a2-d2).
# From _original/plots/3_validation_ASO.R. The published figure was assembled from separate panels;
# this script draws it in one go.
# Inputs: figures/3_validation_ASO/data/<n><product>_ASO_vali.txt — per ASO date, statistics of the
#   500 m product against ASO SWE over the basin (made by _original/pipeline/3_validation_ASO.R).
source("fig_config.R")
suppressPackageStartupMessages({ library(ggpubr); library(forcats) })

path_aso <- file.path(P2_ROOT, "figures", "3_validation_ASO")
fn <- sort(list.files(file.path(path_aso, "data"), "_ASO_vali.txt$", full.names = TRUE))
pid <- c("LRM-FSCA", "LRM-baseline", "SNODAS", "NWM-SWE")
colors <- setNames(c("#7F9F00", "#FC4E07", "#00AFBB", "#8106A9"), pid)
shapes <- setNames(c(15, 16, 17, 10), pid)

# ASO dates in the paper: ASO flights on LRM days that pass the station QC of Fig. 4 (> 40 stations,
# Jan-Jun), except 2015-04-09 and 2015-04-15: very low SWE dates in the extreme low-snow WY2015,
# left out by the author (see paper).
dates <- c(20130403, 20130429, 20140323, 20140407, 20140413, 20140420, 20140428, 20140502, 20150217,
           20150305, 20150325, 20160326, 20160401, 20160407, 20160416, 20170303, 20170401, 20170502,
           20170604, 20180423, 20190324, 20190417, 20190503)

stat <- bind_rows(lapply(seq_along(fn), function(i) read.csv(fn[i]) %>%
  transmute(Date, id = pid[i], R2, RMSE = RMSE / 10, RMAE = MAE / Mean_obs * 100, PBIAS = PBIAS_old))) %>%
  filter(Date %in% dates) %>% mutate(id = factor(id, levels = pid), Date = factor(Date, levels = dates))
common <- stat %>% group_by(Date) %>% filter(n() == length(pid)) %>% ungroup()
lrm <- stat %>% filter(id %in% pid[1:2])
message("dates: ", length(dates), "; with all four products: ", length(unique(common$Date)))

metrics <- c("R2", "RMSE", "RMAE", "PBIAS")
ylabs <- list(expression("R"^2), "RMSE (cm)", "RMAE (%)", "PBIAS (%)")
ymins <- c(0, 0, 0, -70); ymaxs <- c(1.1, 80, 100, 150)
L <- c("A", "B", "C", "D")
wy <- as.numeric(substr(dates, 1, 4))
shade <- do.call(rbind, lapply(unique(wy)[c(TRUE, FALSE)], function(y)
  data.frame(xmin = min(which(wy == y)) - 0.5, xmax = max(which(wy == y)) + 0.5)))
th <- theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 13, colour = "black"),
            axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, colour = "black"),
            axis.text.y = element_text(size = 12, colour = "black"),
            panel.grid.major = element_line(colour = "grey90"), panel.background = element_blank(),
            panel.border = element_rect(linetype = "solid", fill = NA))
hline <- function(j) if (metrics[j] == "PBIAS") geom_hline(yintercept = 0, colour = "red", linetype = "dashed") else NULL
lab <- function(j, s) annotate("text", x = -Inf, y = Inf, label = s, hjust = -0.3, vjust = 1.3, size = 7)

panels <- list()
for (j in seq_along(metrics)) {
  m <- metrics[j]
  ts <- ggplot(stat) +
    geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "gray50", alpha = 0.15, inherit.aes = FALSE) +
    hline(j) +
    geom_point(aes(x = Date, y = .data[[m]], color = id, shape = id), size = 2.5, alpha = 0.8) +
    lab(j, L[j]) + ylab(ylabs[[j]]) + ylim(ymins[j], ymaxs[j]) +
    scale_color_manual(NULL, values = colors) + scale_shape_manual(NULL, values = shapes) + th
  b1 <- ggplot(common) + hline(j) + geom_boxplot(aes(x = id, y = .data[[m]], color = id), alpha = 0.8) +
    lab(j, paste0(tolower(L[j]), "1")) + ylim(ymins[j], ymaxs[j]) + scale_color_manual(NULL, values = colors) +
    th + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
  b2 <- ggplot(lrm) + hline(j) + geom_boxplot(aes(x = id, y = .data[[m]], color = id), alpha = 0.8) +
    lab(j, paste0(tolower(L[j]), "2")) + ylim(ymins[j], ymaxs[j]) + scale_color_manual(NULL, values = colors) +
    th + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
  if (j <= 2) { ts <- ts + theme(axis.text.x = element_blank()); b1 <- b1 + theme(axis.text.x = element_blank()); b2 <- b2 + theme(axis.text.x = element_blank()) }
  panels[[j]] <- ggarrange(ts, b1, b2, nrow = 1, widths = c(8, 2.6, 1.4), align = "h", legend = "none")
  print(stat %>% group_by(id) %>% summarise(mean = mean(.data[[m]], na.rm = TRUE), median = median(.data[[m]], na.rm = TRUE)) %>% mutate(metric = m))
}
leg <- get_legend(ggplot(stat) + geom_point(aes(x = Date, y = R2, color = id, shape = id), size = 3) +
                    scale_color_manual(NULL, values = colors) + scale_shape_manual(NULL, values = shapes) +
                    theme(legend.position = "top", legend.key = element_blank(), legend.text = element_text(size = 13)))
ggarrange(as_ggplot(leg), ggarrange(panels[[1]], panels[[2]], panels[[3]], panels[[4]], nrow = 2, ncol = 2, heights = c(1, 1.35)),
          ncol = 1, heights = c(0.08, 1))
ggsave(file.path(FIG_OUT, "fig06_ASO_validation.jpg"), width = 16, height = 8, units = "in", dpi = 300)
