# Fig. 4 — monthly distribution of daily Monte Carlo cross-validation statistics (snow pillows).
# From _original/plots/1_cross_validation_plot.R (steps 1-3).
# Input: figures/1_cross_validation/data/v2/LRM_{FSCA,baseline}.txt — one row per model day with the
#   cross-validation statistics and `model` = number of stations used. These were summarised from the
#   run outputs by LRM_qc.R (usefulltools repo).
source("fig_config.R")
suppressPackageStartupMessages({ library(ggpubr); library(forcats) })

path_cv <- file.path(P2_ROOT, "figures", "1_cross_validation", "data", "v2")
meg_data <- bind_rows(lapply(c("LRM_baseline", "LRM_FSCA"), function(id)
  cbind.data.frame(read.csv(file.path(path_cv, paste0(id, ".txt"))), ID = id)))

# QC (paper Section 3.4.1): Jan-Jun only, and days with more than 40 stations
qc <- meg_data %>% mutate(month = substr(Date, 5, 6)) %>%
  filter(month %in% sprintf("%02d", 1:6), model > 40)
qc <- qc[complete.cases(qc), ]
qc$rmse <- qc$rmse * 100  # m -> cm
alldate <- merge(qc[qc$ID == "LRM_FSCA", ], qc[qc$ID == "LRM_baseline", ], by = "Date")
message("days with both models: ", nrow(alldate))

colors <- c("#7F9F00", "#FC4E07")
pid <- c("LRM-FSCA", "LRM-baseline")
stat_list <- c(expression("R"^2), "RMSE (cm)", "RMAE (%)", "Basin-wide PBIAS (%)")
id_list <- c("r2", "rmse", "pctmae", "pbias")
ymins <- c(0.25, 0, 20, -10); ymaxs <- c(0.80, 55, 175, 50)

f <- list()
for (i in seq_along(id_list)) {
  p <- rbind(cbind.data.frame(Date = alldate$Date, value = alldate[[paste0(id_list[i], ".y")]], id = "baseline"),
             cbind.data.frame(Date = alldate$Date, value = alldate[[paste0(id_list[i], ".x")]], id = "fsca")) %>%
    mutate(Date = as.Date(as.character(Date), "%Y%m%d"), order = as.numeric(format(Date, "%m")),
           mth2 = format(Date, "%b"), idorder = ifelse(id == "fsca", 1, 2))
  print(p %>% group_by(id) %>% summarise(mean = mean(value), median = median(value), sd = sd(value)))
  f[[i]] <- p %>% mutate(mth2 = fct_reorder(mth2, order), id = fct_reorder(id, idorder)) %>%
    ggplot() +
    geom_boxplot(aes(x = mth2, y = value, fill = id), show.legend = (i == 3), alpha = 0.8, outlier.color = NA) +
    ylab(stat_list[i]) + xlab("Month") + ylim(c(ymins[i], ymaxs[i])) +
    scale_fill_manual(values = colors, labels = pid) +
    theme_bw() +
    theme(legend.text = element_text(size = 12), legend.position = c(0.45, 0.8),
          legend.title = element_blank(), legend.background = element_blank(),
          axis.title = element_text(size = 13, color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.border = element_rect(linetype = "solid", fill = NA),
          axis.line = element_line(colour = "black"))
}
# average number of model days per month per water year (annotated in panel A of the paper)
n_wy <- length(unique(ifelse(as.numeric(substr(alldate$Date, 5, 6)) > 9, 1, 0) + as.numeric(substr(alldate$Date, 1, 4))))
counts <- alldate %>% mutate(m = as.numeric(substr(Date, 5, 6))) %>% count(m) %>%
  mutate(mth2 = month.abb[m], lab = round(n / n_wy))
f[[1]] <- f[[1]] + annotate("text", x = counts$mth2, y = 0.3, label = counts$lab, size = 7)
ggarrange(f[[1]] + rremove("xlab"), f[[2]] + rremove("xlab"), f[[3]], f[[4]],
          heights = c(4, 4.2), widths = c(4.2, 4), nrow = 2, ncol = 2,
          labels = c("A", "B", "C", "D"), font.label = list(size = 16, face = "plain"),
          label.x = 0.10, label.y = 0.95)
ggsave(file.path(FIG_OUT, "fig04_cross_validation.jpg"), width = 10, height = 6, units = "in", dpi = 300)
write.csv(qc, file.path(FIG_OUT, "fig04_qc_th_model_40.csv"), row.names = FALSE)
