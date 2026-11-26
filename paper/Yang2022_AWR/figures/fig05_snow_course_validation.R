# Fig. 5 — modelled SWE vs. FSCA-scaled snow course SWE, coloured by elevation.
# From _original/plots/2_validation_snow_course.R (step 2, "review" version, elevation panels).
# Inputs: figures/2_validation_snow_course/data/*_sc_review.txt — per snow course and date, the model
#   pixel (of the 9 around the course) closest to the FSCA-scaled observation; built by the
#   extraction part of the original script from the ground_ext tables.
source("fig_config.R")
suppressPackageStartupMessages({ library(ggpubr); library(RColorBrewer) })

path_data <- file.path(P2_ROOT, "figures", "2_validation_snow_course", "data")
fn_sc <- sort(list.files(path_data, glob2rx("*sc_review.txt"), full.names = TRUE))
id <- sapply(strsplit(basename(fn_sc), "_sc"), "[", 1)
pid <- c("LRM-FSCA", "LRM-baseline", "SNODAS", "NWM-SWE")

rd <- function(i) {
  d <- read.csv(fn_sc[i], stringsAsFactors = FALSE)
  names(d)[6:9] <- paste0(id[i], c("_sc_scaled", "_fsca", "_ori", "_ori_wofsca"))
  d
}
all <- rd(1)
for (i in 2:length(fn_sc)) {
  d <- rd(i)
  all <- merge(all, d[, c("Site_ID", "DateObs", names(d)[6:9])], by = c("Site_ID", "DateObs"))
}
sc_info <- read.csv(file.path(P2_ROOT, "data", "studyarea", "snowcourse_info.txt"))
names(sc_info)[2] <- "Site_ID"
sc_buff <- read.csv(file.path(P2_ROOT, "data", "studyarea", "snowcourse_info_2kmbuff.txt"))
date_qc <- read.csv(file.path(P2_ROOT, "figures", "2_validation_snow_course", "LRM_date_mdGT40.txt"))
all <- all[all$Site_ID %in% sc_buff$STA & all$DateObs %in% date_qc$Date, ]
all <- merge(all, sc_info[, c("Site_ID", "Elevation")], by = "Site_ID")
all$Elevation_m <- all$Elevation * 0.3048  # ft -> m
message("pairs: ", nrow(all))  # 6592 in the revised paper

myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
xloc <- 120; yloc <- 50; yint <- 15; legend_size <- 4.2
p <- list(); stats <- list()
for (i in seq_along(fn_sc)) {
  df <- data.frame(SWE = all[[paste0(id[i], "_sc_scaled")]], swe_scaled = all[[paste0(id[i], "_ori")]],
                   Elevation = all$Elevation_m)
  r2 <- cor(df$SWE, df$swe_scaled)^2
  rmae <- hydroGOF::mae(df$SWE, df$swe_scaled) / mean(all$SWE) * 100
  rmse <- hydroGOF::rmse(df$SWE, df$swe_scaled) / 10
  pbias <- sum(df$swe_scaled - df$SWE) / sum(df$SWE) * 100
  stats[[i]] <- data.frame(product = pid[i], r2, rmae, rmse_cm = rmse, pbias)
  p[[i]] <- ggplot(df, aes(x = SWE / 10, y = swe_scaled / 10)) +
    geom_point(aes(color = Elevation), alpha = 0.5) +
    geom_abline(slope = 1, color = "black", linewidth = 1) +
    geom_smooth(method = "lm", formula = y ~ x, color = "red", linewidth = 1) +
    annotate("text", x = xloc, y = yloc, label = paste("R^2: ", format(round(r2, 2), nsmall = 2)), hjust = 0, size = legend_size, parse = TRUE) +
    annotate("text", x = xloc, y = yloc - yint, label = paste0("RMAE: ", format(round(rmae, 1), nsmall = 1), " %"), hjust = 0, size = legend_size) +
    annotate("text", x = xloc, y = yloc - 2 * yint, label = paste0("RMSE: ", format(round(rmse, 1), nsmall = 1), " cm"), hjust = 0, size = legend_size) +
    annotate("text", x = xloc, y = yloc - 3 * yint, label = paste0("PBIAS: ", format(round(pbias, 1), nsmall = 1), " %"), hjust = 0, size = legend_size) +
    xlim(c(0, 200)) + ylim(c(0, 200)) +
    ylab(paste(pid[i], "(cm) ")) + xlab("Snow course SWE (cm)") +
    scale_colour_gradientn("Elevation (m)", colours = myPalette(100), limits = range(df$Elevation)) +
    theme(axis.title = element_text(size = 13, color = "black"), axis.text = element_text(size = 12, color = "black"),
          panel.background = element_blank(), panel.grid = element_blank(), axis.line = element_line(colour = "black"),
          legend.key.height = unit(0.5, "cm"), legend.key.width = unit(1.5, "cm"), legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
}
print(do.call(rbind, stats))
ggarrange(plotlist = p, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")
ggsave(file.path(FIG_OUT, "fig05_snow_course_validation.jpg"), width = 8, height = 8.6, units = "in", dpi = 300)
