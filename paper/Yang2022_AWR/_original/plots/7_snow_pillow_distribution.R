a <- raster('D:/GoogleDrive/Research/phd program/DissertationProposal/data/studyarea/NLCD2011_landcover_TU.tif')

plot(a)
summary(getValues(a))
sum(a[]==52) 
sum(!is.na(a[]))
sum(a[]==52) / sum(a[]<100)  # shrub 0.1497941

sum(a[]==42) / sum(a[]<100) # 0.115071 ever green
unique(getValues(a))


# plot snow pillow elevation distributions
dfalpha <- 0.1
dfsize <- 250
axis_x_size <- 16
axis_y_size <- 16
axis_labx_size <- 15
axis_laby_size <- 15
legend_size <- 4.2
dflinecolor <- 'red'
library(ggplot2)


data <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/Stat/snow_pillows_used_20210131.txt')
ggplot(data, aes(x=Elev_m_)) + 
  geom_histogram(aes(y = stat(count)),# Histogram with density instead of count on y-axis
                 binwidth=100,
                 colour="black", fill="white") +
  # geom_histogram(binwidth=100, colour="black", fill="white")+
  geom_density(aes(y = stat(count)*113),alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot+
  ylab('Number of Stations')+
  xlab('Elevation (m)')+
  theme_bw()+
  theme(legend.text = element_text(size=14),
        legend.position = c(0.45,0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title.x =element_text(size = axis_x_size, color = 'black'),
        axis.title.y = element_text(size=axis_y_size, color = 'black'),
        axis.text.x = element_text(size = axis_labx_size, angle =0,vjust = 0.5, color = 'black'),
        axis.text.y = element_text(size = axis_laby_size, color = 'black'),
        panel.background = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.line = element_line(colour = "black"))
outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/0_study_area/snow_pillow_ele_distributions_20210131_v5.jpg'
ggsave(outname, width = 8, height=5, units='in',dpi = 600)
