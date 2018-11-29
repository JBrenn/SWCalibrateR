# Figure5 publication SMCcalibration

x <- c(1.25,1.24,1.26,1.5, 1.7, 2.2,2.3,2,1.8, 2  , 
  5.5,5,5.7,6.8, 7.4, 12,7.6 ,9,12.5)
y <- c(7.4, 0   ,2.3 ,2  ,-2  ,-1  ,0.5  ,1,0.5,-0.5, 
  0  ,0.6 ,0.2,0.5,-0.6,2.3,-7.4,6, 6.3)   
type <- as.factor(c("Outlier", rep("Regular", 9), rep("Leverage", 6), rep("Outlier and Leverage", 3)))

data <- data.frame(x=x, y=y, type=type)

library(ggplot2)

gg <- ggplot(data, aes(x=x, y=y, col=type)) +
  geom_point() + 
  geom_hline(yintercept = c(-2.5, 2.5), col=grey.colors(1,.25,.25,alpha=.5)) +
  geom_vline(xintercept = 3, col=grey.colors(1,.25,.25,alpha=.35)) +
  theme(legend.position="bottom") + 
  xlab("Robust Distances") + 
  ylab("Standardised Robust Residuals") +
  # cols: green, , red, black, blue, 
  scale_color_manual(values=c("#2c7bb6", "#ca0020", "#008837" ,"#404040"))

ggsave(filename = "./data-raw/vis_roblm.png", plot = gg)
