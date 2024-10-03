library(ggplot2)
library(scales)

heights <- c(48, 48.2, 49, 50.1, 50.2, 50.7, 51.1, 51.4, 51.7, 52, 52.4, 53.6, 53.7, 54.1, 54.2, 55, 55.1, 57.8)
weights <- (heights * 1.2) + rnorm(18, 0, 8)
mf <- c(1,0,0,1,1,0,0,1,0,1,0,0,0,0,1,0,1,1)

df <- data.frame('h'=heights, 'w'=weights, 'mf'=mf)

ggplot(df, aes(x = h)) + geom_density(linewidth=2, color="#00AFBB") + 
  xlab("Height") + ylab("Likelihood a random kid is this height") + theme_light(base_size = 16) +
  xlim(40, 64) +
  scale_y_continuous(labels = percent) + theme(panel.grid.minor = element_blank())



ggplot(df, aes(x = h, y = w)) + geom_point(color="#00AFBB", size=3) +
  xlab("Height") + ylab("Weight")  + theme_light(base_size = 16) + theme(panel.grid.minor = element_blank())

ggplot(df, aes(as.factor(mf), fill=as.factor(mf), h)) + geom_boxplot() + 
  theme(panel.grid.minor = element_blank()) + theme_light(base_size = 16) + ylab("Height") + xlab("Sex") +
  scale_fill_manual(labels=c('Female', 'Male'), values = c("#43CB35", "#6DAEEB")) + labs(fill = "Heights") 
