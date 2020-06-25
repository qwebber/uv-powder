
library(data.table)
library(ggplot2)
library(gridExtra)

dust <- fread("output/all.bats.csv")

num_of_samples = 100000
y <- rgamma(num_of_samples, shape = 1, scale = 3)
hist(y)

ks.test(dust$total.inf, y)

hist(y)

binwidth = 0.5
meanDust <- mean(log(dust$total.inf))
sdDust <-  sd(log(dust$total.inf))
n <- nrow(dust)


png("figures/FigureS2.png", width = 4000, height = 2500, units = "px", res = 600)
ggplot(dust) +
  geom_histogram(aes(total.inf), binwidth = 0.05, fill = "grey", color = "black") +
  ylab("Frequency") +
  xlab("Infection Intensity") +
  ggtitle('A)') +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

bb <- ggplot(dust, aes(log(total.inf))) +
  geom_histogram(binwidth = binwidth, fill = "grey", color = "black") +
  stat_function(fun = function(x) dnorm(x, mean = meanDust, 
                                        sd = sdDust) * n * binwidth,
                color = "black", size = 1) + 
  ylab("Frequency") +
  xlab("log(Infection Intensity)") +
  ggtitle('B)') +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
grid.arrange(aa,bb,nrow = 1)
dev.off()

