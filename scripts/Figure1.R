

library(data.table)
library(ggplot2)
library(gridExtra)
library(fitdistrplus)

dust <- fread("output/all.bats.csv")

## Negative Binomial distribution
dust$total.inf2 <- round(dust$total.inf, 2)*100

negbin <- dust[, .N, by = "total.inf2"]
colnames(negbin) <- c("N", "Frequency")

negbin <- negbin[order(N),] 

negbinfill <- data.table(N = c(0:65))

negbin <- merge(negbin, negbinfill, by = "N", all = T)
negbin[is.na(negbin)] <- 0

N <- sum(negbin$Frequency)
RF <- negbin$Frequency/N
DF <- data.frame(negbin, round(RF, 5))
MEAN <- sum(RF * negbin$N)
VAR <- (sum(negbin$N^2*negbin$frequency) - N*MEAN^2)/(N-1) # else use (MEAN+MEAN^2/R)
DISP <- VAR/MEAN # over dispersion
THETA <- 1/DISP
R <- MEAN*THETA/(1-THETA) # MEAN = R(1-THETA)/THETA
cbind(MEAN,VAR,DISP,THETA,R)

x = negbin$N
r = fitdistr(dust$total.inf2,'Negative Binomial')$estimate[1]
negbin2 = data.table(Frequency = round(N * dnbinom(x, size = r, mu = MEAN),5),
                     N = c(0:68))

negbin2$type = "Expected Negative Binomial"
negbin$type = "Observed"

DT <- rbind(negbin, negbin2)

## Add extra rows to add space at the edges of the barplot
aa <- data.table(N = c(-1, -1, 69, 69), 
           Frequency = c(0,0, 0, 0),
           type = c("Observed", "Expected Negative Binomial", 
                    "Observed", "Expected Negative Binomial"))
DT2 <- rbind(DT,aa)
DT2 <- DT2[order(N),] 

## 
chisq.test(rbind(negbin$Frequency,negbin2$Frequency))


png("figures/Figure1.png", width = 8000, height = 4000, units = "px", res = 600)
aa <- 
  ggplot(DT2) +
  geom_bar(aes(y=Frequency, x = factor(N), fill=type), 
           color = "black",
           stat = "identity", 
           position = position_dodge2(preserve = "total")) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c("#d95f02","#7570b3")) + 
  scale_x_discrete(breaks = c("0", "10",  "20", 
                               "30", "40", 
                              "50", "60"), 
                   labels = c("0" = "0", 
                              "10" = "0.10", 
                              "20" = "0.20", 
                              "30" = "0.30",
                              "40" = "0.40", 
                              "50" = "0.50", 
                              "60" = "0.60")) +
  ylab("Frequency") +
  xlab("Infection Intensity") +
  ggtitle('A)') +
  theme(legend.position = c(0.82,0.9),
        legend.title = element_blank(),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
  
meanDust = mean(log(dust$total.inf))
sdDust = sd(log(dust$total.inf))
n = nrow(dust)
binwidth = 0.25

bb <- ggplot(dust, aes(log(total.inf))) +
  geom_histogram(fill = "#7570b3", color = "black") +
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

