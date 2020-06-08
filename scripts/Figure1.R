

library(lme4)
library(data.table)
library(ggplot2)
library(effects)
library(gridExtra)
library(outliers)

### PATIENT ZERO ANALYSIS
M1 = lm(log(total.inf.M) ~ sex + act_hb + exp_hb + act_ym  + soc_ym + min.ta, data = p.zero)
summary(M1)

estM1 <- Effect("exp_hb", partial.residuals=T, M1)
predM1 <- data.table(inf = estM1$fit, 
                     exp = estM1$x, 
                     lwr = estM1$lower,
                     upr = estM1$upper)
colnames(predM1) <- c("inf", "exp", "lwr", "upr")


### ACQUISITION ANALYSIS #####
dust <- fread("output/all.bats.csv")

FFdf <- dust[sex == "F" & sex_pz == "F"] #& exp_hb < 4]
FF = lmer(log(total.inf) ~ soc_ym  + act_ym + exp_hb + act_hb + dawn.ta + (1|Trial), 
          data = FFdf)
summary(FF)

grubbs.test(FFdf$exp_hb, type = 10, opposite = FALSE, two.sided = FALSE)

estF<-Effect("exp_hb", partial.residuals=T, FF)
predF <- data.table(inf = estF$fit, 
                   exp = estF$x, 
                   lwr = estF$lower,
                   upr = estF$upper)
colnames(predF) <- c("inf", "exp", "lwr", "upr")

MMdf <- dust[sex == "M" & sex_pz == "M"]
MM = lmer(log(total.inf) ~ soc_ym  + act_ym + exp_hb + act_hb + dawn.ta + (1|Trial), 
          data = MMdf)

grubbs.test(MMdf$soc_ym, type = 10, opposite = FALSE, two.sided = FALSE)


estM<-Effect("soc_ym", partial.residuals=T, MM)
predM <- data.table(inf = estM$fit, 
                    soc = estM$x, 
                    lwr = estM$lower,
                    upr = estM$upper)
colnames(predM) <- c("inf", "soc", "lwr", "upr")

a1 <- lmer (log(total.inf) ~ sex*sex_pz + soc_ym  + act_ym + exp_hb + act_hb + dawn.ta + (1|Trial)
            , data = dust)
summary(a1)

ggplot(dust) +
  geom_boxplot(aes(sex, total.inf), notch = T) +
  facet_wrap(~sex_pz)

png("figures/Figure1.png", width = 5000, height = 2000, units = "px", res = 600)
aa <- ggplot(p.zero, aes(exp_hb, log(total.inf.M))) +
  geom_point(alpha = 0.25) +
  geom_line(data = predM1, aes(exp, inf), lty = 1, size = 1) +
  geom_line(data = predM1, aes(exp, lwr), lty = 2) +
  geom_line(data = predM1, aes(exp, upr), lty = 2) +
  ggtitle("A) Males (Transmission)") +
  ylab("log(Average Infection Intensity)") +
  xlab(expression("Exploration PC2" [H], ")" )) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
bb <- ggplot(dust[sex == "F" & sex_pz == "F"],
       aes(exp_hb, log(total.inf))) +
  geom_point(alpha = 0.25) +
  geom_line(data = predF, aes(exp, inf), lty = 1, size = 1) +
  geom_line(data = predF, aes(exp, lwr), lty = 2) +
  geom_line(data = predF, aes(exp, upr), lty = 2) +
  ylab("log(Infection Intensity)") +
  xlab(expression("Exploration PC2" [H], ")" )) +
  ggtitle("B) Females (Acquisition)") +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
  
cc <- ggplot(dust[sex == "M" & sex_pz == "M"],
               aes(soc_ym, log(total.inf))) +
    geom_point(alpha = 0.25) +
    geom_line(data = predM, aes(soc, inf), lty = 1, size = 1) +
    geom_line(data = predM, aes(soc, lwr), lty = 2) +
    geom_line(data = predM, aes(soc, upr), lty = 2) +
    ylab("log(Infection Intensity)") +
    xlab(expression("Sociability PC2" [Y], ")" )) +
    ggtitle("C) Males (Acquisition)") +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
grid.arrange(aa,bb,cc, nrow = 1)
dev.off()