

library(data.table)
library(ggplot2)
library(gridExtra)
library(effects)

dust <- fread("output/all.bats.csv")


allMod = lmer(log(total.inf) ~ sex*soc_ym  + act_ym + sex*exp_hb + act_hb + dawn.ta + (1|Trial), 
              data = dust)

summary(allMod)

estMod<-Effect(c("dawn.ta"), partial.residuals = T, allMod)
predMod <- data.table(inf = estMod$fit, 
                      ta = estMod$x, 
                      lwr = estMod$lower,
                      upr = estMod$upper)
colnames(predMod) <- c("inf","ta", "lwr", "upr")

png("figures/Figure2.png", width = 2500, height = 2500, units = "px", res = 600)
ggplot() +
  geom_jitter(data = dust, aes(dawn.ta, log(total.inf), col = "black"), alpha = 0.5, width = 1) +
  geom_line(data = predMod, aes(ta, inf, col = "black"), lty = 1, size = 1.25) +
  geom_line(data = predMod, aes(ta, upr, col = "black"), lty = 2, size = 0.5) +
  geom_line(data = predMod, aes(ta, lwr, col = "black"), lty = 2, size = 0.5) +
  ylab("log(Infection Intensity)") +
  labs(x = expression(paste("Ambient Temperature at Dawn (",degree~C,")"))) +
  scale_color_manual(values = "black") +
  ylim(-6.5, 0) +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()
