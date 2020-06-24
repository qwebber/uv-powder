

library(data.table)
library(ggplot2)
library(gridExtra)
library(effects)

dust <- fread("output/all.bats.csv")


allMod = lmer(log(total.inf) ~ sex*soc_ym  + act_ym + sex*exp_hb + act_hb + dawn.ta + (1|Trial), 
          data = dust)

summary(allMod)

ggplot(dust, aes(exp_hb, log(total.inf), color = sex)) +
  geom_point() +
  geom_smooth(method = "lm")


estMod<-Effect(c("sex", "exp_hb"), partial.residuals = T, allMod)
predMod <- data.table(inf = estMod$fit, 
                     exp = estMod$x, 
                     lwr = estMod$lower,
                     upr = estMod$upper)
colnames(predMod) <- c("inf","Sex" ,"exp", "lwr", "upr")


col = c("#1b9e77", "#1f78b4")

png("figures/Figure3.png", width = 2500, height = 2500, units = "px", res = 600)
ggplot() +
  geom_point(data = dust, aes(exp_hb, log(total.inf), color = sex), alpha = 0.5) +
  geom_line(data = predMod, aes(exp, inf, color = Sex), lty = 1, size = 1) +
  geom_line(data = predMod, aes(exp, upr, color = Sex), lty = 2, size = 0.5) +
  geom_line(data = predMod, aes(exp, lwr, color = Sex), lty = 2, size = 0.5) +
  ylab("log(Infection Intensity)") +
  xlab(expression("Exploration PC2" [H], ")" )) +
  scale_color_manual(values = col) +
  #labs(x = expression(paste("Ambient Temperature at Dawn (",degree~C,")"))) +
  theme(legend.position = c(0.85, 0.15),
        legend.key = element_blank(),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()
