

library(data.table)
library(ggplot2)
library(gridExtra)
library(effects)

dust <- fread("output/all.bats.csv")

dust[, .N, by = "sex"]

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

#labs(x = expression(paste("Ambient Temperature at Dawn (",degree~C,")"))) +


col = c("#1b9e77", "#1f78b4")

png("figures/Figure1.png", width = 5000, height = 2500, units = "px", res = 600)
aa <- ggplot() +
  geom_point(data = dust[sex == "F"], aes(exp_hb, log(total.inf), col = "#1b9e77"), alpha = 0.5) +
  geom_line(data = predMod[Sex == "F"], aes(exp, inf, col = "#1b9e77"), lty = 1, size = 1.25) +
  geom_line(data = predMod[Sex == "F"], aes(exp, upr, col = "#1b9e77"), lty = 2, size = 0.5) +
  geom_line(data = predMod[Sex == "F"], aes(exp, lwr, col = "#1b9e77"), lty = 2, size = 0.5) +
  ylab("log(Infection Intensity)") +
  xlab(expression("Exploration PC2" [H], ")" )) +
  scale_color_manual(values = "#1b9e77") +
  ylim(-6.5, 0) +
  ggtitle("A) Females") +
  #labs(x = expression(paste("Ambient Temperature at Dawn (",degree~C,")"))) +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
bb <- ggplot() +
  geom_point(data = dust[sex == "M"], aes(exp_hb, log(total.inf), col = "#1f78b4"), alpha = 0.5) +
  geom_line(data = predMod[Sex == "M"], aes(exp, inf, col = "#1f78b4"), lty = 1, size = 1.25) +
  geom_line(data = predMod[Sex == "M"], aes(exp, upr, col = "#1f78b4"), lty = 2, size = 0.5) +
  geom_line(data = predMod[Sex == "M"], aes(exp, lwr, col = "#1f78b4"), lty = 2, size = 0.5) +
  ylab("log(Infection Intensity)") +
  xlab(expression("Exploration PC2" [H], ")" )) +
  #labs(x = expression(paste("Ambient Temperature at Dawn (",degree~C,")"))) +
  scale_color_manual(values = "#1f78b4") +
  ggtitle("B) Males") +
  ylim(-6.5, 0) +
  xlim(-2.5, 3.5) +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
grid.arrange(aa,bb,nrow = 1)
dev.off()
