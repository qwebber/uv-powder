

library(data.table)
library(ggplot2)
library(lmerTest)
library(piecewiseSEM)
library(simr)

dust <- fread("output/all.bats.csv")


allMod = lmer(log(total.inf) ~ sex + soc_ym  + act_ym + exp_hb + act_hb + dawn.ta + (1|Trial), 
          data = dust)

r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(allMod)
1-var(residuals(allMod))/(var(model.response(model.frame(allMod))))

power <- powerSim(allMod,nsim = 200)



Vcov <- vcov(allMod, useScale = FALSE)
betas <- fixef(allMod)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval),3)

estMod<-Effect("dawn.ta", partial.residuals=T, allMod)
predMod <- data.table(inf = estMod$fit, 
                     ta = estMod$x, 
                     lwr = estMod$lower,
                     upr = estMod$upper)
colnames(predMod) <- c("inf", "ta", "lwr", "upr")


png("figures/Figure3.png", width = 2500, height = 2500, units = "px", res = 600)
ggplot(dust, aes(dawn.ta, log(total.inf))) +
  geom_jitter(width = 0.25, alpha = 0.25) +
  geom_line(data = predMod, aes(ta, inf), lty = 1, size = 1) +
  geom_line(data = predMod, aes(ta, lwr), lty = 2) +
  geom_line(data = predMod, aes(ta, upr), lty = 2) +
  ylab("log(Infection Intensity)") +
  labs(x = expression(paste("Ambient Temperature at Dawn (",degree~C,")"))) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()
