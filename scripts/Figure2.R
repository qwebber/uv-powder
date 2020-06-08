

library(data.table)
library(ggplot2)
library(lmerTest)
library(piecewiseSEM)
library(simr)
library(gridExtra)

dust <- fread("output/all.bats.csv")


allMod = lmer(log(total.inf) ~ sex*soc_ym  + act_ym + sex*exp_hb + act_hb + dawn.ta + (1|Trial), 
          data = dust)

# fit linear mixed model using data
fit <-summary(lmer(allMod, data= dust))

# get the coefficients you want to test (get p-values for)
obs1 <- fit$coefficients[2,1]
obs2 <- fit$coefficients[3,1]  
obs3 <- fit$coefficients[4,1]
obs4 <- fit$coefficients[5,1]  
obs5 <- fit$coefficients[6,1]
obs6 <- fit$coefficients[7,1]  
obs7 <- fit$coefficients[8,1]
obs8 <- fit$coefficients[9,1]  

# now make null model (which makes the null hypothesis true in your data while keeping the structure realistic)

perms=1000
exp1 <- rep(NA, perms)
exp2 <- rep(NA, perms)
exp3 <- rep(NA, perms)
exp4 <- rep(NA, perms)
exp5 <- rep(NA, perms)
exp6 <- rep(NA, perms)
exp7 <- rep(NA, perms)
exp8 <- rep(NA, perms)



for (i in 1:perms){
  # randomize receiver infection intensities to different receiver bats within each trial
  dust[, fake.intensity := sample(total.inf), by = "Trial"]
  
  # fit model using fake y data
  rfit <-summary(lmer(log(fake.intensity) ~ sex*soc_ym  + act_ym + 
                        sex*exp_hb + act_hb + dawn.ta + (1|Trial), data= dust))
  
  # get the coefficient you want to test
  exp1[i] <- rfit$coefficients[2,1]
  exp2[i] <- rfit$coefficients[3,1] 
  exp3[i] <- rfit$coefficients[4,1]
  exp4[i] <- rfit$coefficients[5,1] 
  exp5[i] <- rfit$coefficients[6,1]
  exp6[i] <- rfit$coefficients[7,1] 
  exp7[i] <- rfit$coefficients[8,1]
  exp8[i] <- rfit$coefficients[9,1] 
  
}

# get one-sided p-values by comparing expected and observed

# probability that observed coefficient is greater than expected by chance, assuming null H
mean(exp1<=obs1)
mean(exp2>=obs2)
mean(exp3>=obs3)
mean(exp4>=obs4)
mean(exp5>=obs5)
mean(exp6>=obs6)
mean(exp7>=obs7)
mean(exp8<=obs8)



exp <- data.table(fit = c(exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8), 
                  coef = c(rep("Sex", 1000),
                           rep("Sociability (YM)", 1000), 
                           rep("Activity (YM)", 1000),
                           rep("Exploration (HB)", 1000), 
                           rep("Activity (HB)", 1000),
                           rep("Temperature", 1000),
                           rep("Sex : Sociability (YM)", 1000),
                           rep("Sex : Exploration (HB)", 1000)))

# plot permutation test results (red is observed, blue is expected)----
aa <- ggplot() +
  geom_histogram(data = exp[coef == "Sex"], 
                 aes(x = fit), color="black",fill="light blue") +
  geom_vline(aes(xintercept = obs1), color="red", size=1) +
  xlab("Coefficients for Sex") + 
  ggtitle(paste("p", 
                ifelse(mean(exp[coef == "Sex"]$fit >= obs1) == 0,paste("<",1/perms), 
                 paste("=",signif(mean(exp[coef == "Sex"]$fit <= obs1),digits=2)))))
bb <- ggplot() +
  geom_histogram(data = exp[coef == "Sociability (YM)"], 
                 aes(x = fit), color="black",fill="light blue") +
  geom_vline(aes(xintercept = obs2), color="red", size=1) +
  xlab("Coefficients for Sociability (YM)") + 
  ggtitle(paste("p", 
                ifelse(mean(exp[coef == "Sociability (YM)"]$fit >= obs2) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sociability (YM)"]$fit >= obs2),digits=2)))))
cc <- ggplot() +
  geom_histogram(data = exp[coef == "Activity (YM)"], 
                 aes(x = fit), color="black",fill="light blue") +
  geom_vline(aes(xintercept = obs3), color="red", size=1) +
  xlab("Coefficients for Activity (YM)") + 
  ggtitle(paste("p", 
                ifelse(mean(exp[coef == "Activity (YM)"]$fit >= obs3) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Activity (YM)"]$fit >= obs3),digits=2)))))
dd <- ggplot() +
  geom_histogram(data = exp[coef == "Exploration (HB)"], 
                 aes(x = fit), color="black",fill="light blue") +
  geom_vline(aes(xintercept = obs4), color="red", size=1) +
  xlab("Coefficients for Exploration (HB)") + 
  ggtitle(paste("p", 
                ifelse(mean(exp[coef == "Exploration (HB)"]$fit >= obs4) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Exploration (HB)"]$fit >= obs4),digits=2)))))
ee <- ggplot() +
  geom_histogram(data = exp[coef == "Activity (HB)"], 
                 aes(x = fit), color="black",fill="light blue") +
  geom_vline(aes(xintercept = obs5), color="red", size=1) +
  xlab("Coefficients for Activity (HB)") + 
  ggtitle(paste("p", 
                ifelse(mean(exp[coef == "Activity (HB)"]$fit >= obs5) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Activity (HB)"]$fit >= obs5),digits=2)))))
ff <- ggplot() +
  geom_histogram(data = exp[coef == "Temperature"], 
                 aes(x = fit), color="black",fill="light blue") +
  geom_vline(aes(xintercept = obs6), color="red", size=1) +
  xlab("Coefficients for Temperature") + 
  ggtitle(paste("p", 
                ifelse(mean(exp[coef == "Temperature"]$fit >= obs6) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Temperature"]$fit >= obs6),digits=2)))))
gg <- ggplot() +
  geom_histogram(data = exp[coef == "Sex : Sociability (YM)"], 
                 aes(x = fit), color="black",fill="light blue") +
  geom_vline(aes(xintercept = obs7), color="red", size=1) +
  xlab("Coefficients for Sex : Sociability (YM)") + 
  ggtitle(paste("p", 
                ifelse(mean(exp[coef == "Sex : Sociability (YM)"]$fit >= obs7) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sex : Sociability (YM)"]$fit >= obs7),digits=2)))))
hh <- ggplot() +
  geom_histogram(data = exp[coef == "Sex : Exploration (HB)"], 
                 aes(x = fit), color="black",fill="light blue") +
  geom_vline(aes(xintercept = obs8), color="red", size=1) +
  xlab("Coefficients for Sex : Exploration (HB)") + 
  ggtitle(paste("p", 
                ifelse(mean(exp[coef == "Sex : Exploration (HB)"]$fit >= obs8) ==0,paste(">",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sex : Exploration (HB)"]$fit <= obs8),digits=2)))))

grid.arrange(aa,bb,cc,dd,ee,ff,gg,hh, nrow = 2)


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
  geom_point(data = dust, aes(exp_hb, log(total.inf), color = Sex), alpha = 0.5) +
  geom_line(data = predMod, aes(exp, inf, color = Sex), lty = 1, size = 1) +
  geom_line(data = predMod, aes(exp, upr, color = Sex), lty = 2, size = 0.5) +
  geom_line(data = predMod, aes(exp, lwr, color = Sex), lty = 2, size = 0.5) +
  ylab("log(Infection Intensity)") +
  xlab("Exploration") +
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
