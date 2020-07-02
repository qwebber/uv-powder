

library(data.table)
library(ggplot2)
library(gridExtra)
library(lme4)
library(car)

dust <- fread("output/all.bats.csv")

allMod = lmer(log(total.inf) ~ sex*soc_ym  + sex*act_ym + 
                sex*exp_hb + sex*act_hb + dawn.ta + (1|Trial), 
              data = dust)

df <- dust[,c("sex", "soc_ym", "act_ym", "exp_hb", "act_hb", "dawn.ta")]

car::vif(allMod)

summary(allMod)

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
obs9 <- fit$coefficients[10,1]
obs10 <- fit$coefficients[11,1]  


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
exp9 <- rep(NA, perms)
exp10 <- rep(NA, perms)



for (i in 1:perms){
  # randomize receiver infection intensities to different receiver bats within each trial
  dust[, fake.intensity := sample(total.inf), by = "Trial"]
  
  # fit model using fake y data
  rfit <-summary(lmer(log(fake.intensity) ~ sex*soc_ym  + sex*act_ym + 
                        sex*exp_hb + sex*act_hb + dawn.ta + (1|Trial), data= dust))
  
  # get the coefficient you want to test
  exp1[i] <- rfit$coefficients[2,1]    # sex
  exp2[i] <- rfit$coefficients[3,1]    # soc (YM)
  exp3[i] <- rfit$coefficients[4,1]    # act (YM)
  exp4[i] <- rfit$coefficients[5,1]    # exp (HB)
  exp5[i] <- rfit$coefficients[6,1]    # act (HB)
  exp6[i] <- rfit$coefficients[7,1]    # dawn.ta
  exp7[i] <- rfit$coefficients[8,1]    # sex : soc (YM)
  exp8[i] <- rfit$coefficients[9,1]    # sex : act (YM)
  exp9[i] <- rfit$coefficients[10,1]   # sex : exp (HB)
  exp10[i] <- rfit$coefficients[11,1]  # sex : act (HB)
  
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
mean(exp9>=obs9)
mean(exp10<=obs10)



exp <- data.table(fit = c(exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9, exp10), 
                  coef = c(rep("Sex", 1000),
                           rep("Sociability (YM)", 1000), 
                           rep("Activity (YM)", 1000),
                           rep("Exploration (HB)", 1000), 
                           rep("Activity (HB)", 1000),
                           rep("Temperature", 1000),
                           rep("Sex : Sociability (YM)", 1000),
                           rep("Sex : Activity (YM)", 1000),
                           rep("Sex : Exploration (HB)", 1000), 
                           rep("Sex : Activity (HB)", 1000)))

theme1 <- theme(legend.position = 'none', 
                axis.title = element_text(size = 12, color = 'black'),
                axis.text = element_text(size = 9, color = 'black'),
                plot.title = element_text(size = 9, color = 'black'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                panel.border = element_rect(colour = "black", fill=NA, size = 1))

# plot permutation test results (red is observed, blue is expected)----
png("figures/FigureS3.png", width = 7000, height = 3500, units = "px", res = 600)
aa <- ggplot() +
  geom_histogram(data = exp[coef == "Sex"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs1), color="red", size=1) +
  xlab("Sex") + 
  ylab("Frequency") +
  ggtitle(paste("A) p", 
                ifelse(mean(exp[coef == "Sex"]$fit >= obs1) == 0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sex"]$fit <= obs1),digits=2))))) +
  theme1
bb <- ggplot() +
  geom_histogram(data = exp[coef == "Sociability (YM)"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs2), color="red", size=1) +
  xlab(expression("Sociability PC2" [Y], ")" )) +
  ylab("Frequency") +
  ggtitle(paste("B) p", 
                ifelse(mean(exp[coef == "Sociability (YM)"]$fit >= obs2) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sociability (YM)"]$fit >= obs2),digits=2))))) +
  theme1
cc <- ggplot() +
  geom_histogram(data = exp[coef == "Activity (YM)"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs3), color="red", size=1) +
  xlab(expression("Activity PC1" [Y], ")" )) +
  ylab("Frequency") +
  ggtitle(paste("C) p", 
                ifelse(mean(exp[coef == "Activity (YM)"]$fit >= obs3) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Activity (YM)"]$fit >= obs3),digits=2)))))+
  theme1
dd <- ggplot() + 
  geom_histogram(data = exp[coef == "Exploration (HB)"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs4), color="red", size=1) +
  xlab(expression("Exploration PC2" [H], ")" )) +
  ylab("Frequency") +
  ggtitle(paste("D) p", 
                ifelse(mean(exp[coef == "Exploration (HB)"]$fit >= obs4) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Exploration (HB)"]$fit >= obs4),digits=2))))) +
  theme1
ee <- ggplot() +
  geom_histogram(data = exp[coef == "Activity (HB)"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs5), color="red", size=1) +
  xlab(expression("Activity PC1" [H], ")" )) +
  ylab("Frequency") +
  ggtitle(paste("E) p", 
                ifelse(mean(exp[coef == "Activity (HB)"]$fit >= obs5) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Activity (HB)"]$fit >= obs5),digits=2))))) +
  theme1
ff <- ggplot() +
  geom_histogram(data = exp[coef == "Temperature"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs6), color="red", size=1) +
  xlab("Temperature") + 
  ylab("Frequency") +
  ggtitle(paste("F) p", 
                ifelse(mean(exp[coef == "Temperature"]$fit >= obs6) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Temperature"]$fit >= obs6),digits=2))))) +
  theme1
gg <- ggplot() +
  geom_histogram(data = exp[coef == "Sex : Sociability (YM)"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs7), color="red", size=1) +
  xlab(expression("Sex:Sociability PC2" [Y], ")" )) +
  ylab("Frequency") +
  ggtitle(paste("G) p", 
                ifelse(mean(exp[coef == "Sex : Sociability (YM)"]$fit >= obs7) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sex : Sociability (YM)"]$fit >= obs7),digits=2))))) +
  theme1
hh <- ggplot() +
  geom_histogram(data = exp[coef == "Sex : Activity (YM)"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs8), color="red", size=1) +
  xlab(expression("Sex:Activity PC1" [Y], ")" )) +
  ylab("Frequency") +
  ggtitle(paste("H) p", 
                ifelse(mean(exp[coef == "Sex : Activity (YM)"]$fit >= obs8) ==0,paste("<",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sex : Activity (YM)"]$fit >= obs8),digits=2))))) +
  theme1 

ii <- ggplot() +
  geom_histogram(data = exp[coef == "Sex : Exploration (HB)"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs9), color="red", size=1) +
  xlab(expression("Sex:Exploration PC2" [H], ")" )) +
  ylab("Frequency") +
  ggtitle(paste("I) p", 
                ifelse(mean(exp[coef == "Sex : Exploration (HB)"]$fit >= obs9) ==0,paste(">",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sex : Exploration (HB)"]$fit <= obs9),digits=2))))) +
  theme1
jj <- ggplot() +
  geom_histogram(data = exp[coef == "Sex : Activity (HB)"], 
                 aes(x = fit), color="black",fill="light grey") +
  geom_vline(aes(xintercept = obs10), color="red", size=1) +
  xlab(expression("Sex:Activity PC1" [H], ")" )) +
  ylab("Frequency") +
  ggtitle(paste("J) p", 
                ifelse(mean(exp[coef == "Sex : Activity (HB)"]$fit >= obs10) ==0,paste(">",1/perms), 
                       paste("=",signif(mean(exp[coef == "Sex : Activity (HB)"]$fit <= obs10),digits=2))))) +
  theme1

grid.arrange(aa,bb,cc,dd,ee,ff,gg,hh,ii,jj, nrow = 2)
dev.off()
