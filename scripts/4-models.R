
library(data.table)
library(lme4)
library(ggplot2)
library(dplyr)
library(merTools)
library(GGally)

dust <- fread("output/all.bats.csv")
p.zero <- fread("output/p.zero.csv")

##sex ratio
aa <- dust[sex == "F"][, .N , by = "Trial"]
colnames(aa)[2] <- "Female"
bb <- dust[sex == "M"][, .N , by = "Trial"]
colnames(bb)[2] <- "Male"

cc <- merge(aa, bb, by = "Trial")
cc$total <- cc$Female + cc$Male

cc$ratio <- cc$Female/cc$total

mean(cc$ratio)
sd(cc$ratio)

## sex of patient zero
dust[, .N, by = c("Trial", "sex")]

## average intensity by sex
dust[sex == "F"][, mean(total.inf), by = c("Trial")]
dust[sex == "F"][, sd(total.inf), by = c("Trial")]
dust[sex == "M"][, mean(total.inf), by = c("Trial")]
dust[sex == "M"][, sd(total.inf), by = c("Trial")]

## global model with all interactions
global = lmer(log(total.inf) ~ sex*act_hb + sex*exp_hb + 
                sex*act_ym + sex*soc_ym  + dawn.ta + (1|Trial), 
          data = dust)
summary(global)

## all data in same model

p.zero$total.inf <- p.zero$total.inf.F + p.zero$total.inf.M

pz <- lm(log(total.inf) ~ sex + act_hb + exp_hb + 
             act_ym + soc_ym  + dawn.ta, 
           data = p.zero)

summary(pz)

pz2 <- tidy(pz)

dwplot(pz2, 
       vline = geom_vline(xintercept = 0, colour = "black", linetype = 2)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(act_ym = "Activity (Y)", 
                       soc_ym = "Sociability (Y)", 
                       dawn.ta = "Temperature",
                       act_hb = "Activity (H)", 
                       sexM = "Sex",   
                       exp_hb = "Exploration (H)")) +
  scale_colour_manual(values = "grey60") +
  xlab("Coefficient Estimate") + 
  ylab("") +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
