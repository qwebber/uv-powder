
library(data.table)
library(lme4)
library(ggplot2)
library(broom)
library(dplyr)
library(dotwhisker)

dust <- fread("output/all.bats.csv")


global = lmer(log(total.inf) ~ sex*act_hb + sex*exp_hb + 
                sex*act_ym + sex*soc_ym  + dawn.ta + (1|Trial), 
              data = dust)
summary(global)

coef <- tidy(global, conf.int = TRUE)

png("figures/Figure2.png", width = 4000, height = 2500, units = "px", res = 600)
dwplot(coef[1:11,], 
       vline = geom_vline(xintercept = 0, colour = "black", linetype = 2)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(`sexM:exp_hb` = "Sex : Exploration (H)", 
                       sexM = "Sex",   
                       dawn.ta = "Temperature",
                       act_ym = "Activity (Y)", 
                       act_hb = "Activity (H)", 
                       `sexM:act_hb` = "Sex : Activity (H)", 
                       soc_ym = "Sociability (Y)", 
                       `sexM:act_ym` = "Sex : Activity (Y)", 
                       `sexM:soc_ym` = "Sex : Sociability (Y)", 
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
dev.off()


