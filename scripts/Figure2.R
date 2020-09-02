
library(data.table)
library(lme4)
library(ggplot2)
library(broom)
library(dplyr)
library(dotwhisker)

dust <- fread("output/all.bats.csv")
p.zero <- fread("output/p.zero.csv")

## Acquisition model
global = lmer(log(total.inf) ~ sex*act_hb + sex*exp_hb + 
                sex*act_ym + sex*soc_ym  + dawn.ta + (1|Trial), 
              data = dust)
coef <- tidy(global, conf.int = TRUE)

## Transmission model
pz <- lm(log(total.inf.M + total.inf.F) ~ sex + act_hb + exp_hb + 
           act_ym + soc_ym  + dawn.ta, 
         data = p.zero)
summary(pz)
pz2 <- tidy(pz)

dwplot(list(pz, global))


png("figures/Figure2.png", width = 5000, height = 3000, units = "px", res = 600)
dwplot(list(pz, global),
             vline = geom_vline(xintercept = 0, colour = "black", linetype = 2))  + #%>% # plot line at zero _behind_ coefs
        scale_y_discrete(labels = c(sexM = "Sex", 
                       dawn.ta = "Temperature",
                       act_hb = "Activity (H)", 
                       exp_hb = "Exploration (H)",
                       act_ym = "Activity (Y)", 
                       soc_ym = "Sociability (Y)",
                       `sexM:act_hb` = "Sex : Activity (H)", 
                       `sexM:exp_hb` = "Sex : Exploration (H)", 
                       `sexM:act_ym` = "Sex : Activity (Y)", 
                       `sexM:soc_ym` = "Sex : Sociability (Y)"), 
                       limits = c("sexM:soc_ym",  "sexM:act_ym", 
                                  "sexM:exp_hb", "sexM:act_hb", 
                                  "soc_ym", "act_ym", 
                                  "exp_hb", "act_hb", 
                                  "dawn.ta", "sexM"
                                  )) + 
  
  scale_colour_manual(labels = c("Transmission model", 
                                 "Acquisition model"), 
                      values = c("#66c2a5", "#fc8d62")) +
  xlab("Coefficient Estimate") + 
  ylab("") +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme(legend.position = c(0.2,0.1),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()