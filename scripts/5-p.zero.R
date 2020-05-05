

library(data.table)
library(lme4)
library(ggplot2)
library(effects)

p.zero <- fread("output/p.zero.csv")

## Social model
F1 = lm(log(total.inf.F) ~ sex + act_hb + exp_hb + act_ym  + soc_ym + min.ta, data = p.zero)
summary(F1)
M1 = lm(log(total.inf.M) ~ sex + act_hb + exp_hb + act_ym  + soc_ym + min.ta, data = p.zero)
summary(M1)

