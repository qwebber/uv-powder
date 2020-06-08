

library(data.table)
library(lme4)
library(ggplot2)
library(effects)

p.zero <- fread("output/p.zero.csv")
p.zero$inf <- p.zero$total.inf.F + p.zero$total.inf.M

## Social model
A1 = lm(log(inf) ~ sex + act_hb + exp_hb + act_ym  + soc_ym + min.ta, data = p.zero)
summary(A1)


M1 = lm(log(total.inf.M) ~ sex + act_hb + exp_hb + act_ym  + soc_ym + min.ta, data = p.zero)
summary(M1)

