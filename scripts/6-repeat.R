

library(data.table)
library(rptR)

dust <- fread("output/all.bats.csv")


ggplot(dust, aes(act_hb, act_ym)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~sex)


hb <- dust[, c("act_hb", "ID", "order_hb", "arena_hb", "sex")]
hb$test = "hb"
colnames(hb) <- c("act", "ID", "order", "arena", "sex", "test")

ym <- dust[, c("act_ym", "ID", "order_ym", "arena_ym", "sex")]
ym$test = "ym"
colnames(ym) <- c("act", "ID", "order", "arena", "sex" , "test")

rep <- rbind(hb, ym)

hist(rep$act)

rpt(act ~ order + test + arena + (1|ID),  grname =c("ID"), datatype = "Gaussian", 
    data = rep[sex == "F"])

rpt(act ~ order + test + arena + (1|ID),  grname =c("ID"), datatype = "Gaussian", 
    data = rep[sex == "M"])
