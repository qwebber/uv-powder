
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


global = lmer(log(total.inf) ~ sex*act_hb + sex*exp_hb + 
                sex*act_ym + sex*soc_ym  + dawn.ta + (1|Trial), 
          data = dust)
summary(global)

## all data in same model

p.zero2 <- p.zero[,c("Trial","act_hb", "exp_hb", "act_ym", "soc_ym", "sex")]
colnames(p.zero2) <- c("Trial", "act_hb_pz", "exp_hb_pz", 
                       "act_ym_pz", "soc_ym_pz" ,"sex_pz")
dust2 <- dust[,c("Trial","total.inf" ,"act_hb", "exp_hb", 
                 "act_ym", "soc_ym", "sex", "dawn.ta" ,"ID")]

df <- merge(dust2, p.zero2, by = "Trial")
df$sex_combo <- paste(dust2$sex, p.zero2$sex_pz, sep = "_")


global2 <- lmer(log(total.inf) ~ sex_combo + act_hb*sex + sex*exp_hb + 
  sex_pz*act_hb_pz + sex_pz*exp_hb_pz + dawn.ta + (1|Trial),  data = df)

summary(global2)

ggplot(df, aes(soc_ym_pz, log(total.inf), color = sex)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F)

ggplot(df, aes(sex_combo, log(total.inf), fill = sex_pz)) +
  geom_boxplot(notch = TRUE,
               outlier.color = NA,
               lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = sex_pz), shape = 16,
              position = position_jitterdodge(0.3),
              size = 2,
              alpha = 0.6) 
  theme1
