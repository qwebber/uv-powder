
library(data.table)
library(lme4)
library(ggplot2)
library(dplyr)
library(merTools)
library(GGally)
library(broom)
library(dotwhisker)

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
