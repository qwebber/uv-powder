
library(data.table)
library(lme4)
library(ggplot2)
library(dplyr)
library(merTools)

dust <- fread("output/all.bats.csv")

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


## female transmitter; female acquirers 
FF = lmer(log(total.inf) ~ act_hb + exp_hb + act_ym + soc_ym + dawn.ta + (1|Trial), 
          data = dust[sex == "F" & sex_pz == "F"])
summary(FF)
Vcov <- vcov(FF, useScale = FALSE)
betas <- fixef(FF)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval),3)

## male transmitter; male acquirers 
MM = lmer(log(total.inf) ~ act_hb + exp_hb + act_ym + soc_ym + dawn.ta + (1|Trial), 
          data = dust[sex == "M" & sex_pz == "M"])
summary(MM)
Vcov <- vcov(MM, useScale = FALSE)
betas <- fixef(MM)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval),3)

## male transmitter; female acquirers 
FM = lmer(log(total.inf) ~  act_hb + exp_hb + act_ym + soc_ym + dawn.ta + (1|Trial), 
          data = dust[sex == "F" & sex_pz == "M"])
summary(FM)
Vcov <- vcov(FM, useScale = FALSE)
betas <- fixef(FM)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval),3)

## female transmitter; male acquirers 
MF = lmer(log(total.inf) ~  act_hb + exp_hb + act_ym + soc_ym + dawn.ta + (1|Trial), 
          data = dust[sex == "M" & sex_pz == "F"])
summary(MF)
Vcov <- vcov(MF, useScale = FALSE)
betas <- fixef(MF)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval),3)

## female transmitter; male acquirers 
global = lmer(log(total.inf) ~ sex + act_hb + exp_hb + 
                act_ym + soc_ym  + dawn.ta + (1|Trial), 
          data = dust)
summary(global)
Vcov <- vcov(global, useScale = FALSE)
betas <- fixef(global)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval),3)


ggplot(dust) +
  geom_jitter(aes(group.size, log(total.inf), color = factor(Trial))) +
  facet_wrap(~sex_pz)
  