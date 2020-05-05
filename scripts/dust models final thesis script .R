

library(nlme)
library(MASS)
library(mgcv)
library(lme4)
library(data.table)
library(ggplot2)


pzero <- fread("input/patient.zero.txt")

pzero <- pzero[3:12,]
pzero$trial <- 1:10

median(pzero$group.size)

soc <- dust2[group.size > 8.7]
soc$avgGS <- "social"
asoc <- dust2[group.size <8.7]
asoc$avgGS <- "social"



boxplot(total.inf~trial, data = dust2, notch = T)

color <- c('orange', 'dodgerblue2')

ggplot(dust2, aes(factor(trial), total.inf)) +
  geom_boxplot(aes(fill = pzeroBat), notch=TRUE,
               outlier.color = NA,lwd = 0.6, alpha = 0.25) +
  geom_jitter(aes(color = pzeroBat), shape=16, 
              position=position_jitter(0.2), size = 2, alpha = 0.6) +
  theme(axis.title.x = element_blank(),
        legend.position='none',
        axis.title.y = element_text(size=12,color = 'black'),
        axis.text=element_text(size=10,color = 'black'),
        plot.title=element_text(size = 10,hjust=0, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = 'black', fill=NA, size=1)) +
  ylab('Infection') +
  scale_fill_manual(values=color) +
  scale_color_manual(values = color) 


ggplot(dust2, aes(PC2_Y_inf, total.inf)) +
  geom_jitter(aes(color = factor(trial)), 
              width = 0.05, alpha = 0.5) +
  geom_smooth(method = "lm")

ggplot(dust2, aes(PC1_Y, PC1_nov)) +
  geom_point() + 
  geom_smooth()

dust2$log.inf=log10(dust2$total.inf)

dust2$PC2_Y_inf=dust2$PC2_Y_inf*(-1)

dust2$trial=as.factor(dust2$trial)

## all bats
L1=lmer(log10(total.inf) ~sex*PC2_Y+ sex*PC2_nov+min.ta+(1|trial),data=dust2)
summary(L1)
Vcov <- vcov(L1, useScale = FALSE)
betas <- fixef(L1)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval),3)

ggplot(dust2, aes(PC2_nov, log10(total.inf))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~sex)

## without PC1_Y
L2=lmer(log10(total.inf) ~group.size+PC2_Y+PC1_nov+PC2_nov+PC3_nov+min.ta+(1|trial),data=dust2)
Vcov <- vcov(L2, useScale = FALSE)
betas <- fixef(L2)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)

## without group.size
L3=lmer(log10(total.inf) ~PC2_Y+PC1_nov+PC2_nov+PC3_nov+min.ta+(1|trial),data=dust2)
Vcov <- vcov(L3, useScale = FALSE)
betas <- fixef(L3)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)

##without PC1_nov
L4=lmer(log10(total.inf) ~PC2_Y+PC2_nov+PC3_nov+min.ta+(1|trial),data=dust2)
Vcov <- vcov(L4, useScale = FALSE)
betas <- fixef(L4)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)

## without PC2_Y
L5=lmer(log10(total.inf) ~PC2_nov+PC3_nov+min.ta+(1|trial),data=dust2)
Vcov <- vcov(L5, useScale = FALSE)
betas <- fixef(L5)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)

## without PC2_nov
L6=lmer(log10(total.inf) ~PC3_nov+min.ta+(1|trial),data=dust2)
Vcov <- vcov(L6, useScale = FALSE)
betas <- fixef(L6)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)

L7=lmer(log10(total.inf) ~min.ta+(1|trial),data=dust2)
summary(L7)
Vcov <- vcov(L7, useScale = FALSE)
betas <- fixef(L7)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)


dust3=subset(dust2,PC3_nov > -3)
L9=lmer(log10(total.inf) ~PC3_nov+min.ta+(1|trial),data=dust3)
summary(L9)
Vcov <- vcov(L9, useScale = FALSE)
betas <- fixef(L9)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)







### test for outliers here: 
library(outliers)
grubbs.test(dust2$PC3_nov,type=11)

boxplot(log.inf~sex,data=dust2)
t.test(dust2[sex == "M"]$log.inf,dust2[sex == "F"]$log.inf)

