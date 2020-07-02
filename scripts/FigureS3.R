


library(data.table)
library(ggplot2)
library(gridExtra)
library(GGally)

dust <- fread("output/all.bats.csv")

# act_hb, exp_hb, act_ym, soc_ym, dawn.ta

theme1 <- theme(legend.position = 'none', 
                axis.title = element_text(size = 12, color = 'black'),
                axis.text = element_text(size = 9, color = 'black'),
                plot.title = element_text(size = 9, color = 'black'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                panel.border = element_rect(colour = "black", fill=NA, size = 1))

col = c("#1b9e77", "#1f78b4")


ggpairs(dust[, c("act_hb", "act_ym", "exp_hb", "soc_ym", "dawn.ta")]) +
  theme1

png("figures/FigureS4.png", width = 7000, height = 3500, units = "px", res = 600)
aa <- ggplot(dust, aes(act_hb, exp_hb)) +
  geom_point(aes(color=sex), alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("A) r = 0.007") +
  scale_color_manual(values = col) +
  ylab(expression("Exploration PC2" [H], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
bb <- ggplot(dust, aes(act_hb, act_ym)) +
  geom_point(aes(color=sex), alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("B) r = 0.009") +
  scale_color_manual(values = col) +
  ylab(expression("Activity PC1" [Y], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
cc <- ggplot(dust, aes(act_hb, soc_ym)) +
  geom_point(aes(color=sex), alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("C) r = 0.11") +
  scale_color_manual(values = col) +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
dd <- ggplot(dust, aes(exp_hb, act_ym)) +
  geom_point(aes(color=sex), alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("D) r = 0.20") +
  scale_color_manual(values = col) +
  ylab(expression("Activity PC1" [Y], ")" )) +
  xlab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ee <- ggplot(dust, aes(exp_hb, soc_ym)) +
  geom_point(aes(color=sex), alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("E) r = 0.02") +
  scale_color_manual(values = col) +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ff <- ggplot(dust, aes(act_ym, soc_ym)) +
  geom_point(aes(color=sex), alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("F) r = -0.004") +
  scale_color_manual(values = col) +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Activity PC1" [Y], ")" )) + 
  theme1
gg <- ggplot(dust, aes(dawn.ta, act_hb)) +
  geom_jitter(aes(color=sex), width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("G) r = 0.15") +
  scale_color_manual(values = col) +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Activity PC1" [H], ")" )) + 
  theme1
hh <- ggplot(dust, aes(dawn.ta, exp_hb)) +
  geom_jitter(aes(color=sex), width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("H) r = -0.06") +
  scale_color_manual(values = col) +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ii <- ggplot(dust, aes(dawn.ta, act_ym)) +
  geom_jitter(aes(color=sex), width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("I) r = 0.27") +
  scale_color_manual(values = col) +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Activity PC1" [Y], ")" )) + 
  theme1
jj <- ggplot(dust, aes(dawn.ta, soc_ym)) +
  geom_jitter(aes(color=sex), width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("J) r = -0.01") +
  scale_color_manual(values = col) +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Sociability PC2" [Y], ")" )) + 
  theme1

grid.arrange(aa,bb,cc,dd,ee,ff,
             gg,hh,ii,jj,nrow = 2)
dev.off()