


library(data.table)
library(ggplot2)
library(gridExtra)

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

png("figures/FigureS4.png", width = 7000, height = 3500, units = "px", res = 600)
aa <- ggplot(dust, aes(act_hb, exp_hb)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("A)") +
  scale_color_manual(values = col) +
  ylab(expression("Exploration PC2" [H], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
bb <- ggplot(dust, aes(act_hb, act_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("B)") +
  scale_color_manual(values = col) +
  ylab(expression("Activity PC1" [Y], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
cc <- ggplot(dust, aes(act_hb, soc_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("C)") +
  scale_color_manual(values = col) +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
dd <- ggplot(dust, aes(exp_hb, act_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("D)") +
  scale_color_manual(values = col) +
  ylab(expression("Activity PC1" [Y], ")" )) +
  xlab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ee <- ggplot(dust, aes(exp_hb, soc_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("E)") +
  scale_color_manual(values = col) +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ff <- ggplot(dust, aes(act_ym, soc_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("F)") +
  scale_color_manual(values = col) +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Activity PC1" [Y], ")" )) + 
  theme1
gg <- ggplot(dust, aes(dawn.ta, act_hb)) +
  geom_jitter(aes(color=sex), width = 1) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("G)") +
  scale_color_manual(values = col) +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Activity PC1" [H], ")" )) + 
  theme1
hh <- ggplot(dust, aes(dawn.ta, exp_hb)) +
  geom_jitter(aes(color=sex), width = 1) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("H)") +
  scale_color_manual(values = col) +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ii <- ggplot(dust, aes(dawn.ta, act_ym)) +
  geom_jitter(aes(color=sex), width = 1) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("I)") +
  scale_color_manual(values = col) +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Activity PC1" [Y], ")" )) + 
  theme1
jj <- ggplot(dust, aes(dawn.ta, soc_ym)) +
  geom_jitter(aes(color=sex), width = 1) +
  geom_smooth(method = "lm", se = F,
              color = "black") +
  ggtitle("J)") +
  scale_color_manual(values = col) +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Sociability PC2" [Y], ")" )) + 
  theme1

grid.arrange(aa,bb,cc,dd,ee,ff,
             gg,hh,ii,jj,nrow = 2)
dev.off()