


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

png("figures/FigureS4.png", width = 7000, height = 3500, units = "px", res = 600)
aa <- ggplot(dust2, aes(act_hb, exp_hb)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("A)") +
  scale_color_brewer(palette = "Dark2") +
  ylab(expression("Exploration PC2" [H], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
bb <- ggplot(dust2, aes(act_hb, act_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("B)") +
  scale_color_brewer(palette = "Dark2") +
  ylab(expression("Activity PC1" [Y], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
cc <- ggplot(dust2, aes(act_hb, soc_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("C)") +
  scale_color_brewer(palette = "Dark2") +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Activity PC1" [H], ")" )) + 
  theme1
dd <- ggplot(dust2, aes(exp_hb, act_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("D)") +
  scale_color_brewer(palette = "Dark2") +
  ylab(expression("Activity PC1" [Y], ")" )) +
  xlab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ee <- ggplot(dust2, aes(exp_hb, soc_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("E)") +
  scale_color_brewer(palette = "Dark2") +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ff <- ggplot(dust2, aes(act_ym, soc_ym)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("F)") +
  scale_color_brewer(palette = "Dark2") +
  ylab(expression("Sociability PC2" [Y], ")" )) +
  xlab(expression("Activity PC1" [Y], ")" )) + 
  theme1
gg <- ggplot(dust2, aes(dawn.ta, act_hb)) +
  geom_jitter(aes(color=sex), width = 1) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("G)") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Activity PC1" [H], ")" )) + 
  theme1
hh <- ggplot(dust2, aes(dawn.ta, exp_hb)) +
  geom_jitter(aes(color=sex), width = 1) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("H)") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Exploration PC2" [H], ")" )) + 
  theme1
ii <- ggplot(dust2, aes(dawn.ta, act_ym)) +
  geom_jitter(aes(color=sex), width = 1) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("I)") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Activity PC1" [Y], ")" )) + 
  theme1
jj <- ggplot(dust2, aes(dawn.ta, soc_ym)) +
  geom_jitter(aes(color=sex), width = 1) +
  geom_smooth(method = "lm", se = F,
              color = "black", lty = 2) +
  ggtitle("J)") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = expression(paste("Temperature (",degree~C,")"))) +
  ylab(expression("Sociability PC2" [Y], ")" )) + 
  theme1

grid.arrange(aa,bb,cc,dd,ee,ff,
             gg,hh,ii,jj,nrow = 2)
dev.off()