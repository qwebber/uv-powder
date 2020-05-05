
library(data.table)
library(ggplot2)

hb <- fread("output/hb-pca.csv")
hb[,c("V1") := NULL]
colnames(hb) <- c("ID", "order_hb", "arena_hb",
                  "date_hb", "act_hb", "exp_hb")
hb[, idate := as.POSIXct(paste(date_hb), format = "%d-%b-%y")]
hb[, idate := as.IDate(idate)]
hb[, JDate_hb := yday(idate)][,c("date_hb") :=NULL]


ym <- fread("output/ym-pca.csv")
ym[,c("V1") := NULL]
colnames(ym) <- c("ID", "order_ym", "arena_ym",
                  "date_ym", "act_ym", "soc_ym")
ym[, idate := as.POSIXct(paste(date_ym), format = "%Y-%m-%d" )]
ym[, idate := as.IDate(idate)]
ym[, JDate_ym := yday(idate)][,c("date_ym") :=NULL]

bats <- fread("input/bat.data.csv")

infect <- fread("input/infection.status.csv")

## merge all datasets together
bats2 <- merge(bats, infect, by = "ID")
all <- merge(hb, ym, by = "ID")
all2 <- merge(all, bats2, by = "ID")

## separate into patient zero dataset and other bats
p.zero <- all2[p.zero == "yes"]
p.zero <- p.zero[order(Trial),]
all.bats <- all2[p.zero == "no"]
all.bats <- all.bats[order(Trial),]

## add sex of patient zero
sex <- p.zero[,c("Trial", "sex")]
colnames(sex) <- c("Trial", "sex_pz")
all.bats <- merge(all.bats, sex, by = "Trial")

## add average infection for areas
p.zero$total.inf.var.M <- all.bats[, var(total.inf), by = c("Trial","sex")][sex == "M"]$V1
p.zero$total.inf.var.F <- all.bats[, var(total.inf), by = c("Trial","sex")][sex == "F"]$V1
p.zero$total.inf.M <- all.bats[, mean(total.inf), by = c("Trial","sex")][sex == "M"]$V1
p.zero$total.inf.F <- all.bats[, mean(total.inf), by = c("Trial","sex")][sex == "F"]$V1
p.zero$total.areas <- all.bats[, mean(total.areas), by = "Trial"]$V1
p.zero$wing.inf <- all.bats[, mean(wing.inf), by = "Trial"]$V1
p.zero$fur.inf <- all.bats[, mean(fur.inf), by = "Trial"]$V1
p.zero$wing.areas <- all.bats[, mean(wing.areas), by = "Trial"]$V1
p.zero$fur.areas <- all.bats[, mean(fur.areas), by = "Trial"]$V1

p.zero[,c("total.inf", "total.areas", "wing.inf", 
          "fur.inf", "wing.areas", "fur.areas") := NULL]


fwrite(p.zero, "output/p.zero.csv")
fwrite(all.bats, "output/all.bats.csv")
