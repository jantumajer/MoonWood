library(reshape2)
library(mgcv)

####################################################
### Load outputs of wavelet power transformation ###
####################################################
# To produce them run CalculateWavelets.R (time consuming!)

A_GRO <- readRDS("A_GRO_60min.Rda")
B_GRO <- readRDS("B_GRO_60min.Rda")
C_GRO <- readRDS("C_GRO_60min.Rda")
O_GRO <- readRDS("O_GRO_60min.Rda")
PISY_GRO <- readRDS("PISY_GRO_60min.Rda")
PCAB_GRO <- readRDS("PCAB_GRO_60min.Rda")
###
A_TWD <- readRDS("A_TWD_60min.Rda")
B_TWD <- readRDS("B_TWD_60min.Rda")
C_TWD <- readRDS("C_TWD_60min.Rda")
O_TWD <- readRDS("O_TWD_60min.Rda")
PISY_TWD <- readRDS("PISY_TWD_60min.Rda")
PCAB_TWD <- readRDS("PCAB_TWD_60min.Rda")
###
A_t <- readRDS("A_t.Rda")
B_t <- readRDS("B_t.Rda")
C_t <- readRDS("C_t.Rda")
O_t <- readRDS("O_t.Rda")
PISY_t <- readRDS("PISY_t.Rda")
PCAB_t <- readRDS("PCAB_t.Rda")
###
A_vpd <- readRDS("A_vpd.Rda")
B_vpd <- readRDS("B_vpd.Rda")
O_vpd <- readRDS("O_vpd.Rda")
PISY_vpd <- readRDS("PISY_vpd.Rda")
PCAB_vpd <- readRDS("PCAB_vpd.Rda")
###
moon <- readRDS("moon.Rda")

################
### Grouping ###
################

dat2 <- rbind(
  cbind(per = c(1:262), pow = A_GRO$Power.avg, spe = "A", var = "Growth rate"),
  cbind(per = c(1:262), pow = B_GRO$Power.avg, spe = "B", var = "Growth rate"),
  cbind(per = c(1:262), pow = C_GRO$Power.avg, spe = "C", var = "Growth rate"),
  cbind(per = c(1:262), pow = O_GRO$Power.avg, spe = "O", var = "Growth rate"),
  cbind(per = c(1:262), pow = PISY_GRO$Power.avg, spe = "PISY", var = "Growth rate"),
  cbind(per = c(1:262), pow = PCAB_GRO$Power.avg, spe = "PCAB", var = "Growth rate"),
  ###
  cbind(per = c(1:262), pow = A_TWD$Power.avg, spe = "A", var = "Tree water deficit"),
  cbind(per = c(1:262), pow = B_TWD$Power.avg, spe = "B", var = "Tree water deficit"),
  cbind(per = c(1:262), pow = C_TWD$Power.avg, spe = "C", var = "Tree water deficit"),
  cbind(per = c(1:262), pow = O_TWD$Power.avg, spe = "O", var = "Tree water deficit"),
  cbind(per = c(1:262), pow = PISY_TWD$Power.avg, spe = "PISY", var = "Tree water deficit"),
  cbind(per = c(1:262), pow = PCAB_TWD$Power.avg, spe = "PCAB", var = "Tree water deficit"),
  ###
  cbind(per = c(1:262), pow = A_t$Power.avg, spe = "A", var = "Temperature"),
  cbind(per = c(1:262), pow = B_t$Power.avg, spe = "B", var = "Temperature"),
  cbind(per = c(1:262), pow = C_t$Power.avg, spe = "C", var = "Temperature"),
  cbind(per = c(1:262), pow = O_t$Power.avg, spe = "O", var = "Temperature"),
  cbind(per = c(1:262), pow = PISY_t$Power.avg, spe = "PISY", var = "Temperature"),
  cbind(per = c(1:262), pow = PCAB_t$Power.avg, spe = "PCAB", var = "Temperature"),
  ###
  cbind(per = c(1:262), pow = A_vpd$Power.avg, spe = "A", var = "VPD"),
  cbind(per = c(1:262), pow = B_vpd$Power.avg, spe = "B", var = "VPD"),
  cbind(per = c(1:262), pow = O_vpd$Power.avg, spe = "O", var = "VPD"),
  cbind(per = c(1:262), pow = PISY_vpd$Power.avg, spe = "PISY", var = "VPD"),
  cbind(per = c(1:262), pow = PCAB_vpd$Power.avg, spe = "PCAB", var = "VPD"),
  ###
  cbind(per = c(1:262), pow = moon$Power.avg, spe = "all", var = "Lunar phase"))

###################
### Reformating ###
###################

dat2 <- as.data.frame(dat2)
dat2$pow <- as.numeric(dat2$pow)
dat2$per <- as.numeric(dat2$per)

dat3 <- dcast(data = dat2, formula = per + spe ~ var, value.var = "pow")

dat4 <- dat3[dat3$per > 40 & !dat3$spe == "all",] # Targeting only wavelets longer than 8 h

dat5 <- merge(dat4, dat3[dat3$spe == "all", c("per", "Lunar phase")], by = "per")
# dat5[dat5$spe == "A", "VPD"] <- dat5[dat5$spe == "B", "VPD"] 
dat5[dat5$spe == "C", "VPD"] <- dat5[dat5$spe == "B", "VPD"]  # Carpinus VPD wavelets are substituted from other broadleaves
colnames(dat5) <- c("per", "spe", "gro", "lunNA", "tem", "twd", "vpd", "lun")

##############
### Models ###
##############

modGRO <- gam(gro ~ spe + s(tem) + s(vpd) + s(lun) ,data = dat5, method="REML")
summary(modGRO)

modTWD <- gam(twd ~ spe + s(tem) + s(vpd) + s(lun), data = dat5, method="REML")
summary(modTWD)

# Exclude individual predictors, refit the model, and compare R2
