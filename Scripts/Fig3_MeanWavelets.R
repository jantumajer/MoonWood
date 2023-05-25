library(reshape2)
library(ggplot2)
library(ggh4x)

####################################################
### Load outputs of wavelet power transformation ###
####################################################
# To produce them run CalculateWavelets.R (time consuming!)
# Alternatively, you might download the wavelet spectra from https://github.com/jantumajer/MoonWood/tree/main/Waveletes

A_GRO <- readRDS("E:/MoonWood/Data/Waves/waves_calc/A_GRO_60min.Rda")
B_GRO <- readRDS("E:/MoonWood/Data/Waves/waves_calc/B_GRO_60min.Rda")
C_GRO <- readRDS("E:/MoonWood/Data/Waves/waves_calc/C_GRO_60min.Rda")
O_GRO <- readRDS("E:/MoonWood/Data/Waves/waves_calc/O_GRO_60min.Rda")
PISY_GRO <- readRDS("E:/MoonWood/Data/Waves/waves_calc/PISY_GRO_60min.Rda")
PCAB_GRO <- readRDS("E:/MoonWood/Data/Waves/waves_calc/PCAB_GRO_60min.Rda")
###
A_TWD <- readRDS("E:/MoonWood/Data/Waves/waves_calc/A_TWD_60min.Rda")
B_TWD <- readRDS("E:/MoonWood/Data/Waves/waves_calc/B_TWD_60min.Rda")
C_TWD <- readRDS("E:/MoonWood/Data/Waves/waves_calc/C_TWD_60min.Rda")
O_TWD <- readRDS("E:/MoonWood/Data/Waves/waves_calc/O_TWD_60min.Rda")
PISY_TWD <- readRDS("E:/MoonWood/Data/Waves/waves_calc/PISY_TWD_60min.Rda")
PCAB_TWD <- readRDS("E:/MoonWood/Data/Waves/waves_calc/PCAB_TWD_60min.Rda")
###
A_t <- readRDS("E:/MoonWood/Data/Waves/clim/A_t.Rda")
B_t <- readRDS("E:/MoonWood/Data/Waves/clim/B_t.Rda")
C_t <- readRDS("E:/MoonWood/Data/Waves/clim/C_t.Rda")
O_t <- readRDS("E:/MoonWood/Data/Waves/clim/O_t.Rda")
PISY_t <- readRDS("E:/MoonWood/Data/Waves/clim/PISY_t.Rda")
PCAB_t <- readRDS("E:/MoonWood/Data/Waves/clim/PCAB_t.Rda")
###
A_vpd <- readRDS("E:/MoonWood/Data/Waves/clim/A_vpd.Rda")
B_vpd <- readRDS("E:/MoonWood/Data/Waves/clim/B_vpd.Rda")
O_vpd <- readRDS("E:/MoonWood/Data/Waves/clim/O_vpd.Rda")
PISY_vpd <- readRDS("E:/MoonWood/Data/Waves/clim/PISY_vpd.Rda")
PCAB_vpd <- readRDS("E:/MoonWood/Data/Waves/clim/PCAB_vpd.Rda")
###
moon <- readRDS("E:/MoonWood/Data/Waves/clim/moon.Rda")

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

dat2 <- as.data.frame(dat2)
dat2$pow <- as.numeric(dat2$pow)
dat2$per <- as.numeric(dat2$per)

##########################################################
### Converting  wavelet order into real-time frequency ###
##########################################################

val.day <- length(A_GRO$Period[A_GRO$Period < 24]) + (min(A_GRO$Period[A_GRO$Period > 24]) - 24)/(min(A_GRO$Period[A_GRO$Period > 24]) - max(A_GRO$Period[A_GRO$Period < 24]))
val.month <- length(A_GRO$Period[A_GRO$Period < 29.53*24]) + (min(A_GRO$Period[A_GRO$Period > 29.53*24]) - 29.53*24)/(min(A_GRO$Period[A_GRO$Period > 29.53*24]) - max(A_GRO$Period[A_GRO$Period < 29.53*24]))
val.halfyear<- length(A_GRO$Period[A_GRO$Period < 365*12]) + (min(A_GRO$Period[A_GRO$Period > 365*12]) - 365*12)/(min(A_GRO$Period[A_GRO$Period > 365*12]) - max(A_GRO$Period[A_GRO$Period < 365*12]))
val.year<- length(A_GRO$Period[A_GRO$Period < 365*24]) + (min(A_GRO$Period[A_GRO$Period > 365*24]) - 365*24)/(min(A_GRO$Period[A_GRO$Period > 365*24]) - max(A_GRO$Period[A_GRO$Period < 365*24]))
val.8h <- length(A_GRO$Period[A_GRO$Period < 8])+1
l <- length(A_GRO$Power.avg)

lines_ALL2 <- ggplot(data = dat2) + 
  geom_line(aes(x = per, y = pow, col = factor(spe, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus"))), alpha = 1) + 
  geom_vline(aes(xintercept = val.month), col = "grey", alpha = 0.75, linetype = "dashed") +
  scale_x_continuous(limits = c(val.8h, l),name = "Period of oscillation", breaks = c(val.8h, val.day, val.month, val.halfyear, val.year), labels = c("8 h", "1 d", "1 m", "1/2 y", "1 y")) +
  ylab("Wavelet power") +
  facet_grid(factor(var, levels = c("Growth rate", "Tree water deficit", "Temperature", "VPD", "Lunar phase")) ~ .,
             scales = "free_y") +
  theme_classic() +     theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              panel.border = element_rect(fill = NA, colour = "grey20"),
                              axis.line = element_line(colour = "black"),
                              axis.text.y = element_text(size = 11, colour = "black"),
                              axis.text.x = element_text(size = 11, colour = "black", angle = 45, hjust = 0.9, vjust = 0.9),
                              axis.title = element_text(size = 12, colour = "black"),
                              axis.ticks.length=unit(.3, "cm"),
                              strip.text = element_text(size = 10, colour = "black"),
                              plot.title = element_text(color = "black", hjust = 0.5, vjust = 0, face = "bold", size = 18),
                              #legend.position = "none"
                              legend.text = element_text(face = "italic"),
                              legend.title = element_blank(),
  )

lines_ALL2

ggsave("e:/MoonWood/Obrazky/waves_hist_ALL_clim.jpeg", width = 15, heigh = 21, units = "cm", dpi = 1200)

