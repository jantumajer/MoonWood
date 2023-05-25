library(ggplot2)

####################################################
### Load outputs of wavelet power transformation ###
####################################################
# To produce them run CalculateWavelets.R (time consuming!)

###
A_TWD <- readRDS("A_TWD_60min.Rda")
B_TWD <- readRDS("B_TWD_60min.Rda")
C_TWD <- readRDS("C_TWD_60min.Rda")
O_TWD <- readRDS("O_TWD_60min.Rda")
PISY_TWD <- readRDS("PISY_TWD_60min.Rda")
PCAB_TWD <- readRDS("PCAB_TWD_60min.Rda")
###
A_TWD_t <- readRDS("A_TWD_60min_TEMP.Rda")
B_TWD_t <- readRDS("B_TWD_60min_TEMP.Rda")
C_TWD_t <- readRDS("C_TWD_60min_TEMP.Rda")
O_TWD_t <- readRDS("O_TWD_60min_TEMP.Rda")
PISY_TWD_t <- readRDS("PISY_TWD_60min_TEMP.Rda")
PCAB_TWD_t <- readRDS("PCAB_TWD_60min_TEMP.Rda")

################
### Grouping ###
################

agg_all <- rbind(
  cbind(per = c(1:262), pow = A_TWD$Power.avg, spe = "A", var = "All timestamps"),
  cbind(per = c(1:262), pow = A_TWD_t$Power.avg, spe = "A", var = "Non-freezing timestamps"),
  cbind(per = c(1:262), pow = B_TWD$Power.avg, spe = "B", var = "All timestamps"),
  cbind(per = c(1:262), pow = B_TWD_t$Power.avg, spe = "B", var = "Non-freezing timestamps"),
  cbind(per = c(1:262), pow = C_TWD$Power.avg, spe = "C", var = "All timestamps"),
  cbind(per = c(1:262), pow = C_TWD_t$Power.avg, spe = "C", var = "Non-freezing timestamps"),
  cbind(per = c(1:262), pow = O_TWD$Power.avg, spe = "O", var = "All timestamps"),
  cbind(per = c(1:262), pow = O_TWD_t$Power.avg, spe = "O", var = "Non-freezing timestamps"),
  cbind(per = c(1:262), pow = PCAB_TWD$Power.avg, spe = "PCAB", var = "All timestamps"),
  cbind(per = c(1:262), pow = PCAB_TWD_t$Power.avg, spe = "PCAB", var = "Non-freezing timestamps"),
  cbind(per = c(1:262), pow = PISY_TWD$Power.avg, spe = "PISY", var = "All timestamps"),
  cbind(per = c(1:262), pow = PISY_TWD_t$Power.avg, spe = "PISY", var = "Non-freezing timestamps"))

##########################################################
### Converting  wavelet order into real-time frequency ###
##########################################################

val.day <- length(A_TWD$Period[A_TWD$Period < 24]) + (min(A_TWD$Period[A_TWD$Period > 24]) - 24)/(min(A_TWD$Period[A_TWD$Period > 24]) - max(A_TWD$Period[A_TWD$Period < 24]))
val.month <- length(A_TWD$Period[A_TWD$Period < 29.53*24]) + (min(A_TWD$Period[A_TWD$Period > 29.53*24]) - 29.53*24)/(min(A_TWD$Period[A_TWD$Period > 29.53*24]) - max(A_TWD$Period[A_TWD$Period < 29.53*24]))
val.halfyear<- length(A_TWD$Period[A_TWD$Period < 365*12]) + (min(A_TWD$Period[A_TWD$Period > 365*12]) - 365*12)/(min(A_TWD$Period[A_TWD$Period > 365*12]) - max(A_TWD$Period[A_TWD$Period < 365*12]))
val.year<- length(A_TWD$Period[A_TWD$Period < 365*24]) + (min(A_TWD$Period[A_TWD$Period > 365*24]) - 365*24)/(min(A_TWD$Period[A_TWD$Period > 365*24]) - max(A_TWD$Period[A_TWD$Period < 365*24]))
val.8h <- length(A_TWD$Period[A_TWD$Period < 8])+1
l <- length(A_TWD$Power.avg)

agg_all <- as.data.frame(agg_all)
agg_all$pow <- as.numeric(agg_all$pow)
agg_all$per <- as.numeric(agg_all$per)

lines_ALL <- ggplot(data = agg_all) + 
  geom_line(aes(x = per, y = pow, col = factor(spe, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus"))), alpha = 1) + 
  geom_vline(aes(xintercept = val.month), col = "grey", alpha = 0.75, linetype = "dashed") +
  scale_x_continuous(limits = c(val.8h, l),name = "Period of oscillation", breaks = c(val.8h, val.day, val.month, val.halfyear, val.year), labels = c("8 h", "1 d", "1 m", "1/2 y", "1 y")) +
  ylab("Wavelet power") +
  facet_grid(var ~ .) +
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

lines_ALL

ggsave("waves_hist_TEMP.jpeg", width = 12, heigh = 11, units = "cm", dpi = 1200)
