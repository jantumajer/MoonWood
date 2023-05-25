library(ggplot2)
library(reshape2)

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

################################################
### Cone of influence converted into polygon ###
################################################
CO <- data.frame(X = A_GRO$coi.1, Y = A_GRO$coi.2)
for (i in c(1:nrow(CO))){
  CO[i, "Yval"] <- sum(A_GRO$axis.2 < A_GRO$axis.2[abs(CO[i, "Y"] - A_GRO$axis.2) == min(abs(CO[i, "Y"] - A_GRO$axis.2))]) + 1
}

CO.ALL <- rbind(cbind(CO, SPE = "A", VAR = "GRO"), cbind(CO, SPE = "A", VAR = "TWD"),
                cbind(CO, SPE = "B", VAR = "GRO"), cbind(CO, SPE = "B", VAR = "TWD"),
                cbind(CO, SPE = "C", VAR = "GRO"), cbind(CO, SPE = "C", VAR = "TWD"),
                cbind(CO, SPE = "O", VAR = "GRO"), cbind(CO, SPE = "O", VAR = "TWD"),
                cbind(CO, SPE = "PCAB", VAR = "GRO"), cbind(CO, SPE = "PCAB", VAR = "TWD"),
                cbind(CO, SPE = "PISY", VAR = "GRO"), cbind(CO, SPE = "PISY", VAR = "TWD"))

########################################################################
### Grouping of wavelet powers and standardizing them into 0-1 scale ###
########################################################################

waves_all <-
rbind(cbind(melt(A_GRO$Power), SIG = melt(A_GRO$Power.pval)[,3], RANK = rank(melt(A_GRO$Power)[,3])/(nrow(A_GRO$Power)*ncol(A_GRO$Power)), SPE = "A", VAR = "GRO"),
      cbind(melt(B_GRO$Power), SIG = melt(B_GRO$Power.pval)[,3], RANK = rank(melt(B_GRO$Power)[,3])/(nrow(B_GRO$Power)*ncol(B_GRO$Power)),SPE = "B", VAR = "GRO"),
      cbind(melt(C_GRO$Power), SIG = melt(C_GRO$Power.pval)[,3], RANK = rank(melt(C_GRO$Power)[,3])/(nrow(C_GRO$Power)*ncol(C_GRO$Power)),SPE = "C", VAR = "GRO"),
      cbind(melt(O_GRO$Power), SIG = melt(O_GRO$Power.pval)[,3], RANK = rank(melt(O_GRO$Power)[,3])/(nrow(O_GRO$Power)*ncol(O_GRO$Power)),SPE = "O", VAR = "GRO"),
      cbind(melt(PCAB_GRO$Power), SIG = melt(PCAB_GRO$Power.pval)[,3], RANK = rank(melt(PCAB_GRO$Power)[,3])/(nrow(PCAB_GRO$Power)*ncol(PCAB_GRO$Power)),SPE = "PCAB", VAR = "GRO"),
      cbind(melt(PISY_GRO$Power), SIG = melt(PISY_GRO$Power.pval)[,3], RANK = rank(melt(PISY_GRO$Power)[,3])/(nrow(PISY_GRO$Power)*ncol(PISY_GRO$Power)),SPE = "PISY", VAR = "GRO"),
      cbind(melt(A_TWD$Power), SIG = melt(A_TWD$Power.pval)[,3], RANK = rank(melt(A_TWD$Power)[,3])/(nrow(A_TWD$Power)*ncol(A_TWD$Power)),SPE = "A", VAR = "TWD"),
      cbind(melt(B_TWD$Power), SIG = melt(B_TWD$Power.pval)[,3], RANK = rank(melt(B_TWD$Power)[,3])/(nrow(B_TWD$Power)*ncol(B_TWD$Power)),SPE = "B", VAR = "TWD"),
      cbind(melt(C_TWD$Power), SIG = melt(C_TWD$Power.pval)[,3], RANK = rank(melt(C_TWD$Power)[,3])/(nrow(C_TWD$Power)*ncol(C_TWD$Power)),SPE = "C", VAR = "TWD"),
      cbind(melt(O_TWD$Power), SIG = melt(O_TWD$Power.pval)[,3], RANK = rank(melt(O_TWD$Power)[,3])/(nrow(O_TWD$Power)*ncol(O_TWD$Power)),SPE = "O", VAR = "TWD"),
      cbind(melt(PCAB_TWD$Power), SIG = melt(PCAB_TWD$Power.pval)[,3], RANK = rank(melt(PCAB_TWD$Power)[,3])/(nrow(PCAB_TWD$Power)*ncol(PCAB_TWD$Power)),SPE = "PCAB", VAR = "TWD"),
      cbind(melt(PISY_TWD$Power), SIG = melt(PISY_TWD$Power.pval)[,3], RANK = rank(melt(PISY_TWD$Power)[,3])/(nrow(PISY_TWD$Power)*ncol(PISY_TWD$Power)),SPE = "PISY", VAR = "TWD"))
colnames(waves_all) <- c("PER", "TIME", "POW", "PVALUE", "RANK", "SPE", "VAR")

waves_all$PVALUE2 <- 0.15 
waves_all[waves_all$PVALUE < 0.05, "PVALUE2"] <- 0.3
waves_all[waves_all$PVALUE < 0.01, "PVALUE2"] <- 1

##########################################################
### Converting  wavelet order into real-time frequency ###
##########################################################

val.day <- length(A_GRO$Period[A_GRO$Period < 24]) + (min(A_GRO$Period[A_GRO$Period > 24]) - 24)/(min(A_GRO$Period[A_GRO$Period > 24]) - max(A_GRO$Period[A_GRO$Period < 24]))
val.month <- length(A_GRO$Period[A_GRO$Period < 29.53*24]) + (min(A_GRO$Period[A_GRO$Period > 29.53*24]) - 29.53*24)/(min(A_GRO$Period[A_GRO$Period > 29.53*24]) - max(A_GRO$Period[A_GRO$Period < 29.53*24]))
val.halfyear<- length(A_GRO$Period[A_GRO$Period < 365*12]) + (min(A_GRO$Period[A_GRO$Period > 365*12]) - 365*12)/(min(A_GRO$Period[A_GRO$Period > 365*12]) - max(A_GRO$Period[A_GRO$Period < 365*12]))
val.year<- length(A_GRO$Period[A_GRO$Period < 365*24]) + (min(A_GRO$Period[A_GRO$Period > 365*24]) - 365*24)/(min(A_GRO$Period[A_GRO$Period > 365*24]) - max(A_GRO$Period[A_GRO$Period < 365*24]))
val.8h <- length(A_GRO$Period[A_GRO$Period < 8])+1
l <- length(A_GRO$Power.avg)

rm(A_GRO, B_GRO, C_GRO, O_GRO, PCAB_GRO, PISY_GRO, A_TWD, B_TWD, C_TWD, O_TWD, PCAB_TWD, PISY_TWD, CO)
gc()

CO.ALL[CO.ALL$Yval < val.8h, "Yval"] <- val.8h

mat <- ggplot() + 
  geom_raster(aes(x = TIME, y = PER, fill = RANK, alpha = PVALUE2), data = waves_all) + 
  geom_polygon(aes(x = X, y = Yval), fill = "white", data = CO.ALL) + 
  scale_x_continuous(name = " ", breaks = c(1, 1+1*24*365, 1+2*24*365, 1+3*24*365, 1+4*24*365, 1+5*24*365),
                                    labels = c("01/01/2015", "01/01/2016", "01/01/2017", "01/01/2018", "01/01/2019", "01/01/2020")) +
  scale_y_continuous(limits = c(val.8h, l), name = "Period of oscillation", breaks = c(val.8h, val.day, val.month, val.year), labels = c("8 h", "1 d", "1 m", "1 y")) +
  scale_fill_gradientn(values = c(0, 0.2, 0.4, 0.6, 0.8, 1), colours = c("blue", "green", "yellow" ,"orange", "red")) +
  scale_alpha_continuous(guide = "none") +
  facet_grid(factor(SPE, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus")) ~ factor(VAR, labels = c("Growth rate", "Tree water deficit"))) +
  theme_classic() +     theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              panel.border = element_rect(fill = NA, colour = "grey20"),
                              axis.line = element_line(colour = "black"),
                              axis.text.y = element_text(size = 11, colour = "black"),
                              axis.text.x = element_text(size = 11, colour = "black", angle = 45, hjust = 0.9, vjust = 0.9),
                              axis.title = element_text(size = 12, colour = "black"),
                              axis.ticks.length=unit(.2, "cm"),
                              strip.text.y = element_text(size = 13, colour = "black", face = "italic"),
                              strip.text.x = element_text(size = 13, colour = "black"),
                              plot.title = element_text(color = "black", hjust = 0.5, vjust = 0, face = "bold", size = 18),
                              #legend.position = "none"
                              legend.title = element_blank(),
                              )

# mat

ggsave(plot = mat, filename = "waves_BOTH.jpeg", width = 26, heigh = 28, units = "cm", dpi = 500)
 
