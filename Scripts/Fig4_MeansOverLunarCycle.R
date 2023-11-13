library(lunar)
library(ggplot2)
library(ggh4x)

################################
### Loading GRO and TWD data ###
################################

ALL <- rbind(read.csv("Acer.csv"),
             read.csv("Fagus.csv"),
             read.csv("Carpinus.csv"),
             read.csv("Quercus.csv"),
             read.csv("Pinus.csv"),
             read.csv("Picea.csv"))
      
####################
###  Moon phases ###
####################

ALL[ALL$Species %in% c("A", "B", "C", "O"), "Longitude"] <- 13.383  # Greifswald
ALL[ALL$Species %in% c("PISY"), "Longitude"] <- 9.912 # Surava-Scuol
ALL[ALL$Species %in% c("PCAB"), "Longitude"] <- 9.848 # Davos-Schmitten

ALL$HOUR <- as.numeric(substr(ALL$Timestep, 12, 13 ))
ALL$MOON <- lunar.phase(as.Date(ALL$Timestep), shift = ALL$Longitude/15 + (ALL$HOUR - 12)) # Moon phase in radians
ALL$MOONcat <- cut(ALL$MOON, breaks = seq(0,2*pi, by = (2*pi)/12), labels = c(1:12)) # Moon phase in 12 interval categories

ALL.2 <- ALL[!(is.na(ALL$Temp)),]
TEMP <- ALL.2[ALL.2$Temp > 0,] # Subsampling of timesteps with air temperature above 0 °C

# Scaling of TWD for conifers
ALL.2[ALL.2$Species %in% c("PCAB", "PISY"), "TWD"] <- ALL.2[ALL.2$Species %in% c("PCAB", "PISY"), "TWD"] * 0.2
TEMP[TEMP$Species %in% c("PCAB", "PISY"), "TWD"] <- TEMP[TEMP$Species %in% c("PCAB", "PISY"), "TWD"] * 0.2

#################
### Aggregate ###
#################

GRO.agg_ALL <- cbind(aggregate(ALL.2$GRO, by = list(SPE = ALL.2$Species, MOONcat = ALL.2$MOONcat), FUN = mean, na.rm = T),
                 aggregate(ALL.2$GRO, by = list(SPE = ALL.2$Species, MOONcat = ALL.2$MOONcat), FUN = sd)[,3],
                 aggregate(ALL.2$GRO, by = list(SPE = ALL.2$Species, MOONcat = ALL.2$MOONcat), FUN = NROW)[,3])

TWD.agg_ALL <- cbind(aggregate(ALL.2$TWD, by = list(SPE = ALL.2$Species, MOONcat = ALL.2$MOONcat), FUN = mean, na.rm = T),
                     aggregate(ALL.2$TWD, by = list(SPE = ALL.2$Species, MOONcat = ALL.2$MOONcat), FUN = sd)[,3],
                     aggregate(ALL.2$TWD, by = list(SPE = ALL.2$Species, MOONcat = ALL.2$MOONcat), FUN = NROW)[,3])

GRO.agg_TEMP <- cbind(aggregate(TEMP$GRO, by = list(SPE = TEMP$Species, MOONcat = TEMP$MOONcat), FUN = mean, na.rm = T),
                     aggregate(TEMP$GRO, by = list(SPE = TEMP$Species, MOONcat = TEMP$MOONcat), FUN = sd)[,3],
                     aggregate(TEMP$GRO, by = list(SPE = TEMP$Species, MOONcat = TEMP$MOONcat), FUN = NROW)[,3])

TWD.agg_TEMP <- cbind(aggregate(TEMP$TWD, by = list(SPE = TEMP$Species, MOONcat = TEMP$MOONcat), FUN = mean, na.rm = T),
                     aggregate(TEMP$TWD, by = list(SPE = TEMP$Species, MOONcat = TEMP$MOONcat), FUN = sd)[,3],
                     aggregate(TEMP$TWD, by = list(SPE = TEMP$Species, MOONcat = TEMP$MOONcat), FUN = NROW)[,3])
colnames(GRO.agg_ALL) <- colnames(TWD.agg_ALL) <- colnames(GRO.agg_TEMP) <- colnames(TWD.agg_TEMP) <- c("SPE", "MOONcat", "MEANm", "SDm", "N")

#############################
### Charts and statistics ###
#############################

agg_ALL <- rbind(cbind(GRO.agg_ALL, VAR = "Growth rate [μm/h]"), cbind(TWD.agg_ALL, VAR = "Tree water deficit [μm]")) 
agg_TEMP <- rbind(cbind(GRO.agg_TEMP, VAR = "Growth rate [μm/h]"), cbind(TWD.agg_TEMP, VAR = "Tree water deficit [μm]")) 

# 95% confidence intervals
agg_ALL$CI <- qt(0.975, agg_ALL$N)*agg_ALL$SDm/sqrt(agg_ALL$N)
agg_TEMP$CI <- qt(0.975, agg_TEMP$N)*agg_TEMP$SDm/sqrt(agg_TEMP$N)

chart.classic <- ggplot() +
  geom_ribbon(aes(x=MOONcat, ymin=MEANm-CI, ymax=MEANm+CI, fill = SPE), group =1, alpha = 0.20, data = agg_ALL[agg_TEMP$VAR == "Growth rate [μm/h]",]) +
  geom_ribbon(aes(x=MOONcat, ymin=MEANm-CI, ymax=MEANm+CI, fill = SPE), group =1, alpha = 0.20, data = agg_TEMP[agg_TEMP$VAR == "Tree water deficit [μm]",]) +
  
  geom_vline(xintercept = 9, linetype = "dashed", col = "blue", alpha = 0.5)+
  geom_line(aes(y=MEANm, x=MOONcat, color = SPE), linetype = "solid",  linewidth = 0.50, group =1, alpha = 1,  data = agg_ALL[agg_TEMP$VAR == "Growth rate [μm/h]",]) +
  geom_line(aes(y=MEANm, x=MOONcat, color = SPE), linetype = "dashed",  linewidth = 0.25, group =1, alpha = 1,  data = agg_ALL[agg_TEMP$VAR == "Tree water deficit [μm]",]) +
  geom_line(aes(y=MEANm, x=MOONcat, color = SPE), linetype = "solid",  linewidth = 0.50, group =1, alpha = 1,  data = agg_TEMP[agg_TEMP$VAR == "Tree water deficit [μm]",]) +
  
  scale_x_discrete(name = xlab("\nInterval of the lunar cycle"), breaks = c(0, 3, 6, 9, 12), labels = c("NM", "1Q", "FM", "3Q", "NM" ))+
  
  ylab("") +
  facet_grid(VAR ~ factor(SPE, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus")),
             scales = "free_y") +
  theme_classic() +  
  theme(panel.spacing = unit(0.3, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 14, colour = "black"),
        axis.ticks.length=unit(.15, "cm"),
        strip.text.x = element_text(size = 12, colour = "black", face = "italic"),
        strip.text.y = element_text(size = 12, colour = "black"),
        plot.title = element_text(color = "black", hjust = 0.5, vjust = 0, face = "bold", size = 22))

chart.classic

ggsave("e:/MoonWood/Obrazky/ALL_withoutSeasons.jpeg", width = 23, heigh = 13, units = "cm", dpi = 1000)

##################################
### Polar chart - experimental ###
##################################

agg_ALL$MOONcat <- as.numeric(agg_ALL$MOONcat)

agg_ALL_112 <- agg_ALL[agg_ALL$MOONcat %in% c(1, 12),]
agg_ALL_112_agg <- aggregate(agg_ALL_112, by = list(SPE = agg_ALL_112$SPE, VAR = agg_ALL_112$VAR), FUN = mean)
agg_ALL <- rbind(cbind(SPE = agg_ALL_112_agg[,c(1)], MOONcat = 0.5, agg_ALL_112_agg[,c(5:7)], VAR = agg_ALL_112_agg[,2], CI = agg_ALL_112_agg[,9]),
                 agg_ALL,
                 cbind(SPE = agg_ALL_112_agg[,c(1)], MOONcat = 12.5, agg_ALL_112_agg[,c(5:7)], VAR = agg_ALL_112_agg[,2], CI = agg_ALL_112_agg[,9]))


agg_TEMP_112 <- agg_TEMP[agg_TEMP$MOONcat %in% c(1, 12),]
agg_TEMP_112_agg <- aggregate(agg_TEMP_112, by = list(SPE = agg_TEMP_112$SPE, VAR = agg_TEMP_112$VAR), FUN = mean)
agg_TEMP <- rbind(cbind(SPE = agg_TEMP_112_agg[,c(1)], MOONcat = 0.5, agg_TEMP_112_agg[,c(5:7)], VAR = agg_TEMP_112_agg[,2], CI = agg_TEMP_112_agg[,9]),
                 agg_TEMP,
                 cbind(SPE = agg_TEMP_112_agg[,c(1)], MOONcat = 12.5, agg_TEMP_112_agg[,c(5:7)], VAR = agg_TEMP_112_agg[,2], CI = agg_TEMP_112_agg[,9]))

chart.polar <- ggplot() +
  geom_vline(xintercept = c(9.5, 12.5), linetype = "dashed", col = "blue1", alpha = 0.8, linewidth = 0.4)+
  geom_vline(xintercept = c(0.5:12.5), linetype = "solid", col = "grey20", alpha = 0.45, linewidth = 0.1)+
  
 # geom_ribbon(aes(x=as.numeric(MOONcat), ymin=MEANm-CI, ymax=MEANm+CI, fill = SPE), group =1, alpha = 0.20, data = agg_ALL[agg_TEMP$VAR == "Growth rate [μm/h]",]) +
 # geom_ribbon(aes(x=as.numeric(MOONcat), ymin=MEANm-CI, ymax=MEANm+CI, fill = SPE), group =1, alpha = 0.20, data = agg_TEMP[agg_TEMP$VAR == "Tree water deficit [μm]",]) +
  
  geom_line(aes(y=MEANm, x=as.numeric(MOONcat), color = SPE), linetype = "solid",  linewidth = 0.50, group =1, alpha = 1,  data = agg_ALL[agg_ALL$VAR == "Growth rate [μm/h]",]) +
  geom_line(aes(y=MEANm, x=as.numeric(MOONcat), color = SPE), linetype = "dashed",  linewidth = 0.25, group =1, alpha = 1,  data = agg_ALL[agg_ALL$VAR == "Tree water deficit [μm]",]) +
  geom_line(aes(y=MEANm, x=as.numeric(MOONcat), color = SPE), linetype = "solid",  linewidth = 0.50, group =1, alpha = 1,  data = agg_TEMP[agg_TEMP$VAR == "Tree water deficit [μm]",]) +

  scale_x_continuous(name = xlab(""), limits = c(0.5,12.5), breaks = c(0.5, 3.5, 6.5, 9.5), labels = c("NM", "1Q", "FM", "3Q"))+
  
  ### TWD ###
  # scale_y_continuous(name = ylab(""), limits = c(0,70), breaks = c(0, 30, 60)) +
  # geom_hline(yintercept = c(30,60), linetype = "solid", col = "grey20", alpha = 0.45, linewidth = 0.1)+
  
  ### GRO ###
   scale_y_continuous(name = ylab(""), limits = c(0,.50), breaks = c(0, .2, .4)) +
   geom_hline(yintercept = c(.2,.4), linetype = "solid", col = "grey20", alpha = 0.45, linewidth = 0.1)+
  
  facet_nested_wrap(VAR ~ factor(SPE, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus")), ncol = 3) +
  theme_classic() + 
  coord_polar() + 
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # panel.border = element_rect(fill = NA, colour = "grey20"),
        legend.position = "none",
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.length=unit(.15, "cm"),
        strip.text.x = element_text(size = 15, colour = "black", face = "italic"),
        strip.text.y = element_text(size = 15, colour = "black"),
        plot.title = element_blank())

  chart.polar
 
ggsave("e:/MoonWood/Obrazky/ALL_withoutSeasons_GRO.jpeg", width = 19, heigh = 30, units = "cm", dpi = 800)
