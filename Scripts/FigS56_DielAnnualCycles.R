library(ggplot2)
library(ggh4x)
library(lunar)

################################
### Loading GRO and TWD data ###
################################

ALL <- rbind(read.csv("E:/MoonWood/Manuscript/DataAvailability/Data/Acer.csv"),
             read.csv("E:/MoonWood/Manuscript/DataAvailability/Data/Fagus.csv"),
             read.csv("E:/MoonWood/Manuscript/DataAvailability/Data/Carpinus.csv"),
             read.csv("E:/MoonWood/Manuscript/DataAvailability/Data/Quercus.csv"),
             read.csv("E:/MoonWood/Manuscript/DataAvailability/Data/Pinus.csv"),
             read.csv("E:/MoonWood/Manuscript/DataAvailability/Data/Picea.csv"))

####################
###  Moon phases ###
####################

ALL$YEAR <- as.numeric(substr(ALL$Timestep, 1, 4))
ALL$DOY <- as.numeric(strftime(ALL$Timestep, format = "%j"))
ALL$HOUR <- as.numeric(substr(ALL$Timestep, 12, 13 ))

################
###  Seasons ###
################

ALL$SEASON <- "Winter"
ALL[as.numeric(substr(ALL$Timestep, 6,7)) %in% c(3:5), "SEASON"] <- "Spring"
ALL[as.numeric(substr(ALL$Timestep, 6,7)) %in% c(6:8), "SEASON"] <- "Summer"
ALL[as.numeric(substr(ALL$Timestep, 6,7)) %in% c(9:11), "SEASON"] <- "Autumn"


#################
### Aggregate ###
#################

GRO.agg_DOY <- aggregate(ALL$GRO, by = list(SPE = ALL$Species, DOY = ALL$DOY, YEAR = ALL$YEAR), FUN = mean, na.rm = T)
TWD.agg_DOY <- aggregate(ALL$TWD, by = list(SPE = ALL$Species, DOY = ALL$DOY, YEAR = ALL$YEAR), FUN = mean, na.rm = T)
TWD.agg_DOY[TWD.agg_DOY$SPE %in% c("PCAB", "PISY"), "x"] <- TWD.agg_DOY[TWD.agg_DOY$SPE %in% c("PCAB", "PISY"), "x"]/5 # Scaling  TWD for conifers
agg_DOY <- rbind(cbind(GRO.agg_DOY, VAR = "Growth rate [μm/h]"), cbind(TWD.agg_DOY, VAR = "Tree water deficit [μm]"))

GRO.agg_H <- aggregate(ALL$GRO, by = list(SPE = ALL$Species, HOUR = ALL$HOUR, SEASON = ALL$SEASON), FUN = mean, na.rm = T)
TWD.agg_H <- aggregate(ALL$TWD, by = list(SPE = ALL$Species, HOUR = ALL$HOUR, SEASON = ALL$SEASON), FUN = mean, na.rm = T)
TWD.agg_H[TWD.agg_H$SPE %in% c("PCAB", "PISY"), "x"] <- TWD.agg_H[TWD.agg_H$SPE %in% c("PCAB", "PISY"), "x"]/5 # Scaling  TWD for conifers
agg_H <- rbind(cbind(GRO.agg_H, VAR = "Growth rate [μm/h]"), cbind(TWD.agg_H, VAR = "Tree water deficit [μm]"))

chart <- ggplot(data = agg_DOY) +
  geom_line(aes(y=x, x=DOY, color = factor(SPE, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus"))), linetype = "solid",  linewidth = 0.35, group =1, alpha = 0.5) +
  geom_smooth(aes(y=x, x=DOY, color = factor(SPE, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus"))), linetype = "solid",  linewidth = 0.5, group =1, alpha = 1, se = F, method = "gam") +
  facet_nested(VAR + as.factor(YEAR) ~ factor(SPE, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus")),
            scales = "free_y") +
  facetted_pos_scales(y = list(VAR == "Growth rate [μm/h]" ~ scale_y_continuous(limits = c(0, 4.1)))) +
  theme_classic() +  
  theme(panel.spacing = unit(0.3, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 0),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 12, colour = "black"),
        axis.ticks.length=unit(.15, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"),
        strip.text.x = element_text(size = 14, colour = "black", face = "italic"),
        strip.text.y = element_text(size = 14, colour = "black"),
        plot.title = element_text(color = "black", hjust = 0.5, vjust = 0, face = "bold", size = 22))

  chart

ggsave("e:/MoonWood/Obrazky/series_anual.jpeg", width = 30, heigh = 30, units = "cm", dpi = 700)



chart2 <- ggplot(data = agg_H[agg_H$SEASON == "Summer" & agg_H$VAR == "Growth rate [μm/h]" |
                              agg_H$SEASON == "Summer" & agg_H$VAR == "Tree water deficit [μm]",]) +
  geom_line(aes(y=x, x=HOUR, color = factor(SPE, levels = c("A", "B", "C", "O", "PCAB", "PISY"), labels = c("Acer", "Fagus", "Carpinus", "Quercus", "Picea", "Pinus"))), linetype = "solid",  linewidth = 0.5, group =1, alpha = 1) +
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
        axis.text = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(angle = 0),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 14, colour = "black"),
        axis.ticks.length=unit(.15, "cm"),
        strip.text.x = element_text(size = 16, colour = "black", face = "italic"),
        strip.text.y = element_text(size = 16, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"),
        plot.title = element_text(color = "black", hjust = 0.5, vjust = 0, face = "bold", size = 22))

chart2

ggsave("e:/MoonWood/Obrazky/series_daily.jpeg", width = 40, heigh = 17, units = "cm", dpi = 700)
