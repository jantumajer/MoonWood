library(ggplot2)
library(ggh4x)
library(lunar)

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

ALL$YEAR <- as.numeric(substr(ALL$Timestep, 1, 4))
ALL$DOY <- as.numeric(strftime(ALL$Timestep, format = "%j"))
ALL$HOUR <- as.numeric(substr(ALL$Timestep, 12, 13 ))
ALL$MOON <- lunar.phase(as.Date(ALL$Timestep), shift = ALL$Longitude/15 + (ALL$HOUR - 12)) # Moon phase in radians
ALL$MOONcat <- cut(ALL$MOON, breaks = seq(0,2*pi, by = (2*pi)/12), labels = c(1:12)) # Moon phase in 12 interval categories

###################
### Aggregation ###
###################

GRO.agg_DOY <- aggregate(ALL$GRO, by = list(SPE = ALL$Species, DOY = ALL$DOY), FUN = mean, na.rm = T)
TWD.agg_DOY <- aggregate(ALL$TWD, by = list(SPE = ALL$Species, DOY = ALL$DOY), FUN = mean, na.rm = T)
agg_DOY <- rbind(cbind(GRO.agg_DOY, VAR = "Growth rate [μm/h]"), cbind(TWD.agg_DOY, VAR = "Tree water deficit [μm]"))

GRO.agg_H <- aggregate(ALL$GRO, by = list(SPE = ALL$Species, HOUR = ALL$HOUR), FUN = mean, na.rm = T)
TWD.agg_H <- aggregate(ALL$TWD, by = list(SPE = ALL$Species, HOUR = ALL$HOUR), FUN = mean, na.rm = T)
agg_H <- rbind(cbind(GRO.agg_H, VAR = "Growth rate [μm/h]"), cbind(TWD.agg_H, VAR = "Tree water deficit [μm]"))

GRO.agg_LUN <- aggregate(ALL$GRO, by = list(SPE = ALL$Species, MOONcat = ALL$MOONcat), FUN = mean, na.rm = T)
TWD.agg_LUN <- aggregate(ALL$TWD, by = list(SPE = ALL$Species, MOONcat = ALL$MOONcat), FUN = mean, na.rm = T)
agg_LUN <- rbind(cbind(GRO.agg_H, VAR = "Growth rate [μm/h]"), cbind(TWD.agg_H, VAR = "Tree water deficit [μm]"))

###########################
### Standard deviations ###
###########################

mean(c(sd(GRO.agg_DOY[GRO.agg_DOY$SPE == "PCAB", "x"]), sd(GRO.agg_DOY[GRO.agg_DOY$SPE == "PISY", "x"]), sd(GRO.agg_DOY[GRO.agg_DOY$SPE == "A", "x"]), sd(GRO.agg_DOY[GRO.agg_DOY$SPE == "B", "x"]), sd(GRO.agg_DOY[GRO.agg_DOY$SPE == "C", "x"]), sd(GRO.agg_DOY[GRO.agg_DOY$SPE == "O", "x"])))
mean(c(sd(GRO.agg_H[GRO.agg_H$SPE == "PCAB", "x"]), sd(GRO.agg_H[GRO.agg_H$SPE == "PISY", "x"]), sd(GRO.agg_H[GRO.agg_H$SPE == "A", "x"]), sd(GRO.agg_H[GRO.agg_H$SPE == "B", "x"]), sd(GRO.agg_H[GRO.agg_H$SPE == "C", "x"]), sd(GRO.agg_H[GRO.agg_H$SPE == "O", "x"])))
mean(c(sd(GRO.agg_LUN[GRO.agg_LUN$SPE == "PCAB", "x"]), sd(GRO.agg_LUN[GRO.agg_LUN$SPE == "PISY", "x"]), sd(GRO.agg_LUN[GRO.agg_LUN$SPE == "A", "x"]), sd(GRO.agg_LUN[GRO.agg_LUN$SPE == "B", "x"]), sd(GRO.agg_LUN[GRO.agg_LUN$SPE == "C", "x"]), sd(GRO.agg_LUN[GRO.agg_LUN$SPE == "O", "x"])))

mean(c(sd(TWD.agg_DOY[TWD.agg_DOY$SPE == "PCAB", "x"]), sd(TWD.agg_DOY[TWD.agg_DOY$SPE == "PISY", "x"]), sd(TWD.agg_DOY[TWD.agg_DOY$SPE == "A", "x"]), sd(TWD.agg_DOY[TWD.agg_DOY$SPE == "B", "x"]), sd(TWD.agg_DOY[TWD.agg_DOY$SPE == "C", "x"]), sd(TWD.agg_DOY[TWD.agg_DOY$SPE == "O", "x"])))
mean(c(sd(TWD.agg_H[TWD.agg_H$SPE == "PCAB", "x"]), sd(TWD.agg_H[TWD.agg_H$SPE == "PISY", "x"]), sd(TWD.agg_H[TWD.agg_H$SPE == "A", "x"]), sd(TWD.agg_H[TWD.agg_H$SPE == "B", "x"]), sd(TWD.agg_H[TWD.agg_H$SPE == "C", "x"]), sd(TWD.agg_H[TWD.agg_H$SPE == "O", "x"])))
mean(c(sd(TWD.agg_LUN[TWD.agg_LUN$SPE == "PCAB", "x"]), sd(TWD.agg_LUN[TWD.agg_LUN$SPE == "PISY", "x"]), sd(TWD.agg_LUN[TWD.agg_LUN$SPE == "A", "x"]), sd(TWD.agg_LUN[TWD.agg_LUN$SPE == "B", "x"]), sd(TWD.agg_LUN[TWD.agg_LUN$SPE == "C", "x"]), sd(TWD.agg_LUN[TWD.agg_LUN$SPE == "O", "x"])))
