### WARNING: Time consuming piece of code. We recommend to run it on strong CPU and, preferentially, over the weekend.

library(WaveletComp)

################################
### Loading GRO and TWD data ###
################################

ALL <- rbind(read.csv("Acer.csv"),
             read.csv("Fagus.csv"),
             read.csv("Carpinus.csv"),
             read.csv("Quercus.csv"),
             read.csv("Pinus.csv"),
             read.csv("Picea.csv"))

###################################
### Equalising freezing periods ###
###################################

### Run the next line of the code if you want to equalise TWD series during periods with temperature < 0°C
# ALL[is.na(ALL$Temperature) |  ALL$Temperature < 0, "TWD"] <- median(ALL$TWD)

############################################
### Running wavelet power transformation ###
############################################

### GRO ###

wv.gro.A <- analyze.wavelet(my.data = ALL[ALL$Species == "A",],
                               my.series = "GRO",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.gro.A, "A_GRO_60min.Rda")

wv.gro.B <- analyze.wavelet(my.data = ALL[ALL$Species == "B",],
                               my.series = "GRO",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.gro.B, "B_GRO_60min.Rda")

wv.gro.C <- analyze.wavelet(my.data = ALL[ALL$Species == "C",],
                               my.series = "GRO",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.gro.C, "C_GRO_60min.Rda")

wv.gro.O <- analyze.wavelet(my.data = ALL[ALL$Species == "O",],
                               my.series = "GRO",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.gro.O, "O_GRO_60min.Rda")

wv.gro.PISY <- analyze.wavelet(my.data = ALL[ALL$Species == "PISY",],
                               my.series = "GRO",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.gro.PISY, "PISY_GRO_60min.Rda")


wv.gro.PCAB <- analyze.wavelet(my.data = ALL[ALL$Species == "PCAB",],
                               my.series = "GRO",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.gro.PCAB, "PCAB_GRO_60min.Rda")

### TWD ###

wv.twd.A <- analyze.wavelet(my.data = ALL[ALL$Species == "A",],
                               my.series = "TWD",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.twd.A, "A_TWD_60min.Rda")

wv.twd.B <- analyze.wavelet(my.data = ALL[ALL$Species == "B",],
                               my.series = "TWD",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.twd.B, "B_TWD_60min.Rda")

wv.twd.C <- analyze.wavelet(my.data = ALL[ALL$Species == "C",],
                               my.series = "TWD",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.twd.C, "C_TWD_60min.Rda")

wv.twd.O <- analyze.wavelet(my.data = ALL[ALL$Species == "O",],
                               my.series = "TWD",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.twd.O, "O_TWD_60min.Rda")

wv.twd.PISY <- analyze.wavelet(my.data = ALL[ALL$Species == "PISY",],
                               my.series = "TWD",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.twd.PISY, "PISY_TWD_60min.Rda")

wv.twd.PCAB <- analyze.wavelet(my.data = ALL[ALL$Species == "PCAB",],
                               my.series = "TWD",
                               lowerPeriod = 2, upperPeriod = 24*365*2,
                               loess.span = 0)
saveRDS(wv.twd.PCAB, "PCAB_TWD_60min.Rda")

### Temperature ###

wv.temp.A <- analyze.wavelet(my.data = ALL[ALL$Species == "A",],
                             my.series = "Temperature",
                             lowerPeriod = 2, upperPeriod = 24*365*2,
                             loess.span = 0)
saveRDS(wv.temp.A, "A_t.Rda")


wv.temp.B <- analyze.wavelet(my.data = ALL[ALL$Species == "B",],
                             my.series = "Temperature",
                             lowerPeriod = 2, upperPeriod = 24*365*2,
                             loess.span = 0)
saveRDS(wv.temp.B, "B_t.Rda")


wv.temp.O <- analyze.wavelet(my.data = ALL[ALL$Species == "O",],
                             my.series = "Temperature",
                             lowerPeriod = 2, upperPeriod = 24*365*2,
                             loess.span = 0)
saveRDS(wv.temp.O, "O_t.Rda")

temp.c <- ALL[ALL$Species == "C",]
wv.temp.C <- analyze.wavelet(my.data = temp.c[c(1:38750),],
                             my.series = "Temperature",
                             lowerPeriod = 2, upperPeriod = 24*365*2,
                             loess.span = 0)
saveRDS(wv.temp.C, "C_t.Rda")

temp.PISY <- ALL[ALL$Species == "PISY",]
PISY.t <- analyze.wavelet(my.data = temp.PISY[c(1:32891),],
                          my.series = "Temperature",
                          lowerPeriod = 2, upperPeriod = 24*365*2,
                          dj = 1/20,
                          loess.span = 0)
saveRDS(PISY.t, "PISY_t.Rda")

temp.PCAB <- ALL[ALL$Species == "PCAB",]
PCAB.t <- analyze.wavelet(my.data = temp.PCAB[c(1:38450),],
                          my.series = "Temperature",
                          lowerPeriod = 2, upperPeriod = 24*365*2,
                          dj = 1/20,
                          loess.span = 0)
saveRDS(PCAB.t, "PCAB_t.Rda")

### VPD ###

a.vpd <- ALL[ALL$Species == "A",]
wv.vpd.A <- analyze.wavelet(my.data = a.vpd[c(14775:52608),],
                            my.series = "VPD",
                            lowerPeriod = 2, upperPeriod = 24*365*2,
                            loess.span = 0)
saveRDS(wv.vpd.A, "A_vpd.Rda")

b.vpd <- ALL[ALL$Species == "B",]
wv.vpd.B <- analyze.wavelet(my.data = b.vpd[c(14775:52608),],
                            my.series = "VPD",
                            lowerPeriod = 2, upperPeriod = 24*365*2,
                            loess.span = 0)
saveRDS(wv.vpd.B, "B_vpd.Rda")

o.vpd <- ALL[ALL$Species == "O",]
wv.vpd.O <- analyze.wavelet(my.data = o.vpd[c(14775:52608),],
                            my.series = "VPD",
                            lowerPeriod = 2, upperPeriod = 24*365*2,
                            loess.span = 0)
saveRDS(wv.vpd.O, "O_vpd.Rda")

vpd.PISY <- ALL[ALL$Species == "PISY",]
PISY.vpd <- analyze.wavelet(my.data = vpd.PISY[c(1:32891),],
                          my.series = "VPD",
                          lowerPeriod = 2, upperPeriod = 24*365*2,
                          dj = 1/20,
                          loess.span = 0)
saveRDS(PISY.vpd, "PISY_vpd.Rda")

vpd.PCAB <- ALL[ALL$Species == "PCAB",]
PCAB.vpd <- analyze.wavelet(my.data = vpd.PCAB[c(1:38450),],
                          my.series = "VPD",
                          lowerPeriod = 2, upperPeriod = 24*365*2,
                          dj = 1/20,
                          loess.span = 0)
saveRDS(PCAB.vpd, "PCAB_vpd.Rda")
