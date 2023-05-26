# Scripts to produce key outputs of the analysis

First run the function `CalculateWavelets.R` which will convert individual dendrometer series into wavelet power spectra and store them in `.Rds` format. Be aware that this piece of code is time consuming. All subsequent functions load these files and further process them.
