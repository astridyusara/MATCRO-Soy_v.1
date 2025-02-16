#### split value for CO2 in the txt file ####
dat <- read.table(paste("./../../../STATS/CO2/ISIMIP/dat/ori/co2_obsclim_annual_1850_2021.txt", sep = ""))


for (YEAR in 1850:2021) {
    OUTFILE <- paste("./../dat/CO2variable/", "CO2PPM_historical_", YEAR, ".txt", sep = "")
    ## varied value from obs data 1850
    write.table(dat[YEAR - 1849, 2], OUTFILE, append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE)
}
