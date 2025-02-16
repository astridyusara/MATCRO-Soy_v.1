############################################################
### Calculate and Parameterize Growing Degree Days (GDD) ###
############################################################

library(ncdf4)

STYR <- 1991
ENYR <- 2010

TB <- 10.0
TO <- 27.0
TH <- 34.0 # from Peninng de Veries

Cname <- "soy"

for (MNG in c("IRR", "RF")) {
    if (MNG == "IRR") {
        MMM <- "ir"
    } else {
        MMM <- "rf"
    }

    NC <- nc_open(paste("./../../input/dat/", Cname, "_", MMM, "_ggcmi_crop_calendar_phase3_v1.01.nc4", sep = ""))
    PLT <- ncvar_get(NC, "planting_day")
    HVT <- ncvar_get(NC, "maturity_day")
    lon <- ncvar_get(NC, "lon")
    lat <- ncvar_get(NC, "lat")
    nc_close(NC)


    PLT.FLG <- array(FALSE, dim = c(720, 360))
    GDDm <- array(0.0, dim = c(720, 360))
    nHVT <- array(0.0, dim = c(720, 360))


    for (YYYY in STYR:ENYR) {
        print(c(YYYY, MNG))

        NC <- nc_open(paste("./../../input/dat/climate/tas_ISIMIP_", YYYY, ".nc", sep = ""))
        TMP <- ncvar_get(NC, "tas")
        nc_close(NC)

        TMP <- TMP - 273.15

        for (I in 1:720) {
            for (J in 1:360) {
                if (!is.na(PLT[I, J])) {
                    for (DAY in 1:365) {
                        if (PLT.FLG[I, J] == FALSE & PLT[I, J] == DAY) {
                            PLT.FLG[I, J] <- TRUE
                        }

                        if (PLT.FLG[I, J] == TRUE) {
                            if (TMP[I, J, DAY] < TB) {
                                DVR <- 0
                            } else if (TMP[I, J, DAY] < TO) {
                                DVR <- TMP[I, J, DAY] - TB
                            } else if (TMP[I, J, DAY] < TH) {
                                DVR <- (TO - TB) / (TH - TO) * (TH - TMP[I, J, DAY])
                            } else {
                                DVR <- 0.0
                            }


                            GDDm[I, J] <- GDDm[I, J] + DVR

                            if (PLT.FLG[I, J] == TRUE & HVT[I, J] == DAY) {
                                PLT.FLG[I, J] <- FALSE
                                nHVT[I, J] <- nHVT[I, J] + 1
                            }
                        }
                    }
                }
            }
        }
    }


    GDDm2 <- ifelse(nHVT == 0, 0.0, GDDm / nHVT)


    xdim <- ncdim_def("lon", "deg", lon)
    ydim <- ncdim_def("lat", "deg", lat)

    ncvar <- ncvar_def("var", "degree day", list(xdim, ydim), NA, longname = "GDD for maturity", prec = "float")
    ncout <- nc_create(filename = paste("./../../input/dat/GDDm_", Cname, "_", MNG, "_ISIMIP_new.nc", sep = ""), list(ncvar))
    ncvar_put(ncout, ncvar, GDDm2, start = c(1, 1), count = c(720, 360))
    nc_close(ncout)
}
