#this code is to calculate the Growing Degree Days (GDD) based on mean temperature
library(ncdf4)

start_year <- 1991
end_year <- 2010
# Constants Penning de Vries et al

TB <- 10.0 #base temperature [Celcius]
TO <- 27.0 #optimum temperature [Celcius]
TH <- 34.0 #high temperature [Celcius]
Cname <- "soy" #crop name

for (management in c("IRR", "RF")) {
 management = "RF"
 mng <- ifelse(management == "IRR", "ir", "rf")
  
    PLT.FLG <- array(FALSE, dim = c(720, 360))
    NC <- nc_open(paste("/home/yusara/GEO/GLOBAL/AGR/GGCMIPhase3/", Cname, "_", mng, "_ggcmi_crop_calendar_phase3_v1.01.nc4", sep = ""))
    PLT <- ncvar_get(NC, "planting_day")
    HVT <- ncvar_get(NC, "maturity_day")
    lon <- ncvar_get(NC, "lon")
    lat <- ncvar_get(NC, "lat")
    nc_close(NC)

  for (year in start_year:end_year) {
    print(c(year, management))
    
    NC <- nc_open(paste("/home/marin/GEO/GLOBAL/CLM/ISIMIP/3a/dat/prc/daily/tas/tas_ISIMIP_", year, ".nc", sep = ""))
    TMP <- ncvar_get(NC, "tas") - 273.15
    nc_close(NC)
    
        for (I in 1:720) {
        for (J in 1:360) {
            if (!is.na(PLT[I, J])) {
            for (DAY in 1:365) {
                if (!PLT.FLG[I, J] && PLT[I, J] == DAY) {
                PLT.FLG[I, J] <- TRUE
                }
                if (PLT.FLG[I, J]) {
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
                if (PLT.FLG[I, J] && HVT[I, J] == DAY) {
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
    ncout <- nc_create(filename = paste("/home/yusara/MATCRO_202307_daily/dat/input/GDDm/GDDm_", Cname, "_", management, "_ISIMIP_new1.nc", sep = ""), list(ncvar))
    ncvar_put(ncout, ncvar, GDDm, start = c(1, 1), count = c(720, 360))
    nc_close(ncout)
  }

