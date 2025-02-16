##############################################################################
###Calculate Growing Degree Days based on phenological information############
#############Requirement: Phenology and Weather data##########################
##############################################################################

rm(list=ls())
library(ncdf4)
library(dplyr)
library(stringr)

#for soybean
TB <<- 10.0  #Penning de Vries et al. 1989
TO <<- 27.0
TH <<- 34.0

indir = "./../dat/input"
PHENO <- read.csv(paste0(indir, "/PHENOALL.csv"), header = TRUE)
WEATHER <- read.csv(paste0(indir, "/WEATHERALL.csv"), header = TRUE)

#output
outdir = "./../prc/"
dir.create(outdir,recursive=TRUE,showWarnings = FALSE) 
DVSfile <- paste0(indir,"DVS")
dir.create(DVSfile,recursive=TRUE,showWarnings = FALSE)

#function to calculate the developmental rate for developmental stage
DVR <- function(x, TB, TO, TH) {
  if (x < TB) {
    return(0.0)
  } else if (x < TO) {
    return(x - TB)
  } else if (x < TH) {
    return((TO - TB) / (TH - TO) * (TH - x))
  } else {
    return(0)
  }
}
#function to find keyword and then rename the column
rename_column <- function(df, old_names, new_name) {
  index <- grep(paste(old_names, collapse = ".*"), names(df), ignore.case = TRUE)
  names(df)[index] <- new_name
  return(df)
}
#function to write output
write_format <- function(data, outdir, outname, format) {
  write.table(data, file = paste0(outdir, outname, format), quote = FALSE, sep = ",", col.names = TRUE, row.names = FALSE)
}
#function to find the expected weather file from WTH_ID 
get_weather_data <- function(exp, Year, cols) {
    SITE = substr(WTH_ID, 1, 3)
    WTHFILE = WEATHER
    if (file.exists(WTHFILE[1])) {
      weather = read.table(WTHFILE, header = TRUE, sep = ",")
      weather[, "CO2"] = NA
      weather$Date <- as.Date(weather$Date, format = "%Y-%m-%d", origin = "1899-12-30")
    } else {
      cat("No weather data found for experiment:", exp, "\n")
      return(NULL)
    }
  weather[, "WTH_ID"] <- WTH_ID
  weather[, "GDD"] = NA
  weather[, "TAVE"] = weather[, "TAVE"] - 273.15 # change to degree Celsius
  weather <- weather[, cols]
  return(weather)
}

cols = c("EXP","SITE","Year","VRT","LON","LAT","SOW","EMR","FLW","SEEDFILL","HVT","HVT_OBS","WTH_ID","CO2ave","IRR","SOIL","NFERT")
dt = PHENO
dt$HVT_OBS = dt$HVT
dt$CO2ave = NA
dt$WTH_ID = paste(dt$SITE, dt$Year, sep ="_")
PHENO = dt[, cols]

list_WTH=list()
list_pheno = list()

for(exp in unique(pheno$EXP)){
  print(exp)
  dt = subset(pheno,pheno$EXP == exp)
 
  Year = dt$Year
  CO2ave = round(dt$CO2ave)
  WTH_ID = dt$WTH_ID
  cols = c("WTH_ID","DOY","TAVE","TMAX","TMIN","Prec","Rad","SH","Wind","RH","Date","Year","CO2","GDD")

  weather = get_weather_data(exp, Year, cols)
  if (is.null(weather)) {
    next
  }

  maturity <- ifelse(is.na(dt$HVT),dt$HVT_OBS,dt$HVT)
  PLT <<- 0
  headGDD<<- 0
  emergenceGDD<<- 0
  seedGDD<<- 0
  aGDD <<- 0
  emergenceGDD_values <- NA
  headGDD_values <- NA
  seedGDD_values <- NA

sow_row = which(weather$DOY == dt$SOW & weather$Year == Year)
get_row <- function(stage) { #function for getting which row is the soybean stage happened
  if (!is.na(stage) && stage < dt$SOW) {
    return(which(weather$DOY == stage & weather$Year == Year + 1))
  } else if (!is.na(stage) && stage > dt$SOW) {
    return(which(weather$DOY == stage & weather$Year == Year))
  } else {
    return(NA)
  }
}
emergence_row = get_row(dt$EMR)
flowering_row = get_row(dt$FLW)
seedfill_row = get_row(dt$SEEDFILL)
harvest_row = get_row(dt$HVT)

#print(c("SOW", dt$SOW))
########################
#start calculating GDD#
k = 0 #check DOY calculated
for (j in 1:nrow(weather)) {
  if (!is.na(j) && !is.na(sow_row) && !is.na(harvest_row) && j >= sow_row && !is.na(weather$TAVE[j])) {
      k <- c(k, j)
      aGDD <- aGDD + DVR(weather$TAVE[j], TB, TO, TH)
      weather$GDD[j] = aGDD
      if (!is.na(emergence_row) && j <= emergence_row) {
        emergenceGDD <- emergenceGDD + DVR(weather$TAVE[j], TB, TO, TH)
        emergenceGDD_values <- c(emergenceGDD_values, emergenceGDD)
      }
      if (!is.na(flowering_row) && j <= flowering_row) {
        headGDD <- headGDD + DVR(weather$TAVE[j], TB, TO, TH)
        headGDD_values <- c(headGDD_values, headGDD)
      }
      if (!is.na(seedfill_row) && j <= seedfill_row) {
        seedGDD <- seedGDD + DVR(weather$TAVE[j], TB, TO, TH)
        seedGDD_values <- c(seedGDD_values, seedGDD)
      }
    } else {
    # Append NA to vectors in the else part
      emergenceGDD_values <- c(emergenceGDD_values, NA)
      headGDD_values <- c(headGDD_values, NA)
      seedGDD_values <- c(seedGDD_values, NA)
    }
  if (j == harvest_row){
    GDDmature = aGDD
  }
}

########################
#print(paste0("check harvest DOY: ", maturity, " at row ", harvest_row))
dt$EMR_DVS = ifelse(any(!is.na(emergenceGDD_values)), max(emergenceGDD_values, na.rm = TRUE) / GDDmature, NA)
dt$FLW_DVS = ifelse(any(!is.na(headGDD_values)), max(headGDD_values, na.rm = TRUE) / GDDmature, NA)
dt$SEED_DVS = ifelse(any(!is.na(seedGDD_values)), max(seedGDD_values, na.rm = TRUE) / GDDmature, NA)

dt$mGDD = GDDmature
weather$DVS = weather$GDD/GDDmature
weather$mGDD = GDDmature
weather$EXP = exp

##############################################################
  list_pheno[[exp]] = dt #phenological information including GDD and excluded one with no weather data
  list_WTH[[exp]] = weather #weather and DVS calculation
####finish calculating GDD and merge weather data with DVS####
##############################################################

  #store the DVS in a specified folder
  if(!is.na(sow_row) && !is.na(harvest_row)){
  DVS <- weather[sow_row:nrow(weather), c("EXP","DOY", "DVS", "TAVE", "TMAX", "TMIN","GDD", "mGDD")]
  DVS$TMAX = DVS$TMAX - 273.15
  DVS$TMIN = DVS$TMIN - 273.15
  write_format(DVS, DVSfile, paste0("/DVS_", exp), ".csv") 
  } 

  #################################################
  #### MAKING NC file for point-scale input data### 
  #################################################
  DOY <- 1
  xdim <- ncdim_def("lon","deg",dt$LON)
  ydim <- ncdim_def("lat","deg",dt$LAT)
  tdim <- ncdim_def("doy","day",DOY)
  
  PLTDOY <- array(NA,dim=c(1,1,length(dt$SOW)))
  PLTDOY[1,1,] <- dt$SOW
  PLT1_VNM <- "planting_day"
  ncvar <- ncvar_def(PLT1_VNM,"day",list(xdim,ydim,tdim),-999.0,prec="float")
  PLTnc <- paste0(outdir,"PLTnc")
  dir.create(PLTnc,recursive=TRUE,showWarnings = FALSE)
  ncout <- nc_create(paste(PLTnc,"/PLTDOY_",exp,"_.nc",sep=""),list(ncvar))
#  ncout <- nc_create(paste(PLTnc,"/PLTDOY_",exp,"_",Year,".nc",sep=""),list(ncvar))
  ncvar_put(ncout,ncvar,PLTDOY)  
  nc_close(ncout)

  
  SOILTXT <- array(NA,dim=c(1,1,length(dt$SOIL)))
  SOILTXT[1,1,] <- dt$SOIL
  SOIL_VNM <- "texture_class"
  ncvar <- ncvar_def(SOIL_VNM,"soil_type",list(xdim,ydim),-999.0,prec="float")
  SOILnc <- paste0(outdir,"SOILnc")
  dir.create(SOILnc,recursive=TRUE, showWarnings = FALSE)
  ncout <- nc_create(paste(SOILnc,"/SOIL_",exp,"_",Year,".nc",sep=""),list(ncvar))
  ncvar_put(ncout,ncvar,SOILTXT)
  nc_close(ncout)
  
  NFERTTXT <- array(NA,dim=c(1,1,length(dt$NFERT)))
  NFERTTXT[1,1,] <- dt$NFERT
  NFERT_VNM <- "fertl_c3nfx"
  ncvar <- ncvar_def(NFERT_VNM,"gNm-2s",list(xdim,ydim,tdim),-999.0,prec="float")
  NFERTnc <- paste0(outdir,"NFERTnc")
  dir.create(NFERTnc,recursive=TRUE, showWarnings = FALSE)
  ncout <- nc_create(paste(NFERTnc,"/NFERT_",exp,"_",Year,".nc",sep=""),list(ncvar))
  ncvar_put(ncout,ncvar,NFERTTXT)
  nc_close(ncout)

  if (!is.na(dt$HVT) && !is.na(dt$SOW) && dt$HVT < dt$SOW) {
  dir.create(NFERTnc,recursive=TRUE, showWarnings = FALSE)
  ncout <- nc_create(paste(NFERTnc,"/NFERT_",exp,"_",Year+1,".nc",sep=""),list(ncvar))
  ncvar_put(ncout,ncvar,NFERTTXT)
  nc_close(ncout)
  }  
  ###########################################################

  GDDm <- array(NA,dim=c(1,1,length(dt$mGDD)))
  GDDm[1,1,] <- dt$mGDD
  GDDm_VNM <- "var"
  ncvar <- ncvar_def(GDDm_VNM,"degreedays",list(xdim,ydim,tdim),-999.0,prec="float")
  GDDnc <- paste0(outdir,"GDDnc")
  dir.create(GDDnc,recursive=TRUE, showWarnings = FALSE)
  ncout <- nc_create(paste(GDDnc,"/GDD_",exp,"_.nc",sep=""),list(ncvar))
  ncvar_put(ncout,ncvar,GDDm)
  
  nc_close(ncout)
  
}

#to write and combine all data in weather and DVS information
WTHDVS <- do.call(rbind, list_WTH) 
write_format(WTHDVS, outdir, "Weather_DVS", ".csv") 

pheno_df <- do.call(rbind, list_pheno)
write_format(pheno_df, outdir, "Phenology", ".csv") 

calculate_mean_median <- function(data, variable) {
  mean_value <- mean(data[[variable]], na.rm = TRUE)
  median_value <- median(data[[variable]], na.rm = TRUE)
  result <- list(mean = mean_value, median = median_value)
  return(result)
}
#run the function to calculate mean and median
mGDD_stats <- calculate_mean_median(pheno_df, "mGDD")
floweringGDD_stats <- calculate_mean_median(pheno_df, "FLW_DVS")
seedGDD_stats <- calculate_mean_median(pheno_df, "SEED_DVS")
emrGDD_stats <- calculate_mean_median(pheno_df, "EMR_DVS")

# Access mean and median values
mGDD_mean <- mGDD_stats$mean
mGDD_median <- mGDD_stats$median

floweringGDD_mean <- floweringGDD_stats$mean
floweringGDD_median <- floweringGDD_stats$median

seedGDD_mean <- seedGDD_stats$mean
seedGDD_median <- seedGDD_stats$median

emrGDD_mean <- emrGDD_stats$mean
emrGDD_median <- emrGDD_stats$median

headGDD = (floweringGDD_mean + seedGDD_mean)/2
headGDD_median = (floweringGDD_median + seedGDD_median)/2

cat("mGDD mean, median:", mGDD_mean, mGDD_median, "\n")
cat("floweringGDD mean, median:", floweringGDD_mean, floweringGDD_median, "\n")
cat("seedGDD mean, median:", seedGDD_mean, seedGDD_median, "\n")
cat("headGDD mean (between flowering and seed):", headGDD, "\n")
cat("headGDD median (between flowering and seed):", headGDD_median, "\n")
cat("emrGDD mean, median:", emrGDD_mean, emrGDD_median, "\n")

write.table(headGDD,paste0(outdir,"headGDD.txt"),append=F,quote=F,row.names=F,col.names=F)
write.table(floweringGDD_median,paste0(outdir,"flowerGDD.txt"),append=F,quote=F,row.names=F,col.names=F)
write.table(seedGDD_median,paste0(outdir,"seedfillGDD.txt"),append=F,quote=F,row.names=F,col.names=F)
write.table(mGDD_median,paste0(outdir,"matureGDD.txt"),append=F,quote=F,row.names=F,col.names=F)

