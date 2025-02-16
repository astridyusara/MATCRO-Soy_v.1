## MAKING observed biomass data combined with DVS calculation##
rm(list = ls())
library(stringr) # substring
library(dplyr)

workdir <- "./../dat/input"
outdir <- "./../prc"
dir.create(outdir, recursive = TRUE)

biomass <- read.table(paste0(workdir, "/BIOMASSALL.csv"), header = TRUE, sep = ",")
pheno <- read.table(paste0(workdir, "/PHENOALL.csv"), header = TRUE, sep = ",")
DVS_dir <- paste0(outdir, "/DVS/")

## fill missing leaf data with interpolation based on DVS##
fill_leaf <- function(dt, leaf_column) {
  if (sum(!is.na(dt[[leaf_column]])) >= 2) {
    if (length(unique(na.omit(dt$DVS))) >= 2) {
      approx_result <- approx(dt$DVS, dt[[leaf_column]], xout = dt$DVS)
      if (sum(!is.na(approx_result$y)) >= 2) {
        return(approx_result$y)
      } else {
        return(rep(NA, length(dt$DVS)))
      }
    } else {
      return(rep(NA, length(dt$DVS)))
    }
  } else {
    return(rep(NA, length(dt$DVS)))
  }
}


biomass <- biomass1[, c("EXP", "SITE", "Year", "DOY", "AGB", "LAI", "Yield", "STM", "LEF", "POD", "ROT")]

pheno[, "MTR"] <- ifelse(is.na(pheno$HVT), pheno$HVT_OBS, pheno$HVT)
pheno_stage <- pheno[c("EXP", "VRT", "CO2ave", "SOW", "EMR", "FLW", "SEEDFILL", "MTR", "IRR")]


# start filling leaf data which is not in the same DOY with other organ##
list_BIODVS <- list()
for (exp in unique(pheno$EXP)) { # this code includes duplicated exp because of summarization
  print(exp)

  # merge BIOMASS data with DVS and pheno#
  DVS_DF <- read.table(paste0(DVS_dir, "DVS_", exp, ".csv"), header = TRUE, sep = ",")
  pheno_stage_set <- subset(pheno_stage, pheno_stage$EXP == exp)
  biomass_set <- subset(biomass, biomass$EXP == exp)

  mergeBIO_set <- merge(biomass_set, pheno_stage_set, by = "EXP")
  mergeBIO_set <- merge(mergeBIO_set, DVS_DF, by = c("EXP", "DOY"))

  dt <- mergeBIO_set

  # remove row with duplicated DVS and insufficient data#
  # dt <- dt[!duplicated(dt$DVS), ]
  if (all(is.na(dt$LEF)) | sum(!is.na(dt$LEF)) < 2) {
    cat("No or insufficient leaf measurement data:", exp, "\n")
    next
  }

  # check if it has enough data for measurement, same code as above
  if (sum(!is.na(dt$LEF)) < 2 || sum(!is.na(dt$LAI)) < 2) {
    # Skip to the next experiment
    cat("not enough data:", exp, "\n")
    next
  }
  dt <- as.data.frame(dt)
  dt <- dt %>% arrange(DVS)

  # data 2005 in US1, the LAI in the excel is strange, the value ranged around 33 - 164 so we removed it
  if (dt$SITE == "US1" && dt$Year == "2005") {
    dt$LAIfill <- NA # changed the value into NA, not considered
  }

  ## use function to fill the leaf and LAI data in the same day with other DOY of other organ measurement
  dt$LEFfill <- fill_leaf(dt, "LEF")
  dt$LAIfill <- fill_leaf(dt, "LAI")
  dt$ROTfill <- fill_leaf(dt, "ROT")
  #############
  SLW <- ifelse(dt$LAIfill > 0, dt$LEFfill / dt$LAIfill, NA) # kg/ha
  dt$SLW <- ifelse(dt$DVS <= 1, SLW, NA)

  ## remove dead leaves data and NA rows in stem and change it with LEFfill, LAIfill##
  dt$LEF <- dt$LEFfill
  dt$LAI <- dt$LAIfill
  dt$ROT <- dt$ROTfill

  for (j in 2:nrow(dt)) {
    if (nrow(dt) <= 1) {
      next # Skip to the next experiment
    }
    prevLEF <- dt$LEFfill[j - 1]
    # currentLEF <- dt$LEFfill[j]
    if (!is.na(prevLEF) && !is.na(dt$LEFfill[j]) && dt$LEFfill[j] <= prevLEF || is.na(dt$SLW[j - 1])) {
      dt$SLW[j] <- NA
    }
  }

  list_BIODVS[[exp]] <- dt
}
list_BIODVS <- list_BIODVS[!duplicated(names(list_BIODVS))]
BIO_DVS <- do.call(rbind, list_BIODVS)

BIO_DVS <- BIO_DVS %>% # arrange the data
  arrange(EXP, DVS, DOY, STM)
write.table(BIO_DVS, file = paste0(outdir, "Biomass_leaffill_DVS.csv"), quote = FALSE, sep = ",", col.names = TRUE, row.names = FALSE)
